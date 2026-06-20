# The patched zellij + the zellij-spiral plugin wasm, as plain derivations, so
# `./home-manager build` produces both with no flakes and no build-on-launch.
# `zellij` (the fork) goes on PATH — it adds the pane-slot-binding API the spiral
# needs, which stock zellij lacks. The wasm store path is what the config loads.
#
# Built against the spiral's own pinned nixpkgs + rust-overlay, not the repo's
# main nixpkgs: that rev's zellij-unwrapped + fetchCargoVendor are what the fork's
# vendor hashes were computed against, so reusing it keeps the hashes valid.
final:
let
  sources = final.bddap.sources;
  pkgs = import sources.zellij-nixpkgs {
    inherit (final.stdenv.hostPlatform) system;
    overlays = [ (import sources.rust-overlay) ];
  };

  # zellij plugins are WASI modules; the stock pinned rustc ships only
  # wasm32-unknown-unknown, so pull a toolchain carrying the wasm32-wasip1 std.
  rustWasm = pkgs.rust-bin.stable.latest.default.override {
    targets = [ "wasm32-wasip1" ];
  };

  # The unpacked-tree hash of the fork checkout. fetchFromGitHub takes it directly;
  # importCargoLock needs the same hash for every git crate it resolves out of that
  # one checkout (zellij-tile and its zellij-utils dep).
  forkHash = "sha256-1JHkuxNYDv/pshs8ErF98b3M5nuAB750CXs+B4mNxJE=";
  forkSrc = pkgs.fetchFromGitHub {
    owner = "bddap-bot";
    repo = "zellij";
    rev = sources.zellij-fork.rev;
    hash = forkHash;
  };

  zellij-spiral = pkgs.stdenv.mkDerivation {
    pname = "zellij-spiral";
    version = "0.3.0";
    src = sources.zellij-spiral;

    cargoDeps = pkgs.rustPlatform.importCargoLock {
      lockFile = "${sources.zellij-spiral}/Cargo.lock";
      outputHashes = {
        "zellij-tile-0.45.0" = forkHash;
        "zellij-utils-0.45.0" = forkHash;
      };
    };
    nativeBuildInputs = [ rustWasm pkgs.rustPlatform.cargoSetupHook ];

    buildPhase = ''
      runHook preBuild
      cargo build --release --target wasm32-wasip1 --offline
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      cp target/wasm32-wasip1/release/zellij-spiral.wasm "$out/zellij-spiral.wasm"
      runHook postInstall
    '';

    doCheck = false;
  };

  # Reuse nixpkgs' whole zellij build (protoc, embedded assets, plugin bundling,
  # manpage, completions); only repoint src + cargo deps at the fork.
  #
  # cargoDeps is passed directly, not via an overridden cargoHash: buildRustPackage
  # reads cargoHash from the raw function argument, which overrideAttrs (a fixpoint
  # over the result) can't reach — so an overridden cargoHash is silently ignored.
  # A prebuilt cargoDeps takes the recipe's `cargoDeps != null` branch instead.
  zellij-forked-unwrapped = pkgs.zellij-unwrapped.overrideAttrs (_: {
    version = "0.45.0-pane-slot-binding";
    src = forkSrc;
    cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
      name = "zellij-fork-deps";
      src = forkSrc;
      hash = "sha256-PLHoJcjyjd1jX/ZPf/Mh6n5VEQ2/Q4RZrZShm8yAeDM=";
    };
    # Our version string carries the fork suffix; the upstream --version check
    # expects a plain semver, so skip it.
    doInstallCheck = false;
  });
in {
  inherit zellij-spiral;
  # The fork is the `zellij` everything else uses.
  zellij = pkgs.zellij.override { zellij-unwrapped = zellij-forked-unwrapped; };
}
