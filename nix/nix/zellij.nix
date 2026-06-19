# The patched zellij + the zellij-spiral plugin wasm, as plain derivations.
# Ported from the spiral repo's flake so `./home-manager build` produces both —
# no flakes, no build-on-launch. `zellij-forked` goes on PATH (stock zellij would
# put the wrong pane in the dominant slot); the wasm store path is what the zellij
# config/layout load.
#
# Built against the spiral's own pinned nixpkgs + rust-overlay (sources.json), not
# the repo's main nixpkgs: that rev's zellij-unwrapped + fetchCargoVendor API are
# what the fork's vendor hashes were computed against, so reusing it keeps the
# hashes valid instead of chasing drift across two nixpkgs.
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

  # zellij-tile and zellij-utils are git crates from this fork checkout;
  # importCargoLock keys them by "name-version" to the unpacked source hash.
  forkHash = "sha256-W9fjq34c0Omgr7lZsLLQg6DtpbGyB9pYXMpGN4nGc+s=";
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

    # All native: these drive build scripts in the dep tree (prost & friends),
    # compiled for the host during the otherwise-wasm build — not wasm inputs.
    nativeBuildInputs = [
      rustWasm
      pkgs.rustPlatform.cargoSetupHook
      pkgs.pkg-config
      pkgs.protobuf
      pkgs.gcc
    ];

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
  zellij-fork-deps = pkgs.rustPlatform.fetchCargoVendor {
    name = "zellij-fork-deps";
    src = forkSrc;
    hash = "sha256-PLHoJcjyjd1jX/ZPf/Mh6n5VEQ2/Q4RZrZShm8yAeDM=";
  };
  zellij-forked-unwrapped = pkgs.zellij-unwrapped.overrideAttrs (_: {
    version = "0.45.0-pane-slot-binding";
    src = forkSrc;
    cargoDeps = zellij-fork-deps;
    # Our version string carries the fork suffix; the upstream --version check
    # expects a plain semver, so skip it.
    doInstallCheck = false;
  });
  zellij-forked = pkgs.zellij.override {
    zellij-unwrapped = zellij-forked-unwrapped;
  };
in {
  inherit zellij-spiral;
  # The fork is the `zellij` everything else uses; expose under both names so
  # home.packages can take it as the plain `zellij`.
  zellij = zellij-forked;
}
