{
  fetchFromGitHub,
  stdenv,
  pnpm,
  nodejs,
  makeWrapper,
  ...
}:
let
  src_rev = "b34ed2ab831d9b11d77ce493371112fd7d67eccb";
  src_sha256 = "sha256-q2pcZfZUZVaG9IY/kgaIJZDUHcU8cUkk5KblZTevGQg=";
  dep_hash = "sha256-XEmStjnyWTHLQUawtLwqn14AO7516M4k4V+TP69YZ9M=";
in
stdenv.mkDerivation rec {
  pname = "codex-cli";
  version = (builtins.fromJSON (builtins.readFile "${src}/codex-cli/package.json")).version;
  src = fetchFromGitHub {
    owner = "openai";
    repo = "codex";
    rev = src_rev;
    sha256 = src_sha256;
  };
  sourceRoot = src.name;
  nativeBuildInputs = [
    nodejs
    pnpm
    pnpm.configHook
    makeWrapper
  ];

  pnpmDeps = pnpm.fetchDeps {
    inherit pname version src;
    hash = dep_hash;
  };

  buildPhase = ''
    cd codex-cli
    pnpm install --frozen-lockfile
    pnpm run build
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/lib

    install -Dm644 dist/cli.js $out/lib/cli.js
    makeWrapper ${nodejs}/bin/node $out/bin/codex \
      --add-flags "$out/lib/cli.js"

    runHook postInstall
  '';
}
