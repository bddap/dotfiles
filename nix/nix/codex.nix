{ stdenv, fetchurl, lib, ... }:

stdenv.mkDerivation rec {
  pname = "codex";
  version = "0.154.0";

  src = fetchurl {
    url =
      "https://github.com/openai/codex/releases/download/rust-v${version}/codex-x86_64-unknown-linux-musl.tar.gz";
    sha256 = "sha256-1+GLJZeujyQvXzHunpDe70jbye3WNNmGj7ZDXQjAfwI=";
  };

  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
    runHook preUnpack
    tar xvf $src
    runHook postUnpack
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp codex-x86_64-unknown-linux-musl $out/bin/codex
  '';

  meta = {
    description = "Codex command-line tool";
    license = lib.licenses.asl20;
    platforms = [ "x86_64-linux" ];
  };
}
