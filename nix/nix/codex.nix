{ stdenv, fetchurl, lib, ... }:

stdenv.mkDerivation rec {
  pname = "codex";
  version = "0.156.1";

  src = fetchurl {
    url =
      "https://github.com/openai/codex/releases/download/rust-v${version}/codex-x86_64-unknown-linux-musl.tar.gz";
    sha256 = "sha256-r/RlOag6/4bjxixZK84sUNlTkfnfKJr68DpQwB0UUz0=";
  };

  codeModeHostSrc = fetchurl {
    url =
      "https://github.com/openai/codex/releases/download/rust-v${version}/codex-code-mode-host-x86_64-unknown-linux-musl.tar.gz";
    sha256 = "sha256-qSnaqfagvdwAwMnmQC3xF7ElrNlvnVVPbJnDLH5mxgg=";
  };

  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
    runHook preUnpack
    tar xvf $src
    tar xvf $codeModeHostSrc
    runHook postUnpack
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp codex-x86_64-unknown-linux-musl $out/bin/codex
    cp codex-code-mode-host-x86_64-unknown-linux-musl $out/bin/codex-code-mode-host
  '';

  meta = {
    description = "Codex command-line tool";
    license = lib.licenses.asl20;
    platforms = [ "x86_64-linux" ];
  };
}
