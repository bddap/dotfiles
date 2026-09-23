{ claude-code-bin, fetchurl, ... }:

claude-code-bin.overrideAttrs (finalAttrs: _: {
  version = "2.1.280";

  src = fetchurl {
    url =
      "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases/${finalAttrs.version}/linux-x64/claude";
    sha256 = "sha256-HghQPb3zwssNcG0y80CCdziNHHbvEIZz6P5CwbMikls=";
  };
})
