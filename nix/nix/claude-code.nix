{ claude-code, fetchurl, ... }:

claude-code.overrideAttrs (finalAttrs: _: {
  version = "2.1.308";

  src = fetchurl {
    url =
      "https://downloads.claude.ai/claude-code-releases/${finalAttrs.version}/linux-x64/claude";
    sha256 = "356733f185fa988e6bf3aa9a555269fdd167982a2e2a80d3c8a78037bb92ad58";
  };
})
