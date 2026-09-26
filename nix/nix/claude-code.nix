{ claude-code-bin, fetchurl, ... }:

claude-code-bin.overrideAttrs (finalAttrs: _: {
  version = "2.1.280";

  src = fetchurl {
    url =
      "https://downloads.claude.ai/claude-code-releases/${finalAttrs.version}/linux-x64/claude";
    sha256 = "1e08503dbdf3c2cb0d706d32f3408277388d1c76ef108673e8fe42c1b322925b";
  };
})
