# zellij + zellij-spiral

[zellij-spiral](https://github.com/bddap-bot/zellij-spiral) re-tiles the tab so the
focused pane takes the dominant slot of a recursive golden spiral. Plain `zellij`
loads it (default layout); `Ctrl \` then `g` brings it to the focused tab.

`zellij` on PATH is the patched fork (`bddap-bot/zellij @ pane-slot-binding`), built
as a derivation in `nix/nix/zellij.nix`. Spiral knobs live in `config.kdl`.
