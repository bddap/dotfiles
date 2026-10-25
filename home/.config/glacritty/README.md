# Glacritty shaders

All seven `.glsl` examples are copied unchanged from
[bddap-bot/glacritty](https://github.com/bddap-bot/glacritty/tree/3fc59fe0bd23e137f524541c73ef4124b931d7e8/extra/shaders)
at the revision pinned in `nix/nix/sources.json`.
Upstream license notices are preserved in `LICENSE-MIT` and `LICENSE-APACHE`.

After running `./activate` from the dotfiles repository, edit
`~/.config/glacritty/glacritty.toml` and choose one shader under `[window]`:

```toml
background_shader = "aurora.glsl"
```

Available examples: `aurora.glsl`, `dusk.glsl`, `paper.glsl`,
`paper-drift.glsl`, `plasma.glsl`, `starfield.glsl`, and `synthgrid.glsl`.
Paths are relative to the config file. Only one shader is active at a time;
`paper.glsl` remains the default. The dark examples work best with a dark palette.
