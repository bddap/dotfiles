{
  buildFHSUserEnv,
  zoom-us,
  xdg-desktop-portal,
  ...
}:
let
  zoomInJail = buildFHSUserEnv {
    name = "zoom";

    # zoom is hard-coded to check for xdg-desktop-portal at /usr/libexec/xdg-desktop-portal
    extraBuildCommands = ''
      mkdir -p $out/usr/libexec
      ln -s ${xdg-desktop-portal}/libexec/xdg-desktop-portal \
            $out/usr/libexec/xdg-desktop-portal
    '';

    runScript = "${zoom-us}/bin/zoom";
  };
in
zoom-us.overrideAttrs (old: {
  postFixup =
    old.postFixup
    + ''
      # replace the zoom binary with the fhs version
      cp ${zoomInJail}/bin/zoom $out/bin/zoom
    '';
})
