{ bddap, lib, cmake, pkg-config, python3, fontconfig, freetype, libGL, libxkbcommon
, wayland, xorg, ... }:
let
  src = bddap.sources.glacritty;
  # Loaded with dlopen at runtime, so the linker never records them.
  dlopened = [ libGL libxkbcommon wayland xorg.libX11 xorg.libXcursor xorg.libXi xorg.libXrandr xorg.libXxf86vm xorg.libxcb ];
  common = {
    pname = "glacritty";
    version = builtins.substring 0 7 src.rev;
    inherit src;
    nativeBuildInputs = [ cmake pkg-config python3 ];
    buildInputs = [ fontconfig freetype ] ++ dlopened;
  };
in bddap.craneLib.buildPackage (common // {
  cargoArtifacts = bddap.craneLib.buildDepsOnly common;
  postInstall = ''
    install -Dm644 extra/logo/alacritty-term.svg $out/share/icons/hicolor/scalable/apps/glacritty.svg
    install -Dm644 -t $out/share/glacritty/shaders extra/shaders/*.glsl
    install -Dm644 /dev/stdin $out/share/applications/glacritty.desktop <<DESKTOP
    [Desktop Entry]
    Type=Application
    Name=Glacritty
    GenericName=Terminal
    Comment=Alacritty with a shader background
    Exec=$out/bin/glacritty
    Icon=glacritty
    Terminal=false
    Categories=System;TerminalEmulator;
    StartupWMClass=Glacritty
    DESKTOP
  '';
  postFixup = ''
    patchelf --add-rpath ${lib.makeLibraryPath dlopened} $out/bin/glacritty
  '';
})
