{ lib, stdenv, fetchurl, dpkg, autoPatchelfHook, makeWrapper
, alsa-lib, at-spi2-atk, at-spi2-core, cairo, cups, dbus, expat
, gdk-pixbuf, glib, gtk3, libdrm, libgbm, libGL, libnotify, libusb1
, libxkbcommon, nspr, nss, openssl, pango, systemd, tpm2-tss
, xorg, wayland, xdg-utils }:

stdenv.mkDerivation rec {
  pname = "chatgpt";
  version = "26.928.20755";
  src = fetchurl {
    url = "https://persistent.oaistatic.com/codex-app-prod/linux/deb/pool/main/c/chatgpt/chatgpt_${version}_amd64.deb";
    hash = "sha256-RYbcGmyGmJgsqFn4aqoWg18zgyoJ4kBC36dVcapg2NE=";
  };
  nativeBuildInputs = [ dpkg autoPatchelfHook makeWrapper ];
  buildInputs = [
    alsa-lib at-spi2-atk at-spi2-core cairo cups dbus expat gdk-pixbuf
    glib gtk3 libdrm libgbm libGL libnotify libusb1 libxkbcommon nspr nss
    openssl pango systemd tpm2-tss wayland
    stdenv.cc.cc.lib
  ] ++ (with xorg; [ libX11 libXcomposite libXdamage libXext libXfixes
    libXrandr libxcb libxshmfence ]);
  runtimeDependencies = [ (lib.getLib systemd) (lib.getLib libGL) ];
  unpackPhase = ''
    runHook preUnpack
    dpkg-deb -x "$src" .
    runHook postUnpack
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/lib" "$out/bin" "$out/share"
    cp -r usr/lib/chatgpt "$out/lib/"
    rm "$out/lib/chatgpt/libqt5_shim.so" "$out/lib/chatgpt/libqt6_shim.so"
    find "$out/lib/chatgpt/resources/app.asar.unpacked" -path '*/prebuilds/*musl*' -type f -delete
    cp -r usr/share/applications usr/share/pixmaps "$out/share/"
    makeWrapper "$out/lib/chatgpt/ChatGPT" "$out/bin/chatgpt" \
      --prefix PATH : ${lib.makeBinPath [ xdg-utils ]}
    substituteInPlace "$out/share/applications/chatgpt.desktop" \
      --replace-fail 'Exec=chatgpt' "Exec=$out/bin/chatgpt"
    runHook postInstall
  '';
  meta = {
    description = "ChatGPT desktop app";
    homepage = "https://learn.chatgpt.com/docs/linux/linux-app";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
    mainProgram = "chatgpt";
  };
}
