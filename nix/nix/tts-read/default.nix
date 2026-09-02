{ stdenv, fetchurl, python3, gtk4, gobject-introspection, wrapGAppsHook4, gst_all_1, pipewire, ... }:
let
  kokoro = name: hash:
    fetchurl {
      url = "https://huggingface.co/hexgrad/Kokoro-82M/resolve/main/${name}";
      inherit hash;
    };
  deps = ps: [ ps.kokoro ps.spacy-models.en_core_web_sm ps.pygobject3 ps.numpy ];
  python = python3.withPackages deps;
  checkPython = python3.withPackages (ps: deps ps ++ [ ps.mypy ps.pygobject-stubs ]);
in stdenv.mkDerivation {
  pname = "tts-read";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [ wrapGAppsHook4 gobject-introspection ];
  buildInputs = [
    gtk4
    python
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    pipewire
  ];

  postPatch = ''
    substituteInPlace tts_read.py \
      --replace-fail "#!/usr/bin/env python3" "#!${python}/bin/python3" \
      --replace-fail @kokoro_config@ ${
        kokoro "config.json" "sha256-WrsB4kA7ByvwPQT94WBEPiCdeg2tSaQjvhUZa5tDwX8="
      } \
      --replace-fail @kokoro_model@ ${
        kokoro "kokoro-v1_0.pth" "sha256-SW26EY0aWPXz2y78iNvcIW4Eg/yJ/m5H7h8sU/GK0eQ="
      } \
      --replace-fail @kokoro_voice@ ${
        kokoro "voices/af_heart.pt" "sha256-CrVwm4/6sZv9hJzRHZj3W2CvdzMlOtDWexI4KhAstP8="
      }
    substituteInPlace app.tts_read.desktop --replace-fail @out@ $out
  '';

  preFixup = ''
    gappsWrapperArgs+=(--set HF_HUB_OFFLINE 1)
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    MYPY_CACHE_DIR=$TMPDIR/mypy ${checkPython}/bin/python -m mypy --strict --follow-untyped-imports tts_read.py test_tts_read.py
    HOME=$TMPDIR HF_HUB_OFFLINE=1 ${python}/bin/python -m unittest -v test_tts_read
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 tts_read.py $out/bin/tts-read
    install -Dm644 app.tts_read.desktop $out/share/applications/app.tts_read.desktop
    runHook postInstall
  '';
}
