{ stdenv, fetchurl, lib, makeWrapper, python3, pipewire, gnome-shell, ... }:
let
  model = stdenv.mkDerivation {
    name = "parakeet-tdt-0.6b-v2-int8";
    src = fetchurl {
      url =
        "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v2-int8.tar.bz2";
      hash = "sha256-b8RCa1SqVJoAGbinW43qWRZOcXkKN6xWoEWtV/kV9xQ=";
    };
    installPhase = "cp -r . $out";
  };
  deps = ps: [ ps.sherpa-onnx ps.numpy ];
  python = python3.withPackages deps;
  checkPython = python3.withPackages (ps: deps ps ++ [ ps.mypy ]);
  extension = "dictate@dotfiles";
  metadata = builtins.toFile "metadata.json" (builtins.toJSON {
    uuid = extension;
    name = "Dictate";
    description = "Types dictated text into the focused window";
    shell-version = [ (lib.versions.major gnome-shell.version) ];
  });
in stdenv.mkDerivation {
  pname = "dictate";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  postPatch = ''
    substituteInPlace dictate.py \
      --replace-fail "#!/usr/bin/env python3" "#!${python}/bin/python3" \
      --replace-fail @model@ ${model}
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    MYPY_CACHE_DIR=$TMPDIR/mypy ${checkPython}/bin/python -m mypy --strict --follow-untyped-imports dictate.py test_dictate.py
    ${python}/bin/python -m unittest -v test_dictate
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 dictate.py $out/bin/dictate
    wrapProgram $out/bin/dictate --prefix PATH : ${lib.makeBinPath [ pipewire ]}
    install -Dm644 ${metadata} $out/share/gnome-shell/extensions/${extension}/metadata.json
    substitute extension/extension.js $out/share/gnome-shell/extensions/${extension}/extension.js \
      --replace-fail @dictate@ $out/bin/dictate
    runHook postInstall
  '';

  passthru = { inherit model extension; };
}
