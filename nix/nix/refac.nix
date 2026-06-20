{ pkg-config, openssl, cacert, bddap, ... }:
let
  craneLib = bddap.craneLib;
  sources = import ./sources.nix;
in craneLib.buildPackage {
  src = sources.refac;
  buildInputs = [ pkg-config openssl ];
  # refac's wire-shape tests build a reqwest TLS client; the hermetic nix
  # sandbox ships no CA bundle, so without this they panic at client construction.
  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
}
