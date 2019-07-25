{ mkDerivation, base, bytestring, numeric-limits, stdenv, vector }:
mkDerivation {
  pname = "tinyraytracer-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring numeric-limits vector
  ];
  license = stdenv.lib.licenses.publicDomain;
}
