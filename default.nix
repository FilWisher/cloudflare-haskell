{ mkDerivation, aeson, base, bytestring, lens, mtl, stdenv, text
, wreq, zlib
}:
mkDerivation {
  pname = "cloudflare-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring lens mtl text wreq
  ];
  libraryPkgconfigDepends = [ zlib ];
  license = stdenv.lib.licenses.bsd3;
}
