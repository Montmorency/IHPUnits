{ mkDerivation, base, binary, binary-orphans, bytestring, Cabal
, cabal-doctest, cereal, containers, directory, doctest, filepath
, hashable, mtl, scientific, stdenv, text, time, transformers
, transformers-compat, unordered-containers, void
}:
mkDerivation {
  pname = "bytes";
  version = "0.17";
  sha256 = "23f2ecf0179b44f3c629d56ba74381de66ceeb898fc21b54d9c325cbba63ea85";
  revision = "2";
  editedCabalFile = "1i49q0zdh1a1pnbhcrbla9l8xgays6gvq9bbx9k5iv469n4ppjbd";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    base binary binary-orphans bytestring cereal containers hashable
    mtl scientific text time transformers transformers-compat
    unordered-containers void
  ];
  testHaskellDepends = [ base directory doctest filepath ];
  homepage = "https://github.com/ekmett/bytes";
  description = "Sharing code for serialization between binary and cereal";
  license = stdenv.lib.licenses.bsd3;
}
