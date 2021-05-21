{ mkDerivation, aeson, base, bytestring, directory, fsnotify, hpack
, ilist, lib, monad-parallel, optparse-applicative, process, split
, text, unordered-containers, yaml
}:
mkDerivation {
  pname = "scripts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory fsnotify ilist monad-parallel
    optparse-applicative process split text unordered-containers yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring directory fsnotify ilist monad-parallel
    optparse-applicative process split text unordered-containers yaml
  ];
  prePatch = "hpack";
  homepage = "https://github.com/DerekMaffett/scripts#readme";
  license = lib.licenses.bsd3;
}
