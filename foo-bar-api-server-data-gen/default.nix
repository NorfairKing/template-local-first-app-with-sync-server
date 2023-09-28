{ mkDerivation, base, bytestring, foo-bar-api-server-data
, foo-bar-data-gen, genvalidity, genvalidity-hspec
, genvalidity-mergeful, genvalidity-persistent, genvalidity-text
, hspec, lib, password, QuickCheck, text
}:
mkDerivation {
  pname = "foo-bar-api-server-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring foo-bar-api-server-data foo-bar-data-gen
    genvalidity genvalidity-mergeful genvalidity-persistent
    genvalidity-text password QuickCheck text
  ];
  testHaskellDepends = [
    base foo-bar-api-server-data genvalidity-hspec hspec
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
