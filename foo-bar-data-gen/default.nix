{ mkDerivation, autodocodec, base, foo-bar-data, genvalidity
, genvalidity-bytestring, genvalidity-containers, genvalidity-hspec
, genvalidity-hspec-aeson, genvalidity-hspec-persistent
, genvalidity-path, genvalidity-text, genvalidity-time, hspec, lib
, path, path-io, text
}:
mkDerivation {
  pname = "foo-bar-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base foo-bar-data genvalidity genvalidity-bytestring
    genvalidity-containers genvalidity-path genvalidity-text
    genvalidity-time text
  ];
  testHaskellDepends = [
    autodocodec base foo-bar-data genvalidity-hspec
    genvalidity-hspec-aeson genvalidity-hspec-persistent hspec path
    path-io
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
