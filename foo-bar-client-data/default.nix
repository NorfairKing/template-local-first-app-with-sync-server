{ mkDerivation, base, foo-bar-api-server-data, foo-bar-data
, genvalidity-hspec, genvalidity-hspec-persistent, hspec, lib
, mergeful, mergeful-persistent, persistent, persistent-sqlite
, persistent-template, safe, time
}:
mkDerivation {
  pname = "foo-bar-client-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base foo-bar-api-server-data foo-bar-data mergeful
    mergeful-persistent persistent persistent-sqlite
    persistent-template safe time
  ];
  testHaskellDepends = [
    base genvalidity-hspec genvalidity-hspec-persistent hspec
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
