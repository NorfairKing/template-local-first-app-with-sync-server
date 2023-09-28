{ mkDerivation, base, foo-bar-api, foo-bar-api-server-data-gen
, genvalidity, genvalidity-appendful, genvalidity-hspec
, genvalidity-mergeful, genvalidity-mergeless, genvalidity-text
, hspec, lib, text
}:
mkDerivation {
  pname = "foo-bar-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base foo-bar-api foo-bar-api-server-data-gen genvalidity
    genvalidity-appendful genvalidity-mergeful genvalidity-mergeless
    genvalidity-text text
  ];
  testHaskellDepends = [ base foo-bar-api genvalidity-hspec hspec ];
  homepage = "https://github.com/NorfairKing/foo-bar-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
