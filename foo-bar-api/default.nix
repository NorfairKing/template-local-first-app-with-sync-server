{ mkDerivation, aeson, appendful, autodocodec, base
, foo-bar-api-server-data, foo-bar-client-data, foo-bar-data, jose
, lib, mergeful, mergeless, persistent, servant, servant-auth
, servant-auth-server, text, validity, validity-text
}:
mkDerivation {
  pname = "foo-bar-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful autodocodec base foo-bar-api-server-data
    foo-bar-client-data foo-bar-data jose mergeful mergeless persistent
    servant servant-auth servant-auth-server text validity
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/foo-bar-api-cli-login#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
