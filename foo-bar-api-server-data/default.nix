{ mkDerivation, aeson, appendful, appendful-persistent, autodocodec
, base, foo-bar-data, lib, mergeful, mergeful-persistent, mergeless
, mergeless-persistent, password, password-instances, persistent
, persistent-sqlite, persistent-template, text, time, validity
, validity-persistent, validity-text
}:
mkDerivation {
  pname = "foo-bar-api-server-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful appendful-persistent autodocodec base foo-bar-data
    mergeful mergeful-persistent mergeless mergeless-persistent
    password password-instances persistent persistent-sqlite
    persistent-template text time validity validity-persistent
    validity-text
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
