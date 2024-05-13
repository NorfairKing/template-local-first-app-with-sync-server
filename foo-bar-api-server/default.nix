{ mkDerivation, aeson, appendful, appendful-persistent, autodocodec
, autodocodec-yaml, autoexporter, base, bytestring, envparse
, foo-bar-api, foo-bar-api-server-data, foo-bar-data, jose, lib
, mergeful, mergeful-persistent, mergeless, mergeless-persistent
, monad-logger, mtl, optparse-applicative, password
, password-instances, path, path-io, persistent, persistent-sqlite
, persistent-template, servant-auth-server, servant-server, text
, time, wai, warp, yaml
}:
mkDerivation {
  pname = "foo-bar-api-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson appendful appendful-persistent autodocodec autodocodec-yaml
    base bytestring envparse foo-bar-api foo-bar-api-server-data
    foo-bar-data jose mergeful mergeful-persistent mergeless
    mergeless-persistent monad-logger mtl optparse-applicative password
    password-instances path path-io persistent persistent-sqlite
    persistent-template servant-auth-server servant-server text time
    wai warp yaml
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "foo-bar-api-server";
}
