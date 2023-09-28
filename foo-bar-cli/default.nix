{ mkDerivation, appendful, appendful-persistent, autodocodec
, autodocodec-yaml, base, cookie, envparse, filelock, foo-bar-api
, foo-bar-api-server-data, foo-bar-api-server-gen, foo-bar-client
, foo-bar-client-data, foo-bar-data, genvalidity, genvalidity-hspec
, hspec, http-client, http-client-tls, lib, mergeful
, mergeful-persistent, mergeless, mergeless-persistent
, monad-logger, mtl, optparse-applicative, path, path-io
, persistent, persistent-sqlite, servant, servant-auth-client
, servant-client, text, yaml
}:
mkDerivation {
  pname = "foo-bar-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    appendful appendful-persistent autodocodec autodocodec-yaml base
    cookie envparse filelock foo-bar-api foo-bar-api-server-data
    foo-bar-client foo-bar-client-data foo-bar-data http-client
    http-client-tls mergeful mergeful-persistent mergeless
    mergeless-persistent monad-logger mtl optparse-applicative path
    path-io persistent persistent-sqlite servant servant-auth-client
    servant-client text yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base foo-bar-api foo-bar-api-server-data foo-bar-api-server-gen
    genvalidity genvalidity-hspec hspec path path-io servant-client
    text
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "foo-bar";
}
