{ mkDerivation, aeson, autodocodec, base, base16, bytestring
, containers, cryptohash-sha256, directory, filepath, lib, path
, path-io, persistent, random-shuffle, safe, text, time, validity
, validity-bytestring, validity-containers, validity-path
, validity-text, validity-time, yaml
}:
mkDerivation {
  pname = "foo-bar-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base16 bytestring containers
    cryptohash-sha256 directory filepath path path-io persistent
    random-shuffle safe text time validity validity-bytestring
    validity-containers validity-path validity-text validity-time yaml
  ];
  homepage = "https://github.com/NorfairKing/foo-bar#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
