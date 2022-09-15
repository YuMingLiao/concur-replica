{ mkDerivation, aeson, base, bytestring, concur-core, containers
, free, hpack, mtl, random, replica, lib, stm, text
, transformers, wai, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "concur-replica";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring concur-core containers free replica stm text
    transformers wai wai-websockets warp websockets
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring concur-core containers free mtl random
    replica stm text transformers wai wai-websockets warp websockets
  ];
  prePatch = "hpack";
  homepage = "https://github.com/pkamenarsky/concur-replica#readme";
  description = "Replica backend for Concur";
  license = lib.licenses.bsd3;
}
