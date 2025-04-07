{ mkDerivation, aeson, amazonka, amazonka-s3, async, base
, bytestring, clay, containers, cookie, cryptohash-sha256
, directory, effectful, filepath, fuzzy, generic-lens, hashable
, hashtables, http-api-data, http-types, lib, log-base
, log-effectful, lucid, microlens, microlens-platform, mime-types
, modern-uri, optparse-applicative, servant-lucid
, servant-multipart, servant-server, string-interpolate, suspend
, text, time, timers, transformers, unix, unliftio, uri-encode
, uuid, wai, wai-app-static, wai-extra, warp, zip-archive
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson amazonka amazonka-s3 async base bytestring clay containers
    cookie cryptohash-sha256 directory effectful filepath fuzzy
    generic-lens hashable hashtables http-api-data http-types log-base
    log-effectful lucid microlens microlens-platform mime-types
    modern-uri optparse-applicative servant-lucid servant-multipart
    servant-server string-interpolate suspend text time timers
    transformers unix unliftio uri-encode uuid wai wai-app-static
    wai-extra warp zip-archive
  ];
  license = "unknown";
  mainProgram = "filehub";
}
