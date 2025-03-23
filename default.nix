{ mkDerivation, aeson, async, base, bytestring, clay, containers
, cookie, cryptohash-sha256, directory, effectful, filepath, fuzzy
, generic-lens, hashable, hashtables, http-api-data, lib, log-base
, log-effectful, lucid, microlens, microlens-platform, mime-types
, modern-uri, optparse-applicative, servant-lucid
, servant-multipart, servant-server, string-interpolate, suspend
, text, time, timers, transformers, unix, unliftio, uri-encode
, uuid, wai-extra, warp, zip-archive
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson async base bytestring clay containers cookie
    cryptohash-sha256 directory effectful filepath fuzzy generic-lens
    hashable hashtables http-api-data log-base log-effectful lucid
    microlens microlens-platform mime-types modern-uri
    optparse-applicative servant-lucid servant-multipart servant-server
    string-interpolate suspend text time timers transformers unix
    unliftio uri-encode uuid wai-extra warp zip-archive
  ];
  license = "unknown";
  mainProgram = "filehub";
}
