{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3, async
, base, bcrypt, bytestring, case-insensitive, conduit, containers
, cookie, cryptohash-sha256, directory, effectful, file-embed
, filepath, fuzzy, generic-lens, hashable, hashtables, hspec
, hspec-wai, http-api-data, http-types, JuicyPixels
, JuicyPixels-stbir, lib, log-base, log-effectful, lucid, microlens
, microlens-platform, mime-types, network-uri, optparse-applicative
, QuickCheck, random, servant, servant-conduit, servant-lucid
, servant-multipart, servant-server, split, string-interpolate
, suspend, text, time, timers, transformers, unliftio
, unordered-containers, uri-encode, uuid, vault, wai
, wai-app-static, wai-extra, warp, zip, zip-archive
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 async base bcrypt
    bytestring case-insensitive conduit containers cookie
    cryptohash-sha256 directory effectful file-embed filepath fuzzy
    generic-lens hashable hashtables http-api-data http-types
    JuicyPixels JuicyPixels-stbir log-base log-effectful lucid
    microlens microlens-platform mime-types network-uri
    optparse-applicative random servant servant-conduit servant-lucid
    servant-multipart servant-server split string-interpolate suspend
    text time timers transformers unliftio unordered-containers
    uri-encode uuid vault wai wai-app-static wai-extra warp zip
    zip-archive
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bcrypt bytestring containers cookie directory effectful
    filepath hspec hspec-wai http-api-data http-types log-base
    log-effectful QuickCheck servant-server time uri-encode uuid wai
    wai-extra
  ];
  license = "unknown";
  mainProgram = "filehub";
}
