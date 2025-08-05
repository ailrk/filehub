{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3, async
, base, bytestring, case-insensitive, conduit, containers, cookie
, cryptohash-sha256, directory, effectful, file-embed, filepath
, fuzzy, generic-lens, hashable, hashtables, hspec, hspec-wai
, http-api-data, http-types, lib, log-base, log-effectful, lucid
, microlens, microlens-platform, mime-types, network-uri
, optparse-applicative, QuickCheck, servant, servant-conduit
, servant-lucid, servant-multipart, servant-server
, string-interpolate, suspend, text, time, timers, transformers
, unix, unliftio, unordered-containers, uri-encode, uuid, vault
, wai, wai-app-static, wai-extra, warp, zip-archive
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 async base bytestring
    case-insensitive conduit containers cookie cryptohash-sha256
    directory effectful file-embed filepath fuzzy generic-lens hashable
    hashtables http-api-data http-types log-base log-effectful lucid
    microlens microlens-platform mime-types network-uri
    optparse-applicative servant servant-conduit servant-lucid
    servant-multipart servant-server string-interpolate suspend text
    time timers transformers unix unliftio unordered-containers
    uri-encode uuid vault wai wai-app-static wai-extra warp zip-archive
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring containers directory effectful filepath hspec
    hspec-wai http-types log-base log-effectful QuickCheck
    servant-server time uri-encode uuid wai wai-extra
  ];
  license = "unknown";
  mainProgram = "filehub";
}
