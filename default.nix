{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3, async
, base, base64-bytestring, bcrypt, binary, bytestring
, case-insensitive, conduit, containers, cookie, criterion
, cryptohash-sha256, cryptonite, data-default, directory, effectful
, file-embed, filepath, fuzzy, generic-lens, hashable, hashtables
, hspec, hspec-wai, http-api-data, http-client, http-client-tls
, http-types, JuicyPixels, JuicyPixels-stbir, jwt, lib, log-base
, log-effectful, lucid, microlens, microlens-platform, mime-types
, network-uri, optparse-applicative, pretty-simple, process
, psqueues, QuickCheck, random, req, retry, servant, servant-client
, servant-conduit, servant-event-stream, servant-lucid
, servant-multipart, servant-server, split, sqlite-simple, stm
, string-interpolate, suspend, temporary, text, time, timers
, tomland, transformers, unliftio, unordered-containers, uri-encode
, uuid, vault, vector, wai, wai-app-static, wai-extra, warp, zip
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 async base
    base64-bytestring bcrypt binary bytestring case-insensitive conduit
    containers cookie cryptohash-sha256 cryptonite data-default
    directory effectful file-embed filepath fuzzy generic-lens hashable
    hashtables http-api-data http-client http-client-tls http-types
    JuicyPixels JuicyPixels-stbir jwt log-base log-effectful lucid
    microlens microlens-platform mime-types network-uri
    optparse-applicative pretty-simple psqueues random retry servant
    servant-client servant-conduit servant-event-stream servant-lucid
    servant-multipart servant-server split sqlite-simple stm
    string-interpolate suspend temporary text time timers tomland
    transformers unliftio unordered-containers uri-encode uuid vault
    vector wai wai-app-static wai-extra warp zip
  ];
  executableHaskellDepends = [
    base bytestring directory effectful filepath http-client http-types
    lucid mime-types network-uri optparse-applicative process
    string-interpolate text time wai warp
  ];
  testHaskellDepends = [
    base bcrypt bytestring containers cookie directory effectful
    filepath hspec hspec-wai http-api-data http-client http-client-tls
    http-types log-base log-effectful QuickCheck servant-server time
    uri-encode uuid wai wai-extra
  ];
  benchmarkHaskellDepends = [
    async base criterion http-types process req temporary time
  ];
  license = "unknown";
}
