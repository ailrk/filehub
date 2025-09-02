{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3, async
, base, base64-bytestring, bcrypt, bytestring, case-insensitive
, conduit, containers, cookie, criterion, cryptohash-sha256
, data-default, directory, effectful, file-embed, filepath, fuzzy
, generic-lens, hashable, hashtables, hspec, hspec-wai
, http-api-data, http-client, http-types, JuicyPixels
, JuicyPixels-stbir, jwt, lib, log-base, log-effectful, lucid
, microlens, microlens-platform, mime-types, network-uri
, optparse-applicative, process, QuickCheck, random, req, servant
, servant-client, servant-conduit, servant-lucid, servant-multipart
, servant-server, split, string-interpolate, suspend, temporary
, text, time, timers, tomland, transformers, unliftio
, unordered-containers, uri-encode, uuid, vault, wai
, wai-app-static, wai-extra, warp, zip
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
    base64-bytestring bcrypt bytestring case-insensitive conduit
    containers cookie cryptohash-sha256 data-default directory
    effectful file-embed filepath fuzzy generic-lens hashable
    hashtables http-api-data http-client http-types JuicyPixels
    JuicyPixels-stbir jwt log-base log-effectful lucid microlens
    microlens-platform mime-types network-uri optparse-applicative
    random servant servant-client servant-conduit servant-lucid
    servant-multipart servant-server split string-interpolate suspend
    temporary text time timers tomland transformers unliftio
    unordered-containers uri-encode uuid vault wai wai-app-static
    wai-extra warp zip
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bcrypt bytestring containers cookie directory effectful
    filepath hspec hspec-wai http-api-data http-types log-base
    log-effectful QuickCheck servant-server time uri-encode uuid wai
    wai-extra
  ];
  benchmarkHaskellDepends = [
    async base criterion http-types process req temporary time
  ];
  license = "unknown";
  mainProgram = "filehub";
}
