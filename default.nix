{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-s3, base
, base64-bytestring, bcrypt, binary, breakpoint, bytestring
, case-insensitive, conduit, conduit-extra, containers, cookie
, criterion, cryptohash-sha256, cryptonite, data-default, directory
, file-embed, filepath, fuzzy, generic-lens, hashable, hashtables
, hspec, hspec-wai, http-api-data, http-client, http-client-tls
, http-types, jwt, lib, log-base, lucid, microlens
, microlens-platform, mime-types, mtl, network-uri
, optparse-applicative, pretty-simple, psqueues, QuickCheck, random
, req, resourcet, retry, servant, servant-client, servant-conduit
, servant-event-stream, servant-lucid, servant-multipart
, servant-server, split, sqlite-simple, stm, string-interpolate
, suspend, template-haskell, temporary, text, time, timers, tomland
, transformers, transformers-base, unliftio, unordered-containers
, uri-encode, uuid, vault, vector, wai, wai-app-static, wai-extra
, warp, zip
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-s3 base base64-bytestring
    bcrypt binary breakpoint bytestring case-insensitive conduit
    conduit-extra containers cookie cryptohash-sha256 cryptonite
    data-default directory file-embed filepath fuzzy generic-lens
    hashable hashtables http-api-data http-client http-client-tls
    http-types jwt log-base lucid microlens microlens-platform
    mime-types mtl network-uri optparse-applicative pretty-simple
    psqueues random resourcet retry servant servant-client
    servant-conduit servant-event-stream servant-lucid
    servant-multipart servant-server split sqlite-simple stm
    string-interpolate suspend template-haskell temporary text time
    timers tomland transformers transformers-base unliftio
    unordered-containers uri-encode uuid vault vector wai
    wai-app-static wai-extra warp zip
  ];
  executableHaskellDepends = [ base breakpoint ];
  testHaskellDepends = [
    base bcrypt breakpoint bytestring containers cookie directory
    filepath hspec hspec-wai http-api-data http-client http-client-tls
    http-types log-base QuickCheck servant-server text time unliftio
    uri-encode uuid wai wai-extra
  ];
  benchmarkHaskellDepends = [
    base breakpoint criterion http-types req time unliftio
  ];
  license = "unknown";
  mainProgram = "filehub";
}
