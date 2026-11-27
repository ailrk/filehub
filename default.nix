{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-core
, amazonka-s3, base, base64-bytestring, bcrypt, binary, breakpoint
, bytestring, case-insensitive, conduit, conduit-extra, containers
, cookie, criterion, cryptohash-sha256, cryptonite, deepseq
, directory, file-embed, filepath, hashable
, hashtables, hspec, hspec-wai, http-api-data, http-client
, http-client-tls, http-types, jwt, lib, log-base, lucid
, mime-types, mtl, network, network-uri, optparse-applicative
, psqueues, QuickCheck, random, resourcet, servant
, servant-client, servant-conduit, servant-event-stream
, servant-lucid, servant-multipart, servant-server, split
, sqlite-simple, stm, string-interpolate, suspend, template-haskell
, text, time, timers, transformers
, transformers-base, unliftio, uri-encode, uuid, vector, wai
, wai-extra, warp, zip
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-core amazonka-s3 base
    base64-bytestring bcrypt binary breakpoint bytestring
    case-insensitive conduit conduit-extra containers cookie
    cryptohash-sha256 cryptonite directory file-embed filepath
    hashable hashtables http-api-data http-client
    http-client-tls http-types jwt log-base lucid mime-types
    mtl network network-uri optparse-applicative psqueues random
    resourcet servant servant-client servant-conduit
    servant-event-stream servant-lucid servant-multipart servant-server
    split sqlite-simple stm string-interpolate suspend template-haskell
    text time timers transformers transformers-base
    unliftio uri-encode uuid vector wai wai-extra warp zip
  ];
  executableHaskellDepends = [
    aeson base breakpoint bytestring text
  ];
  testHaskellDepends = [
    aeson base bcrypt breakpoint bytestring containers cookie directory
    filepath hspec hspec-wai http-api-data http-client http-client-tls
    http-types log-base QuickCheck servant-server text time unliftio
    uri-encode uuid wai wai-extra
  ];
  benchmarkHaskellDepends = [
    aeson base breakpoint bytestring containers criterion deepseq lucid
    text time unliftio uuid
  ];
  doHaddock = false;
  license = "unknown";
  mainProgram = "filehub";
}
