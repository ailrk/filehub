{ mkDerivation, aeson, async, aws, base, bytestring, clay, conduit
, containers, directory, effectful, effectful-th, envparse
, filepath, fuzzy, generic-lens, http-api-data, http-conduit
, http-types, lib, log-base, log-effectful, lucid, microlens
, modern-uri, optparse-applicative, pandoc, pretty-simple
, resourcet-effectful, servant, servant-client, servant-lucid
, servant-multipart, servant-server, string-interpolate, text, time
, transformers, unliftio, wai, wai-app-static, wai-extra, warp
}:
mkDerivation {
  pname = "filehub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson async aws base bytestring clay conduit containers directory
    effectful effectful-th envparse filepath fuzzy generic-lens
    http-api-data http-conduit http-types log-base log-effectful lucid
    microlens modern-uri optparse-applicative pandoc pretty-simple
    resourcet-effectful servant servant-client servant-lucid
    servant-multipart servant-server string-interpolate text time
    transformers unliftio wai wai-app-static wai-extra warp
  ];
  license = "unknown";
  mainProgram = "filehub";
}
