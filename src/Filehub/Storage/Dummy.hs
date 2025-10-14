module Filehub.Storage.Dummy (storage) where

import Conduit (yield)
import Data.File (File(..), FileWithContent, extractFileInfo, FileContent (..))
import Effectful (Eff, Eff)
import Lens.Micro.Platform ()
import Prelude hiding (read, readFile, writeFile)
import Target.Storage (Storage(..))


storage :: [(FilePath, FileWithContent)] -> Storage (Eff es)
storage mockFS =
  Storage
    { get = \path -> pure do fmap extractFileInfo (lookup path mockFS)

    , read = \file ->
        case lookup file.path mockFS of
          Just (File { content = FileContentRaw bytes }) -> pure bytes
          _ -> error "storage dummy: read"

    , readStream  = \file ->
        case lookup file.path mockFS of
          Just (File { content = FileContentRaw bytes }) -> pure (yield bytes)
          _ -> error "storage dummy: readStream"

    , ls = \case
        "/"  -> pure $ fmap (extractFileInfo . snd) mockFS
        path ->
          case lookup path mockFS of
            Just (File { content = FileContentDir dir }) -> pure $ fmap extractFileInfo dir
            _ -> pure []

    , cd = \_ -> pure ()

    , isDirectory = \path ->
        case lookup path mockFS of
          Just (File { content = FileContentDir _ }) -> pure True
          _ -> pure False

    , write     = error "not implemented"
    , mv        = error "not implemented"
    , delete    = error "not implemented"
    , new       = error "not implemented"
    , newFolder = error "not implemented"
    , lsCwd     = error "not implemented"
    , upload    = error "not implemented"
    , download  = error "not implemented"
    }
