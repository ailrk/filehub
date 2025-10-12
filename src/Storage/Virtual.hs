{-# LANGUAGE NamedFieldPuns #-}
module Storage.Virtual where

import Target.Virtual (Virtual, TargetBackend (..))
import Effectful (Eff)
import Data.File (FileInfo, FileWithContent, File (..))
import Data.ByteString (ByteString)
import Conduit (ConduitT, ResourceT)
import Servant.Multipart (FileData, Mem)


get
  :: forall es cacheType cacheName
  . TargetBackend Virtual -> FilePath -> Eff es (Maybe FileInfo)
get v@VirtualBackend { targetId } path = undefined


isDirectory
  :: forall es
  . TargetBackend Virtual -> FilePath -> Eff es Bool
isDirectory v@VirtualBackend { targetId } filePath = undefined


read
  :: forall es cacheType cacheName
  . TargetBackend Virtual -> FileInfo -> Eff es ByteString
read v@VirtualBackend { targetId } file = undefined


readStream :: TargetBackend Virtual -> FileInfo -> Eff es (ConduitT () ByteString (ResourceT IO) ())
readStream v@VirtualBackend { targetId } file = undefined


new
  :: TargetBackend Virtual -> FilePath -> Eff es ()
new v@VirtualBackend { targetId } filePath = undefined


write
  :: TargetBackend Virtual -> FilePath -> FileWithContent -> Eff es ()
write v@VirtualBackend { targetId } filePath File { content, size = mSize } = undefined


mv
  :: TargetBackend Virtual -> [(FilePath, FilePath)] -> Eff es ()
mv v = undefined


delete
  :: TargetBackend Virtual -> FilePath -> Eff es ()
delete v@VirtualBackend { targetId } filePath = undefined


ls
  :: forall es cacheType cacheName
  . TargetBackend Virtual -> FilePath -> Eff es [FileInfo]
ls v@VirtualBackend { targetId } _ = undefined


lsCwd
  :: TargetBackend Virtual -> Eff es [FileInfo]
lsCwd v = ls v ""


upload
  :: TargetBackend Virtual -> FileData Mem -> Eff es ()
upload s3 file = undefined


download
  :: TargetBackend Virtual -> FilePath -> Eff es (ConduitT () ByteString (ResourceT IO) ())
download s3 path = undefined
