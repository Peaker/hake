module FileData(FileData(..), inFileData) where

import Data.Binary.Get(getByteString, remaining)
import Data.Binary.Put(putByteString)
import Data.ByteString(ByteString)
import Data.Binary(Binary(..))

newtype FileData = FileData { unFileData :: ByteString }
inFileData :: (ByteString -> ByteString)
              -> FileData -> FileData
inFileData f = FileData . f . unFileData

instance Binary FileData where
  get = FileData `fmap` (getByteString . fromIntegral =<< remaining)
  put = putByteString . unFileData
