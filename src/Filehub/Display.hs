module Filehub.Display where

import Lens.Micro.Platform ()
import Data.Generics.Labels ()
import Servant (ToHttpApiData(..), FromHttpApiData(..))
import Web.FormUrlEncoded (FromForm(..), parseUnique)
import Data.Text qualified as Text
import Text.Read (readMaybe)


data Resolution = Resolution
  { width  :: Int
  , height :: Int
  }
  deriving (Show, Eq, Ord)


instance ToHttpApiData Resolution where
  toUrlPiece (Resolution w h) = toUrlPiece (show w ++ "x" ++ show h)


-- | e.g 1920x1080
instance FromHttpApiData Resolution where
  parseUrlPiece res =
    case Text.splitOn "x" res of
      x:y:_ -> do
        maybe (Left "invalid resolution") (\(w, h) -> pure $ Resolution w h) do
          w <- readMaybe (Text.unpack x)
          h <- readMaybe (Text.unpack y)
          pure (w, h)
      _ -> Left "unknown resolution"


instance FromForm Resolution where
  fromForm f = do
    res <- parseUnique "res" f
    parseUrlPiece res


data Display
  = Mobile
  | Desktop
  | NoDisplay
  deriving (Show, Eq, Ord)


instance ToHttpApiData Display where
  toUrlPiece = toUrlPiece . show


instance FromHttpApiData Display where
  parseUrlPiece "Mobile"    = pure Mobile
  parseUrlPiece "Desktop"   = pure Desktop
  parseUrlPiece "NoDisplay" = pure NoDisplay
  parseUrlPiece _           = Left "unknown display"



classify :: Resolution -> Display
classify (Resolution w h)
  | isMobile  = Mobile
  | otherwise = Desktop
  where
    aspect   = fromIntegral w / fromIntegral h :: Double
    isMobile = w < 768 || aspect < 0.75
