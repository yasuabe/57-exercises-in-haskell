module Ex55 where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteArray.Encoding as BE (convertToBase, Base(Base16))
import qualified Crypto.Hash.MD5 as MD5
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

generateSlug :: T.Text -> IO T.Text
generateSlug snippet = nextRandom <&> (\uuid -> uuid  
  & toText 
  & (snippet <>)
  & TE.encodeUtf8
  & MD5.hash
  & BE.convertToBase BE.Base16
  & TE.decodeUtf8
  )
