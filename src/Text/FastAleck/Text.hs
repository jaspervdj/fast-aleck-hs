module Text.FastAleck.Text
    ( module Text.FastAleck
    , fastAleck
    ) where


--------------------------------------------------------------------------------
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as T


--------------------------------------------------------------------------------
import           Text.FastAleck
import qualified Text.FastAleck.Internal as I


--------------------------------------------------------------------------------
fastAleck :: FastAleckConfig -> Text -> Text
fastAleck config = T.decodeUtf8 . I.fastAleck config . T.encodeUtf8
{-# INLINE fastAleck #-}
