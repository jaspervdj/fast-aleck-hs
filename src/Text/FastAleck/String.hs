module Text.FastAleck.String
    ( module Text.FastAleck
    , fastAleck
    ) where


--------------------------------------------------------------------------------
import qualified Data.Text           as T


--------------------------------------------------------------------------------
import           Text.FastAleck
import qualified Text.FastAleck.Text as T


--------------------------------------------------------------------------------
fastAleck :: FastAleckConfig -> String -> String
fastAleck config = T.unpack . T.fastAleck config . T.pack
{-# INLINE fastAleck #-}
