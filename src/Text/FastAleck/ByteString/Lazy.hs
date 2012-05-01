module Text.FastAleck.ByteString.Lazy
    ( module Text.FastAleck
    , fastAleck
    ) where

import qualified Data.ByteString.Lazy as BL

import Text.FastAleck
import qualified Text.FastAleck.Internal as I

fastAleck :: FastAleckConfig -> BL.ByteString -> BL.ByteString
fastAleck config = BL.fromChunks . map (I.fastAleck config) . BL.toChunks
{-# INLINE fastAleck #-}
