module Text.FastAleck.Text.Lazy
    ( module Text.FastAleck
    , fastAleck
    ) where

import qualified Data.Text.Lazy as TL

import Text.FastAleck
import qualified Text.FastAleck.Text as T

fastAleck :: FastAleckConfig -> TL.Text -> TL.Text
fastAleck config = TL.fromChunks . map (T.fastAleck config) . TL.toChunks
{-# INLINE fastAleck #-}
