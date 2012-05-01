{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Text.FastAleck.Internal
    ( FastAleckConfig (..)
    , fastAleck
    ) where

import Control.Applicative ((<$>))
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BI

#include <fast-aleck/fast-aleck.h>

data FastAleckConfig = FastAleckConfig
    { wrapAmps   :: Bool
    , wrapQuotes :: Bool
    } deriving (Show)

instance Storable FastAleckConfig where
    sizeOf _     = #{size fast_aleck_config}
    alignment _  = 1
    peek p     = do
        amps   <- toFaBool <$> #{peek fast_aleck_config, wrap_amps} p
        quotes <- toFaBool <$> #{peek fast_aleck_config, wrap_quotes} p
        return $ FastAleckConfig amps quotes
    poke p fac = do
        #{poke fast_aleck_config, wrap_amps} p   $ fromFaBool $ wrapAmps fac
        #{poke fast_aleck_config, wrap_quotes} p $ fromFaBool $ wrapQuotes fac

foreign import ccall unsafe "fast_aleck_"
    fast_aleck :: Ptr FastAleckConfig -> Ptr CChar -> CSize -> IO (Ptr CChar)
        
toFaBool :: CChar -> Bool
toFaBool = (/= 0)

fromFaBool :: Bool -> CChar
fromFaBool True  = 1
fromFaBool False = 0

fastAleck :: FastAleckConfig -> ByteString -> ByteString
fastAleck config bs = unsafePerformIO $
    with config $ \configp -> do
        withForeignPtr str_fptr $ \str_ptr -> do
            let str     = castPtr $ str_ptr `plusPtr` offset
                str_len = fromIntegral len
            out_ptr  <- fast_aleck configp str str_len
            out_len  <- BI.c_strlen out_ptr
            out_fptr <- newForeignPtr BI.c_free_finalizer $ castPtr out_ptr

            return $ BI.fromForeignPtr out_fptr 0 (fromIntegral out_len)
  where
    (str_fptr, offset, len) = BI.toForeignPtr bs
