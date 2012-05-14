{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Text.FastAleck.Internal
    ( FastAleckConfig (..)
    , defaultFastAleckConfig
    , fastAleck
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative      ((<$>))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as BI
import           Foreign                  hiding (unsafePerformIO)
import           Foreign.C.Types          (CChar, CSize (..))
import           System.IO.Unsafe         (unsafePerformIO)


--------------------------------------------------------------------------------
#include <fast-aleck/fast-aleck.h>
        

--------------------------------------------------------------------------------
toFaBool :: CChar -> Bool
toFaBool = (/= 0)
{-# INLINE toFaBool #-}


--------------------------------------------------------------------------------
fromFaBool :: Bool -> CChar
fromFaBool True  = 1
fromFaBool False = 0
{-# INLINE fromFaBool #-}


--------------------------------------------------------------------------------
-- | Configuration for the fast-aleck library
data FastAleckConfig = FastAleckConfig
    { wrapAmps   :: Bool
    , wrapCaps   :: Bool
    , wrapQuotes :: Bool
    , widont     :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
instance Storable FastAleckConfig where
    sizeOf _     = #{size fast_aleck_config}
    alignment _  = 1
    peek p     = do
        amps    <- toFaBool <$> #{peek fast_aleck_config, wrap_amps}   p
        caps    <- toFaBool <$> #{peek fast_aleck_config, wrap_caps}   p
        quotes  <- toFaBool <$> #{peek fast_aleck_config, wrap_quotes} p
        widont' <- toFaBool <$> #{peek fast_aleck_config, widont}      p
        return $ FastAleckConfig amps caps quotes widont'
    poke p fac = do
        #{poke fast_aleck_config, wrap_amps}   p $ fromFaBool $ wrapAmps fac
        #{poke fast_aleck_config, wrap_caps}   p $ fromFaBool $ wrapCaps fac
        #{poke fast_aleck_config, wrap_quotes} p $ fromFaBool $ wrapQuotes fac
        #{poke fast_aleck_config, widont}      p $ fromFaBool $ widont fac


--------------------------------------------------------------------------------
foreign import ccall unsafe "fast_aleck_config_init" fast_aleck_config_init
    :: Ptr FastAleckConfig -> IO ()


--------------------------------------------------------------------------------
defaultFastAleckConfig :: FastAleckConfig
defaultFastAleckConfig = unsafePerformIO $ do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr -> do
        fast_aleck_config_init ptr
        peek ptr


--------------------------------------------------------------------------------
foreign import ccall unsafe "fast_aleck_wrapper" fast_aleck
    :: Ptr FastAleckConfig -> Ptr CChar -> CSize -> Ptr CSize -> IO (Ptr CChar)


--------------------------------------------------------------------------------
fastAleck :: FastAleckConfig -> ByteString -> ByteString
fastAleck config bs = unsafePerformIO $
    with config $ \configp ->
        with 0 $ \out_len_ptr ->
            withForeignPtr str_fptr $ \str_ptr -> do
                let str     = castPtr $ str_ptr `plusPtr` offset
                    str_len = fromIntegral len
                out_ptr  <- fast_aleck configp str str_len out_len_ptr
                out_len  <- peek out_len_ptr
                out_fptr <- newForeignPtr BI.c_free_finalizer $ castPtr out_ptr

                return $ BI.fromForeignPtr out_fptr 0 (fromIntegral out_len)
  where
    (str_fptr, offset, len) = BI.toForeignPtr bs
{-# INLINE fastAleck #-}
