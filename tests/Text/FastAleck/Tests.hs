module Text.FastAleck.Tests
    ( tests
    ) where

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Text.FastAleck (FastAleckConfig (..))
import qualified Text.FastAleck.ByteString as B
import qualified Text.FastAleck.ByteString.Lazy as BL
import qualified Text.FastAleck.Text as T
import qualified Text.FastAleck.Text.Lazy as TL

tests :: [Test]
tests =
    [ testCase "01" $ fastAleckTest "Herp—derp" "Herp--derp"
    ]

fastAleckTest :: String -> String -> Assertion
fastAleckTest e i = do
    assertEqual "ByteString"      e $ fa (fromString i :: B.ByteString)
    assertEqual "ByteString.Lazy" e $ fa (fromString i :: BL.ByteString)
    assertEqual "Text"            e $ fa (fromString i :: T.Text)
    assertEqual "Text.Lazy"       e $ fa (fromString i :: TL.Text)
  where
    fa :: FastAleckString a => a -> String
    fa = toString . fastAleck (FastAleckConfig False False)

class FastAleckString a where
    fromString :: String -> a
    toString   :: a -> String
    fastAleck  :: FastAleckConfig -> a -> a

instance FastAleckString B.ByteString where
    fromString = T.encodeUtf8 . T.pack
    toString   = T.unpack . T.decodeUtf8
    fastAleck  = B.fastAleck

instance FastAleckString BL.ByteString where
    fromString = TL.encodeUtf8 . TL.pack
    toString   = TL.unpack . TL.decodeUtf8
    fastAleck  = BL.fastAleck

instance FastAleckString T.Text where
    fromString = T.pack
    toString   = T.unpack
    fastAleck  = T.fastAleck

instance FastAleckString TL.Text where
    fromString = TL.pack
    toString   = TL.unpack
    fastAleck  = TL.fastAleck
