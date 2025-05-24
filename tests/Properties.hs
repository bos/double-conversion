import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Double.Conversion.Convertable as C
import Data.Double.Conversion.Convertable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Regressions

shortest :: (Double -> String) -> Double -> Double -> Bool
shortest f a b = case read (f ab) of
                   ba | isNaN ba      -> isNaN ab
                      | isInfinite ba -> isInfinite ab && signum ba == signum ab
                      | otherwise     -> ba == ab
  where ab = a / b

-- | Compare concatenated sequence of numbers as Text and Builder
catTextEqualsCatBuilder :: [Either Float Double] -> Bool
catTextEqualsCatBuilder xs =
  mconcat texts == (TL.toStrict . TLB.toLazyText . mconcat) builders
  where
    texts :: [T.Text]
    texts = either (toExponential 3) (toExponential 3) <$> xs
    builders :: [TLB.Builder]
    builders = either (toExponential 3) (toExponential 3) <$> xs

-- | Compare concatenated sequence of numbers as ByteString and Builder
catByteStringEqualsCatBuilder :: [Either Float Double] -> Bool
catByteStringEqualsCatBuilder xs =
  mconcat byteStrings == (B.toStrict . BB.toLazyByteString . mconcat) builders
  where
    byteStrings :: [B.ByteString]
    byteStrings = either (toExponential 3) (toExponential 3) <$> xs
    builders :: [BB.Builder]
    builders = either (toExponential 3) (toExponential 3) <$> xs

tests :: Test
tests = testGroup "Properties" [
    testProperty "b_shortest" $ shortest (B.unpack . C.toShortest)
  , testProperty "t_shortest" $ shortest (T.unpack . C.toShortest)
  , testProperty "catTextEqualsCatBuilder" catTextEqualsCatBuilder
  , testProperty "catByteStringEqualsCatBuilder" catByteStringEqualsCatBuilder
  ]

main :: IO ()
main = defaultMain [tests, Regressions.tests]
