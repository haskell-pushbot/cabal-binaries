{-# LANGUAGE ScopedTypeVariables #-}
module UnitTests.Distribution.Described where

import Data.Proxy            (Proxy (..))
import Data.Typeable         (Typeable, typeOf)
import Test.QuickCheck       (Arbitrary (..), Gen, Property, choose, counterexample, elements)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.FieldGrammar.Described (Described (..), RTerm (..), generate)
import Distribution.Parsec                 (Parsec, eitherParsec)

import qualified Distribution.Types.Version as C

tests :: TestTree
tests = testGroup "Described"
    [ testProperty "dummy" $ 'x' == 'x'
    , testDescribed (Proxy :: Proxy C.Version)
    ]

testDescribed
    :: forall a. (Described a, Parsec a, Typeable a, Show a)
    => Proxy a
    -> TestTree
testDescribed _ = testProperty name prop
  where
    name = show (typeOf (undefined :: a))

    prop :: Ex a -> Property
    prop (Example str) = counterexample (show res) $ case res of
        Right _ -> True
        Left _  -> False
      where
        res :: Either String a
        res = eitherParsec str

newtype Ex a = Example String
  deriving (Show)

instance Described a => Arbitrary (Ex a) where
    arbitrary = fmap Example (generate genInt genRTerm (describe (Proxy :: Proxy a)))

genInt :: Int -> Int -> Gen Int
genInt lo hi = choose (lo, hi)

-- TODO: use Regex itself.
genRTerm :: RTerm -> Gen String
genRTerm RUnqualName    = elements ["foo", "bar"]
genRTerm RHaskellString = return (show "foo")
