{-# OPTIONS_GHC -fglasgow-exts  #-}
import Control.Monad

import Data.Binary
import Data.Char (chr)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map

import Test.QuickCheck.Batch
import Test.QuickCheck

import Data.BERT

instance Arbitrary Char where 
  -- NB! We use a restricted notion of Char here, as we Char8-pack
  -- strings in our implementation, truncating chars over 8 bits.
  arbitrary = liftM chr $ choose (0, 255)
  coarbitrary = undefined

instance (Arbitrary a, Ord a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = liftM Map.fromList arbitrary
  coarbitrary = undefined

options = TestOptions
          { no_of_tests     = 500
          , length_of_tests = 100
          , debug_tests     = False }

type T a = a -> Bool
-- value -> Term -> encoded -> Term -> value
t a = Right a == (readBERT . decode . encode . showBERT) a
-- value -> Term -> Packet -> encoded -> Packet -> Term -> value
p a = Right a == (readBERT . fromPacket . decode . encode . Packet . showBERT) a

main = do
  runTests "simple terms" options
       [ run (t :: T Bool)
       , run (t :: T Integer)
       , run (t :: T String)
       , run (t :: T (String, String))
       , run (t :: T (String, [String]))
       , run (t :: T [String])
       , run (t :: T (Map String String))
       , run (t :: T (String, Int, Int, Int))
       , run (t :: T (Int, Int, Int, Int))
       ]

  runTests "simple packets" options
       [ run (p :: T Bool)
       , run (p :: T Integer)
       , run (p :: T String)
       , run (p :: T (String, String))
       , run (p :: T (String, [String]))
       , run (p :: T [String])
       , run (p :: T (Map String String))
       , run (p :: T (String, Int, Int, Int))
       ]
