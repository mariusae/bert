{-# LANGUAGE OverlappingInstances, TypeSynonymInstances #-}
-- |
-- Module      : Data.BERT.Term
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Define BERT termsm their binary encoding & decoding and a typeclass
-- for converting Haskell values to BERT terms and back.
-- 
-- We define a number of convenient instances for 'BERT'. Users will
-- probably want to define their own instances for composite types.
module Data.BERT.Term
  ( Term(..)
  , BERT(..)
  ) where

import Control.Monad.Error
import Control.Monad (forM_, replicateM, liftM2, liftM3, liftM4)
import Control.Applicative ((<$>))
import Data.Bits (shiftR, (.&.))
import Data.Char (chr, isAsciiLower, isAscii)
import Data.Binary (Binary(..), Word8)
import Data.Binary.Put (
  Put, putWord8, putWord16be, 
  putWord32be, putLazyByteString)
import Data.Binary.Get (
  Get, getWord8, getWord16be, getWord32be,
  getLazyByteString)
import Data.List (intercalate)
import Data.Time (UTCTime(..), diffUTCTime, addUTCTime, Day(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

-- The 0th-hour as per the BERT spec.
zeroHour = UTCTime (read "1970-01-01") 0

decomposeTime :: UTCTime -> (Int, Int, Int)
decomposeTime t = (mS, s, uS)
  where
    d       = diffUTCTime t zeroHour
    (mS, s) = (floor d) `divMod` 1000000
    uS      = floor $ 1000000 * (snd $ properFraction d)

composeTime :: (Int, Int, Int) -> UTCTime
composeTime (mS, s, uS) = addUTCTime seconds zeroHour
  where
    mS'     = fromIntegral mS
    s'      = fromIntegral s
    uS'     = fromIntegral uS
    seconds = ((mS' * 1000000) + s' + (uS' / 1000000))

fromAtom (AtomTerm a) = a

-- | A single BERT term.
data Term
  -- Simple (erlang) terms:
  = IntTerm        Int
  | FloatTerm      Float
  | AtomTerm       String
  | TupleTerm      [Term]
  | BytelistTerm   ByteString
  | ListTerm       [Term]
  | BinaryTerm     ByteString
  | BigintTerm     Integer
  | BigbigintTerm  Integer
  -- Composite (BERT specific) terms:
  | NilTerm
  | BoolTerm       Bool
  | DictionaryTerm [(Term, Term)]
  | TimeTerm       UTCTime
  | RegexTerm      String [String]
    deriving (Eq, Ord)

-- Another design would be to split the Term type into
-- SimpleTerm|CompositeTerm, and then do everything in one go, but
-- that complicates syntax and semantics for end users. Let's do this
-- one ugly thing instead, eh?
ct b rest = TupleTerm $ [AtomTerm "bert", AtomTerm b] ++ rest
compose NilTerm = ListTerm []
compose (BoolTerm True) = ct "true" []
compose (BoolTerm False) = ct "false" []
compose (DictionaryTerm kvs) = 
  ct "dict" [ListTerm $ map (\(k, v) -> TupleTerm [k, v]) kvs]
compose (TimeTerm t) =
  ct "time" [IntTerm mS, IntTerm s, IntTerm uS]
  where
    (mS, s, uS) = decomposeTime t
compose (RegexTerm s os) = 
  ct "regex" [BytelistTerm (C.pack s), 
              TupleTerm [ListTerm $ map AtomTerm os]]

instance Show Term where
  -- Provide an erlang-compatible 'show' for terms. The results of
  -- this should be parseable as erlang source. 
  show = showTerm

showTerm (IntTerm x) = show x
showTerm (FloatTerm x) = printf "%15.15e" x
showTerm (AtomTerm "") = ""
showTerm (AtomTerm a@(x:xs))
  | isAsciiLower x = a
  | otherwise      = "'" ++ a ++ "'"
showTerm (TupleTerm ts) = 
  "{" ++ intercalate ", " (map showTerm ts) ++ "}"
showTerm (BytelistTerm bs) = show $ C.unpack bs
showTerm (ListTerm ts) = 
  "[" ++ intercalate ", " (map showTerm ts) ++ "]"
showTerm (BinaryTerm b)
  | all (isAscii . chr . fromIntegral) (B.unpack b) = 
      wrap $ "\"" ++ C.unpack b ++ "\""
  | otherwise = 
      wrap $ intercalate ", " $ map show $ B.unpack b
  where
    wrap x = "<<" ++ x ++ ">>"
showTerm (BigintTerm x) = show x
showTerm (BigbigintTerm x) = show x
-- All other terms are composite:
showTerm t = showTerm . compose $ t

class BERT a where
  -- | Introduce a 'Term' from a Haskell value.
  showBERT :: a -> Term
  -- | Attempt to read a haskell value from a 'Term'.
  readBERT :: Term -> (Either String a)

-- Herein are some instances for common Haskell data types. To do
-- anything more complicated, you should make your own instance.

instance BERT Term where
  showBERT = id
  readBERT = return . id

instance BERT Int where
  showBERT = IntTerm
  readBERT (IntTerm value) = return value
  readBERT _ = fail "Invalid integer type"

instance BERT Bool where
  showBERT = BoolTerm
  readBERT (BoolTerm x) = return x
  readBERT _ = fail "Invalid bool type"

instance BERT Integer where
  showBERT = BigbigintTerm
  readBERT (BigintTerm x) = return x
  readBERT (BigbigintTerm x) = return x
  readBERT _ = fail "Invalid integer type"

instance BERT Float where
  showBERT = FloatTerm
  readBERT (FloatTerm value) = return value
  readBERT _ = fail "Invalid floating point type"

instance BERT String where
  showBERT = BytelistTerm . C.pack
  readBERT (BytelistTerm x) = return $ C.unpack x
  readBERT (BinaryTerm x) = return $ C.unpack x
  readBERT (AtomTerm x) = return x
  readBERT (ListTerm xs) = mapM readBERT xs >>= return . map chr
  readBERT _ = fail "Invalid string type"

instance BERT ByteString where
  showBERT = BytelistTerm
  readBERT (BytelistTerm value) = return value
  readBERT _ = fail "Invalid bytestring type"

instance (BERT a) => BERT [a] where
  showBERT xs = ListTerm $ map showBERT xs
  readBERT (ListTerm xs) = mapM readBERT xs
  readBERT _ = fail "Invalid list type"

instance (BERT a, BERT b) => BERT (a, b) where
  showBERT (a, b) = TupleTerm [showBERT a, showBERT b]
  readBERT (TupleTerm [a, b]) = liftM2 (,) (readBERT a) (readBERT b)
  readBERT _ = fail "Invalid tuple(2) type"

instance (BERT a, BERT b, BERT c) => BERT (a, b, c) where
  showBERT (a, b, c) = TupleTerm [showBERT a, showBERT b, showBERT c]
  readBERT (TupleTerm [a, b, c]) = 
    liftM3 (,,) (readBERT a) (readBERT b) (readBERT c)
  readBERT _ = fail "Invalid tuple(3) type"

instance (BERT a, BERT b, BERT c, BERT d) => BERT (a, b, c, d) where
  showBERT (a, b, c, d) = 
    TupleTerm [showBERT a, showBERT b, showBERT c, showBERT d]
  readBERT (TupleTerm [a, b, c, d]) = 
    liftM4 (,,,) (readBERT a) (readBERT b) (readBERT c) (readBERT d)
  readBERT _ = fail "Invalid tuple(4) type"

instance (Ord k, BERT k, BERT v) => BERT (Map k v) where
  showBERT m = DictionaryTerm 
             $ map (\(k, v) -> (showBERT k, showBERT v)) (Map.toList m)
  readBERT (DictionaryTerm kvs) = 
    mapM (\(k, v) -> liftM2 (,) (readBERT k) (readBERT v)) kvs >>=
      return . Map.fromList
  readBERT _ = fail "Invalid map type"

-- Binary encoding & decoding.
instance Binary Term where
  put term = putWord8 131 >> putTerm term
  get      = getWord8 >>= \version ->
               case version of 
                 131 -> getTerm
                 _ -> fail "bad magic"

-- | Binary encoding of a single term (without header)
putTerm (IntTerm value) = tag 98 >> put32i value
putTerm (FloatTerm value) =
  tag 99 >> (putL . C.pack . pad $ printf "%15.15e" value)
  where
    pad s = s ++ replicate (31 - (length s)) '\0'

putTerm (AtomTerm value)
  | len < 256 = tag 100 >> put16i len >> (putL $ C.pack value)
  | otherwise = fail "BERT atom too long (>= 256)"
  where
    len = length value
putTerm (TupleTerm value)
  | len < 256 = tag 104 >> put8i len  >> forM_ value putTerm
  | otherwise = tag 105 >> put32i len >> forM_ value putTerm
  where
    len = length value
putTerm (BytelistTerm value)
  | len < 65536 = tag 107 >> put16i len >> putL value
  | otherwise = do  -- too big: encode as a list.
      tag 108
      put32i len
      forM_ (B.unpack value) $ \v -> do 
        tag 97
        putWord8 v
  where 
    len = B.length value
putTerm (ListTerm value)
  | len == 0 = putNil  -- this is mentioend in the BERT spec.
  | otherwise= do
      tag 108
      put32i $ length value
      forM_ value putTerm
      putNil
  where 
    len = length value
    putNil = putWord8 106
putTerm (BinaryTerm value) = tag 109 >> (put32i $ B.length value) >> putL value
putTerm (BigintTerm value) = tag 110 >> putBigint put8i value
putTerm (BigbigintTerm value) = tag 111 >> putBigint put32i value
-- All other terms are composite:
putTerm t = putTerm . compose $ t

-- | Binary decoding of a single term (without header)
getTerm = do
  tag <- get8i
  case tag of
    97  -> IntTerm <$> get8i
    98  -> IntTerm <$> get32i
    99  -> getL 31 >>= return . FloatTerm . read . C.unpack
    100 -> get16i >>= getL >>= return . AtomTerm . C.unpack
    104 -> get8i >>= getN >>= tupleTerm
    105 -> get32i >>= getN >>= tupleTerm 
    106 -> return $ ListTerm []
    107 -> get16i >>= getL >>= return . BytelistTerm
    108 -> get32i >>= getN >>= return . ListTerm
    109 -> get32i >>= getL >>= return . BinaryTerm
    110 -> getBigint get8i >>= return . BigintTerm . fromIntegral
    111 -> getBigint get32i >>= return . BigintTerm . fromIntegral
  where
    getN n = replicateM n getTerm
    -- First try & decode composite terms.
    tupleTerm [AtomTerm "bert", AtomTerm "true"]  = return $ BoolTerm True
    tupleTerm [AtomTerm "bert", AtomTerm "false"] = return $ BoolTerm False
    tupleTerm [AtomTerm "bert", AtomTerm "dict", ListTerm kvs] =
      mapM toTuple kvs >>= return . DictionaryTerm
      where
        toTuple (TupleTerm [k, v]) = return $ (k, v)
        toTuple _ = fail "invalid dictionary"
    tupleTerm [AtomTerm "bert", AtomTerm "time", 
               IntTerm mS, IntTerm s, IntTerm uS] = 
      return $ TimeTerm $ composeTime (mS, s, uS)
    tupleTerm [AtomTerm "bert", AtomTerm "regex",
               BytelistTerm s, ListTerm os] =
      options os >>= return . RegexTerm (C.unpack s)
      where
        -- TODO: type-check the options values as well
        options []                = return []
        options ((AtomTerm o):os) = options os >>= return . (o:)
        options _                 = fail "regex options must be atoms"
    -- All other tuples are just .. tuples
    tupleTerm xs = return $ TupleTerm xs

putBigint putter value = do
  putter len  -- TODO: verify size?
  if value < 0
    then put8i 1
    else put8i 0
  putL $ B.pack $ map (fromIntegral . digit) [0..len-1]
  where
    value'    = abs value
    len       = ceiling $ logBase 256 (fromIntegral $ value' + 1)
    digit pos = (value' `shiftR` (8 * pos)) .&. 0xFF

getBigint getter = do
  len   <- fromIntegral <$> getter
  sign  <- get8i
  bytes <- getL len
  multiplier <- 
    case sign of 
      0 -> return 1
      1 -> return (-1)
      _ -> fail "Invalid sign byte"
  return $ (*) multiplier
         $ foldl (\s (n, d) -> s + d*(256^n)) 0
         $ zip [0..len-1] (map fromIntegral $ B.unpack bytes)

put8i :: (Integral a) => a -> Put
put8i = putWord8 . fromIntegral
put16i :: (Integral a) => a -> Put
put16i = putWord16be . fromIntegral
put32i :: (Integral a) => a -> Put
put32i = putWord32be . fromIntegral
putL = putLazyByteString

get8i  = fromIntegral <$> getWord8
get16i = fromIntegral <$> getWord16be
get32i = fromIntegral <$> getWord32be
getL :: (Integral a) => a -> Get ByteString
getL = getLazyByteString . fromIntegral

tag :: Word8 -> Put
tag which = putWord8 which
