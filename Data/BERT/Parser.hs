-- |
-- Module      : Data.BERT.Parser
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Parse (simple) BERTs.
module Data.BERT.Parser
  ( parseTerm
  ) where

import Data.Char (ord)
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Numeric (readSigned, readFloat, readDec)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.BERT.Types (Term(..))

instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap
instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus

-- | Parse a simple BERT (erlang) term from a string in the erlang
-- grammar. Does not attempt to decompose complex terms.
parseTerm :: String -> Either ParseError Term
parseTerm = parse p_term "term" 

p_term :: Parser Term
p_term = t <* spaces    
  where 
    t =     IntTerm               <$> p_num (readSigned readDec)
        <|> FloatTerm             <$> p_num (readSigned readFloat)
        <|> AtomTerm              <$> p_atom
        <|> TupleTerm             <$> p_tuple
        <|> BytelistTerm . C.pack <$> p_string
        <|> ListTerm              <$> p_list
        <|> BinaryTerm   . B.pack <$> p_binary

p_num which = do
  s <- getInput
  case which s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

p_atom = unquoted <|> quoted
  where
    unquoted = many1 $ lower <|> oneOf ['_', '@']
    quoted   = quote >> many1 letter <* quote
    quote    = char '\''

p_seq open close elem = 
  between (open >> spaces) (spaces >> close) $
    elem `sepBy` (spaces >> char ',' >> spaces)

p_tuple = p_seq (char '{') (char '}') p_term

p_list = p_seq (char '[') (char ']') p_term

p_string = char '"' >> many strchar <* char '"'
  where
    strchar = noneOf ['\\', '"'] <|> (char '\\' >> anyChar)

p_binary = string "<<" >> (bstr <|> bseq) <* string ">>"
  where
    bseq = (p_num readDec) `sepBy` (spaces >> char ',' >> spaces)
    bstr = map (fromIntegral . ord) <$> p_string
