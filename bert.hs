-- |
-- Copyright   : (c) marius a. eriksen 2009
-- 
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- Tool to issue or serve BERT requests.
module Main where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Data.Maybe (maybe)
import Text.Printf (printf)
import Data.BERT (Term(..))
import qualified Network.BERT as BERT

data Flags
  = Help
    deriving (Show, Eq, Ord)

data Mode
  = Call String String String [Term]
  | Serve Int
    deriving (Show, Eq, Ord)

options =   
  [ Option ['h'] [] (NoArg Help) "show help"
  ]

usage = do
  header <- getProgName >>= 
    return . printf ("Usage: %s [OPTION...] " ++
                     "[call <uri> <mod> <fun> [args..]|serve PORT]" )
  return $ usageInfo header options

parseArgs argv = 
  case getOpt Permute options argv of
    (o, n, []) -> 
      if Help `elem` o
        then return $ Nothing
        else do
          m <- parse n
          return $ Just (o, m)
  where
    parse ("call":uri:mod:fun:args) = return $ Call uri mod fun (map read args)
    parse ["serve", port]           = return $ Serve (read port)
    parse _                         = help "Cannot parse command"

    help = fail . flip (++) " [-h for help]"

main = do
  args <- getArgs >>= parseArgs
  case args of
    Just (_, Serve port)            -> doServe port
    Just (_, Call uri mod fun args) -> doCall uri mod fun args
    Nothing                         -> usage >>= putStr
    
doServe = undefined

doCall :: String -> String -> String -> [Term] -> IO ()
doCall uri mod fun args = do
  t <- BERT.fromURI uri
  r <- BERT.call t mod fun args :: BERT.Call Term
  case r of
    Right res  -> putStrLn $ printf "reply: %s" $ show res
    Left error -> putStrLn $ printf "error: %s" $ show error
  return ()
