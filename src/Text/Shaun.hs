module Text.Shaun
  ( ShaunValue(..)
  , Shaun(..)
  , prout
  )
where

import Text.Shaun.Lexer
import Text.Shaun.Parser
import Text.Shaun.Types
import Text.Shaun.Sweeper

import Data.List (intersperse, concat)
import Data.Maybe (fromMaybe)

import Control.Monad
import Data.ByteString.Char8 (pack)

indent :: String -> String
indent s = l ++ "\n" ++ unlines (map ("  " ++) ls)
  where (l:ls) = lines s

instance Show ShaunValue where
  show (SBool b) = if b then "true" else "false"
  show (SString s) = '"' : s ++ "\""
  show (SNumber n mu) = show n ++ fromMaybe "" mu

  show (SObject l) = "\n{ " ++ indent ((foldl showObj "" l)) ++ "}"
    where
      showObj prev (k,v) = prev ++ k ++ ": " ++ show v ++ "\n"

  show (SList l) = "\n[ " ++ indent (unlines (map show l)) ++ "]"

instance Read ShaunValue where
  readsPrec _ = \s ->
   case makeLexer (pack s) >>= parseShaun of
      Left _ -> []
      Right r -> [(r, "")]