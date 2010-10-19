-----------------------------------------------------------------------------
-- |
-- Module      : Japec.AbsSyntax.Lexer
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-----------------------------------------------------------------------------

{
module Japec.AbsSyntax.Lexer 
        ( Token(..), Alex, alexError, runAlex, lexer
        , optionKeywords, controlValues, boolValues, keyStringScope
        , consumeJavaBlock, alexSrcLoc
        ) where

import Japec.AbsSyntax
import qualified StringUtils.PackedString as PS
import qualified StringUtils.KeyString as KS
import qualified StringUtils.KeyStringMap as KSM
import StringUtils.StringBuffer
import StringUtils.SrcLoc
import System.IO.Unsafe
import Data.Char (toLower)
import Debug.Trace
}

%wrapper "stringutils"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

@nss = ([\0-\255] # [\*])*(\*)+
@bcomment = \/\*@nss(([\0-\255] # [\*\/])@nss)*\/

tokens :-

  $white+				;
  "//".*				;
  @bcomment                             ;
  "!"                                   { token (const TPling)  }
  ":"                                   { token (const TColon)  }
  "="                                   { token (const TAssign) }
  "=="                                  { token (const TEqual)  }
  "-->"                                 { token (const TArrow)  }
  "("                                   { token (const TLParen) }
  ")"                                   { token (const TRParen) }
  "{"                                   { token (const TLCurly) }
  "}"                                   { token (const TRCurly) }
  "?"                                   { token (const TQuery)  }
  "*"                                   { token (const TStar)   }
  "+"                                   { token (const TPlus)   }
  "."                                   { token (const TPeriod) }
  "|"                                   { token (const TBar)    }
  ","                                   { token (const TComma)  }
  $digit+				{ token (\sb -> TInteger (read (lexemeToString sb))) }
  $digit+\.$digit+			{ token (\sb -> TFloat   (read (lexemeToString sb))) }
  $alpha [$alpha $digit \- \_ \']*	{ \sb ->
                                            if currentChar sb == ':'
                                              then let tk  = lexemeToPackedString sb
                                                       kwd = KS.keyString keyStringScope (PS.map toLower tk)
                                                   in case KSM.lookup kwd keywords of
                                                        Nothing -> alexError "Unknown keyword"
                                                        Just t  -> do alexSetInput (stepOn sb)
                                                                      return t
                                              else let kwd = lexemeToKeyString keyStringScope sb
						   in return (TIdent kwd)
                                        }
  \"                                    { lex_string }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = TMultiPhase
  | TPhase
  | TPhases
  | TInput
  | TOptions
  | TRule
  | TMacro
  | TPriority
  | TControl
  | TDebug
  | TMatchGroup
  | TPling
  | TColon
  | TAssign
  | TEqual
  | TLParen
  | TRParen
  | TLCurly
  | TRCurly
  | TQuery
  | TStar
  | TPlus
  | TArrow
  | TPeriod
  | TBar
  | TComma
  | TIdent KS.KeyString
  | TInteger Int
  | TFloat Double
  | TString String
  | TEOF
  deriving (Eq,Show)

keyStringScope :: KS.KeyStringScope
keyStringScope = unsafePerformIO KS.newScope
{-# NOINLINE keyStringScope #-}

mkKeyword s v = (KS.pack keyStringScope s, v)

keywords = KSM.fromList
  [ mkKeyword "multiphase" TMultiPhase
  , mkKeyword "phase"      TPhase
  , mkKeyword "phases"     TPhases
  , mkKeyword "input"      TInput
  , mkKeyword "options"    TOptions
  , mkKeyword "rule"       TRule
  , mkKeyword "macro"      TMacro
  , mkKeyword "priority"   TPriority
  ]

optionKeywords = KSM.fromList
  [ mkKeyword "control"    TControl
  , mkKeyword "debug"      TDebug
  , mkKeyword "matchGroup" TMatchGroup
  ]

controlValues = KSM.fromList
  [ mkKeyword "appelt" Appelt
  , mkKeyword "first"  First
  , mkKeyword "brill"  Brill
  , mkKeyword "once"   Once
  , mkKeyword "all"    All
  ]

boolValues = KSM.fromList
  [ mkKeyword "true"  True
  , mkKeyword "false" False
  , mkKeyword "yes"   True
  , mkKeyword "no"    False
  , mkKeyword "y"     True
  , mkKeyword "n"     False
  ]

alexEOF = return TEOF

lexer :: (Token -> Alex a) -> Alex a
lexer cont = alexMonadScan >>= cont

lex_string sb = lex_string "" sb
  where
    nonTerminatedError = alexError "Nonterminated string constant"
    
    lex_string cs sb
      | atEnd sb  = nonTerminatedError
      | otherwise =
          case currentChar sb of
            '"' -> do alexSetInput (stepOn sb)
                      return (TString (reverse cs))
            '\\'-> let sb'  = stepOn sb
                       sb'' = stepOn sb'
                   in if atEnd sb'
                        then nonTerminatedError
                        else
                          case currentChar sb' of
                            'n' -> lex_string ('\n':cs) sb''
                            'r' -> lex_string ('\r':cs) sb''
                            't' -> lex_string ('\t':cs) sb''
                            'b' -> lex_string ('\b':cs) sb''
                            'v' -> lex_string ('\v':cs) sb''
                            'f' -> lex_string ('\f':cs) sb''
                            '"' -> lex_string ('"' :cs) sb''
                            '\n'-> nonTerminatedError
                            c   -> alexError "Invalid string escape character"
            '\n'-> nonTerminatedError
            c   -> lex_string (c:cs) (stepOn sb)
            
consumeJavaBlock :: Token -> Alex PS.PackedString
consumeJavaBlock TRCurly = return PS.empty
consumeJavaBlock tk      = do
  sb <- alexGetInput
  sb <- consume 1 sb
  alexSetInput sb
  return (PS.init (lexemeToPackedString sb))
  where
    consume n sb
      | atEnd sb = alexError "Nonterminated Java block"
      | otherwise =
          case currentChar sb of
            '{' -> consume (n+1) sb'
            '}' | n == 1    -> return sb'
                | otherwise -> consume (n-1) sb'
            _   | n == 0    -> alexError "'{' expected"
                | otherwise -> consume n sb'
          where
            sb' = stepOn sb

alexSrcLoc :: Alex SrcLoc
alexSrcLoc = do
  sb <- alexGetInput
  return (getSrcLoc sb)
}