{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LINE 13 "src\Japec\AbsSyntax\Lexer.x" #-}
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

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#else
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
alex_base :: AlexAddr
alex_base = AlexA# "\xf8\xff\xff\xff\xfd\xff\xff\xff\x6d\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\xe3\xff\xff\xff\x00\x00\x00\x00\xe4\xff\xff\xff\xe5\xff\xff\xff\xe6\xff\xff\xff\xe7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xd9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x5e\x00\x00\x00\x68\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\xff\xff\xff\xff\x09\x00\x07\x00\x08\x00\x07\x00\x08\x00\x04\x00\x06\x00\x06\x00\x0e\x00\x11\x00\x0f\x00\x02\x00\x0b\x00\x20\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x12\x00\x13\x00\x17\x00\x18\x00\x1b\x00\x10\x00\x19\x00\x05\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x16\x00\x00\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x14\x00\x1a\x00\x15\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x1e\x00\x00\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1d\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0a\x00\x0a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2a\x00\x2f\x00\x2f\x00\x2f\x00\x3d\x00\x2d\x00\x3e\x00\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\xff\xff\xff\xff\x3d\x00\xff\xff\x3f\x00\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x7c\x00\x7d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x2e\x00\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x5f\x00\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\xff\xff\xff\xff\xff\xff\x03\x00\x03\x00\xff\xff\xff\xff\x0a\x00\x0a\x00\x09\x00\x0a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,32) [[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[(AlexAccSkip)],[],[],[],[],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[],[],[(AlexAcc (alex_action_8))],[(AlexAcc (alex_action_9))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_12))],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_14))],[(AlexAcc (alex_action_15))],[(AlexAcc (alex_action_16))],[(AlexAcc (alex_action_17))],[(AlexAcc (alex_action_18))],[(AlexAcc (alex_action_19))],[],[(AlexAcc (alex_action_20))],[(AlexAcc (alex_action_21))]]
{-# LINE 74 "src\Japec\AbsSyntax\Lexer.x" #-}
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

alex_action_3 = token (const TPling)  
alex_action_4 = token (const TColon)  
alex_action_5 = token (const TAssign) 
alex_action_6 = token (const TEqual)  
alex_action_7 = token (const TArrow)  
alex_action_8 = token (const TLParen) 
alex_action_9 = token (const TRParen) 
alex_action_10 = token (const TLCurly) 
alex_action_11 = token (const TRCurly) 
alex_action_12 = token (const TQuery)  
alex_action_13 = token (const TStar)   
alex_action_14 = token (const TPlus)   
alex_action_15 = token (const TPeriod) 
alex_action_16 = token (const TBar)    
alex_action_17 = token (const TComma)  
alex_action_18 = token (\sb -> TInteger (read (lexemeToString sb))) 
alex_action_19 = token (\sb -> TFloat   (read (lexemeToString sb))) 
alex_action_20 = \sb ->
                                            if currentChar sb == ':'
                                              then let tk  = lexemeToPackedString sb
                                                       kwd = KS.keyString keyStringScope (PS.map toLower tk)
                                                   in case KSM.lookup kwd keywords of
                                                        Nothing -> alexError "Unknown keyword"
                                                        Just t  -> do alexSetInput (stepOn sb)
                                                                      return t
                                              else let kwd = lexemeToKeyString keyStringScope sb
						   in return (TIdent kwd)
                                        
alex_action_21 = lex_string 
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine













{-# LINE 34 "GenericTemplate.hs" #-}













data AlexAddr = AlexA# Addr#

#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
	i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#
#else
  indexInt16OffAddr# arr off
#endif





{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off = 
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
		     (b2 `uncheckedShiftL#` 16#) `or#`
		     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
  indexInt32OffAddr# arr off
#endif





#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif




-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> Maybe (AlexInput,Int,act)
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  case s of 
    -1# -> (last_acc, input)
    _ -> alex_scan_tkn' user orig_input len input s last_acc

alex_scan_tkn' user orig_input len input s last_acc =
  let 
	new_acc = check_accs (alex_accept `quickIndex` (I# (s)))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(I# (ord_c)) = ord c
		offset = (base +# ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >=# 0#) && (check ==# ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	alex_scan_tkn user orig_input (len +# 1#) new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (I# (len))
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastSkip input (I# (len))
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (I# (i)) = i
{-# LINE 2 "AlexWrapper-stringutils" #-}

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = StringBuffer

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar sb = prevChar sb '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar sb
  | atEnd sb  = Nothing
  | otherwise = Just (currentChar sb, stepOn sb)

-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
	alex_inp :: StringBuffer, -- the current input	
	alex_scd :: !Int 	  -- the current startcode
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: StringBuffer -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_inp = input,	
			alex_scd = 0}) of Left msg -> Left msg
					  Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
				Left msg -> Left msg
				Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_inp=sb} -> Right (s, sb)

alexSetInput :: AlexInput -> Alex ()
alexSetInput sb
 = Alex $ \s -> case s{alex_inp=sb} of
		  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> 
  let srcLoc = getSrcLoc (alex_inp s)
      file   = PS.unpack (srcLocFile srcLoc)
      line   = srcLocLine srcLoc
      col    = srcLocCol  srcLoc
  in Left (file++":"++show line++":"++show col++":"++message)

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan (startLexeme inp) sc of
    AlexEOF -> alexEOF
    AlexError inp' -> do
        alexSetInput inp
        alexError "lexical error"
    AlexSkip  inp' len -> do
	alexSetInput inp'
	alexMonadScan
    AlexToken inp' len action -> do
	alexSetInput inp'
	action inp'

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (AlexInput -> token) -> AlexAction token
token t input = return (t input)

