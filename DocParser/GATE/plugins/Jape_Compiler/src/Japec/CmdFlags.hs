-----------------------------------------------------------------------------
-- |
-- Module      : Japec.CmdFlags
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The module contains the definition of the command line interface
-- to the JAPE compiler
--
-----------------------------------------------------------------------------

module Japec.CmdFlags
         ( CmdFlags(..)
         , OutputFormat(..)
         , FinalPhase(..)
         , parseCmdFlags
         , japecUsageInfo
         ) where

import System.Console.GetOpt

data CmdFlags
  = CmdFlags
      { outputFormat :: OutputFormat
      , finalPhase   :: FinalPhase
      , showHelp     :: Bool
      , outputDir    :: FilePath
      , package      :: String
      }

data OutputFormat
  = Dot
  | PS
  | Jpeg
  | Japec
  | Java
  deriving Eq
  
data FinalPhase
  = Parsed
  | Epsilon
  | Determined
  | Minimized
  deriving Eq

parseCmdFlags :: [String] -> (CmdFlags,[String],[String])
parseCmdFlags args = (foldr ($) (CmdFlags Java Minimized False "." "") fs, non_opts, errs)
  where
    (fs, non_opts, errs)      = getOpt Permute options args

japecUsageInfo :: String -> String
japecUsageInfo prgName =
  usageInfo ("JAPE grammars compiler\n" ++ prgName ++ " <path> <options>") options

options = 
  [ Option ['?'] ["help"]       (NoArg (setHelp   True))       "Display this help screen"
    -- Output formats
  , Option []    ["dot"]        (NoArg (setFormat Dot ))       "Dump the transducer graph in .dot format"
  , Option []    ["ps"]         (NoArg (setFormat PS  ))       "Dump the transducer graph in .ps format"
  , Option []    ["jpeg"]       (NoArg (setFormat Jpeg))       "Dump the transducer graph in .jpeg format"
  , Option []    ["japec"]      (NoArg (setFormat Japec))      "Translate the transducer to intermediate Japec code"
  , Option []    ["java"]       (NoArg (setFormat Java))       "Translate the transducer to Java code"

    -- The final phase
  , Option []    ["parse"]      (NoArg (setPhase  Parsed))     "Stop after parsing"
  , Option []    ["epsilon"]    (NoArg (setPhase  Epsilon))    "Stop after epsilon closure"
  , Option []    ["determined"] (NoArg (setPhase  Determined)) "Stop after determinisation"
  , Option []    ["minimized"]  (NoArg (setPhase  Minimized))  "Stop after minimization"  
  
    -- File options
  , Option ['d'] ["odir"]       (ReqArg setDir "<dir>")        "Specify the output directory"

    -- Code generation
  , Option ['p'] ["package"]    (ReqArg setPkg "<package>")    "Set Java package"
  ]
  where
    setFormat format flags = flags{outputFormat=format}
    setPhase  phase  flags = flags{finalPhase  =phase }
    setHelp   help   flags = flags{showHelp    =help  }
    setDir    dir    flags = flags{outputDir   =dir   }
    setPkg    pkg    flags = flags{package     =pkg   }
