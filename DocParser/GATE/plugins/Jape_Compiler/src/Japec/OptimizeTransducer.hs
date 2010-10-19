-----------------------------------------------------------------------------
-- |
-- Module      : Japec.OptimizeTransducer
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- The @OptimizeTransducer@ module is the front door
-- to all optimizations steps in the JAPE compiler.
-- The steps are standard algorithms over FSM automatas:
-- 
--    * epsilon closure
--
--    * determinisation
--
--    * minimization
--
-----------------------------------------------------------------------------

module Japec.OptimizeTransducer(optimizeTransducer) where

import Japec.CmdFlags
import Japec.TSyntax
import Japec.OptimizeTransducer.EpsilonClosure
import Japec.OptimizeTransducer.Determinisation
import Japec.OptimizeTransducer.Minimization

-- | @optimizeTransducer@ performs all optimization steps over the given transducer
optimizeTransducer :: FinalPhase -> Transducer -> Transducer
optimizeTransducer phase = map (optimizeTransducerPhase phase)

optimizeTransducerPhase phase (TransducerPhase name input control debug match tgraph) =
  TransducerPhase name input control debug match tgraph'
  where
    tgraph1 = epsilonClosure  tgraph
    tgraph2 = determinisation tgraph1
    tgraph3 = minimization    tgraph2

    tgraph' =
       case phase of
         Parsed     -> tgraph
    	 Epsilon    -> tgraph1
    	 Determined -> tgraph2
	 Minimized  -> tgraph3
