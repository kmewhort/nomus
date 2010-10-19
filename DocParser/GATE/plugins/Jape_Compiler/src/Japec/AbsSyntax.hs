-----------------------------------------------------------------------------
-- |
-- Module      : Japec.AbsSyntax
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-- Representation of the abstract JAPE syntax.
--
-----------------------------------------------------------------------------

module Japec.AbsSyntax
         ( JapeGrammar(..)
         , JapeGrammar_(..)
         , JapeRule(..)
         , Priority, FileLine
         , JapeControlType(..)
         , JapePattern(..)
         , JapeConstraint(..)
         , JapeAttribute(..)
         , JapeValue(..)
         , KleeneOperator(..)
         , JapeAction(..)
         , JapeAssigment(..)
         ) where

import StringUtils.KeyString as KS
import StringUtils.KeyStringMap as KSM
import StringUtils.PackedString as PS

-- | @JapeGrammar_@ is quite similar to 'JapeGrammar' but the
-- difference is that the @MultiPhase_@ contains list of phase names
-- instead of the real phases. The reason is that the parser is pure
-- function and it can't read the sub-phases. This type is used only
-- in the parser and it is converted to 'JapeGrammar' after that.
data JapeGrammar_
  = MultiPhase_ KS.KeyString [KS.KeyString]
  | SinglePhase_ KS.KeyString [KS.KeyString] JapeControlType Bool Bool [JapeRule]

-- | The @JapeGrammar@ type is always the root of the JAPE grammar abstract tree. 
-- The grammar is either multi or single phase transducer.
data JapeGrammar
  = MultiPhase String [JapeGrammar]      -- ^ Represents multi phase transducer. 
                                         -- Contains its name and a list of grammars, where
                                         -- each grammar in the list is one of the phases.
  | SinglePhase String [KS.KeyString] JapeControlType Bool Bool [JapeRule]
                                         -- ^ Represents single phase transducer.
					 -- Contains its name, list of input annotation types and a list of rules.

type Priority = Int
type FileLine = Int

-- | The @JapeRule@ represents either single JAPE rule or single macro definition.
data JapeRule
  = JapeRule  KS.KeyString Priority FileLine [[JapePattern]] [JapeAction] 
                                               -- ^ JAPE rule. The rule has name, priority, left hand side (LHS) and 
                                               -- right hand side (RHS). Since the LHS is a disjunction of conjunctions 
                                               -- of patterns it is represented as list of lists of patterns.
                                               -- The RHS can contain zero of few Java actions so it is represented as
                                               -- list of actions.
  | JapePatternMacro KS.KeyString    [[JapePattern]]
                                               -- ^ JAPE pattern macro. The macro has name and content. The content syntax 
                                               -- is similar to the LHS in the rules, so it has the same representation.
  | JapeActionMacro KS.KeyString     JapeAction
                                               -- ^ JAPE action macro. The macro has name and content. The content syntax 
                                               -- is similar to the RHS in the rules, so it has the same representation.

-- | The @JapeControlType@ represents the rule activation strategies
data JapeControlType
  = Appelt
  | First
  | Brill
  | Once
  | All

-- | The @JapePattern@ represents a single pattern in given rule
data JapePattern
  = MacroReference KS.KeyString                -- ^ The macro reference is generated when the parser see any macro name 
                                               -- in the LHS of the rule.
  | BasicPattern   [JapeConstraint]            -- ^ The basic pattern is a annotation type and list of features enclosed
                                               -- with curly braces
  | ComplexPattern [[JapePattern]] KleeneOperator (Maybe KS.KeyString)
                                               -- ^ The complex pattern is generated when there is a sub-pattern enclosed 
                                               -- parens. The JAPE syntax allows to use \*, \? or \+ operator after the
                                               -- right paren. The operator is represented with @KleeneOperator@ type.
                                               -- If there is a variable binding then the last constructor argument is 
                                               -- @Just <binding name>@.

-- | The @JapeConstraint@ is the content of the basic pattern.
-- Usually the basic pattern is of the form: @{Token}@ or @{Lookup.class = "..."}@.
-- Token and Lookup are the annotation types and they are represented as 'KeyString'
-- in the @JapeConstraint@. If there is a feature constraint, then the 'JapeAttribute'
-- has value ('AttributeConstraint' ...), otherwise the value is 'NoConstraint'.
data JapeConstraint
  = NegativeConstraint KS.KeyString JapeAttribute       -- ^ The constraint might be negative i.e. the operator might be @!=@
                                                        -- instead of @=@
  | PositiveConstraint KS.KeyString JapeAttribute       -- ^ Represents the usual positive constraint
  deriving Show

-- | Represents feature contraints in the basic pattern.
data JapeAttribute
  = AttributeConstraint KS.KeyString JapeValue          -- ^ See 'JapeConstraint'
  | NoConstraint                                        -- ^ There isn't any constraint
  deriving Show

-- | The value in the right hand side of the feature contraint
-- in the basic patterns can be: identifier, integer or string.
data JapeValue
  = IndentifierValue KS.KeyString       -- ^ identifier constraint
  | IntegerValue     Int                -- ^ integer constraint
  | StringValue      String             -- ^ string constraint
  | BooleanValue     Bool
  | FloatValue       Double
  deriving (Eq,Ord,Show)

-- | Kleene operator i.e.: \*, \+, \?. If there isn't any operator then
-- 'KleeneNoop' is used.
data KleeneOperator
  = KleeneQuery
  | KleeneStar
  | KleenePlus
  | KleeneNoop

-- | RHS in the rule
data JapeAction
  = JavaAction (Maybe KS.KeyString) PS.PackedString
                                      -- ^ arbitrary jave action
  | AssigmentAction KS.KeyString KS.KeyString [JapeAssigment]
                                      -- ^ syntax sugar for new annotation creation
  | MacroRefAction  KS.KeyString

-- | Represents single feature name and value pair in given annotation
data JapeAssigment
  = JapeAssigment    KS.KeyString JapeValue
  | JapeRefAssigment KS.KeyString KS.KeyString KS.KeyString KS.KeyString
