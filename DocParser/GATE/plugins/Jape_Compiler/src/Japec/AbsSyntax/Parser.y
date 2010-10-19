-----------------------------------------------------------------------------
-- |
-- Module      : Japec.AbsSyntax.Parser
-- Copyright   : (C) Ontotext 2005
-- License     : LGPL
--
-- Maintainer  : Krasimir Angelov <kr.angelov@sirma.bg>
-- Stability   : beta
-- Portability : portable
--
-----------------------------------------------------------------------------

{
module Japec.AbsSyntax.Parser(parseJapeGrammar) where

import Japec.AbsSyntax
import Japec.AbsSyntax.Lexer
import StringUtils.KeyString as KS
import StringUtils.KeyStringMap as KSM
import StringUtils.SrcLoc (srcLocLine)

}

%token
  'multiphase:'{ TMultiPhase }
  'phase:'     { TPhase      }
  'phases:'    { TPhases     }
  'input:'     { TInput      }
  'options:'   { TOptions    }
  'rule:'      { TRule       }
  'macro:'     { TMacro      }
  'priority:'  { TPriority   }
  '!'          { TPling      }
  ':'          { TColon      }
  '='          { TAssign     }
  '=='         { TEqual      }
  '('          { TLParen     }
  ')'          { TRParen     }
  '{'          { TLCurly     }
  '}'          { TRCurly     }
  '?'          { TQuery      }
  '*'          { TStar       }
  '+'          { TPlus       }
  '-->'        { TArrow      }
  '|'          { TBar        }
  '.'          { TPeriod     }
  ','          { TComma      }
  identifier   { TIdent $$   }
  integer      { TInteger $$ }
  float        { TFloat $$   }
  string       { TString  $$ }

%monad { Alex } { >>= } { return }
%lexer { lexer } { TEOF }
%name parseJapeGrammar jape
%tokentype { Token }
%%

jape :: { JapeGrammar_ }
     : 'multiphase:' identifier 'phases:' phases_list { MultiPhase_ $2 $4 }
     | 'phase:' identifier input_clause options_clause rules_list 
            { let (ctrl,debug,matchGroup) = $4
              in SinglePhase_ $2 $3 ctrl debug matchGroup $5
            }

phases_list :: { [KS.KeyString] }
phases_list 
     : identifier phases_list { $1 : $2 }
     |                        { []      }

input_clause :: { [KS.KeyString] }
     : 'input:' identifier_list { $2 }
     |                          { [] }

identifier_list :: { [KS.KeyString] }
     : identifier identifier_list { $1 : $2 }
     |                            { []      }

options_clause :: { (JapeControlType,Bool,Bool) }
     : 'options:' options_list { $2 }
     |                         { defaultOptions }

options_list :: { (JapeControlType,Bool,Bool) }
     : identifier '=' identifier options_list 
            {% let (ctrl,debug,matchGroup) = $4
               in case KSM.lookup $1 optionKeywords of
                    Nothing          -> alexError "Only control, debug and matchGroup options are supported"
                    Just TControl    -> case KSM.lookup $3 controlValues of
                                          Nothing   -> alexError "Only appelt, first, brill, once and all control types are supported"
                                          Just ctrl -> return (ctrl,debug,matchGroup)
                    Just TDebug      -> case KSM.lookup $3 boolValues of
                                          Nothing    -> alexError "debug option must have boolean value"
                                          Just debug -> return (ctrl,debug,matchGroup)
                    Just TMatchGroup -> case KSM.lookup $3 boolValues of
                                          Nothing         -> alexError "matchGroup option must have boolean value"
                                          Just matchGroup -> return (ctrl,debug,matchGroup)
            }
     |      { (First,False,False) }

rules_list :: { [JapeRule] }
     : rule_clause rules_list { $1 : $2 }
     |                        { []      }

rule_clause :: { JapeRule }
     : 'rule:' identifier priority_clause constraint_group '-->' action_list {% alexSrcLoc >>=
                                                                                (\srcLoc -> return (JapeRule $2 $3 (srcLocLine srcLoc) $4 $6)) }
     | 'macro:' identifier constraint_group { JapePatternMacro $2 $3 }
     | 'macro:' identifier action           { JapeActionMacro  $2 $3 }

priority_clause :: { Int }
     : 'priority:' integer { $2              }
     |                     { defaultPriority }

constraint_group :: { [[JapePattern]] }
     : pattern_elements '|' constraint_group { $1 : $3 }
     | pattern_elements                      { [$1]    }

pattern_elements :: { [JapePattern] }
     : pattern_element pattern_elements { $1 : $2 }
     | pattern_element                  { [$1]    }

pattern_element :: { JapePattern }
     : identifier                 { MacroReference $1 }
     | '{' constraint_list '}'    { BasicPattern   $2 }
     | '(' constraint_group ')' kleene_operator binding_name
                                  { ComplexPattern $2 $4 $5 }

constraint_list :: { [JapeConstraint] }
     : constraint ',' constraint_list { $1 : $3 }
     | constraint                     { [$1]    }

constraint :: { JapeConstraint }
     : '!' identifier attribute  { NegativeConstraint $2 $3 }
     | identifier attribute      { PositiveConstraint $1 $2 }

attribute :: { JapeAttribute }
     : '.' identifier '==' attribute_value { AttributeConstraint $2 $4 }
     |                                     { NoConstraint              }

attribute_value :: { JapeValue }
     : identifier     { case KSM.lookup $1 boolValues of
                          Nothing  -> IndentifierValue $1
                          Just val -> BooleanValue val    }
     | integer        { IntegerValue     $1 }
     | string         { StringValue      $1 }
     | float          { FloatValue       $1 }

kleene_operator :: { KleeneOperator }
     : '?'  { KleeneQuery }
     | '*'  { KleeneStar  }
     | '+'  { KleenePlus  }
     |      { KleeneNoop  }

binding_name :: { Maybe KeyString }
     : ':' identifier { Just $2 }
     | ':' integer    { Just (pack keyStringScope (show $2)) }
     |                { Nothing }

action_list :: { [JapeAction] }
     : action ',' action_list { $1 : $3 }
     | action                 { [$1]    }

action :: { JapeAction }
     : ':' identifier '{'   {%% \tk -> consumeJavaBlock tk >>= \block -> return (JavaAction (Just $2) block) }
     | '{'                  {%% \tk -> consumeJavaBlock tk >>= \block -> return (JavaAction Nothing   block) }
     | ':' identifier '.' identifier '=' '{' assigment_list '}'
                               { AssigmentAction $2 $4 $7 }
     | identifier              { MacroRefAction $1        }

assigment_list :: { [JapeAssigment] }
     : assigment ',' assigment_list { $1 : $3 }
     | assigment                    { [$1]    }
     |                              { []      }

assigment :: { JapeAssigment }
     : identifier '=' attribute_value { JapeAssigment $1 $3 }
     | identifier '=' ':' identifier '.' identifier '.' identifier { JapeRefAssigment $1 $4 $6 $8 }

{

happyError :: Alex a
happyError = alexError "Parse error"

defaultOptions  = (First,False,False)
defaultPriority = 0

}
