{-# OPTIONS -fglasgow-exts -cpp #-}
module Japec.AbsSyntax.Parser(parseJapeGrammar) where

import Japec.AbsSyntax
import Japec.AbsSyntax.Lexer
import StringUtils.KeyString as KS
import StringUtils.KeyStringMap as KSM
import StringUtils.SrcLoc (srcLocLine)
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.15

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn4 :: (JapeGrammar_) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (JapeGrammar_)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ([KS.KeyString]) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ([KS.KeyString])
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ([KS.KeyString]) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ([KS.KeyString])
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ([KS.KeyString]) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ([KS.KeyString])
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((JapeControlType,Bool,Bool)) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((JapeControlType,Bool,Bool))
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((JapeControlType,Bool,Bool)) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((JapeControlType,Bool,Bool))
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([JapeRule]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([JapeRule])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (JapeRule) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (JapeRule)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (Int) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (Int)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([[JapePattern]]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([[JapePattern]])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([JapePattern]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([JapePattern])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (JapePattern) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (JapePattern)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ([JapeConstraint]) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ([JapeConstraint])
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (JapeConstraint) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (JapeConstraint)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (JapeAttribute) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (JapeAttribute)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (JapeValue) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (JapeValue)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (KleeneOperator) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (KleeneOperator)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Maybe KeyString) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Maybe KeyString)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([JapeAction]) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([JapeAction])
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (JapeAction) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (JapeAction)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([JapeAssigment]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([JapeAssigment])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (JapeAssigment) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (JapeAssigment)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x46\x00\x83\x00\x70\x00\x6f\x00\x6e\x00\x76\x00\x63\x00\x6b\x00\x72\x00\x6a\x00\x00\x00\x6a\x00\x3f\x00\x69\x00\x00\x00\x68\x00\x00\x00\x00\x00\x6d\x00\x00\x00\x3f\x00\x67\x00\x66\x00\x00\x00\xfe\xff\x6c\x00\x00\x00\x65\x00\x64\x00\xf8\xff\x62\x00\x00\x00\x5e\x00\xf8\xff\x00\x00\x61\x00\xf8\xff\xfd\xff\xfb\xff\x60\x00\x5f\x00\x5d\x00\x5c\x00\x5a\x00\xfd\xff\x00\x00\x0f\x00\x00\x00\xf8\xff\x00\x00\x5b\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x59\x00\x1f\x00\x00\x00\x56\x00\x55\x00\xfd\xff\x00\x00\x00\x00\x00\x00\x57\x00\x58\x00\x00\x00\x00\x00\x00\x00\x54\x00\x00\x00\x53\x00\x00\x00\x00\x00\x0e\x00\x52\x00\x00\x00\x2b\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x4e\x00\x45\x00\x4d\x00\xf9\xff\x43\x00\x00\x00\x00\x00\x00\x00\x41\x00\x40\x00\x3d\x00\x3e\x00\x3b\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x4f\x00\x49\x00\x4c\x00\x00\x00\x4b\x00\x3a\x00\x47\x00\x00\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x00\x00\x00\x00\x00\x00\x00\x10\x00\x42\x00\x00\x00\x00\x00\x44\x00\x24\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x21\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x00\x00\x00\x00\x25\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\xf5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\xfb\xff\xf5\xff\xf7\xff\xfa\xff\xf7\xff\xf1\xff\xf3\xff\xfe\xff\xfb\xff\xfc\xff\xf6\xff\x00\x00\xfd\xff\xf1\xff\x00\x00\x00\x00\xf8\xff\x00\x00\xec\xff\xf2\xff\x00\x00\xf3\xff\x00\x00\x00\x00\xef\xff\xea\xff\xe8\xff\xee\xff\x00\x00\x00\x00\xd0\xff\xe7\xff\x00\x00\xe3\xff\x00\x00\xdf\xff\x00\x00\x00\x00\xe7\xff\x00\x00\xe9\xff\x00\x00\xed\xff\x00\x00\xf4\xff\x00\x00\xeb\xff\xd1\xff\x00\x00\xd7\xff\xe1\xff\x00\x00\xdf\xff\x00\x00\xe6\xff\xe4\xff\xe2\xff\x00\x00\xd4\xff\xda\xff\xd9\xff\xd8\xff\x00\x00\xf0\xff\xd2\xff\xd0\xff\xce\xff\x00\x00\x00\x00\xe5\xff\x00\x00\x00\x00\xe0\xff\xde\xff\xdd\xff\xdb\xff\xdc\xff\xd6\xff\xd5\xff\xcb\xff\xd3\xff\x00\x00\xcc\xff\x00\x00\x00\x00\xcb\xff\xcf\xff\xcd\xff\xca\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x06\x00\x07\x00\x0a\x00\x11\x00\x0d\x00\x09\x00\x0f\x00\x0a\x00\x14\x00\x15\x00\x0d\x00\x10\x00\x0f\x00\x14\x00\x15\x00\x18\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x18\x00\x18\x00\x1c\x00\x0a\x00\x09\x00\x0a\x00\x0b\x00\x0f\x00\x0f\x00\x0f\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x13\x00\x0f\x00\x16\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x11\x00\x12\x00\x13\x00\x0e\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x12\x00\x13\x00\x0c\x00\x0d\x00\x0a\x00\x0b\x00\x06\x00\x07\x00\x06\x00\x07\x00\x0e\x00\x18\x00\x19\x00\x06\x00\x07\x00\x01\x00\x02\x00\x05\x00\x08\x00\x01\x00\x05\x00\x04\x00\x03\x00\x03\x00\x01\x00\x00\x00\x02\x00\x18\x00\x16\x00\x18\x00\x16\x00\xff\xff\x0b\x00\x18\x00\xff\xff\x18\x00\x17\x00\xff\xff\x10\x00\x0b\x00\x18\x00\x0f\x00\x0a\x00\x0c\x00\xff\xff\xff\xff\x03\x00\xff\xff\x0e\x00\xff\xff\x17\x00\x16\x00\xff\xff\xff\xff\x18\x00\x14\x00\x10\x00\x18\x00\x16\x00\x15\x00\x08\x00\x18\x00\x17\x00\x05\x00\x0b\x00\x18\x00\x04\x00\x19\x00\x18\x00\x18\x00\x18\x00\x18\x00\x18\x00\x18\x00\x18\x00\x18\x00\x01\x00\xff\xff\x18\x00\xff\xff\x18\x00\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xce\xff\xce\xff\x61\x00\x4c\x00\x25\x00\x2a\x00\x2d\x00\x24\x00\x5e\x00\x59\x00\x25\x00\x41\x00\x26\x00\x58\x00\x59\x00\x2e\x00\x51\x00\x52\x00\x53\x00\x54\x00\x2b\x00\x27\x00\xce\xff\x24\x00\x1f\x00\x20\x00\x21\x00\x5f\x00\x49\x00\x37\x00\x51\x00\x52\x00\x53\x00\x54\x00\x22\x00\x4f\x00\x38\x00\x4a\x00\x35\x00\x20\x00\x21\x00\x2b\x00\x20\x00\x21\x00\x32\x00\x20\x00\x21\x00\x43\x00\x44\x00\x45\x00\x3f\x00\x57\x00\x47\x00\x3e\x00\x28\x00\x46\x00\x47\x00\x27\x00\x28\x00\x2f\x00\x21\x00\x1a\x00\x14\x00\x13\x00\x14\x00\x39\x00\x55\x00\x56\x00\x16\x00\x17\x00\x03\x00\x05\x00\x33\x00\x1d\x00\x10\x00\x11\x00\x0c\x00\x17\x00\x0a\x00\x0e\x00\x03\x00\x08\x00\x66\x00\x65\x00\x64\x00\x63\x00\x00\x00\x5c\x00\x62\x00\x00\x00\x5b\x00\x5d\x00\x00\x00\x5e\x00\x4c\x00\x5b\x00\x57\x00\x4e\x00\x4f\x00\x00\x00\x00\x00\x08\x00\x00\x00\x39\x00\x00\x00\x4b\x00\x3b\x00\x00\x00\x00\x00\x41\x00\x35\x00\x3e\x00\x46\x00\x3b\x00\x31\x00\x1f\x00\x3c\x00\x3d\x00\x0e\x00\x1c\x00\x2f\x00\x0a\x00\x32\x00\x13\x00\x1d\x00\x19\x00\x1a\x00\x10\x00\x13\x00\x0c\x00\x10\x00\x03\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 54) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54)
	]

happy_n_terms = 29 :: Int
happy_n_nonterms = 22 :: Int

happyReduce_1 = happyReduce 4# 0# happyReduction_1
happyReduction_1 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn4
		 (MultiPhase_ happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_2 = happyReduce 5# 0# happyReduction_2
happyReduction_2 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	case happyOut8 happy_x_4 of { happy_var_4 -> 
	case happyOut10 happy_x_5 of { happy_var_5 -> 
	happyIn4
		 (let (ctrl,debug,matchGroup) = happy_var_4
              in SinglePhase_ happy_var_2 happy_var_3 ctrl debug matchGroup happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_3 = happySpecReduce_2 1# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_4 = happySpecReduce_0 1# happyReduction_4
happyReduction_4  =  happyIn5
		 ([]
	)

happyReduce_5 = happySpecReduce_2 2# happyReduction_5
happyReduction_5 happy_x_2
	happy_x_1
	 =  case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (happy_var_2
	)}

happyReduce_6 = happySpecReduce_0 2# happyReduction_6
happyReduction_6  =  happyIn6
		 ([]
	)

happyReduce_7 = happySpecReduce_2 3# happyReduction_7
happyReduction_7 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	happyIn7
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_8 = happySpecReduce_0 3# happyReduction_8
happyReduction_8  =  happyIn7
		 ([]
	)

happyReduce_9 = happySpecReduce_2 4# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (happy_var_2
	)}

happyReduce_10 = happySpecReduce_0 4# happyReduction_10
happyReduction_10  =  happyIn8
		 (defaultOptions
	)

happyReduce_11 = happyMonadReduce 4# 5# happyReduction_11
happyReduction_11 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOutTok happy_x_3 of { (TIdent happy_var_3) -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	 let (ctrl,debug,matchGroup) = happy_var_4
               in case KSM.lookup happy_var_1 optionKeywords of
                    Nothing          -> alexError "Only control, debug and matchGroup options are supported"
                    Just TControl    -> case KSM.lookup happy_var_3 controlValues of
                                          Nothing   -> alexError "Only appelt, first, brill, once and all control types are supported"
                                          Just ctrl -> return (ctrl,debug,matchGroup)
                    Just TDebug      -> case KSM.lookup happy_var_3 boolValues of
                                          Nothing    -> alexError "debug option must have boolean value"
                                          Just debug -> return (ctrl,debug,matchGroup)
                    Just TMatchGroup -> case KSM.lookup happy_var_3 boolValues of
                                          Nothing         -> alexError "matchGroup option must have boolean value"
                                          Just matchGroup -> return (ctrl,debug,matchGroup)}}}
	) (\r -> happyReturn (happyIn9 r))

happyReduce_12 = happySpecReduce_0 5# happyReduction_12
happyReduction_12  =  happyIn9
		 ((First,False,False)
	)

happyReduce_13 = happySpecReduce_2 6# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_14 = happySpecReduce_0 6# happyReduction_14
happyReduction_14  =  happyIn10
		 ([]
	)

happyReduce_15 = happyMonadReduce 6# 7# happyReduction_15
happyReduction_15 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut22 happy_x_6 of { happy_var_6 -> 
	 alexSrcLoc >>=
                                                                                (\srcLoc -> return (JapeRule happy_var_2 happy_var_3 (srcLocLine srcLoc) happy_var_4 happy_var_6))}}}}
	) (\r -> happyReturn (happyIn11 r))

happyReduce_16 = happySpecReduce_3 7# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (JapePatternMacro happy_var_2 happy_var_3
	)}}

happyReduce_17 = happySpecReduce_3 7# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (JapeActionMacro  happy_var_2 happy_var_3
	)}}

happyReduce_18 = happySpecReduce_2 8# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TInteger happy_var_2) -> 
	happyIn12
		 (happy_var_2
	)}

happyReduce_19 = happySpecReduce_0 8# happyReduction_19
happyReduction_19  =  happyIn12
		 (defaultPriority
	)

happyReduce_20 = happySpecReduce_3 9# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_21 = happySpecReduce_1 9# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ([happy_var_1]
	)}

happyReduce_22 = happySpecReduce_2 10# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 (happy_var_1 : happy_var_2
	)}}

happyReduce_23 = happySpecReduce_1 10# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_24 = happySpecReduce_1 11# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	happyIn15
		 (MacroReference happy_var_1
	)}

happyReduce_25 = happySpecReduce_3 11# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 (BasicPattern   happy_var_2
	)}

happyReduce_26 = happyReduce 5# 11# happyReduction_26
happyReduction_26 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut20 happy_x_4 of { happy_var_4 -> 
	case happyOut21 happy_x_5 of { happy_var_5 -> 
	happyIn15
		 (ComplexPattern happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_27 = happySpecReduce_3 12# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_28 = happySpecReduce_1 12# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ([happy_var_1]
	)}

happyReduce_29 = happySpecReduce_3 13# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (NegativeConstraint happy_var_2 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_2 13# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (PositiveConstraint happy_var_1 happy_var_2
	)}}

happyReduce_31 = happyReduce 4# 14# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOut19 happy_x_4 of { happy_var_4 -> 
	happyIn18
		 (AttributeConstraint happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_32 = happySpecReduce_0 14# happyReduction_32
happyReduction_32  =  happyIn18
		 (NoConstraint
	)

happyReduce_33 = happySpecReduce_1 15# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	happyIn19
		 (case KSM.lookup happy_var_1 boolValues of
                          Nothing  -> IndentifierValue happy_var_1
                          Just val -> BooleanValue val
	)}

happyReduce_34 = happySpecReduce_1 15# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TInteger happy_var_1) -> 
	happyIn19
		 (IntegerValue     happy_var_1
	)}

happyReduce_35 = happySpecReduce_1 15# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TString  happy_var_1) -> 
	happyIn19
		 (StringValue      happy_var_1
	)}

happyReduce_36 = happySpecReduce_1 15# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TFloat happy_var_1) -> 
	happyIn19
		 (FloatValue       happy_var_1
	)}

happyReduce_37 = happySpecReduce_1 16# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn20
		 (KleeneQuery
	)

happyReduce_38 = happySpecReduce_1 16# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn20
		 (KleeneStar
	)

happyReduce_39 = happySpecReduce_1 16# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn20
		 (KleenePlus
	)

happyReduce_40 = happySpecReduce_0 16# happyReduction_40
happyReduction_40  =  happyIn20
		 (KleeneNoop
	)

happyReduce_41 = happySpecReduce_2 17# happyReduction_41
happyReduction_41 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	happyIn21
		 (Just happy_var_2
	)}

happyReduce_42 = happySpecReduce_2 17# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TInteger happy_var_2) -> 
	happyIn21
		 (Just (pack keyStringScope (show happy_var_2))
	)}

happyReduce_43 = happySpecReduce_0 17# happyReduction_43
happyReduction_43  =  happyIn21
		 (Nothing
	)

happyReduce_44 = happySpecReduce_3 18# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1 18# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ([happy_var_1]
	)}

happyReduce_46 = happyMonad2Reduce 3# 19# happyReduction_46
happyReduction_46 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	( \tk -> consumeJavaBlock tk >>= \block -> return (JavaAction (Just happy_var_2) block)) tk}
	) (\r -> happyReturn (happyIn23 r))

happyReduce_47 = happyMonad2Reduce 1# 19# happyReduction_47
happyReduction_47 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (( \tk -> consumeJavaBlock tk >>= \block -> return (JavaAction Nothing   block)) tk
	) (\r -> happyReturn (happyIn23 r))

happyReduce_48 = happyReduce 8# 19# happyReduction_48
happyReduction_48 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (TIdent happy_var_2) -> 
	case happyOutTok happy_x_4 of { (TIdent happy_var_4) -> 
	case happyOut24 happy_x_7 of { happy_var_7 -> 
	happyIn23
		 (AssigmentAction happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_1 19# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	happyIn23
		 (MacroRefAction happy_var_1
	)}

happyReduce_50 = happySpecReduce_3 20# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (happy_var_1 : happy_var_3
	)}}

happyReduce_51 = happySpecReduce_1 20# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ([happy_var_1]
	)}

happyReduce_52 = happySpecReduce_0 20# happyReduction_52
happyReduction_52  =  happyIn24
		 ([]
	)

happyReduce_53 = happySpecReduce_3 21# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOut19 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (JapeAssigment happy_var_1 happy_var_3
	)}}

happyReduce_54 = happyReduce 8# 21# happyReduction_54
happyReduction_54 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TIdent happy_var_1) -> 
	case happyOutTok happy_x_4 of { (TIdent happy_var_4) -> 
	case happyOutTok happy_x_6 of { (TIdent happy_var_6) -> 
	case happyOutTok happy_x_8 of { (TIdent happy_var_8) -> 
	happyIn25
		 (JapeRefAssigment happy_var_1 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	TEOF -> happyDoAction 28# (error "reading EOF!") action sts stk;
	TMultiPhase -> cont 1#;
	TPhase -> cont 2#;
	TPhases -> cont 3#;
	TInput -> cont 4#;
	TOptions -> cont 5#;
	TRule -> cont 6#;
	TMacro -> cont 7#;
	TPriority -> cont 8#;
	TPling -> cont 9#;
	TColon -> cont 10#;
	TAssign -> cont 11#;
	TEqual -> cont 12#;
	TLParen -> cont 13#;
	TRParen -> cont 14#;
	TLCurly -> cont 15#;
	TRCurly -> cont 16#;
	TQuery -> cont 17#;
	TStar -> cont 18#;
	TPlus -> cont 19#;
	TArrow -> cont 20#;
	TBar -> cont 21#;
	TPeriod -> cont 22#;
	TComma -> cont 23#;
	TIdent happy_dollar_dollar -> cont 24#;
	TInteger happy_dollar_dollar -> cont 25#;
	TFloat happy_dollar_dollar -> cont 26#;
	TString  happy_dollar_dollar -> cont 27#;
	_ -> happyError'
	})

happyError_ tk = happyError'

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => Alex a
happyError' = happyError

parseJapeGrammar = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

happyError :: Alex a
happyError = alexError "Parse error"

defaultOptions  = (First,False,False)
defaultPriority = 0
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













{-# LINE 27 "GenericTemplate.hs" #-}



data Happy_IntList = HappyCons Int# Happy_IntList






































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 169 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
