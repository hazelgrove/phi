{-# OPTIONS_GHC -w #-}
module Parser where

import Algo
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,140) ([8960,17,35584,0,62464,16384,0,0,0,0,0,3904,0,0,0,0,7808,2048,0,2304,0,0,0,0,128,0,556,0,4,8192,0,256,16384,0,4656,1,4608,16384,0,1024,0,32,6144,137,8192,9,1024,0,278,61440,8,2048,0,556,0,32,0,0,288,0,1952,0,0,512,0,0,0,122,0,0,0,0,1,2048,0,278,0,0,12800,0,2,1024,0,16384,0,1112,0,16,0,61,0,32768,85,32768,0,1024,6144,137,0,0,8896,0,278,0,0,17792,0,0,0,0,9216,0,288,51200,35,7808,1,512,0,16,9312,2,4387,0,0,0,1,8790,0,64,37248,8,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_expParser","%start_typParser","%start_kndParser","Exp","Typ","Knd","'type'","'let'","'='","'in'","':'","var","bse","'\8853'","'\955'","'::'","' '","'.'","'('","')'","'Type'","'KHole'","'S'","'\928'","%eof"]
        bit_start = st Prelude.* 27
        bit_end = (st Prelude.+ 1) Prelude.* 27
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..26]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_17
action_0 (10) = happyShift action_18
action_0 (14) = happyShift action_4
action_0 (17) = happyShift action_19
action_0 (21) = happyShift action_20
action_0 (6) = happyGoto action_16
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (14) = happyShift action_12
action_1 (15) = happyShift action_13
action_1 (17) = happyShift action_14
action_1 (21) = happyShift action_15
action_1 (7) = happyGoto action_11
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (21) = happyShift action_6
action_2 (23) = happyShift action_7
action_2 (24) = happyShift action_8
action_2 (25) = happyShift action_9
action_2 (26) = happyShift action_10
action_2 (8) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (14) = happyShift action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (27) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (21) = happyShift action_6
action_6 (23) = happyShift action_7
action_6 (24) = happyShift action_8
action_6 (25) = happyShift action_9
action_6 (26) = happyShift action_10
action_6 (8) = happyGoto action_32
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_17

action_8 _ = happyReduce_18

action_9 (21) = happyShift action_6
action_9 (23) = happyShift action_7
action_9 (24) = happyShift action_8
action_9 (25) = happyShift action_9
action_9 (26) = happyShift action_10
action_9 (8) = happyGoto action_31
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_30
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (16) = happyShift action_28
action_11 (19) = happyShift action_29
action_11 (27) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_9

action_13 _ = happyReduce_10

action_14 (14) = happyShift action_27
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_12
action_15 (15) = happyShift action_13
action_15 (17) = happyShift action_14
action_15 (21) = happyShift action_15
action_15 (7) = happyGoto action_26
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (19) = happyShift action_25
action_16 (27) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (19) = happyShift action_24
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_23
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_22
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (9) = happyShift action_17
action_20 (10) = happyShift action_18
action_20 (14) = happyShift action_4
action_20 (17) = happyShift action_19
action_20 (21) = happyShift action_20
action_20 (6) = happyGoto action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (19) = happyShift action_25
action_21 (22) = happyShift action_45
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (13) = happyShift action_44
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (14) = happyShift action_42
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (9) = happyShift action_17
action_25 (10) = happyShift action_18
action_25 (14) = happyShift action_4
action_25 (17) = happyShift action_19
action_25 (21) = happyShift action_20
action_25 (6) = happyGoto action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (16) = happyShift action_28
action_26 (19) = happyShift action_29
action_26 (22) = happyShift action_40
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_39
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_12
action_28 (15) = happyShift action_13
action_28 (17) = happyShift action_14
action_28 (21) = happyShift action_15
action_28 (7) = happyGoto action_38
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_12
action_29 (15) = happyShift action_13
action_29 (16) = happyShift action_37
action_29 (17) = happyShift action_14
action_29 (21) = happyShift action_15
action_29 (7) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (18) = happyShift action_35
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (14) = happyShift action_12
action_31 (15) = happyShift action_13
action_31 (17) = happyShift action_14
action_31 (21) = happyShift action_15
action_31 (7) = happyGoto action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (22) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_21

action_34 (16) = happyShift action_28
action_34 (19) = happyShift action_29
action_34 _ = happyReduce_19

action_35 (21) = happyShift action_6
action_35 (23) = happyShift action_7
action_35 (24) = happyShift action_8
action_35 (25) = happyShift action_9
action_35 (26) = happyShift action_10
action_35 (8) = happyGoto action_51
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (16) = happyShift action_28
action_36 (19) = happyShift action_29
action_36 _ = happyReduce_15

action_37 (19) = happyShift action_50
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (16) = happyShift action_28
action_38 (19) = happyShift action_29
action_38 _ = happyReduce_11

action_39 (21) = happyShift action_6
action_39 (23) = happyShift action_7
action_39 (24) = happyShift action_8
action_39 (25) = happyShift action_9
action_39 (26) = happyShift action_10
action_39 (8) = happyGoto action_49
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_16

action_41 (19) = happyShift action_25
action_41 _ = happyReduce_5

action_42 (19) = happyShift action_48
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (19) = happyShift action_47
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (14) = happyShift action_12
action_44 (15) = happyShift action_13
action_44 (17) = happyShift action_14
action_44 (21) = happyShift action_15
action_44 (7) = happyGoto action_46
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_8

action_46 (16) = happyShift action_28
action_46 (19) = happyShift action_29
action_46 (20) = happyShift action_57
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (13) = happyShift action_56
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (11) = happyShift action_55
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (20) = happyShift action_54
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (14) = happyShift action_12
action_50 (15) = happyShift action_13
action_50 (17) = happyShift action_14
action_50 (21) = happyShift action_15
action_50 (7) = happyGoto action_53
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (20) = happyShift action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (21) = happyShift action_6
action_52 (23) = happyShift action_7
action_52 (24) = happyShift action_8
action_52 (25) = happyShift action_9
action_52 (26) = happyShift action_10
action_52 (8) = happyGoto action_63
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (16) = happyShift action_28
action_53 (19) = happyShift action_29
action_53 _ = happyReduce_12

action_54 (14) = happyShift action_12
action_54 (15) = happyShift action_13
action_54 (17) = happyShift action_14
action_54 (19) = happyShift action_62
action_54 (21) = happyShift action_15
action_54 (7) = happyGoto action_61
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (19) = happyShift action_60
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (19) = happyShift action_59
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (9) = happyShift action_17
action_57 (10) = happyShift action_18
action_57 (14) = happyShift action_4
action_57 (17) = happyShift action_19
action_57 (21) = happyShift action_20
action_57 (6) = happyGoto action_58
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (19) = happyShift action_25
action_58 _ = happyReduce_4

action_59 (14) = happyShift action_12
action_59 (15) = happyShift action_13
action_59 (17) = happyShift action_14
action_59 (21) = happyShift action_15
action_59 (7) = happyGoto action_66
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (14) = happyShift action_12
action_60 (15) = happyShift action_13
action_60 (17) = happyShift action_14
action_60 (21) = happyShift action_15
action_60 (7) = happyGoto action_65
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (16) = happyShift action_28
action_61 (19) = happyShift action_29
action_61 _ = happyReduce_13

action_62 (14) = happyShift action_12
action_62 (15) = happyShift action_13
action_62 (17) = happyShift action_14
action_62 (21) = happyShift action_15
action_62 (7) = happyGoto action_64
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_20

action_64 (16) = happyShift action_28
action_64 (19) = happyShift action_29
action_64 _ = happyReduce_14

action_65 (16) = happyShift action_28
action_65 (19) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (16) = happyShift action_28
action_66 (19) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (11) = happyShift action_70
action_67 (14) = happyShift action_12
action_67 (15) = happyShift action_13
action_67 (16) = happyShift action_37
action_67 (17) = happyShift action_14
action_67 (21) = happyShift action_15
action_67 (7) = happyGoto action_36
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (12) = happyShift action_69
action_68 (14) = happyShift action_12
action_68 (15) = happyShift action_13
action_68 (16) = happyShift action_37
action_68 (17) = happyShift action_14
action_68 (21) = happyShift action_15
action_68 (7) = happyGoto action_36
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (19) = happyShift action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (19) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (9) = happyShift action_17
action_71 (10) = happyShift action_18
action_71 (14) = happyShift action_4
action_71 (17) = happyShift action_19
action_71 (21) = happyShift action_20
action_71 (6) = happyGoto action_74
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (9) = happyShift action_17
action_72 (10) = happyShift action_18
action_72 (14) = happyShift action_4
action_72 (17) = happyShift action_19
action_72 (21) = happyShift action_20
action_72 (6) = happyGoto action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (19) = happyShift action_25
action_73 _ = happyReduce_6

action_74 (19) = happyShift action_75
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (9) = happyShift action_17
action_75 (10) = happyShift action_18
action_75 (12) = happyShift action_76
action_75 (14) = happyShift action_4
action_75 (17) = happyShift action_19
action_75 (21) = happyShift action_20
action_75 (6) = happyGoto action_41
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (19) = happyShift action_77
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (9) = happyShift action_17
action_77 (10) = happyShift action_18
action_77 (14) = happyShift action_4
action_77 (17) = happyShift action_19
action_77 (21) = happyShift action_20
action_77 (6) = happyGoto action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (19) = happyShift action_25
action_78 _ = happyReduce_7

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TKVar happy_var_1))
	 =  HappyAbsSyn6
		 (EVar happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Eλ happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EAp happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 11 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ETypLet happy_var_3 happy_var_7 happy_var_11
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 15 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn6  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (EExpLet happy_var_3 happy_var_7 happy_var_11 happy_var_15
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyTerminal (TKVar happy_var_1))
	 =  HappyAbsSyn7
		 (TVar happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (Bse
	)

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :⊕ happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 :⊕ happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Tλ happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 7 7 happyReduction_14
happyReduction_14 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Tλ happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TAp happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn8
		 (Type
	)

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn8
		 (KHole
	)

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (S happy_var_2 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 8 happyReduction_20
happyReduction_20 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Π happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  8 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TKTypLet -> cont 9;
	TKExpLet -> cont 10;
	TKEq -> cont 11;
	TKIn -> cont 12;
	TKTypAsc -> cont 13;
	TKVar happy_dollar_dollar -> cont 14;
	TKBse -> cont 15;
	TKBinop -> cont 16;
	TKλ -> cont 17;
	TKKndAsc -> cont 18;
	TKAp -> cont 19;
	TKDot -> cont 20;
	TKLP -> cont 21;
	TKRP -> cont 22;
	TKType -> cont 23;
	TKKHole -> cont 24;
	TKS -> cont 25;
	TKΠ -> cont 26;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 27 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
expParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

typParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

kndParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error!\n"

parseExp  :: String -> Exp
parseExp = expParser . alexScanTokens

parseTyp :: String -> Typ
parseTyp = typParser . alexScanTokens

parseKnd :: String -> Knd
parseKnd = kndParser . alexScanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
