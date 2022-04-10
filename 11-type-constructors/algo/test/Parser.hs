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
happyExpList = Happy_Data_Array.listArray (0,124) ([8960,1,35584,0,62464,16384,0,0,0,0,0,3904,0,0,0,0,7808,2048,0,2304,0,0,0,0,128,0,556,0,0,8192,0,256,16384,0,256,0,16,32768,0,36864,4,512,0,139,30720,4,1024,0,278,0,16,0,0,144,0,976,0,0,256,0,0,0,61,0,0,16,32768,0,4448,0,100,1024,0,8,0,128,45056,8,8192,0,31232,0,0,43776,0,256,0,8,4656,0,0,32768,69,11264,2,0,0,139,0,0,0,0,72,16384,2,18320,0,573,0,4,8192,49152,72,17920,2,0,0,512,8192,0,32768,0,291,0,0,0
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
action_6 (8) = happyGoto action_29
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_15

action_8 _ = happyReduce_16

action_9 (21) = happyShift action_6
action_9 (23) = happyShift action_7
action_9 (24) = happyShift action_8
action_9 (25) = happyShift action_9
action_9 (26) = happyShift action_10
action_9 (8) = happyGoto action_28
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (14) = happyShift action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (16) = happyShift action_25
action_11 (19) = happyShift action_26
action_11 (27) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_7

action_13 _ = happyReduce_8

action_14 (14) = happyShift action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_12
action_15 (15) = happyShift action_13
action_15 (17) = happyShift action_14
action_15 (21) = happyShift action_15
action_15 (7) = happyGoto action_23
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (27) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (19) = happyShift action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (13) = happyShift action_40
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_39
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (16) = happyShift action_25
action_23 (19) = happyShift action_26
action_23 (22) = happyShift action_37
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (18) = happyShift action_36
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_12
action_25 (15) = happyShift action_13
action_25 (17) = happyShift action_14
action_25 (21) = happyShift action_15
action_25 (7) = happyGoto action_35
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_12
action_26 (15) = happyShift action_13
action_26 (16) = happyShift action_34
action_26 (17) = happyShift action_14
action_26 (21) = happyShift action_15
action_26 (7) = happyGoto action_33
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_32
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (14) = happyShift action_12
action_28 (15) = happyShift action_13
action_28 (17) = happyShift action_14
action_28 (21) = happyShift action_15
action_28 (7) = happyGoto action_31
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (22) = happyShift action_30
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_19

action_31 (16) = happyShift action_25
action_31 (19) = happyShift action_26
action_31 _ = happyReduce_17

action_32 (21) = happyShift action_6
action_32 (23) = happyShift action_7
action_32 (24) = happyShift action_8
action_32 (25) = happyShift action_9
action_32 (26) = happyShift action_10
action_32 (8) = happyGoto action_46
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (16) = happyShift action_25
action_33 (19) = happyShift action_26
action_33 _ = happyReduce_13

action_34 (19) = happyShift action_45
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (16) = happyShift action_25
action_35 (19) = happyShift action_26
action_35 _ = happyReduce_9

action_36 (21) = happyShift action_6
action_36 (23) = happyShift action_7
action_36 (24) = happyShift action_8
action_36 (25) = happyShift action_9
action_36 (26) = happyShift action_10
action_36 (8) = happyGoto action_44
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_14

action_38 (19) = happyShift action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (14) = happyShift action_12
action_40 (15) = happyShift action_13
action_40 (17) = happyShift action_14
action_40 (21) = happyShift action_15
action_40 (7) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (16) = happyShift action_25
action_41 (19) = happyShift action_26
action_41 (20) = happyShift action_52
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (13) = happyShift action_51
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (11) = happyShift action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (20) = happyShift action_49
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (14) = happyShift action_12
action_45 (15) = happyShift action_13
action_45 (17) = happyShift action_14
action_45 (21) = happyShift action_15
action_45 (7) = happyGoto action_48
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (20) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (21) = happyShift action_6
action_47 (23) = happyShift action_7
action_47 (24) = happyShift action_8
action_47 (25) = happyShift action_9
action_47 (26) = happyShift action_10
action_47 (8) = happyGoto action_58
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (16) = happyShift action_25
action_48 (19) = happyShift action_26
action_48 _ = happyReduce_10

action_49 (14) = happyShift action_12
action_49 (15) = happyShift action_13
action_49 (17) = happyShift action_14
action_49 (19) = happyShift action_57
action_49 (21) = happyShift action_15
action_49 (7) = happyGoto action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (19) = happyShift action_55
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (19) = happyShift action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (9) = happyShift action_17
action_52 (10) = happyShift action_18
action_52 (14) = happyShift action_4
action_52 (17) = happyShift action_19
action_52 (6) = happyGoto action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_4

action_54 (14) = happyShift action_12
action_54 (15) = happyShift action_13
action_54 (17) = happyShift action_14
action_54 (21) = happyShift action_15
action_54 (7) = happyGoto action_61
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (14) = happyShift action_12
action_55 (15) = happyShift action_13
action_55 (17) = happyShift action_14
action_55 (21) = happyShift action_15
action_55 (7) = happyGoto action_60
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (16) = happyShift action_25
action_56 (19) = happyShift action_26
action_56 _ = happyReduce_11

action_57 (14) = happyShift action_12
action_57 (15) = happyShift action_13
action_57 (17) = happyShift action_14
action_57 (21) = happyShift action_15
action_57 (7) = happyGoto action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_18

action_59 (16) = happyShift action_25
action_59 (19) = happyShift action_26
action_59 _ = happyReduce_12

action_60 (16) = happyShift action_25
action_60 (19) = happyShift action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (16) = happyShift action_25
action_61 (19) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (11) = happyShift action_65
action_62 (14) = happyShift action_12
action_62 (15) = happyShift action_13
action_62 (16) = happyShift action_34
action_62 (17) = happyShift action_14
action_62 (21) = happyShift action_15
action_62 (7) = happyGoto action_33
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (12) = happyShift action_64
action_63 (14) = happyShift action_12
action_63 (15) = happyShift action_13
action_63 (16) = happyShift action_34
action_63 (17) = happyShift action_14
action_63 (21) = happyShift action_15
action_63 (7) = happyGoto action_33
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (19) = happyShift action_67
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (19) = happyShift action_66
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (9) = happyShift action_17
action_66 (10) = happyShift action_18
action_66 (14) = happyShift action_4
action_66 (17) = happyShift action_19
action_66 (6) = happyGoto action_69
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (9) = happyShift action_17
action_67 (10) = happyShift action_18
action_67 (14) = happyShift action_4
action_67 (17) = happyShift action_19
action_67 (6) = happyGoto action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_5

action_69 (19) = happyShift action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (12) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (19) = happyShift action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (9) = happyShift action_17
action_72 (10) = happyShift action_18
action_72 (14) = happyShift action_4
action_72 (17) = happyShift action_19
action_72 (6) = happyGoto action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_6

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

happyReduce_5 = happyReduce 11 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn6  happy_var_11) `HappyStk`
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

happyReduce_6 = happyReduce 15 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_15) `HappyStk`
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

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyTerminal (TKVar happy_var_1))
	 =  HappyAbsSyn7
		 (TVar happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (Bse
	)

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 :⊕ happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_1 :⊕ happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 6 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Tλ happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 7 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn7  happy_var_7) `HappyStk`
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

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TAp happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (Type
	)

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn8
		 (KHole
	)

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (S happy_var_2 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 6 8 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Π happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

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
