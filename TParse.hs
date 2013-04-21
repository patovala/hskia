{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module TParse
( tParse
, parseFile
, pretty
, prex
, echo
, echoFile
) where

import Syntax
import Data.Char (ord,isSpace,isDigit,isLower,isUpper,isAlphaNum)
import Data.List (intersperse)

-- parser produced by Happy Version 1.18.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Stmt])
	| HappyAbsSyn6 (Stmt)
	| HappyAbsSyn7 (Exp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (20) = happyShift action_4
action_0 (22) = happyShift action_5
action_0 (24) = happyShift action_6
action_0 (26) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (20) = happyShift action_4
action_1 (22) = happyShift action_5
action_1 (24) = happyShift action_6
action_1 (26) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (20) = happyShift action_4
action_2 (22) = happyShift action_5
action_2 (24) = happyShift action_6
action_2 (26) = happyShift action_7
action_2 (6) = happyGoto action_17
action_2 _ = happyReduce_1

action_3 _ = happyReduce_3

action_4 (15) = happyShift action_16
action_4 _ = happyFail

action_5 (15) = happyShift action_15
action_5 _ = happyFail

action_6 (15) = happyShift action_11
action_6 (23) = happyShift action_12
action_6 (25) = happyShift action_13
action_6 (26) = happyShift action_14
action_6 (7) = happyGoto action_10
action_6 _ = happyFail

action_7 (14) = happyShift action_9
action_7 _ = happyFail

action_8 (27) = happyAccept
action_8 _ = happyFail

action_9 (15) = happyShift action_11
action_9 (23) = happyShift action_12
action_9 (25) = happyShift action_13
action_9 (26) = happyShift action_14
action_9 (7) = happyGoto action_28
action_9 _ = happyFail

action_10 (8) = happyShift action_21
action_10 (9) = happyShift action_22
action_10 (10) = happyShift action_23
action_10 (11) = happyShift action_24
action_10 (12) = happyShift action_25
action_10 (13) = happyShift action_26
action_10 (19) = happyShift action_27
action_10 _ = happyFail

action_11 (15) = happyShift action_11
action_11 (23) = happyShift action_12
action_11 (25) = happyShift action_13
action_11 (26) = happyShift action_14
action_11 (7) = happyGoto action_20
action_11 _ = happyFail

action_12 _ = happyReduce_18

action_13 _ = happyReduce_9

action_14 _ = happyReduce_10

action_15 (15) = happyShift action_11
action_15 (23) = happyShift action_12
action_15 (25) = happyShift action_13
action_15 (26) = happyShift action_14
action_15 (7) = happyGoto action_19
action_15 _ = happyFail

action_16 (15) = happyShift action_11
action_16 (23) = happyShift action_12
action_16 (25) = happyShift action_13
action_16 (26) = happyShift action_14
action_16 (7) = happyGoto action_18
action_16 _ = happyFail

action_17 _ = happyReduce_2

action_18 (8) = happyShift action_21
action_18 (9) = happyShift action_22
action_18 (10) = happyShift action_23
action_18 (11) = happyShift action_24
action_18 (12) = happyShift action_25
action_18 (13) = happyShift action_26
action_18 (16) = happyShift action_38
action_18 _ = happyFail

action_19 (8) = happyShift action_21
action_19 (9) = happyShift action_22
action_19 (10) = happyShift action_23
action_19 (11) = happyShift action_24
action_19 (12) = happyShift action_25
action_19 (13) = happyShift action_26
action_19 (16) = happyShift action_37
action_19 _ = happyFail

action_20 (8) = happyShift action_21
action_20 (9) = happyShift action_22
action_20 (10) = happyShift action_23
action_20 (11) = happyShift action_24
action_20 (12) = happyShift action_25
action_20 (13) = happyShift action_26
action_20 (16) = happyShift action_36
action_20 _ = happyFail

action_21 (15) = happyShift action_11
action_21 (23) = happyShift action_12
action_21 (25) = happyShift action_13
action_21 (26) = happyShift action_14
action_21 (7) = happyGoto action_35
action_21 _ = happyFail

action_22 (15) = happyShift action_11
action_22 (23) = happyShift action_12
action_22 (25) = happyShift action_13
action_22 (26) = happyShift action_14
action_22 (7) = happyGoto action_34
action_22 _ = happyFail

action_23 (15) = happyShift action_11
action_23 (23) = happyShift action_12
action_23 (25) = happyShift action_13
action_23 (26) = happyShift action_14
action_23 (7) = happyGoto action_33
action_23 _ = happyFail

action_24 (15) = happyShift action_11
action_24 (23) = happyShift action_12
action_24 (25) = happyShift action_13
action_24 (26) = happyShift action_14
action_24 (7) = happyGoto action_32
action_24 _ = happyFail

action_25 (15) = happyShift action_11
action_25 (23) = happyShift action_12
action_25 (25) = happyShift action_13
action_25 (26) = happyShift action_14
action_25 (7) = happyGoto action_31
action_25 _ = happyFail

action_26 (15) = happyShift action_11
action_26 (23) = happyShift action_12
action_26 (25) = happyShift action_13
action_26 (26) = happyShift action_14
action_26 (7) = happyGoto action_30
action_26 _ = happyFail

action_27 _ = happyReduce_8

action_28 (8) = happyShift action_21
action_28 (9) = happyShift action_22
action_28 (10) = happyShift action_23
action_28 (11) = happyShift action_24
action_28 (12) = happyShift action_25
action_28 (13) = happyShift action_26
action_28 (19) = happyShift action_29
action_28 _ = happyFail

action_29 _ = happyReduce_4

action_30 (8) = happyShift action_21
action_30 (9) = happyShift action_22
action_30 (10) = happyShift action_23
action_30 (11) = happyShift action_24
action_30 _ = happyReduce_16

action_31 (8) = happyShift action_21
action_31 (9) = happyShift action_22
action_31 (10) = happyShift action_23
action_31 (11) = happyShift action_24
action_31 _ = happyReduce_15

action_32 _ = happyReduce_14

action_33 _ = happyReduce_13

action_34 (10) = happyShift action_23
action_34 (11) = happyShift action_24
action_34 _ = happyReduce_12

action_35 (10) = happyShift action_23
action_35 (11) = happyShift action_24
action_35 _ = happyReduce_11

action_36 _ = happyReduce_17

action_37 (17) = happyShift action_40
action_37 _ = happyFail

action_38 (17) = happyShift action_39
action_38 _ = happyFail

action_39 (20) = happyShift action_4
action_39 (22) = happyShift action_5
action_39 (24) = happyShift action_6
action_39 (26) = happyShift action_7
action_39 (5) = happyGoto action_42
action_39 (6) = happyGoto action_3
action_39 _ = happyFail

action_40 (20) = happyShift action_4
action_40 (22) = happyShift action_5
action_40 (24) = happyShift action_6
action_40 (26) = happyShift action_7
action_40 (5) = happyGoto action_41
action_40 (6) = happyGoto action_3
action_40 _ = happyFail

action_41 (18) = happyShift action_44
action_41 (20) = happyShift action_4
action_41 (22) = happyShift action_5
action_41 (24) = happyShift action_6
action_41 (26) = happyShift action_7
action_41 (6) = happyGoto action_17
action_41 _ = happyFail

action_42 (18) = happyShift action_43
action_42 (20) = happyShift action_4
action_42 (22) = happyShift action_5
action_42 (24) = happyShift action_6
action_42 (26) = happyShift action_7
action_42 (6) = happyGoto action_17
action_42 _ = happyFail

action_43 (21) = happyShift action_45
action_43 _ = happyReduce_5

action_44 _ = happyReduce_7

action_45 (17) = happyShift action_46
action_45 _ = happyFail

action_46 (20) = happyShift action_4
action_46 (22) = happyShift action_5
action_46 (24) = happyShift action_6
action_46 (26) = happyShift action_7
action_46 (5) = happyGoto action_47
action_46 (6) = happyGoto action_3
action_46 _ = happyFail

action_47 (18) = happyShift action_48
action_47 (20) = happyShift action_4
action_47 (22) = happyShift action_5
action_47 (24) = happyShift action_6
action_47 (26) = happyShift action_7
action_47 (6) = happyGoto action_17
action_47 _ = happyFail

action_48 _ = happyReduce_6

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVAR happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Asg happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 7 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (If happy_var_3 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 11 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Ite happy_var_3 (reverse happy_var_6) (reverse happy_var_10)
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 6 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (While happy_var_3 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Output happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyTerminal (TCON happy_var_1))
	 =  HappyAbsSyn7
		 (Con (read happy_var_1 :: Int)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyTerminal (TVAR happy_var_1))
	 =  HappyAbsSyn7
		 (Var happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op Plus happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op Minus happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op Mult happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op Div happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op More happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Op Equal happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn7
		 (Input
	)

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TPLUS -> cont 8;
	TMINUS -> cont 9;
	TMULT -> cont 10;
	TDIV -> cont 11;
	TMORE -> cont 12;
	TEQUAL -> cont 13;
	TASG -> cont 14;
	TOPENPAR -> cont 15;
	TCLOSEPAR -> cont 16;
	TOPENBRACE -> cont 17;
	TCLOSEBRACE -> cont 18;
	TSEMICOLON -> cont 19;
	TIF -> cont 20;
	TELSE -> cont 21;
	TWHILE -> cont 22;
	TINPUT -> cont 23;
	TOUTPUT -> cont 24;
	TCON happy_dollar_dollar -> cont 25;
	TVAR happy_dollar_dollar -> cont 26;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

generatedParser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tks
  = error $ "Parse error at token " ++ 
      spaced_list (map show (take 12 tks)) ++ " ..."

data Token
  = TCON String | TVAR String 
  | TPLUS | TMINUS | TMULT | TDIV | TMORE | TEQUAL 
  | TOPENPAR | TCLOSEPAR | TOPENBRACE | TCLOSEBRACE
  | TINPUT | TOUTPUT | TSEMICOLON 
  | TASG | TIF | TELSE | TWHILE
  deriving Show

lexer :: String -> [Token]
lexer (x:xs) | isSpace x           = lexer xs
lexer []                           = []
lexer ('+':xs)                     = TPLUS       : lexer xs
lexer ('-':xs)                     = TMINUS      : lexer xs
lexer ('*':xs)                     = TMULT       : lexer xs
lexer ('/':xs)                     = TDIV        : lexer xs
lexer ('>':xs)                     = TMORE       : lexer xs
lexer ('=':'=':xs)                 = TEQUAL      : lexer xs
lexer ('=':xs)                     = TASG        : lexer xs
lexer ('(':xs)                     = TOPENPAR    : lexer xs
lexer (')':xs)                     = TCLOSEPAR   : lexer xs
lexer ('{':xs)                     = TOPENBRACE  : lexer xs
lexer ('}':xs)                     = TCLOSEBRACE : lexer xs
lexer (';':xs)                     = TSEMICOLON  : lexer xs
lexer ('i':'f':xs)                 = TIF         : lexer xs
lexer ('e':'l':'s':'e':xs)         = TELSE       : lexer xs
lexer ('w':'h':'i':'l':'e':xs)     = TWHILE      : lexer xs
lexer ('i':'n':'p':'u':'t':xs)     = TINPUT      : lexer xs
lexer ('o':'u':'t':'p':'u':'t':xs) = TOUTPUT     : lexer xs
lexer (x:xs) | isDigit x
  = let (digits,rest) = span isDigit xs
    in TCON (x:digits) : lexer rest
lexer (x:xs) | isLower x
  = let (chars,rest) = span isAlphaNum xs
    in TVAR (x:chars) : lexer rest
lexer xs
  = error $ "lexical error: "
      ++ spaced_list (map show (take 12 xs)) ++ " ..."

tParse :: String -> [Stmt]
tParse
  = generatedParser . lexer

parseFile :: FilePath -> IO [Stmt]
parseFile fp
 = do tipProgram <- readFile fp
      return (tParse tipProgram)

echo :: String -> String
echo = pretty . tParse 

echoFile :: FilePath -> IO ()
echoFile fp
 = do tipProgram <- readFile fp
      putStr (pretty $ tParse tipProgram)
      return ()

pretty :: [Stmt] -> String
pretty stmts
  = pr stmts 0

pr (Asg v e : stmts) n
  = replicate n ' ' ++ v ++ " = " ++ prex e ++ ";\n" 
      ++ pr stmts n
pr (Output e : stmts) n
  = replicate n ' ' ++ "output " ++ prex e ++ ";\n" 
      ++ pr stmts n
pr (If e ss : stmts) n
  = replicate n ' ' ++ "if (" ++ prex e ++ ") {\n" 
      ++ pr ss (n+2) ++ replicate n ' ' ++ "}\n" 
      ++ pr stmts n
pr (Ite e ss1 ss2 : stmts) n
  = replicate n ' ' ++ "if (" ++ prex e ++ ") {\n" 
      ++ pr ss1 (n+2) ++ replicate n ' ' ++ "} else {\n"
      ++ pr ss2 (n+2) ++ replicate n ' ' ++ "}\n"
      ++ pr stmts n
pr (While e ss : stmts) n
  = replicate n ' ' ++ "while (" ++ prex e ++ ") {\n" 
      ++ pr ss (n+2) ++ replicate n ' ' ++ "}\n" 
      ++ pr stmts n
pr [] _
  = ""

prex :: Exp -> String
prex (Con k)
  = show k
prex (Var v)
  = v
prex Input
  = "input"
prex (Op kind e1 e2)
  = prex' e1 ++ prop kind ++ prex' e2
    where
      prex' (Op kind e1 e2) 
        = '(' : prex' e1 ++ prop kind ++ prex' e2 ++ ")"
      prex' e 
        = prex e

prop :: Opkind -> String
prop op
  = case op of
      Plus   -> " + "
      Minus  -> " - "
      Mult   -> " * "
      Div    -> " / "
      More   -> " > "
      Equal  -> " == "

spaced_list :: [String] -> String
spaced_list = concat . intersperse " "
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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

{-# LINE 310 "templates/GenericTemplate.hs" #-}
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
