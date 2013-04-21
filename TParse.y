{
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
}

%name generatedParser
%tokentype { Token }
%error { parseError }

%left '>' '=='
%left '+' '-'
%left '*' '/' 

%token
  '+'    { TPLUS }
  '-'    { TMINUS }
  '*'    { TMULT }
  '/'    { TDIV }
  '>'    { TMORE }
  '=='   { TEQUAL }
  '='    { TASG }
  '('    { TOPENPAR }
  ')'    { TCLOSEPAR }
  '{'    { TOPENBRACE }
  '}'    { TCLOSEBRACE }
  ';'    { TSEMICOLON }
  if     { TIF }
  else   { TELSE }
  while  { TWHILE }
  input  { TINPUT }
  output { TOUTPUT }
  con    { TCON $$ }
  var    { TVAR $$ }
%%

Prog :: { [Stmt] }
  : Seq                { reverse $1 }

Seq :: { [Stmt] }
  : Seq Stmt           { $2 : $1 }
  | Stmt               { [$1] }

Stmt :: { Stmt }
  : var '=' Exp ';'    { Asg $1 $3 }
  | if '(' Exp ')' 
      '{' Seq '}'      { If $3 (reverse $6) }
  | if '(' Exp ')' 
      '{' Seq '}' else
      '{' Seq '}'      { Ite $3 (reverse $6) (reverse $10) }
  | while '(' Exp ')'
      '{' Seq '}'      { While $3 (reverse $6) }
  | output Exp ';'     { Output $2 }

Exp  :: { Exp }
  : con                { Con (read $1 :: Int) }
  | var                { Var $1 }
  | Exp '+' Exp        { Op Plus $1 $3 }
  | Exp '-' Exp        { Op Minus $1 $3 }
  | Exp '*' Exp        { Op Mult $1 $3 }
  | Exp '/' Exp        { Op Div $1 $3 }
  | Exp '>' Exp        { Op More $1 $3 }
  | Exp '==' Exp       { Op Equal $1 $3 }
  | '(' Exp ')'        { $2 }
  | input              { Input }

{
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
}
