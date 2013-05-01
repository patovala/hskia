----------------------------------------------------------------
--
--  This is the Main module entry point, for parsing input from
--  a src file or as a string.
--
--  The ideas has been taken from Harald Sondergaard's Main 
--  Module.
--
--  Shows how to process command line arguments and take input
--  from a file which contains a sequence of expressions to be
--  evaluated.  
--  
--  This program accepts filenames as the first argument, or 
--  with the option "-t filename" it prints the parse tree
--
--  To compile:  ghc -o calc Main.hs
--
--  (c) 
--  Ivan Valarezo 
--
----------------------------------------------------------------

module Main (main) 
where
import TCFlow (flowFile, evalwrpr, eval)
import TParse (tParse, pretty, parseFile)
import TControl (showctrpoints, getctrpoints, putids, Pos)
import System.Environment (getProgName, getArgs)

help :: String -> IO ()
help tipprog 
  = do
    putStrLn ("Usage: " ++ tipprog ++ " [-t -p -a] filename\n")
    putStrLn ("      -t: process file\n ")
    putStrLn ("      -a: get the production list from file\n ")
    putStrLn ("      -p: get the predecesors list from file\n ")

main :: IO ()
main 
  = do
      tipprog <- getProgName
      args <- getArgs
      processArgs tipprog args 

processArgs :: String -> [String] -> IO ()
processArgs _ [inputFile]
  = do
      flowFile inputFile
processArgs tipprog [option, inputFile] 
  = do
      case option of
            "-a" -> flowFile inputFile 
            "-p" -> filePred inputFile 
            _ -> help tipprog

processArgs tipprog _
  = do
      help tipprog

filePred :: FilePath -> IO() 
filePred fp
 = do tipProgram <- readFile fp
      let productions = eval (tParse tipProgram) 1  -- 1 cause entry 
      let productions' = evalwrpr productions       -- Node has not 
      let sproductions = putids productions' 0
      let controlpoints = getctrpoints sproductions sproductions
      showctrpoints controlpoints 0

--normalise :: String -> IO ()
--normalise inputFile
--  = do
--      exps <- parseFile inputFile
--      let normalisedExps = map pretty exps
--      putStrLn (unlines normalisedExps)

--reduceAll :: String -> IO ()
--reduceAll inputFile
--  = do 
--      text <- readFile inputFile
--      let values = map process (lines text)
--      putStrLn (unlines values)

