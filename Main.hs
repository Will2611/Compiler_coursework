{-
Compilers Course (COMP3012), 2020
  Venanzio Capretta (based on Henrik Nilsson 2006-2013)

Compiler and evaluator for Arithmetic expressions
  with Booleans, Relations, Conditional
Main executable
-}

module Main where
import ExpCompiler
import NewTAM
import ExpParser
import DecParser
import ComParser
import MTParser
import ExpTAMNew

import System.Environment
import Data.Char

{- If input file has extension .mt, compile the expression to TAM code
   If input file has extension .tam, execute tam code
-}

data FileType = TAM |MT
  deriving (Eq,Show)

main :: IO ()
main = do
  args <- getArgs
  
  let inputName = head args
  
  let (fileName,extension) = fileNE args
  case extension of
    TAM -> do
     src <- readFile (fileName++".tam")
     let (tamInst,tamerr) = (getTAMInst src) in
      case tamerr of 
       Nothing -> do 
                   putStrLn "Executing TAM code: " 
                   stk <- execTAM tamInst 
                   putStrLn (show $ (stk) )
       Just e -> error e
    MT -> do
     src <- readFile (fileName ++ ".mt")
     let (tamProg,err) = (compileProgTAM src) in
      case err of
       Nothing ->writeFile (fileName++".tam") tamProg >>
                 putStrLn ("compiled to TAM file: " ++ fileName ++ ".tam")
       Just e -> error e

-- Finding the base name and extension of a file name

baseName :: String -> String
baseName = takeWhile (/='.')

fileExt :: String -> String
fileExt fn = let ext = dropWhile (/='.') fn
             in if ext == "" then ".mt" else ext

extType :: String -> Maybe FileType
extType ".tam" = Just TAM
extType ".mt" = Just MT
extType _ = Nothing

parseFileName :: String -> Maybe (String,FileType)
parseFileName arg = do
  if isAlpha (head arg)
    then let name = baseName arg
             ext  = extType (fileExt arg)
         in case ext of
              Just t -> Just (name,t)
              Nothing -> Nothing
    else Nothing


unJust :: [Maybe a] -> [a]
unJust [] = []
unJust (Nothing:as) = unJust as
unJust (Just a:as) = a : unJust as

-- We assume one of the arguments is a file name
fileNE :: [String] -> (String,FileType)
fileNE = head . unJust . (map parseFileName)

