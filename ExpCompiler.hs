{-
Compilers Course (COMP3012), 2020
  Venanzio Capretta (based on Henrik Nilsson 2006-2013)

-}

module ExpCompiler where
import FunParser
import NewTAM
import ExpParser
import DecParser
import ComParser
import MTParser
import ExpTAMNew
import States
import Data.Char



compProg :: String -> ([TAMInst],Error)
compProg src = let
 (prog,err) = app (programParse src) Nothing in
  case prog of 
   Just progtrue-> app (progCodeTAMwCtx progtrue) err
   Nothing -> ([],err)

 

-- reading from a file

compileProgTAM :: String -> (String,Error)
compileProgTAM src= let (taminst,err)= (compProg src) in
 case err of 
  Nothing -> ((writeTAM taminst), err)
  Just e -> ("" , err)
 
-- .tam file
getTAMInst :: String -> ([TAMInst] ,Error)
getTAMInst src = let 
 (taminst, err) = app (parseTAM src) Nothing in
 case taminst of
  Nothing -> ([], err)
  Just ts -> (ts, err)