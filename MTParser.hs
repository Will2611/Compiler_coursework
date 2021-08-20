module MTParser where

import FunParser
import Control.Applicative
import ExpParser
import DecParser
import ComParser
import States

data ProgramFile = Program [Declaration] ComInst
 deriving (Eq, Show)

programParsing :: Parser ProgramFile
programParsing = do 
 symbol "let"
 decls <- listDec
 symbol "in"
 comm <- command
 return (Program decls comm)

programParse :: String -> ST (Error) (Maybe ProgramFile)
programParse src = do
 err<- stState
 case parse programParsing src of
  [(p,"")] -> stUpdate (noError)>>
              return (Just p )
  _        -> stUpdate (errMsg "Parsing program error") >> 
              return Nothing
  -- is main error shown when compiled
