{-

Used to parse commands

-}

module ComParser where

import FunParser
import ExpParser
import Control.Applicative

{-

command ::= identifier := expr
            | if expr then command else command
            | while expr do command
            | getint ( identifier )
            | printint ( expr )
            | begin commands end

commands ::= command | command ; commands

-}

data ComInst = Assignment Identity EXPR
             | PrintInt EXPR
             | GetInt Identity -- Reads from terminal, set to read only
             | BeginEnd [ComInst]
             | WhileDo EXPR ComInst
             | IfThenElse EXPR ComInst ComInst
 deriving (Eq,Show)

command :: Parser ComInst
command = (do symbol "printint"
              p <- parens expr
              return (PrintInt p))
          <|> (do symbol "getint"
                  g <- parens notReservedNamesParse
                  return (GetInt g))
          <|> (do symbol "if"
                  exif <- expr
                  symbol "then"
                  comif1 <- command
                  symbol "else"
                  comif2 <- command
                  return (IfThenElse exif comif1 comif2)) 
          <|> (do symbol "while"
                  exwh <- expr
                  symbol "do"
                  comwh <- command
                  return (WhileDo exwh comwh))
          <|> (do symbol "begin"
                  combloc <- commandlist
                  symbol "end"
                  return (BeginEnd combloc))
          <|> (do ident <- notReservedNamesParse
                  symbol ":="
                  expnew <- expr
                  return (Assignment ident expnew)) 
                  -- doesn't allow assignment of functions, only variable name

commandlist :: Parser [ComInst]
commandlist = do c <- command
                 cs <- many (do symbol ";"
                                command)
                 return (c:cs)

comParse :: String -> ComInst
comParse src = case parse command src of
                    [(t,"")] -> t
                    _ -> error "Parsing commands error"

