{-

Used to parse Declarations

-}

module DecParser where

import Control.Applicative
import FunParser
import ExpParser

{-
--identifier must be unique at all times (only when defining parameter can they be reused)
--remember type checking phase

declaration ::= var identifier:type 
             | var identifier:type := expr
             |fun identifier (vardecls) : type = expr
type ::= Integer | Boolean (similar to LName)
vardecl ::= identifier : type
vardecls ::= vardecls ' | []
vardecls' vardecl, vardecls' | vardecl
declarations ::= declaration | declaration ; declarations

function can now be modified to change expression
eg. fun names (x:Integer, y:Integer ): Boolean = x/14*y
-}

data Declaration = VarDecl Identity Type
                 | VarInit Identity Type EXPR
                 | FunDecl Identity ParamDeclMult Type EXPR
 deriving (Eq,Show)

data ParamDecl =  VDecSingle Identity Type -- define single item on of param
 deriving (Eq, Show)
data ParamDeclMult =  ParamD [ParamDecl]
                    | NoParamD
 deriving (Eq, Show)

declare :: Parser Declaration
declare = do symbol "var"
             a <- notReservedNamesParse
             
             (do symbol ":"
                 t1 <- typeParsed
                 symbol ":="
                 assigned <- expr
                 return (VarInit a t1 assigned)
              <|>
              do symbol ":"
                 t2 <- typeParsed
                 return (VarDecl a t2)
              )
          <|>
          do symbol "fun"
             funid <- notReservedNamesParse
             p1 <-paramdeclsParse
             symbol ":"
             t3 <- typeParsed
             symbol "="
             fexpr <- expr
             return (FunDecl funid p1 t3 fexpr)

listDec :: Parser [Declaration]
listDec = do a <- declare
             as <- many (do symbol ";"
                            declare)
             return (a:as)

decParse :: String -> [Declaration]
decParse src = case parse listDec src of 
 [(t,"")]->t
 _ -> error "Parsing declarations error"

--decCompile :: [Declaration] -> (VarEnv, [TAMInst])

paramdeclsParse :: Parser ParamDeclMult
paramdeclsParse =  (parens paramdeclListParse >>= return . ParamD)
                 <|> 
                 (do symbol "("
                     symbol ")"
                     return (NoParamD))


paramdeclListParse :: Parser [ParamDecl]
paramdeclListParse = do a <- paramdeclSingleParse
                        as <- many (do symbol ","
                                       paramdeclSingleParse)
                        return (a:as)

paramdeclSingleParse :: Parser ParamDecl
paramdeclSingleParse = do a1 <- notReservedNamesParse 
                          symbol ":"
                          t1 <- typeParsed
                          return (VDecSingle a1 t1)

{-
original version, cw1
data Declaration = VarDecl Identity
                 | VarInit Identity EXPR
--                 | FunDecl Identity Vardecls Type EXPR
 deriving (Eq,Show)

declare :: Parser Declaration
declare = do symbol "var"
             a <- notReservedNamesParse
             (do symbol ":="
                 assigned <- expr
                 return (VarInit a assigned)
              <|>
               return (VarDecl a)
              )
-}