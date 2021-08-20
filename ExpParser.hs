{-

Parser for Arithmetic expressions
Generate and Abstract Syntax Tree from an expression
Using functional parsers

-}

module ExpParser where

import FunParser
import Control.Applicative

type Identity = String

-- expr needed, splitting soon
data BinOperator = Addition | Subtraction | Multiplication | Division
                 | Conjunction | Disjunction
                 | LssOp | LeqOp | GtrOp | GeqOp | EqOp | NeqOp
  deriving (Eq,Show,Enum)

data UnOperator = Negation | NegBool
  deriving (Eq,Show)

data EXPR = LitInteger TyInt
         | BinOp BinOperator EXPR EXPR
         | UnOp  UnOperator EXPR
         | Conditional EXPR EXPR EXPR
         | Var Identity
         | LitBool TyBool
         | Func Identity ARGS
  deriving (Eq,Show)

data ARGS =  Param [ARGS']
          | NoParam
 deriving (Eq, Show)

data ARGS' =  ArgSingle EXPR -- define single item on of param
 deriving (Eq, Show)
-- include type checker

-- Parse the top string: error if parsers fail or input not consumed

expParse :: String -> EXPR
expParse src = case parse expr src of
  [(t,"")] -> t
  _ -> error "Parsing expression error"

{-
Grammar for expressions (either arith or bool):
We allow arith expressions to occur as bool expressions in parentheses
Conditionals must be in parentheses, except at the top level

  exp ::= aexpInt| bexpBool
  aexpInt::= aexp | bexp ? aexp : aexp 
  bexpBool::= bexp | bexp ? bexp : bexp

  bexp ::= cexp | cexp || bexp
  cexp ::= bterm | bterm && cexp
  bterm ::= atermB | aexp `op` aexp
             where `op` is one of <,<=,>,>=,==,!=

  aexp ::= mexp | mexp + aexp | mexp - aexp
  mexp ::= aterm | aterm * mexp | aterm / mexp
  aterm ::= intLit | - aterm | ( aexpInt ) 
  atermB::= !aterm| tyBool | (bexpBool)

-}

expr :: Parser EXPR
expr = do b <- bexp
          (do symbol "?"
              e0 <- bexp
              symbol ":"
              e1 <- bexp
              return (Conditional b e0 e1)
           <|>
           return b)

bexp :: Parser EXPR
bexp = do e0 <- cexp
          (do symbol "||"
              e1 <- bexp
              return (BinOp Disjunction e0 e1)
           <|>
           return e0)

cexp :: Parser EXPR
cexp = do e0 <- bterm
          (do symbol "&&"
              e1 <- cexp
              return (BinOp Conjunction e0 e1)
           <|>
           return e0)

-- Longer operators (eg "<=") must come before shorter ones ("<")
relop :: Parser BinOperator
relop = choice [ symbol "<=" >> return LeqOp
               , symbol "<"  >> return LssOp
               , symbol ">=" >> return GeqOp
               , symbol ">"  >> return GtrOp
               , symbol "==" >> return EqOp
               , symbol "!=" >> return NeqOp
               ]

bterm :: Parser EXPR
bterm = do e0 <- aexp
           (do op <- relop
               e1 <- aexp
               return (BinOp op e0 e1)
            <|>
            return e0) 


addminus :: Parser BinOperator
addminus = choice [ symbol "+" >> return Addition
                  , symbol "-" >> return Subtraction
                  ]

-- For left-associativity, we use an auxiliary function aexp'
--    that keeps a functional accumulator

aexp :: Parser EXPR
aexp = aexp' id

aexp' :: (EXPR -> EXPR) -> Parser EXPR
aexp' f = do e0 <- mexp
             (do op <- addminus
                 aexp' (BinOp op (f e0))
              <|>
              return (f e0))

multdiv :: Parser BinOperator
multdiv = choice [ symbol "*" >> return Multiplication
                 , symbol "/" >> return Division
                 ]

mexp :: Parser EXPR
mexp = mexp' id

mexp' :: (EXPR -> EXPR) -> Parser EXPR
mexp' f = do e0 <- aterm
             (do op <- multdiv
                 mexp' (BinOp op (f e0))
              <|>
              return (f e0))

aterm :: Parser EXPR
aterm = (natural >>= return . LitInteger)
        <|> parseForDec -- change this to parse either identity or function
        <|> (do symbol "-"
                e <- aterm
                return (UnOp Negation e))
        <|> (do symbol "!"
                b <- aterm
                return (UnOp NegBool b))
        <|> parens expr
        <|>(tyBoolean>>= return .LitBool)

-- type checking for variables

argsParse :: Parser ARGS
argsParse =  (parens argsListParse >>= return . Param)
             <|> 
             (do symbol "("
                 symbol ")"
                 return (NoParam))

argsListParse :: Parser [ARGS']
argsListParse = do a <- argsSingleParse
                   as <- many (do symbol ","
                                  argsSingleParse)
                   return (a:as)

argsSingleParse :: Parser ARGS'
argsSingleParse = do a1 <- expr -- include type checking here
                     return (ArgSingle a1)

parseForDec :: Parser EXPR
parseForDec = (do id <- notReservedNamesParse
                  arg <- argsParse
                  return (Func id arg)) -- check of declration call is function first
              <|>
              (do id <- (notReservedNamesParse >>= return . Var)
                  return (id))



