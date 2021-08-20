{-

Functional parsing library

-}

module FunParser where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

data Type = TyInt | TyBool
 deriving (Eq, Show)

data VFType = VarType Type 
            | FunType [Type] Type -- for type checking
 deriving (Eq,Show)

type TyInt = Int
type TyBool = Bool
type Error = Maybe String

noError :: Error
noError = Nothing

errMsg :: String -> Error
errMsg = Just


parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])


-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = P (\src -> [ (g x, src1) | (x,src1) <- parse pa src ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\src -> [(x,src)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P (\src -> [ (f x,src2) | (f,src1) <- parse pf src,
                                        (x,src2) <- parse pa src1 ] )

instance Monad Parser where
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = P (\src -> [r | (x,src1) <- parse pa src,
                               r <- parse (fpb x) src1 ] )

--Making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\rsc -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\src -> case parse p1 src of
                    [] -> parse p2 src
                    rs -> rs)

-- Chosing among many alternatives
choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

{-
Parallel parsing: getting the results of both parsers
  for ambiguous grammars
Use with caution: it can cause inefficiency
-}

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> (parse p1 inp) ++ (parse p2 inp))

-- Derived primitives

-- verify that the parsed object satisfy a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

reservedProgram :: String -> Bool
reservedProgram "let" = True
reservedProgram "in" = True
reservedProgram _ = False

reservedType :: String -> Bool
reservedType "Boolean" = True
reservedType "Integer" = True
reservedType _ = False


reservedBool :: String -> Bool
reservedBool "true" = True
reservedBool "false" = True
reservedBool _ = False

reservedDec :: String -> Bool
reservedDec "var" = True
reservedDec _ = False

reservedCom :: String -> Bool
reservedCom "if" = True
reservedCom "then" = True
reservedCom "else" = True
reservedCom "while" = True
reservedCom "do" = True
reservedCom "getint" = True
reservedCom "printint" = True
reservedCom "begin" = True
reservedCom "end" = True
reservedCom _ = False

reservedAll :: String -> Bool
reservedAll src = reservedCom src || reservedDec src || reservedProgram src || reservedType src || reservedBool src

--can now be used for uppercase letter starting variable name
varName :: Parser String
varName = do x  <- letter
             xs <- many alphanum
             return (x:xs)

reservedSat :: (String -> Bool) -> Parser String
reservedSat = satisfy varName

notReservedNameParse :: Parser String
notReservedNameParse = reservedSat (not . reservedAll)

notReservedNamesParse :: Parser String
notReservedNamesParse = token notReservedNameParse

typeSat :: (String -> Bool) -> Parser String
typeSat = satisfy ident

isType :: String -> Bool
isType "Integer" = True
isType "Boolean" = True
isType _ = False

whatType :: String -> Type
whatType "Integer" = TyInt
whatType "Boolean" = TyBool

-- only parse if is type declaration
parseType :: Parser String
parseType = typeSat isType

-- parse to detect type decalaration
typeParsed :: Parser Type
typeParsed = fmap whatType (token parseType)

whatTyBool :: String -> TyBool
whatTyBool "true" = True
whatTyBool "false" = False
whatTyBool _ = error "Not even a boolean type"

--parse whether is tyBoolean ("true" and "false")
tyBoolean :: Parser TyBool
tyBoolean = fmap whatTyBool identifier

ident :: Parser String
ident = do x  <- letter
           xs <- many alphanum
           return (x:xs)

nat :: (Num int, Read int) => Parser int
nat = do xs <- some digit
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

-- parsing a list of integers
nats :: Parser [Integer]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)
