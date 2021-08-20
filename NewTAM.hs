{-

TAM Virtual Machine

Added: Boolean, Relational, and Conditional Operators
Added: Halting, GetInt, PutInt, Labeling, Jump-to-label, JumpIfz-to-label, Load-from-address-a, Store-to-address-a

-}

module NewTAM where

import FunParser
import States
import Data.List
import Control.Applicative
import Data.Char

type MTInt = Int -- TAM Integer type (values in stack)
type Counter = MTInt
type StkAddress = MTInt
type LName = String

-- Instructions of the Virtual Machine
data TAMInst
  = LOADL MTInt   -- push Integer into the stack
  -- Arithmetic operations
  | ADD           -- adds two top values in the stack
  | SUB           -- subtract second element of stack from top
  | MUL           -- multiplies top values in the stack
  | DIV           -- divides the second value by the top (integer division)
  | NEG           -- negates the top of the stack
  -- Boolean operations
  | AND           -- Boolean conjunction (non-zero values are True)
  | OR            -- Boolean disjunction
  | NOT           -- Boolean negation
  -- Relational operations
  | LSS           -- order operation <
  | GTR           -- order operation >
  | EQL           -- equality operator
  --
  | HALT         -- stops execution and halts machine
  | GETINT       -- Reads an integer from the terminal
  | PUTINT       -- Pops the top of the stack and prints it to the terminal
  | Label LName --l    --Marks a place in the code with label l, doesn’t execute any operation on the stack
  | JUMP LName --l     -- Execution control jumps unconditionally to location identified by label l
  | JUMPIFZ LName--l  -- Pops the top of the stack, if it is 0, execution control jumps to location identified by l, if it is not 0 continues with next instruction
  | LOAD StkAddressBase     -- Reads the content of the stack location with address a and pushes the value to the top of the stack
  | STORE StkAddressBase    -- Pops the top of the stack and writes the value to the stack location withaddress a
  -- CW2 extensions
  | CALL LName -- calls a subprogram begining at location label l, sets up activation record, and returns address
  | RETURN MTInt MTInt -- m = number of return values, n= number of local varaible
  deriving (Eq,Show)

type Stack = [MTInt]


emptyStack :: Stack
emptyStack = []

data TAMState = TAMState{
    tsCode::[TAMInst],
    tsCounter :: Counter,
    tsStack:: Stack,
    tsStackBase :: StkAddress,
    tsLocalBase :: StkAddress
}
 deriving (Eq, Show)

data StkAddressBase = SAddAbs Int  -- Addresses in the Stack
                    | SAddSB  Int    -- Relative to Stack Base
                    | SAddLB  Int    -- Relative to Local Base
 deriving (Eq,Show,Read)


reading :: String -> StkAddressBase
reading = haveread . words where
 haveread ("SB":"+":x:src) = SAddSB (read x)
 haveread ("SB":"-":x:src) = SAddSB (-(read x))
 haveread ("LB":"-":x:src) = SAddSB (read x)
 haveread ("LB":"+":x:src) = SAddSB (-(read x))
 haveread (x:src) = SAddSB (read x)


showing :: StkAddressBase -> String
showing (SAddAbs a) = show a
showing (SAddSB b) = "SB + " ++ (show b)
showing (SAddLB c) = "LB - " ++ (show c)

-- Correspondence between Booleans and integers
boolInt :: Bool -> MTInt
boolInt False = 0
boolInt True = 1

-- All non-zero integers correspond to Boolean false
intBool :: MTInt -> Bool
intBool x = x/=0

-- Convenient composition operators

-- Pre-composing with a 2-argument function
infixr 8 .<
(.<) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
g .< f = \ a1 a2 -> g (f a1 a2)

-- Post-composing with a 2-argument function
infixr 8 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
g <. f = \ a1 a2 -> g (f a1) (f a2)


-- Implementation of boolean operations on Integers, always return 0 or 1

intAND :: MTInt -> MTInt -> MTInt
intAND = boolInt .< (&&) <. intBool

intOR :: MTInt -> MTInt -> MTInt
intOR = boolInt .< (||) <. intBool

intNOT :: MTInt -> MTInt
intNOT = boolInt . not . intBool

-- Relational operations, return 0 (False) or 1 (True)

intLSS :: MTInt -> MTInt -> MTInt
intLSS = boolInt .< (<)

intGTR :: MTInt -> MTInt -> MTInt
intGTR = boolInt .< (>)

intEQL :: MTInt -> MTInt -> MTInt
intEQL = boolInt .< (==)

executeState :: TAMInst -> TAMState -> TAMState
executeState = undefined

tsPush :: MTInt -> TAMState -> TAMState
tsPush c ts0 = ts0 {tsStack = (c:tsStack ts0)}

tsPop :: TAMState -> (MTInt, TAMState)
tsPop ts0 = let x= (tsStack ts0) in
                   (head x,ts0 {tsStack = tail x})

--tsInst:: TAMState -> TAMInst
--tsInst TAMState{tsCode=a, tsCounter=b, tsStack=c, tsStackBase =d, tsLocalBase = e} = a

type TAMSt a = StateIO TAMState a

stackT :: TAMSt Stack
stackT = do
 ts <- stStateIO
 return (tsStack ts)
 
codeT :: TAMSt [TAMInst]
codeT = do
 ts <- stStateIO
 return (tsCode ts)

stkBaseT:: TAMSt (MTInt)
stkBaseT = do
 ts <- stStateIO
 return (tsStackBase ts)

locBaseT:: TAMSt (MTInt)
locBaseT = do
 ts <- stStateIO
 return (tsLocalBase ts)

bothBaseT :: TAMSt (MTInt,MTInt)
bothBaseT = do
 sb <-stkBaseT
 lb <-locBaseT
 return (sb,lb)

counterT :: TAMSt Counter
counterT = do
 ts <- stStateIO
 return (tsCounter ts)

stkUpdateT :: Stack -> TAMSt ()
stkUpdateT stk = do
 ts <- stStateIO
 stUpdateIO (ts {tsStack = stk})

counterUpdateT :: Counter -> TAMSt ()
counterUpdateT count = do
 ts <- stStateIO
 stUpdateIO (ts {tsCounter = count})

lbUpdateT :: StkAddress -> TAMSt ()
lbUpdateT newadd = do
 ts <- stStateIO
 stUpdateIO (ts {tsLocalBase = newadd})


popT :: TAMSt MTInt
popT = do
 stk <- stackT
 stkUpdateT (tail stk)
 return  (head stk)

popTMult :: MTInt -> TAMSt [MTInt]
popTMult popnum = do
 stk <- stackT
 stkUpdateT (drop popnum stk)
 return (take popnum stk)

pushT :: MTInt -> TAMSt ()
pushT x = do 
 stk <- stackT
 stkUpdateT (x:stk)

pushTMult :: [MTInt] -> TAMSt ()
pushTMult xs = do
 stk <- stackT
 stkUpdateT (xs++stk)

continueT :: TAMSt ()
continueT = do
 c<- counterT
 counterUpdateT (c+1) -- check length

findLabelT :: LName -> TAMSt Counter
findLabelT l = do
 tam <- codeT
 return (lCounter l tam) 
lCounter :: LName -> [TAMInst] -> Counter
lCounter l tam = case elemIndex (Label l) tam of
 Nothing -> error "label non existenet"
 Just a -> a


--mt n = term popped from stack
execute :: TAMInst -> TAMSt()
execute HALT = return ()
execute (LOADL x) =do
 pushT x
 continueT
execute ADD = do
 mt1 <- popT
 mt2 <- popT
 pushT (mt1+mt2)
 continueT
execute SUB = do
 mt3 <- popT
 mt4 <- popT
 pushT (mt4-mt3)
 continueT
execute MUL = do
 mt5 <- popT
 mt6 <- popT
 pushT (mt5*mt6)
 continueT
execute DIV = do
 mt7 <- popT
 mt8 <- popT
 pushT (mt8 `div` mt7)
 continueT
execute NEG = do
 mt9 <- popT
 pushT (-mt9)
 continueT
--Bool
execute AND = do
 mt10 <- popT
 mt11 <- popT
 pushT (mt11 `intAND` mt10)
 continueT
execute OR = do
 mt12 <- popT
 mt13 <- popT
 pushT (mt13 `intOR` mt12)
 continueT
execute NOT = do
 mt14 <- popT
 pushT (intNOT mt14)
 continueT
--relational
execute LSS = do
 mt15 <- popT
 mt16 <- popT
 pushT (mt16 `intLSS` mt15)
 continueT
execute GTR = do
 mt17 <- popT
 mt18 <- popT
 pushT (mt18 `intGTR` mt17)
 continueT
execute EQL = do
 mt19 <- popT
 mt20 <- popT
 pushT (mt20 `intEQL` mt19)
 continueT
execute (JUMP l)= do
 c<-findLabelT l
 counterUpdateT c
execute (JUMPIFZ l)= do
 mt21<-popT
 --lift (putStrLn ("JUMPIFZ condition:" ++ (show mt21)))
 case (mt21) of
  0 -> do
   c<-findLabelT l
   counterUpdateT c
  _ -> continueT
execute (Label l) = do
 continueT
execute (LOAD x)= do
 stk <-stackT
 bothBases <- bothBaseT
 pushT (head(getAddress x bothBases stk))
 continueT
execute (STORE x)= do
 mt22 <- popT
 stk <-stackT
 bothBases <- bothBaseT
 stkUpdateT ((remainderStk x bothBases stk) ++ (replaceAtIndex mt22 x bothBases stk))
 continueT
execute (PUTINT) = do
 mt22<- popT
 lift (putStrLn ("output:" ++ (show mt22)))
 continueT
execute (GETINT) = do
 lift (putStrLn "Enter a number:")
 s <- lift (getLine)
 pushT (read s::MTInt) 
 continueT
execute (CALL l)= do
 locbaseOld <- locBaseT
 pcOld <- counterT
 pushT (locbaseOld)
 stk <-stackT
 lbUpdateT ((length stk)-1)
 pushT (pcOld)
 c <-findLabelT l
 counterUpdateT c
execute (RETURN m n) = do
 result <- popTMult m
 pcOld <- popT
 lbOld <-popT
 lbUpdateT (lbOld)
 varremoved <- popTMult n
 pushTMult (result)
 counterUpdateT (pcOld+1)
 
 

nextInst :: TAMSt TAMInst
nextInst = do
 ct <- counterT
 tamls <- codeT
 if snd (splitAt ct tamls) == [] then
  return HALT
  else return (head (snd (splitAt ct tamls)))

initTS :: [TAMInst] -> TAMState
initTS tam = TAMState {tsCode=tam, tsCounter=0, tsStack=[], tsStackBase = 0, tsLocalBase = 0} 

splitToIndex :: Int -> [a] -> ([a],[a])
splitToIndex x = (\stk -> splitAt (length stk -x-1) stk)

getAddress :: StkAddressBase -> (MTInt,MTInt)->[MTInt] -> [MTInt]
getAddress (SAddSB x) (sb,lb)= (\stk -> snd ( splitToIndex (sb+x) stk ))
getAddress (SAddLB x) (sb,lb)= (\stk -> snd ( splitToIndex (lb-x) stk ))
getAddress (SAddAbs x) (sb,lb)= (\stk -> snd ( splitToIndex x stk ))

remainderStk :: StkAddressBase ->(MTInt,MTInt)->[MTInt] -> [MTInt]
remainderStk (SAddSB x) (sb,lb) = (\stk -> fst ( splitToIndex (sb+x) stk ))
remainderStk (SAddLB x) (sb,lb) = (\stk -> fst ( splitToIndex (lb-x) stk ))
remainderStk (SAddAbs x) (sb,lb) = (\stk -> fst ( splitToIndex (x) stk ))

replaceHead :: a ->[a] -> [a]
replaceHead x ys= x: (tail ys)

replaceAtIndex :: MTInt -> StkAddressBase ->(MTInt,MTInt)->[MTInt] -> [MTInt]
replaceAtIndex x adr bothbase stk2= replaceHead x (getAddress adr bothbase stk2)

execT :: TAMSt Stack
execT = do 
 inst <- nextInst
 execute inst
 if inst == HALT then stackT
                 else execT

execTAM :: [TAMInst] -> IO Stack
execTAM tam = do 
 (stk,_)<-appIO execT (initTS tam)
 return stk

-- writing out a TAM program
writeTAM :: [TAMInst] -> String
writeTAM = foldl (\s inst -> s ++ showTAM inst ++ "\n") ""

showTAM :: TAMInst -> String
showTAM (LOAD a) = "LOAD ["++ (showing a) ++"]"
showTAM (STORE a)= "STORE ["++ (showing a) ++"]"
showTAM a = show a


-- parsing a TAM program
parseTAM :: String -> ST (Error) (Maybe [TAMInst])
parseTAM src = do
 case parse testparseTAM2 src of
  [(tInst,"")] -> stUpdate (noError)>>
                  return (Just tInst )
  _            -> stUpdate (errMsg "Parsing TAM error") >> 
                  return Nothing
testparseTAM2 :: Parser [TAMInst]
testparseTAM2 = do t1 <- parseTAM2
                   ts <- many (do parseTAM2)
                   return (t1:ts)

parseTAM2 :: Parser TAMInst
parseTAM2 = do symbol "LOAD ["
               a <- readingBase
               symbol"]"
               return (LOAD a)
            <|>do symbol "LOADL"
                  a <- integer
                  return (LOADL a)
            <|>do symbol "ADD"
                  return ADD
            <|>do symbol "SUB"
                  return SUB
            <|>do symbol "MUL"
                  return MUL
            <|>do symbol "DIV"
                  return DIV
            <|>do symbol "NEG"
                  return NEG
            <|>do symbol "AND"
                  return AND
            <|>do symbol "OR"
                  return OR
            <|>do symbol "NOT"
                  return NOT
            <|>do symbol "LSS"
                  return LSS
            <|>do symbol "GTR"
                  return GTR
            <|>do symbol "EQL"
                  return EQL
            <|>do symbol "HALT"
                  return HALT
            <|>do symbol "GETINT"
                  return GETINT
            <|>do symbol "PUTINT"
                  return PUTINT
            <|>do symbol "Label"
                  l <- labelsParse
                  return (Label l)
            <|>do symbol "JUMP"
                  l <- labelsParse
                  return (JUMP l)
            <|>do symbol "JUMPIFZ"
                  l <- labelsParse
                  return (JUMPIFZ l)
            <|>do symbol "CALL"
                  l <- labelsParse
                  return (CALL l)
            <|>do symbol "RETURN"
                  res <- natural
                  var <- natural
                  return (RETURN res var)
            <|>do symbol "STORE ["
                  a <- readingBase
                  symbol"]"
                  return (STORE a)
            

readingBase :: Parser StkAddressBase
readingBase = do symbol "SB"
                 a <- intBase
                 return (SAddSB a)
              <|>(do symbol "LB"
                     a <- intBase
                     return (SAddLB (-a)))
              <|>(do gbv <- nat
                     return (SAddAbs gbv))

intBase :: (Num int, Read int) => Parser int
intBase = do symbol "-"
             n <- nat
             return (-n)
          <|> 
          do symbol "+"
             n <- nat
             return (n)


labelsParse:: Parser String
labelsParse = do symbol "\""
                 x <- stopWhenhitQuote
                 symbol "\""
                 return x                  

isNotQuoteMark :: Parser Char 
isNotQuoteMark = sat (not. isItQuoteMark)

stopWhenhitQuote :: Parser String
stopWhenhitQuote = do xs <- some isNotQuoteMark
                      return xs

isItQuoteMark :: Char -> Bool
isItQuoteMark c= (c== '"')

{-
Teacher example
-- Generate the trace of the TAM computation
--   list of pairs of instruction and stack after execution of the instruction
execTrace :: Stack -> [TAMInst] -> [(TAMInst,Stack)]
execTrace stk [] = []
execTrace stk (i:is) =
  let stk' = execute stk i
      trace' = execTrace stk' is
  in ((i,stk'):trace')

-- Printing pairs of value in a two-column table
printTable :: [(String,String)] -> String
printTable pairs = intercalate "\n" $ map (\(a,b) -> fitStr a ++ b) pairs
  where n = maximum (map (length.fst) pairs) + 5
        fitStr a = a ++ replicate (n - length a) ' ' 
                       
-- print the trace of the computation, return the final stack
traceTAM :: Stack -> [TAMInst] -> IO Stack
traceTAM stk tam = do
  let trace = execTrace stk tam
      traceStr = ("Initial stack:", show stk) :
                 map (\(a,b)->(show a,show b)) trace
      finalStk = snd (last trace)
  putStrLn (printTable traceStr)
  return finalStk
-}