module ExpTAMNew where

import States
import FunParser
import ExpParser
import DecParser
import ComParser
import MTParser
import NewTAM
import TypeChecker
import Control.Applicative
import Data.Maybe

type VarEnv = [(Identity, StkAddress)]
type VarEnvNew = [(Identity,StkAddressBase)]

removeLB :: VarEnvNew -> VarEnvNew
removeLB [] = []
removeLB ((a,b):as) = (removeL (a,b)) ++ removeLB as

removeL :: (Identity,StkAddressBase) -> VarEnvNew
removeL (a,SAddAbs b) =[(a,SAddAbs b)]
removeL (a,SAddSB b) =[(a,SAddSB b)]
removeL (a,SAddLB b) =[]



address :: VarEnvNew -> Identity -> StkAddressBase
address ve v = case lookup v ve of 
 Nothing -> error "Variable not in environment"
 Just a -> a


isVaraiableReal :: VarCon -> Identity -> Bool
isVaraiableReal vc v = case lookup v vc of 
 Nothing -> False
 Just a -> True

{-
--reminder
data VFType = VarType Type 
            | FunType [Type] Type -- for type checking
 deriving (Eq,Show)

 Also include 
 absolute address
 stack base address 
 local base address (relative to absolute)
-}
-- can only parse serially;i.e cannot use var x unless declared before use, e.g var x:=y;var y:=1 will lead to error in code generation

progCodeTAMwCtx :: ProgramFile -> ST (Error) [TAMInst]
progCodeTAMwCtx (Program decls com)=do
 err_ori <- stState
 let (err,vc)= (app (chkDecType decls) []) in
  case err_ori of 
   Nothing -> case err of
    Nothing -> return (progCodeTAM (Program decls com) vc)
    Just e ->stUpdate (errMsg e) >> return []
   Just e -> return [] 


progCodeTAM :: ProgramFile -> VarCon-> [TAMInst]
progCodeTAM (Program decls com) vc =let 
 (ve,(tamv,tamf), label)= (declsCode decls vc) -- create a tamf instance
 tamc = (comCode ve vc label com) in
 (tamv ++ tamc ++ [HALT]++tamf) 

declsCode :: [Declaration] -> VarCon-> (VarEnvNew,([TAMInst],[TAMInst]),Integer)
declsCode ds vc= let ((tamv,tamf), (ve,a,label)) = app (declsTAM ds vc) ([],0,0)
               in (ve,(tamv,tamf), label)

declsTAM :: [Declaration] -> VarCon-> ST (VarEnvNew,StkAddress, Integer) ([TAMInst],[TAMInst])
declsTAM [] vc= return ([],[])
declsTAM (d:ds) vc = do 
 (td,tf) <- declTAM d vc
 (tds,tfs) <- declsTAM ds vc
 return ((td++tds),(tf++tfs))

declTAM :: Declaration -> VarCon-> ST (VarEnvNew,StkAddress,Integer) ([TAMInst],[TAMInst])
declTAM (VarDecl v t) vc= do 
 (ve,a,labeler) <- stState
 stUpdate (((v,SAddSB a):ve),a+1,labeler)
 return ([LOADL 0],[])
declTAM (VarInit v t e) vc = do
 (ve,a, labeler) <- stState
 let (expDec, newlabel) = app (expCode ve vc e) labeler
 stUpdate (((v,SAddSB a):ve),a+1,newlabel)
 return (expDec,[])

declTAM (FunDecl v param t expr) vc = do
 (ve,a, labeler) <- stState
 let (tamexpr,newlabeler) = app (expCode ((createLocalenvironment param)++ve) (vc) expr) labeler
 stUpdate (ve,a,newlabeler)
 return ([],([Label v]++tamexpr++[RETURN (1) (paramLength param)]))

paramLength :: ParamDeclMult -> Int
paramLength (ParamD ps) = length ps
paramLength (NoParamD) = 0

createLocalenvironment :: ParamDeclMult -> VarEnvNew
createLocalenvironment NoParamD = []
createLocalenvironment (ParamD params) = let (locdisplacement,localenv )= app (createLocalEnv params (length (params))) ([])
                                      in localenv

createLocalEnv :: [ParamDecl] -> StkAddress-> ST (VarEnvNew) StkAddress
createLocalEnv [] locDisp= return locDisp
createLocalEnv (param:paramls) locDisp = do
 (locDispNew) <- createLocalvar param locDisp
 locDispNewest <- createLocalEnv paramls locDispNew 
 return locDispNewest
createLocalvar :: ParamDecl -> StkAddress-> ST (VarEnvNew) StkAddress
createLocalvar (VDecSingle locvarid typeMT) locDisplace= do
 (ve) <- stState
 stUpdate ((locvarid,(SAddLB locDisplace)):ve)
 return (locDisplace-1)

fresh :: ST Integer LName
fresh = do
 n <- stState
 stUpdate (n+1)
 return ('#':(show n))


comCode :: VarEnvNew-> VarCon-> Integer -> ComInst -> [TAMInst]
comCode ve vc intL cs = let (a,b)= app (comCodeTAM ve vc cs) intL
                     in a

comCodeTAM :: VarEnvNew -> VarCon -> ComInst -> ST Integer [TAMInst]
comCodeTAM ve vc (Assignment varIdent ex1) = do
 let typeofex1=typeCheckEXPR vc ex1
 let typeofvarID =typeCheckEXPR vc (Var varIdent)
 -- check compatible types, not if variable or function
 if typeofex1 == typeofvarID && typeofex1/=Nothing
 then 
  case typeofex1 of 
   Nothing -> error "types are not valid"
   _ -> do
    te <- (expCode ve vc ex1)
    let adr = (address ve varIdent)
    return (te ++ [STORE adr])-- would throw error if not a variable
 else
  error "Type not matching in assignment"
comCodeTAM ve vc (PrintInt ex2) = do
 let typeofex2 = typeCheckEXPR vc ex2
 case typeofex2 of 
  Just TyInt -> do
   te <- (expCode ve vc ex2)
   return (te ++ [PUTINT])
  _ -> error "Expression Type is not Integer"
comCodeTAM ve vc (GetInt varIdent2) = do 
 -- make sure only Integer var can take int
 let typevarID2 = typeCheckEXPR vc (Var varIdent2)
 case typevarID2 of
  Just TyInt -> do
   let adr = (address ve varIdent2)
   return ([GETINT] ++ [STORE adr]) -- could be changed to include reading expression
  _-> error "Variable and expression must be Integer"
--JUMPIFZ, must include NOT because if condition true; i.e. 1; to cause jump jump to label
comCodeTAM ve vc (WhileDo ex3 comInst0)= do
 let typeofex3 = typeCheckEXPR vc ex3
 case typeofex3 of 
  Just TyBool -> do
   te1 <- (expCode ve vc ex3)
   e0 <- fresh
   e1 <- fresh
   tc <- (comCodeTAM ve vc comInst0)
   return ([Label e1] ++ te1 ++ [JUMPIFZ e0] ++ tc ++ [JUMP e1]++[Label e0])
  _ -> error "Expression Type is must be Boolean for condition"
 
comCodeTAM ve vc (IfThenElse ex4 comInst1 comInst2)= do
 let typeofex4 = typeCheckEXPR vc ex4
 case typeofex4 of
  Just TyBool -> do
   te1 <- (expCode ve vc ex4)
   e2 <- fresh
   e3 <- fresh
   tc1 <- (comCodeTAM ve vc comInst1) 
   tc2 <- (comCodeTAM ve vc comInst2)
   return (te1 ++ [JUMPIFZ e2] ++ tc1 ++ [JUMP e3, Label e2] ++ tc2 ++ [Label e3])
  _ -> error "Expression Type is must be Boolean for condition"

comCodeTAM ve vc (BeginEnd comList)= do
 cs <- (comsCodeTAM ve vc comList)
 return (cs)

comsCodeTAM :: VarEnvNew ->VarCon-> [ComInst] -> ST Integer [TAMInst]
comsCodeTAM ve vc [] = return []
comsCodeTAM ve vc (d:ds) = do 
 td <- comCodeTAM ve vc d
 tds <- comsCodeTAM ve vc ds
 return (td++tds)

getFParamE :: ARGS -> [EXPR]
getFParamE NoParam = []
getFParamE (Param arglist) =  getListArgs arglist

getListArgs :: [ARGS'] -> [EXPR]
getListArgs [] = []
getListArgs (a:as)= ((getARGsingle a) : (getListArgs as))

getARGsingle :: ARGS' -> EXPR
getARGsingle (ArgSingle e) = e

binOpTAM :: BinOperator -> TAMInst
binOpTAM Addition       = ADD
binOpTAM Subtraction    = SUB
binOpTAM Multiplication = MUL
binOpTAM Division       = DIV
binOpTAM Conjunction    = AND
binOpTAM Disjunction    = OR
binOpTAM LssOp          = LSS
binOpTAM GtrOp          = GTR
binOpTAM EqOp           = EQL

unOpTAM :: UnOperator -> TAMInst
unOpTAM Negation = NEG
unOpTAM NegBool  = NOT

expCode :: VarEnvNew -> VarCon-> EXPR -> ST Integer [TAMInst]
expCode ve vc (LitInteger x) = 
 return ([LOADL x])
expCode ve vc (LitBool True) = 
 return ([LOADL 1])
expCode ve vc (LitBool False) = 
 return ([LOADL 0])
-- Relational operators that don't have TAM instructions
expCode ve vc(BinOp LeqOp t1 t2) =
  expCode ve vc (UnOp NegBool (BinOp GtrOp t1 t2))
expCode ve vc (BinOp GeqOp t1 t2) =
  expCode ve vc (UnOp NegBool (BinOp LssOp t1 t2))
expCode ve vc(BinOp NeqOp t1 t2) =
  expCode ve vc(UnOp NegBool (BinOp EqOp t1 t2))
-- Conditional expressions (double negation to normalize the Boolean value)
--   b ? e1 : e2  ~  (!!b) * e1 + (!b) * e2
expCode ve vc(Conditional b t1 t2) = do
  bCond <- (expCode ve vc(b))
  e1 <- fresh
  e2 <- fresh
  ifPassed <- (expCode ve vc(t1))
  ifFailed <- (expCode ve vc(t2))
  return (bCond ++ [JUMPIFZ e1] ++ ifPassed ++ [JUMP e2, Label e1] ++ ifFailed ++ [Label e2])

-- General cases
expCode ve vc(BinOp op t1 t2) = do
 t1Code <-(expCode ve vc t1)
 t2Code <- (expCode ve vc t2)
 return (t1Code ++ t2Code ++ [binOpTAM op])
expCode ve vc (UnOp op t) = do
 tUnOp <- (expCode ve vc t)
 return (tUnOp ++ [unOpTAM op])

-- state based 
expCode ve vc (Var v) = 
 return ([LOAD (address ve v)])

expCode ve vc (Func id args) =
 if (isVaraiableReal vc id)
 then 
  let es= getFParamE args in
  do
   preplocal <- expCodeParamList ve vc es
   return (preplocal ++ [CALL id])
 else 
  error "Function doesn't exist"

expCodeParamList :: VarEnvNew-> VarCon-> [EXPR] -> ST Integer [TAMInst]
expCodeParamList ve vc [] = return []
expCodeParamList ve vc (e:es) = do
 taminst <-(expCode ve vc e)
 taminstls <- (expCodeParamList ve vc es)
 return (taminst++taminstls)

-- call for function after halt
 -- CALL f = call for function f
 --begin by evaluate expr of arguement input; they will be in the stack in the order required
 --call f is for TAMCODE generation
 -- begin eith label f (id)
 --middle is TAM code for e (remember to include a vc for local variables) 
 --Return m n ; m for values to return, n for local varaible, always 1 value returned
 -- call requires activation record, not just a jump and jump back
 -- CALL:transfer control, by adding old localBase and Program counter to stack (index) as a local base
 -- return : 
 --Label f , TAM for e (w VarEnv to include local base, RETURN 1 n)
 -- RETURN clear the stack 