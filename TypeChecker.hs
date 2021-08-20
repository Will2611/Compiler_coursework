module TypeChecker where

import FunParser
import ExpParser
import DecParser
import States

import Control.Applicative
import Data.Maybe

type VarCon = [(Identity, VFType)]

identifierFreeCtx :: VarCon -> Identity -> Bool
identifierFreeCtx vc v = case lookup v vc of 
 Nothing -> True
 Just a -> False

--expression; function args to actual
typeCheckParamE :: VarCon -> ARGS -> [Maybe Type]
typeCheckParamE vc (Param arglist) =  typeCheckArgListE vc arglist
typeCheckParamE vc NoParam = []

typeCheckArgListE :: VarCon -> [ARGS'] -> [Maybe Type]
typeCheckArgListE vc [] = []
typeCheckArgListE vc (a:as) = (typeCheckArgSingleE vc a :(typeCheckArgListE vc as))

typeCheckArgSingleE :: VarCon -> ARGS'-> Maybe Type
typeCheckArgSingleE vc (ArgSingle e) = typeCheckEXPR vc e

--typeCheck for expr gen case
typeCheckEXPR :: VarCon-> EXPR -> Maybe Type
typeCheckEXPR vc (LitInteger x) = Just TyInt
typeCheckEXPR vc (LitBool b) = Just TyBool

typeCheckEXPR vc (BinOp op e1 e2) = do
 t1 <- typeCheckEXPR vc e1
 t2 <- typeCheckEXPR vc e2
 binType op t1 t2

typeCheckEXPR vc (Conditional e1 e2 e3) = do
 t1 <- typeCheckEXPR vc e1
 t2 <- typeCheckEXPR vc e2
 t3 <- typeCheckEXPR vc e3
 if t1==TyBool && t2==t3 
 then return t3
 else Nothing
typeCheckEXPR vc (UnOp op e1) = do
 t1 <- typeCheckEXPR vc e1
 unType op t1
typeCheckEXPR vc (Var v) = case lookup v vc of 
 Nothing -> Nothing
 Just a -> Just (unPackType a)

typeCheckEXPR vc (Func id param) = case lookup id vc of
 Nothing -> Nothing -- doesnt exist
 Just a -> do 
  let argTlist = typeCheckParamE vc param
  if (length argTlist) == (length (getFunListType a))
  then
   if (argTlist) == (getFunListType a)
   then
    (Just (unPackType a))
   else
    Nothing
  else
   Nothing  

binType :: BinOperator -> Type -> Type -> Maybe Type
-- Int -> Int -> Int
binType Addition TyInt TyInt = Just TyInt
binType Subtraction TyInt TyInt = Just TyInt
binType Multiplication TyInt TyInt = Just TyInt
binType Division TyInt TyInt = Just TyInt
--Int -> Int  -> Bool
binType LeqOp TyInt TyInt = Just TyBool
binType LssOp TyInt TyInt = Just TyBool
binType GeqOp TyInt TyInt = Just TyBool
binType GtrOp TyInt TyInt = Just TyBool
--a-> a -> Bool
binType EqOp t1 t2 = if t1==t2 then Just TyBool else Nothing
binType NeqOp t1 t2 = if t1==t2 then Just TyBool else Nothing
--Bool-> Bool-> Bool
binType Conjunction TyBool TyBool = Just TyBool
binType Disjunction TyBool TyBool = Just TyBool
binType _ _ _ = Nothing

unType :: UnOperator -> Type -> Maybe Type
unType Negation TyInt = Just TyInt
unType NegBool TyBool = Just TyBool
unType _ _ = Nothing

--getting Type for context
unPackType :: VFType -> Type 
unPackType (VarType t) = t
unPackType (FunType ts ta) =ta 

-- ts for list, ta for actual
getFunListType :: VFType -> [Maybe Type]
getFunListType (FunType ts ta) = toMaybe ts where
 toMaybe (typeSin:typeList) = ((Just typeSin):(toMaybe typeList))
 toMaybe [] = []
getFunListType _ = [Nothing] 
-- will not occur, but just im case of variable written as function

--creating Type context from declaration

--create VarCon from declaration
--individual match declaration to var context
checkVar :: String -> ST (VarCon) Error
checkVar src = do
 vc <- stState
 case lookup src vc of 
  Nothing -> return noError
  Just a -> return (errMsg "Multiple Declaration of same varaible")

checkVType ::EXPR -> Type -> ST (VarCon) Error
checkVType expr t = do
 vc <- stState
 case (typeCheckEXPR vc expr) of
  Nothing -> return (errMsg "Expression not Valid, incompatible types detected")
  Just t1 -> if t1 == t then return noError
                        else return (errMsg "Type does not match declared Type")

checkFType ::EXPR ->ParamDeclMult-> Type -> ST (VarCon) Error
checkFType expr funparam t = do
 vc <- stState
 let (err,localctx)= app (typeContextFunParamMult funparam)[]
 case err of
  Nothing -> case (typeCheckEXPR (localctx++vc) expr) of
              Nothing -> return (errMsg "Expression not Valid, incompatible types detected")
              Just t1 -> if t1 == t then return noError
                                    else return (errMsg "Expression Type does not match declared Type")
  Just e -> return err

--typeCheckEXPR on Declaration
chkDecSingle :: Declaration -> ST (VarCon) Error
chkDecSingle (VarDecl v t) = do
 isAvl <- checkVar v
 case isAvl of 
  Nothing -> stRevise ((v,(VarType t)):) >> return noError
  _ -> return isAvl
chkDecSingle (VarInit v t e) = do
 isAvl <- checkVar v
 isMatch <- checkVType e t
 case isAvl of 
  Nothing -> case isMatch of
             Nothing -> stRevise ((v,(VarType t)):) >> return noError
             _ -> return isMatch
  _ -> return isAvl
chkDecSingle (FunDecl v param t e) = do
 let typelist= (typeCreateFunParamOrNo param)
 isAvl <- checkVar v
 stRevise ((v,(FunType typelist t)):) --temporarily, to allow recursion
 isMatch <- checkFType e param t
 case isAvl of 
  Nothing -> case isMatch of
             Nothing -> return noError
             _ -> stRevise tail >> return isMatch --failure, so remove from context
  _ -> stRevise tail >> return isAvl --failure, so remove from context

chkDecType :: [Declaration] -> ST (VarCon) Error
chkDecType [] = return noError
chkDecType (d:ds) = do
 e <- chkDecSingle d
 case e of 
  Nothing -> chkDecType ds
  _ -> return e


--Create [Type] for FunType [Type] from declaration, also checking local
typeCreateFunParamOrNo :: ParamDeclMult -> [Type]
typeCreateFunParamOrNo (ParamD vds) = typeCreateFunParam vds
typeCreateFunParamOrNo NoParamD = []

typeCreateFunParam :: [ParamDecl] -> [Type]
typeCreateFunParam = fmap typeCreateFunParamSingle

typeCreateFunParamSingle :: ParamDecl -> Type
typeCreateFunParamSingle (VDecSingle id t) = t

--creating local VarContext, for FunType [Type] Type from declaration
typeContextFunParamMult :: ParamDeclMult -> ST (VarCon) Error
typeContextFunParamMult (ParamD params) = do
 locCTX <- (typeContextFunParam params)
 return locCTX
typeContextFunParamMult NoParamD = do 
 return Nothing

typeContextFunParam ::[ParamDecl] -> ST (VarCon) Error
typeContextFunParam [] = return noError
typeContextFunParam (p:ps)= do
 e <- typeContextFunParamSingle p
 case e of
  Nothing -> typeContextFunParam ps
  _  -> return e

typeContextFunParamSingle :: ParamDecl -> ST (VarCon) Error
typeContextFunParamSingle (VDecSingle id t) =do  -- change to fit var context
 isAvl <- checkVar id
 case isAvl of 
  Nothing -> stRevise ((id,(VarType t)):) >> return noError
  _ -> return isAvl


