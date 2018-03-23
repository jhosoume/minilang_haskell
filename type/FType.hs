module FType where 

import Prelude hiding (lookup)

-- | Several type synonymous to improve readability 
type Name = String  
type FormalArg = String 
type Id = String 

-- | A type for representing function declaration
data FunDec = FunDec Name FormalArg Exp 
     deriving(Show, Eq)

-- | The abstract syntax of FType  
data Exp = Num Integer
         | Bl  Bool
         | Add Exp Exp 
         | Sub Exp Exp 
         | Mult Exp Exp
         | Div Exp Exp
         | Let Id Exp Exp
         | Ref Id 
         | App Name Exp 
         | Lambda (FormalArg, Type) Exp
         | LambdaApp Exp Exp
         | If0 Exp Exp Exp 
         | Less Exp Exp
         | Greater Exp Exp
         | Equal Exp Exp
         | IfThenElse Exp Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
     deriving(Show, Eq)     

-- | The abstract syntax for types
data Type = TInt
          | TBool
          | TArrow Type Type
          | TError
    deriving(Show, Eq)

-- | The environment with the list of deferred substitutions. 
type DefrdSub = [(Id, Value)] 

-- | The environment with the list of types. 
type Gamma = [(Id, Type)] 

-- | The value data type.
-- 
-- In this language (F4LAE), an expression
-- is reduced to a value: either a number or
-- a closure. A closure is a lambda expression
-- that *closes* together its definition with the
-- pending substitutions at definition time. 
--
data Value = NumValue Integer
           | BoolValue Bool
           | Closure FormalArg Exp DefrdSub
           | ExpV Exp DefrdSub  {-used to implement outermost-}
     deriving(Show, Eq)

-- | The interpreter function \0/
interp :: Exp -> DefrdSub -> [FunDec] -> Value

-- the simple interpreter for numbers. 
interp (Num n) ds decs = NumValue n

-- the simple interpreter for booleans. 
interp (Bl n) ds decs = BoolValue n

-- the interpreter for an add expression.                            
interp (Add lhs rhs) subs decs = binOperation (+) lhs rhs subs decs

-- the interpreter for a sub expression. 
interp (Sub lhs rhs) subs decs = binOperation (-) lhs rhs subs decs

-- the interpreter for a multiplication expression.
interp (Mult lhs rhs) subs decs = binOperation (*) lhs rhs subs decs

-- the interpreter for a div expression. 
interp (Div lhs rhs) subs decs = binOperation (div) lhs rhs subs decs 

-- the interpreter for a let expression.
-- note here that, to improve reuse, we actually
-- convert a let exprssion in the equivalent
-- lambda application (ela), and then interpret it.  
-- In here, there is no problem setting any type
-- since the interp will check types
interp (Let v e1 e2) ds decs = interp ela ds decs
  where ela = (LambdaApp (Lambda (v, TInt) e2) e1) 

-- the interpreter for a reference to a variable.
-- here, we make a lookup for the reference, in the
-- list of deferred substitutions. 
interp (Ref v) ds decs =
  let res = lookup v fst ds
  in case res of
    (Nothing) -> error $ "Variable " ++ v ++ " not found"
    (Just (_, value)) -> value 

-- the interpreter for a function application.
-- here, we first make a lookup for the function declaration,
-- evaluates the actual argument (leading to the parameter pmt),  
-- and then we interpret the function body in a new "local" environment
-- (env). 
interp (App n e) ds decs =
  let res = lookup n (\(FunDec n _ _) -> n) decs
  in case res of
    (Nothing) -> error $ "Function " ++ n ++ " not found"
    (Just (FunDec _ farg body)) -> interp body env decs
     where
       expV = ExpV e ds
       env = [(farg, expV)] 

-- the interpreter for a lambda abstraction.
-- that is the most interesting case (IMO). it
-- just returns a closure!
interp (Lambda (farg, _) body) ds decs = Closure farg body ds

-- the interpreter for a lambda application.
-- plese, infer what is going on here in this
-- case
interp (LambdaApp e1 e2) ds decs = interp body env decs -- we interpret considering the new environment
  where
    Closure farg body ds0 = strict (interp e1 ds decs) decs  -- we expect e1 to evaluate to a closure.
    expV = ExpV e2 ds                   -- ds0 is the deferred substitutions at the lambda declaration
    env  = (farg,expV):ds0              -- env is the original environment (ds0) + a new mapping

interp (If0 c t e) subs decs = if (cond == 0) then (interp t subs' decs) else (interp e subs' decs)
  where
    (NumValue cond) = strict (interp c subs decs) decs
    subs' = case c of
      (Ref id) -> (id, NumValue cond):subs
      otherwise -> subs

interp (Less lhs rhs) subs decs = boolCompareOp (<) lhs rhs subs decs

interp (Greater lhs rhs) subs decs = boolCompareOp (>) lhs rhs subs decs

interp (Equal lhs rhs) subs decs = boolCompareOp (==) lhs rhs subs decs

interp (IfThenElse c t e) subs decs = if (cond) then (interp t subs' decs) else (interp e subs' decs)
  where
    (BoolValue cond) = strict (interp c subs decs) decs
    subs' = case c of
      (Ref id) -> (id, BoolValue cond):subs
      otherwise -> subs

interp (And lhs rhs) subs decs = boolBinOp (&&) lhs rhs subs decs 

interp (Or lhs rhs) subs decs = boolBinOp (||) lhs rhs subs decs 

interp (Not c) subs decs = BoolValue (not b)
    where
    (BoolValue b) = strict (interp c subs decs) decs

strict :: Value -> [FunDec] -> Value
strict n@(NumValue v) _ = n
strict bv@(BoolValue b) _ = bv
strict c@(Closure farg body ds) _ = c
strict (ExpV e1 ds) decs = strict (interp e1 ds decs) decs

-- a new lookup function.   
lookup :: Id -> (a -> String) -> [a] -> Maybe a
lookup _ f [] = Nothing
lookup v f (x:xs)  
 | v == f x = Just x
 | otherwise = lookup v f xs

-- Binary Operation Helper
binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value
binOperation op e1 e2 subs decs = NumValue (op n1 n2)
  where
    NumValue n1 = strict (interp e1 subs decs) decs
    NumValue n2 = strict (interp e2 subs' decs) decs
    subs' = case e1 of
      (Ref id) -> (id, NumValue n1):subs
      otherwise -> subs

-- Boolean Binary Operation and Comparison Helper
boolCompareOp:: (Integer -> Integer -> Bool) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value
boolCompareOp op e1 e2 subs decs = BoolValue (op n1 n2)
  where
    NumValue n1 = strict (interp e1 subs decs) decs
    NumValue n2 = strict (interp e2 subs' decs) decs
    subs' = case e1 of
      (Ref id) -> (id, NumValue n1):subs
      otherwise -> subs

boolBinOp :: (Bool -> Bool -> Bool) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value
boolBinOp op e1 e2 subs decs = BoolValue (op n1 n2)
  where
    BoolValue n1 = strict (interp e1 subs decs) decs
    BoolValue n2 = strict (interp e2 subs' decs) decs
    subs' = case e1 of
      (Ref id) -> (id, BoolValue n1):subs
      otherwise -> subs



(|-) :: Gamma -> Exp -> Type
(|-) _     (Num n)          = TInt 
(|-) _     (Bl b)           = TBool
(|-) gamma (Add lhs rhs)    = typeBinOperation gamma lhs rhs TInt     
(|-) gamma (Sub lhs rhs)    = typeBinOperation gamma lhs rhs TInt     
(|-) gamma (Mult lhs rhs)   = typeBinOperation gamma lhs rhs TInt     
(|-) gamma (Div lhs rhs)    = typeBinOperation gamma lhs rhs TInt     

(|-) gamma (Let var e1 e2)  = (|-) gamma' e2 
    where
      t1 = (|-) gamma e1
      gamma' = (var, t1):gamma 

(|-) gamma (Ref var) =
    let res = lookup var fst gamma
    in case res of 
        (Nothing) -> error $ "Variable" ++ var ++ " not found"
        (Just (_, val)) -> val

(|-) gamma (Lambda (var, t) b) = (TArrow t tb)
    where 
        tb = (|-) ((var, t):gamma) b  

(|-) gamma (LambdaApp f val) = if (targ == tval) then tres else TError 
    where
        (TArrow targ tres) = (|-) gamma f
        tval = (|-) gamma val

(|-) gamma (If0 cond t e) = 
    if ((|-) gamma cond) == TInt
    then 
        let 
          tT = (|-) gamma t
          tE = (|-) gamma e
        in if (tT == tE) then tT else TError
    else 
        TError 

(|-) gamma (Less lhs rhs)    = typeCompareOperation gamma lhs rhs     
(|-) gamma (Greater lhs rhs) = typeCompareOperation gamma lhs rhs     
(|-) gamma (Equal lhs rhs)   = typeCompareOperation gamma lhs rhs     

(|-) gamma (IfThenElse cond t e) = 
    if ((|-) gamma cond) == TBool
    then 
        let 
          tT = (|-) gamma t
          tE = (|-) gamma e
        in if (tT == tE) then tT else TError
    else 
        TError 

(|-) gamma (And lhs rhs)  = typeBinOperation gamma lhs rhs TBool    
(|-) gamma (Or lhs rhs)   = typeBinOperation gamma lhs rhs TBool    
(|-) gamma (Not s)   = if ((|-) gamma s) == TBool then TBool else TError    

--Type Operation Helper
typeBinOperation ::  Gamma -> Exp -> Exp -> Type -> Type
typeBinOperation gamma lhs rhs t = if (l == t && r == t) then t else TError
  where
    l = (|-) gamma lhs
    r = (|-) gamma rhs

--Type Comparison Operation Helper
typeCompareOperation ::  Gamma -> Exp -> Exp -> Type
typeCompareOperation gamma lhs rhs = if (l == TInt && r == TInt) then TBool else TError
  where
    l = (|-) gamma lhs
    r = (|-) gamma rhs


