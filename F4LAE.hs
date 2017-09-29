module F4LAE where 

import Prelude hiding (lookup)
import Test.HUnit

type Name = String  
type FormalArg = String 
type Id = String 
type Args = [(FormalArg, Exp)]

type DefrdSub = [(Id, Value)] 

data FunDec = FunDec Name [FormalArg] Exp 

data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Let Args Exp 
         | Ref Id
         | App Name [Exp] 
         | Lambda [FormalArg] Exp
         | LambdaApp Exp [Exp] 
         | IfZero Exp Exp Exp 
    deriving(Read, Show, Eq)


data Value = NumValue Integer
           | Closure [FormalArg] Exp DefrdSub  
    deriving(Read, Show, Eq)

interp :: Exp -> DefrdSub -> [FunDec] -> Value
interp (Num n) _ _                  = NumValue n  
interp (Add l r) subs decs          = binOperation (+) l r subs decs
interp (Sub l r) subs decs          = binOperation (-) l r subs decs
interp (Let args e2) subs decs      = 
  let (fa, vs) = unzip args 
  in interp (LambdaApp (Lambda fa e2) vs) subs decs 
interp (Ref v) subs decs            = 
  let val = lookup v subs
  in case val of
    (Nothing) -> error "Reference not found"
    (Just val)  -> val
interp (Lambda v b) subs decs       = Closure v b subs
interp (LambdaApp e1 es) subs decs  = 
    let (Closure xs e fsubs) = (interp e1 subs decs)
        ss = [(interp e2 subs decs) | e2 <- es]
    in interp e ((zip xs ss)++fsubs) decs
interp (App n es) subs decs =
  let f = lookupFun n decs
    in case f of
        (Nothing) -> error "Function not declared"
        (Just (FunDec m as b)) -> 
            let ss = [(interp e subs decs) | e <- es]
                in interp b ((zip as ss)++subs) decs
interp (IfZero c t e) subs decs =
    let (NumValue cond) = interp c subs decs
        in if (cond == 0) then (interp t subs decs) else (interp e subs decs)  


--
lookupFun :: Name -> [FunDec] -> Maybe FunDec
lookupFun _ [] = Nothing
lookupFun f (fun@(FunDec n a b):fs)
  | f == n = Just fun
  | otherwise = lookupFun f fs

lookup :: Name -> DefrdSub -> Maybe Value
lookup _ [] = Nothing
lookup n ((i, val):vs)
  | n == i = Just val 
  | otherwise = lookup n vs


--
binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value
binOperation op e1 e2 subs decs = NumValue (op n1 n2)
 where
  (NumValue n1) = interp e1 subs decs
  (NumValue n2) = interp e2 subs decs

parse :: String -> Exp
parse = read

-- some HUnit tests cases
exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9, exp10, exp11, exp12 :: String

exp1 = "Num 5"
t1 = TestCase (assertEqual "Number" (NumValue 5) (interp (parse exp1) [] []))

exp2 = "Add (Num 5) (Num 5)"
t2 = TestCase (assertEqual "Add" (interp (parse exp2) [] []) (NumValue 10))

exp3 = "Let \"x\" (Add (Num 5) (Num 5)) (Add (Ref \"x\") (Ref \"x\"))"
t3 = TestCase (assertEqual "Let1" (interp (parse exp3) [] []) (NumValue 20))

exp4 = "Let \"x\" (Num 5) (Let \"y\" (Ref \"x\") (Ref \"y\"))"
t4 = TestCase (assertEqual "Let2" (interp (parse exp4) [] []) (NumValue  5))

exp5 = "Let \"x\" (Num 5) (Let \"x\" (Ref \"x\") (Ref \"x\"))"
t5 = TestCase (assertEqual "Let3" (interp (parse exp5) [] []) (NumValue  5))

exp6 = "LambdaApp (Lambda \"x\" (Add (Ref \"x\") (Num 2)) ) (Num 1)"
t6 = TestCase (assertEqual "Lambda" (interp (parse exp6) [] []) (NumValue  3))

exp7 = "Let \"x\" (Num 3) (Let \"f\" (Lambda \"y\" (Add (Ref \"y\") (Ref \"x\"))) (Let \"x\" (Num 5) (LambdaApp (Ref \"f\") (Num 4))))"
t7 = TestCase (assertEqual "Estatic" (interp (parse exp7) [] []) (NumValue  7))

exp8 = "Let \"x\" (Num 3) (Let \"x\" (Num 4) (Add (Ref \"x\") (Ref\"x\")))"
t8 = TestCase (assertEqual "Precedence" (interp (parse exp8) [] []) (NumValue 8))

exp9 = "Let \"x\" (Num 10) (Let \"y\" (Num 5) (Let \"f\" (Lambda \"z\" (Add (Ref \"x\") (Num 2))) (Let \"x\" (Num 20) (LambdaApp (Ref \"f\") (Add (Ref \"x\") (Ref \"y\"))))))"
t9 = TestCase (assertEqual "Closure" (interp (parse exp9) [] []) (NumValue 12))

exp10 = "Let \"f\" (Lambda \"x\" (Add (Ref \"x\") (Num 2))) (LambdaApp (Ref \"f\") (Num 1))"
t10 = TestCase (assertEqual "LambdaApp" (interp (parse exp10) [] []) (NumValue 3))

exp11 = "Let \"x\" (Num 4) (Let \"y\" (Num 5) (Let \"f\" (Lambda \"x\" (Add (Ref \"x\") (Ref \"y\"))) (LambdaApp (Ref \"f\") (Num 10))))"
t11 = TestCase (assertEqual "Scope1" (interp (parse exp11) [] []) (NumValue 15))

exp12 = "Let \"x\" (Num 3) (Let \"f\" (Lambda \"y\" (Add (Ref \"x\")(Ref \"y\"))) (Let \"x\" (Num 5) (LambdaApp (Ref \"f\") (Num 4))))"
t12 = TestCase (assertEqual "Scope2" (interp (parse exp12) [] []) (NumValue 7))


