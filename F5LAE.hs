module F5LAE where 

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
  let nsubs = [(farg, interp e1 subs decs) | (farg, e1) <- args]
  in interp e2 (nsubs++subs) decs
interp (Ref v) subs decs            = 
  let val = lookup v subs
  in case val of
    (Nothing) -> error "Reference not found"
    (Just val)  -> val
interp (Lambda v b) subs decs       = Closure v b subs
interp (LambdaApp e1 es) subs decs  = 
    let (Closure xs e fsubs) = (interp e1 subs decs)
        ss = [(interp e2 subs decs) | e2 <- es]
    in interp e ((zip xs ss)++subs) decs
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

-- sample functions
inc = FunDec "inc" ["x"] (Add (Ref "x") (Num 1))

-- some HUnit tests cases
exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9, exp10, exp11, exp12, exp13, exp14, exp15 :: String

exp1 = "Num 5"
t1 = TestCase (assertEqual "Number" (NumValue 5) (interp (parse exp1) [] []))

exp2 = "Add (Num 5) (Num 5)"
t2 = TestCase (assertEqual "Add" (NumValue 10) (interp (parse exp2) [] []))

exp3 = "Let [(\"x\", (Add (Num 5) (Num 5)))] (Add (Ref \"x\") (Ref \"x\"))"
t3 = TestCase (assertEqual "Let1" (NumValue 20) (interp (parse exp3) [] []))

exp4 = "Let [(\"x\", (Num 5))] (Let [(\"y\", (Ref \"x\"))] (Ref \"y\"))"
t4 = TestCase (assertEqual "Let2" (NumValue  5) (interp (parse exp4) [] []))

exp5 = "Let [(\"x\",(Num 5))] (Let [(\"x\", (Ref \"x\"))] (Ref \"x\"))"
t5 = TestCase (assertEqual "Let3" (NumValue  5) (interp (parse exp5) [] []))

exp6 = "LambdaApp (Lambda [(\"x\")] (Add (Ref \"x\") (Num 2)) ) [(Num 1)]"
t6 = TestCase (assertEqual "Lambda" (NumValue  3) (interp (parse exp6) [] []))

exp7 = "Let [(\"x\", (Num 3))] (Let [(\"f\", (Lambda [\"y\"] (Add (Ref \"y\") (Ref \"x\"))))] (Let [(\"x\", (Num 10))] (LambdaApp (Ref \"f\") [(Num 4)])))"
t7 = TestCase (assertEqual "Estatic" (NumValue  14) (interp (parse exp7) [] []))

exp8 = "Let [(\"x\", (Num 3))] (Let [(\"x\", (Num 4))] (Add (Ref \"x\") (Ref\"x\")))"
t8 = TestCase (assertEqual "Precedence" (NumValue 8) (interp (parse exp8) [] []))

exp9 = "Let [(\"x\", (Num 10))] (Let [(\"y\", (Num 5))] (Let [(\"f\", (Lambda [\"z\"] (Add (Ref \"x\") (Num 2))))] (Let [(\"x\", (Num 20))] (LambdaApp (Ref \"f\") [(Add (Ref \"x\") (Ref \"y\"))]))))"
t9 = TestCase (assertEqual "Closure" (NumValue 22) (interp (parse exp9) [] []))

exp10 = "Let [(\"f\", (Lambda [\"x\"] (Add (Ref \"x\") (Num 2))))] (LambdaApp (Ref \"f\") [(Num 1)])"
t10 = TestCase (assertEqual "LambdaApp" (NumValue 3) (interp (parse exp10) [] []))

exp11 = "Let [(\"x\", (Num 4))] (Let [(\"y\", (Num 5))] (Let [(\"f\", (Lambda [\"x\"] (Add (Ref \"x\") (Ref \"y\"))))] (LambdaApp (Ref \"f\") [(Num 10)])))"
t11 = TestCase (assertEqual "Scope1" (NumValue 15) (interp (parse exp11) [] []))

exp12 = "Let [(\"x\", (Num 3))] (Let [(\"f\", (Lambda [\"y\"] (Add (Ref \"x\")(Ref \"y\"))))] (Let [(\"x\", (Num 5))] (LambdaApp (Ref \"f\") [(Num 4)])))"
t12 = TestCase (assertEqual "Scope2" (NumValue 9) (interp (parse exp12) [] []))

exp13 = "Let [(\"n\", (Num 1))] (Let [(\"f\", (Lambda [\"x\"] (Add (Ref \"x\") (Ref \"n\"))))] (LambdaApp (Ref \"f\") [(Num 2)]))"
t13 = TestCase (assertEqual "Scope3" (NumValue 3) (interp (parse exp13) [] []))

exp14 = "Let [(\"n\", (Num 1))] (Let [(\"f\", (Lambda [\"x\"] (Add (Ref \"x\") (Ref \"n\"))))] (Let [(\"n\", (Num 3))] (LambdaApp (Ref \"f\") [(Num 1)])))"
t14 = TestCase (assertEqual "Scope4" (NumValue 4) (interp (parse exp14) [] []))

exp15 = "App \"inc\" [(Num 5)]"
t15 = TestCase (assertEqual "App" (NumValue 6) (interp (parse exp15) [] [inc]))

exp16 = "Let [(\"x\", Num 2), (\"y\", Num 10)] (Add (Ref \"x\") (Ref \"y\"))"
t16 = TestCase (assertEqual "Multiple args" (NumValue 12) (interp (parse exp16) [] []))

exp17 = "Let [(\"x\", Num 2), (\"y\", Num 10)] (IfZero (Add (Ref \"x\") (Ref \"y\")) (Num 2) (Num 1))"
t17 = TestCase (assertEqual "If Zero 1" (NumValue 1) (interp (parse exp17) [] []))

exp18 = "Let [(\"x\", Num 2), (\"y\", Num 2)] (IfZero (Sub (Ref \"x\") (Ref \"y\")) (Num 2) (Num 1))"
t18 = TestCase (assertEqual "If Zero 2" (NumValue 2) (interp (parse exp18) [] []))
tests = TestList [TestLabel "Number"     t1,
                  TestLabel "Addition"   t2,
                  TestLabel "Let 1"      t3,
                  TestLabel "Let 2"      t4,
                  TestLabel "Let 3"      t5,
                  TestLabel "Lambda"     t6,
                  TestLabel "Estatic"    t7,
                  TestLabel "Precedence" t8,
                  TestLabel "Closure"    t9,
                  TestLabel "LambdaApp"  t10,
                  TestLabel "Scope 1"    t11,
                  TestLabel "Scope 2"    t12,
                  TestLabel "Scope 3"    t13,
                  TestLabel "Scope 4"    t14,
                  TestLabel "Func App"   t15,
                  TestLabel "Mult Args"  t16,
                  TestLabel "If Zero 1"  t17,
                  TestLabel "If Zero 2"  t18]

