module FTypeTests where

import Test.HUnit

import FType

eval :: Exp -> Value
eval exp = interp exp [] []

checkType :: Exp -> Type
checkType exp = (|-) [] exp

tc01 = TestCase (assertEqual "for eval 4"  (NumValue 4) (eval (Num 4)))

tc02 = TestCase (assertEqual "for let x = 3 in x" 
                             (ExpV (Num 3) [])
                             (eval (Let "x" (Num 3) (Ref "x"))))

tc03 = TestCase (assertEqual "for let x = 3 in x + x"
                             (NumValue 6)
                             (eval (Let "x" (Num 3) (Add (Ref "x") (Ref "x")))))

tc04 = TestCase (assertEqual "for let x = div (4,0) in 5"
                             (NumValue 5)
                             (eval (Let "x" (Div (Num 4) (Num 0)) (Num 5))))


tc05 = TestCase (assertEqual "for let x = div (4,0) in let y = 5 in y"
                             (ExpV (Num 5) [("x",ExpV (Div (Num 4) (Num 0)) [])])
                             (eval (Let "x" (Div (Num 4) (Num 0)) (Let "y" (Num 5) (Ref "y")))))

tc06 = TestCase (assertEqual "for let x = 10 in let f = y -> x + y in let x = 5 in f (x + 3)"
                              (NumValue 18) 
                              (eval (Let "x" (Num 10)
                                             (Let "f" (Lambda ("y", TInt) (Add (Ref "x") (Ref "y")))
                                                      (Let "x" (Num 5) (LambdaApp (Ref "f")
                                                                        (Add (Ref "x") (Num 3))))))))

tc07 = TestCase (assertEqual "for eval 4 / 4" 
                             (NumValue 1)
                             (eval (Div (Num 4) (Num 4))))

tc08 = TestCase (assertEqual "for eval True"
                              (BoolValue True)
                              (eval (Bl True)))

tc09 = TestCase (assertEqual "for eval 4 < 3"
                              (BoolValue False)
                              (eval (Less (Num 4) (Num 3))))

tc10 = TestCase (assertEqual "for eval 4 > 3"
                              (BoolValue True)
                              (eval (Greater (Num 4) (Num 3))))

tc11 = TestCase (assertEqual "for eval 7 == 7"
                              (BoolValue True)
                              (eval (Equal (Num 7) (Num 7))))

tc12 = TestCase (assertEqual "for If (4/4 == 6/6) Then 4 * 6 Else 6/0"
                    (NumValue 24)
                    (eval (IfThenElse 
                        (Equal (Div (Num 4) (Num 4)) (Div (Num 6) (Num 6)))
                        (Mult (Num 4) (Num 6))
                        (Div (Num 6) (Num 0)))))

tc13 = TestCase (assertEqual "for (5 == 4) and (3 > 2/0)"
                                (BoolValue False)
                                (eval (And (Equal (Num 5) (Num 4)) 
                                           (Greater (Num 3) 
                                               (Div (Num 2) (Num 0))))))

tc14 = TestCase (assertEqual "for (5 == 4) or (3 > 2)"
                                (BoolValue True)
                                (eval (Or (Equal (Num 5) (Num 4)) 
                                           (Greater (Num 3) (Num 2)))))

tc15 = TestCase (assertEqual "for not (5 == 4)"
                                (BoolValue True)
                                (eval (Not (Equal (Num 5) (Num 4)))))


tc16 = TestCase (assertEqual "for |- Num 4"
                              (TInt)
                              (checkType (Num 4)))

tc17 = TestCase (assertEqual "for |- Bl true"
                              (TBool)
                              (checkType (Bl True)))

tc18 = TestCase (assertEqual "for |- 4 + 4"
                              (TInt)
                              (checkType (Add (Num 4) (Num 4))))

tc19 = TestCase (assertEqual "for |- true + 4"
                              (TError)
                              (checkType (Add (Bl True) (Num 4))))

tc20 = TestCase (assertEqual "for |- let x = 3 in x + 2"
                              (TInt)
                              (checkType (Let "x" (Num 3) 
                                            (Add (Ref "x") (Num 2))) ))

tc21 = TestCase (assertEqual "for |- lambda (x, TInt) x + 4"
                              (TArrow TInt TInt)
                              (checkType (Lambda ("x", TInt) 
                                                (Add (Ref "x") (Num 2)))))

tc22 = TestCase (assertEqual "for |- lambda (x, TBool) x"
                              (TArrow TBool TBool)
                              (checkType (Lambda ("x", TBool) 
                                                (Ref "x"))))

tc23 = TestCase (assertEqual "for |-  ((x, TInt) ->  x + 2) 3"
                              (TInt)
                              (checkType (LambdaApp (Lambda ("x", TInt) 
                                                (Add (Ref "x") (Num 2)))
                                                        (Num 3))))

tc24 = TestCase (assertEqual "for |- If0 1 - 1 then 1 < 10 else 1 > 10"
                              (TBool)
                              (checkType (If0 (Sub (Num 1) (Num 1)) 
                                              (Less (Num 1) (Num 10)) 
                                              (Greater (Num 1) (Num 10)))))

tc25 = TestCase (assertEqual "for |- If (1 less 10) and (0 equals 0) then 7 mult 3 else 10 div 2"
                              (TInt)
                              (checkType (IfThenElse (And 
                                      (Less  (Num 1) (Num 10))
                                      (Equal (Num 0) (Num 0))) 
                                              (Mult (Num 7) (Num 3)) 
                                              (Div (Num 10) (Num 2)))))

tc26 = TestCase (assertEqual "for |-  ((x, TInt) ->  x == 2) 3"
                              (TBool)
                              (checkType (LambdaApp (Lambda ("x", TInt) 
                                                (Equal (Ref "x") (Num 2)))
                                                        (Num 3))))

allTCs = TestList $ map (\tc -> TestLabel "test" tc) [tc01, tc02, tc03, tc04, tc05, tc06, tc07, tc08, tc09, tc10, tc11, tc12, tc13, tc14, tc15, tc16, tc17, tc18, tc19, tc20, tc21, tc22, tc23, tc24, tc25, tc26]       
