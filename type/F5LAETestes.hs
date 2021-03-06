module F5LAETestes where

import Test.HUnit

import F5LAE

eval :: Exp -> Value
eval exp = interp exp [] []


tc01 = TestCase (assertEqual "for eval 4" (eval (Num 4)) (NumValue 4))

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
                                             (Let "f" (Lambda "y" (Add (Ref "x") (Ref "y")))
                                                      (Let "x" (Num 5) (LambdaApp (Ref "f")
                                                                        (Add (Ref "x") (Num 3))))))))

tc07 = TestCase (assertEqual "for eval 4 / 4" 
                             (NumValue 1)
                             (eval (Div (Num 4) (Num 4))))

tc08 = TestCase (assertEqual "Multiplication 5 * 5"
                              (NumValue 25)
                              (eval (Mult (Num 5) (Num 5))))

tc09 = TestCase (assertEqual "rec fac 2"
           (NumValue 2)           
           (eval (Rec "fac" (Lambda "x" (If0 (Ref "x") (Num 1)
                     (Mult (Ref "x") 
                            (Rec "fac" (Ref "fac") (Sub (Ref "x") (Num 1))))))
                                        (Num 2))))

tc10 = TestCase (assertEqual "rec fac 6"
           (NumValue 720)           
           (eval (Rec "fac" (Lambda "x" (If0 (Ref "x") (Num 1)
                     (Mult (Ref "x") 
                            (Rec "fac" (Ref "fac") (Sub (Ref "x") (Num 1))))))
                                        (Num 6))))

                              
allTCs = TestList $ map (\tc -> TestLabel "test" tc) [tc01, tc02, tc03, tc04, tc05, tc06, tc07, tc08, tc09, tc10]       
