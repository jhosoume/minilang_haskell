module Monadas where

import Prelude hiding (return , (>>=))

data M a - State -> (a, State)

return :: a -> M a
return a = \s -> (a, s)

(>>=) :: M a -> (a -> M b) -> M b

m >>= f = \s0 -> let (v, s1) = m s0
                 in f v s1

type State = Int

data Term = Con a | Div Term Term

eval :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = eval t >>= \v1 -> eval u >>= \v2 -> return v1 'div' v2 





-- Old way
{-eval (Con a) = \s -> (a, s) -- igual a: eval (Con a) s = (a, s)-}
{-eval (Div t u) =  \s0 ->-}
    {-let -}
        {-(v1, s1) = eval t s0-}
        {-(v2, s2) = eval u s1-}
    {-in (v1 'div' v2, s2+1)-}
