{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
import GHC.TypeLits
import TypeLevelMisc (GetElem,SetElem,Length,IfThenElse,Span,type (++),Repeat)

type AddIdentity :: [[Nat]] -> [[Nat]]
type family AddIdentity xss where
    AddIdentity (xs ': xss) = AddIdentity' (Length xs) 0 (xs ': xss)
type AddIdentity' :: Nat -> Nat -> [[Nat]] -> [[Nat]]
type family AddIdentity' n m xss where
    AddIdentity' _ _ '[] = '[]
    AddIdentity' n m (xs ': xss) = (xs ++ Repeat m 0 ++ 1 ': Repeat (n - m - 1) 0) ': AddIdentity' n (m + 1) xss
type UpdateAll :: [[Nat]] -> [[Nat]]
type family UpdateAll xss where
    UpdateAll xss = UpdateAll' 0 xss

-- type Go1Row :: Nat -> [[Nat]] -> [[Nat]] 
-- type family Go1Row n xss where 
--     Go1Row n xs = Go1Row' n (Span n xs)
type family 


type Update :: [[Nat]] -> [[Nat]]
type family Update xss where
    Update xss = Reverse (Update' 0 ('[] ,xss))

type Update' :: Nat -> ([[Nat]],[[Nat]]) -> [[Nat]]
type family Update' n b where
    Update' n '(a, '[]) = a
    Update' n '(x,sx ': xss) = Update' (n+1) (sx ':ChangeLines n sx x, ChangeLines n sx xss)

type ChangeLines :: Nat -> [Nat] -> [[Nat]] -> [[Nat]]
type family ChangeLines n xs yss where
    ChangeLines n xs (ys ':yss) = ChangeLine n xs ys ': ChangeLines n xs yss

type ChangeLine :: Nat -> [Nat] -> [Nat] -> [Nat]
type family ChangeLine n xs ys where
    ChangeLine n xs ys = ChangeLine' (GetElem n xs) (GetElem n ys) xs ys
type ChangeLine' :: Nat -> Nat -> [Nat] -> [Nat] -> [Nat]
type family ChangeLine' n m xs ys where
    ChangeLine' _ _ '[] '[] = '[]
    ChangeLine' n m (x ': xs) (y ': ys) = (n * y - m * x) ': ChangeLine' n m xs ys

-- type SubtractLinesUp :: Nat -> [Nat] -> [[Nat]] -> [[Nat]]
-- type family SubtractLinesUp n xs yss where
--     SubtractLinesUp 0 xs yss = yss
--     SubtractLinesUp n xs yss = SubtractLinesUp (n - 1) xs (SetElem (n - 1) (Subtract2Lines (GetElem (n-1)  yss) xs) yss)
-- type SubtractLinesDown :: Nat -> [Nat] -> [[Nat]] -> [[Nat]]
-- type family SubtractLinesDown n xs yss where
--     SubtractLinesDown n xs yss = IfThenElse (Length yss <=? n) yss (SubTractLinesDown' n xs yss)
-- type SubTractLinesDown' :: Nat -> [Nat] -> [[Nat]] -> [[Nat]]
-- type family SubTractLinesDown' n xs yss where
--     SubTractLinesDown' n xs yss = SubTractLinesDown' (n + 1) xs (SetElem (n+1) (Subtract2Lines (GetElem (n+1) yss) xs) yss)
-- type Subtract2Lines :: [Nat] -> [Nat] -> [Nat]
-- type family Subtract2Lines xs ys where
--     Subtract2Lines '[] '[] = '[]
--     Subtract2Lines (x ': xs) (y ': ys) = x - y ': Subtract2Lines xs ys
-- 