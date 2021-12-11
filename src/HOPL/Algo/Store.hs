
module HOPL.Algo.Store
  ( Store,
    emptyStore,
    newref,
    deref,
    setref,
    makePair,
    left,
    right,
    setLeft,
    setRight,
  )
where

import HOPL.Algo.DataStructures (MutPair, Pair1, Pair2, StoVal)
import HOPL.Types (Reference)

type Store = [StoVal]

emptyStore :: Store
emptyStore = []

-- Free store represented as a list
newref :: StoVal -> Store -> (Reference, Store)
newref val store = (length store, store ++ [val])

deref :: Reference -> Store -> StoVal
deref ref store = store !! ref

setref :: Reference -> StoVal -> Store -> Store
setref ref val store = take ref store ++ (val : drop (ref + 1) store)

{- Basic interface to a mutable pair representation -}

makePair :: Store -> StoVal -> StoVal -> (MutPair, Store)
makePair = makePair2

left :: Store -> MutPair -> StoVal
left = left2

right :: Store -> MutPair -> StoVal
right = right2

setLeft :: Store -> MutPair -> StoVal -> Store
setLeft = setLeft2

setRight :: Store -> MutPair -> StoVal -> Store
setRight = setRight2

makePair1 :: Store -> StoVal -> StoVal -> (Pair1, Store)
makePair1 σ v₁ v₂ = ((addr₁, addr₂), σ₂)
  where
    (addr₁, σ₁) = newref v₁ σ
    (addr₂, σ₂) = newref v₂ σ₁

left1 :: Store -> Pair1 -> StoVal
left1 σ (addr, _) = deref addr σ

right1 :: Store -> Pair1 -> StoVal
right1 σ (_, addr) = deref addr σ

setLeft1 :: Store -> Pair1 -> StoVal -> Store
setLeft1 σ (addr, _) v = setref addr v σ

setRight1 :: Store -> Pair1 -> StoVal -> Store
setRight1 σ (_, addr) v = setref addr v σ

makePair2 :: Store -> StoVal -> StoVal -> (Pair2, Store)
makePair2 σ v₁ v₂ = (addr, σ₂)
  where
    (addr, σ₁) = newref v₁ σ
    (_, σ₂) = newref v₂ σ₁

left2 :: Store -> Pair2 -> StoVal
left2 σ addr = deref addr σ

right2 :: Store -> Pair2 -> StoVal
right2 σ addr = deref (addr + 1) σ

setLeft2 :: Store -> Pair2 -> StoVal -> Store
setLeft2 σ addr v = setref addr v σ

setRight2 :: Store -> Pair2 -> StoVal -> Store
setRight2 σ addr v = setref (addr + 1) v σ
