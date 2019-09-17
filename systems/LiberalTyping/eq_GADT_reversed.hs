{-# OPTIONS_GHC -XGADTs #-}

data Nat = Z | S Nat
data Pair a b = Pair a b

data Rep x where
  RNat :: Rep Nat
  RBool :: Rep Bool
  RPair :: Rep a -> Rep b -> Rep (Pair a b)
  
myand :: Bool -> Bool -> Bool
myand True x  = x
myand False _ = False

eq :: a -> a -> Rep a -> Bool
eq True True RBool    = True
eq False False RBool  = True
eq False True RBool   = False
eq True False RBool   = False

eq Z Z RNat         = True
eq Z (S y) RNat     = False
eq (S x) Z RNat     = False
eq (S x) (S y) RNat = eq x y RNat

eq (Pair a1 b1) (Pair a2 b2) (RPair ra rb) = myand (eq a1 a2 ra) (eq b1 b2 rb)
