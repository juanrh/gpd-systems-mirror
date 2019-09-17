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

eq :: Rep a -> a -> a -> Bool
eq RBool True True    = True
eq RBool False False  = True
eq RBool False True   = False
eq RBool True False   = False

eq RNat Z Z         = True
eq RNat Z (S y)     = False
eq RNat (S x) Z     = False
eq RNat (S x) (S y) = eq RNat x y

eq (RPair ra rb) (Pair a1 b1) (Pair a2 b2) = myand (eq ra a1 a2) (eq rb b1 b2)
