module Data.List.Zipper where

import Data.List (foldl')
import Prelude

data Zipper a = Zip ![a] ![a]
  deriving (Show, Eq)

instance Functor Zipper where
  fmap f (Zip l r) = Zip (fmap f l) (fmap f r)

instance Foldable Zipper where
  foldMap f (Zip l r) = foldMap f (reverse l) <> foldMap f r

empty :: Zipper a
empty = Zip [] []

singleton :: a -> Zipper a
singleton x = Zip [] [x]

fromList :: [a] -> Zipper a
fromList = Zip []

left :: Zipper a -> Zipper a
left (Zip (l : ls) r) = Zip ls (l : r)
left z = z

right :: Zipper a -> Zipper a
right (Zip l (r : rs)) = Zip (r : l) rs
right z = z

leftN :: Int -> Zipper a -> Zipper a
leftN 0 z = z
leftN n z = leftN (n - 1) (left z)

rightN :: Int -> Zipper a -> Zipper a
rightN 0 z = z
rightN n z = rightN (n - 1) (right z)

-- | Insert into the zipper and focus on the next element
push :: a -> Zipper a -> Zipper a
push x (Zip l r) = Zip (x : l) r

pushN :: Int -> a -> Zipper a -> Zipper a
pushN 0 _ z = z
pushN n x z = pushN (n - 1) x (push x z)

-- | Insert into the zipper and focus on the inserted element
insert :: a -> Zipper a -> Zipper a
insert x (Zip l r) = Zip l (x : r)

insertN :: Int -> a -> Zipper a -> Zipper a
insertN 0 _ z = z
insertN n x z = insertN (n - 1) x (insert x z)

cursor :: Zipper a -> a
cursor (Zip _ (x : _)) = x
cursor _ = error "Called cursor on empty zipper"

replace :: a -> Zipper a -> Zipper a
replace x (Zip l (_ : rs)) = Zip l (x : rs)
replace _ z = z

-- | Apply f to the current focus of the zipper and move forward
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zip l (x : rs)) = Zip (f x : l) rs
modify _ z = z

modifyN :: Int -> (a -> a) -> Zipper a -> Zipper a
modifyN 0 _ z = z
modifyN n f z = modifyN (n - 1) f (modify f z)

toList :: Zipper a -> [a]
toList (Zip l r) = foldl' (flip (:)) r l
