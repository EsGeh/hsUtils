--{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFoldable #-}
module Util.CountingList(
	CountingList,
	empty,
	get,(Util.CountingList.!!),cons,clLength,toList,fromList,
	module Util.Card.Card,
	module Util.Card.Unary
) where
import Util.Card.Card
import Util.Card.Unary

import Data.Foldable hiding(toList)
import Data.Generics hiding(empty)

data CountingList count a = CL [a]
	deriving(Show,Typeable,Data)
instance (Card count) => Functor (CountingList count) where
	fmap f cl@(CL list) = CL (fmap f (toList cl))
instance (Card count) => Foldable (CountingList count) where
	foldMap toMonoid (CL l) = foldMap toMonoid l
	
{--instance (Show a) => Show (CountingList count a) where
	show (CL [a]) = show [a]
	--}

--class StaticLength l where

{--
nx :: forall n . (Card n, Add N0 N1 n) => Int -> n
nx 0 = n0
n i = Succ (n (i-1))
--}
	

length' :: (Card count) => CountingList count a -> count
length' (CL list) = undefined

clLength :: (Card count) => CountingList count a -> Int
clLength cl = length $ toList cl 

toList :: (Card count) => CountingList count a -> [a]
toList (CL list) = list

fromList' :: (Card count) => count -> [a] -> CountingList count a
fromList' length list = CL list

fromList :: (Card count) => count -> [a] -> CountingList count a
fromList listLength list
	| not (toInt listLength == length list) =
		error "missmatch between dynamic size and static size"
	| otherwise = fromList' listLength list

--it should be possible to check the compile-time-type, but how??
get :: (Card count) => Int -> CountingList count a -> Maybe a
get i cl@(CL list)
	| (i >= 0) && (i< length list) = Just $ cl Util.CountingList.!! i
	| otherwise = Nothing

(!!) :: (Card count) => CountingList count a -> Int -> a
(!!) list index = (toList list) Prelude.!! index

--fromList length [] = emptyCL
--fromList length (x:xs) = consCL x (fromList length xs)

empty :: CountingList Zero a
empty = CL []

cons :: (Card n) => a -> CountingList n a -> CountingList (Succ n) a
cons x (CL l) = CL (x:l)

--uncons :: CountingList (Succ n) a -> CountingList n a
head :: (Card count) => CountingList (Succ count) a -> a 
head (CL (x:xs)) = x

tail :: (Card count) => CountingList (Succ count) a -> CountingList count a
tail (CL (x:xs)) = CL xs

(++) :: (Card countL, Card countR, Card count, Add countL countR count) =>
	CountingList countL a -> CountingList countR a -> CountingList count a
(++) ll lr = CL $ Prelude.concat [ toList ll, toList lr ]

map :: (Card count) => (a -> b) -> CountingList count a -> CountingList count b
map = fmap

foldl :: (Card count ) => (a -> b -> a) -> a -> CountingList count b -> a
foldl f z = Prelude.foldl f z . toList

foldr :: (Card count ) => (a -> b -> b) -> b -> CountingList count a -> b
foldr f z = Prelude.foldr f z . toList
{--
instance Show Peano where
	show n = show $ toInt n
--}

