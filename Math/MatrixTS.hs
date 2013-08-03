-- | This module exports a matrix of static size. If you know the size at compile time, use this module, it enables the type checker to exclude many errors at compile time. If the size is not known at compile time you should use the module 'Math.Matrix'
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.MatrixTS(
	Matrix(),
	MatrIndex,IndexRow,IndexCol,
	mGetAllIndexRow,mGetAllIndexCol,mGetAllIndex,
	m,mSqr,mGetHeight,mGetWidth,mGet,
	WithOriginMatr,WithLog,Log(..),Origin(..),LogVal(..),
	mGetWithOrigin 
	) where
import Util.CountingList as CL
import qualified Text.PrettyShow as Pretty

import Data.Foldable hiding(concat,toList)
import Data.Foldable as Fold hiding(concat,toList)
import Data.List hiding(foldl,foldr,(!!))
--import qualified Data.List as List
import Prelude hiding(foldl,foldr,Left,Right,(!!))
import Data.Monoid 
import Data.Maybe
import Control.Monad.Writer
import Data.Ratio


data Matrix m n t = M (CountingList m (CountingList n t))
-- instances
instance (Show t,Card n, Card m) => Show (Matrix m n t) where
	show m@(M listLines) = 
		concat $ intersperse "\n" $ map (prettyShow " | " ((fromIntegral maxLength)%1) 0 . toList) $ toList listLines
			where
				maxLength = Fold.maximum $ fmap (length . show) m
				prettyShow = Pretty.showContainer "" "" " " " " Pretty.LeftJust
instance (Card m, Card n) => Functor (Matrix m n) where
	fmap f (M listLines) = M $ fmap (fmap f) listLines
instance (Card m, Card n) => Foldable (Matrix m n) where
	foldMap toMonoid (M l) = foldMap (foldMap toMonoid) l

-- function
m :: (Card countX, Card countY) => countY -> countX -> [[t]] -> Matrix countY countX t
m countLines countCol listLines = M $ fromList countLines (map (fromList countCol) listLines)

mGetHeight :: (Card height, Card width) => Matrix height width t -> Int
mGetHeight (M listLines) = clLength listLines
mGetWidth :: (Card m, Card n) => Matrix m n t -> Int
mGetWidth (M listLines) = clLength $ listLines !! 0


mSqr count = m count count

mGet :: (Card height, Card width) => MatrIndex -> Matrix height width t -> t
mGet index (M listLines) = (listLines CL.!! row) CL.!! col
	where
		row = fst index; col = snd index

mIndex m n = (m,n)

mGetWithOrigin :: (Card height, Card width, Show t) => MatrIndex -> Matrix height width t -> WithOriginMatr t
mGetWithOrigin index matr = do
	tell $ Log [(ValO index)]
	return $ val
		where val = mGet index matr
mGetAllIndexRow matr = [0..(mGetHeight matr -1)]
mGetAllIndexCol matr = [0..(mGetWidth matr -1)]
mGetAllIndex matr = [(row,col) | row <- mGetAllIndexRow matr, col <- mGetAllIndexCol matr ]

--mGetAllIndexDist matr = [ index | index <- mGetAllIndex matr, (fst index /= snd index) ]


--mGetAllWC matr = [ mGetWC m n matr | m <- [0..(mGetHeight matr - 1)], n <- [0..(mGetWidth matr - 1)] ]

type MatrIndex = (IndexRow,IndexCol)
type IndexRow = Int
type IndexCol = Int
type WithOriginMatr t = Writer (Log (Origin MatrIndex)) t
type WithLog t = Writer (Log (LogVal t)) t
--type WC w t = (Writer w t)

newtype Log logType = Log { getLog :: [logType] }
	deriving(Monoid)
instance (Show logType) => Show (Log logType) where
	show (Log list) = foldl conc "" $ map show list
		where
			conc "" y = y
			conc x y = x ++ " -> " ++ y

data Origin t = NilO | ValO t 
instance (Show t) => Show (Origin t) where
	show NilO = ""
	show (ValO val) = show val

data LogVal t = NilL | ValL t | Fun String
instance (Show t) => Show (LogVal t) where
	show NilL = ""
	show (ValL val) = show val
	show (Fun str) = str
