-- | This module exports a matrix type as well as some functions to work with it
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
module Math.Matrix(
	-- * Types
	Matrix(),
	-- ** type aliases for indexes and width or height
	MatrIndex,IndexRow,IndexCol,Width,Height,
	-- * Matrix Pseudo Constructors
	m,mUnsafe,mSqr,
	-- * Getter
	mGet,mGetHeight,mGetWidth,
	-- * Setter
	mSet,
	-- * enhanced mapping
	mapWithIndex,
	-- ** Monadic Getters
	mGetWithOrigin,
	-- ** Lists of Indices
	mGetAllIndexRow,mGetAllIndexCol,mGetAllIndex,
	-- * Special Monads (experimental)
	LogOrigin,
	Origin(..),
	WithLog,Log(..),
	LogVal(..)
	) where
--import Card as Unary
import qualified Text.PrettyShow as Pretty

import Data.Foldable hiding(concat,toList)
import Data.Foldable as Fold hiding(concat,toList)
import Control.Applicative
import Data.Traversable
import Data.List hiding(foldl,foldr)
--import qualified Data.List as List
import Prelude hiding(foldl,foldr,Left,Right)
import Data.Monoid 
import Data.Maybe
import Control.Monad.Writer
import Data.Ratio
import Data.Array

-- |a matrix. The constructor is hidden, so it cannot be used directly - use 'm' or 'mUnsafe' instead
data Matrix t = M (Array Int (Array Int t))
	deriving(Traversable) -- don't know how to implement that, so I am using the "deriving" clause, ...
-- |index to access elements in a matrix
type MatrIndex = (IndexRow,IndexCol)
type IndexRow = Int
type IndexCol = Int
type Width = Int
type Height = Int

---------------------------------------------------------------------------------------
-- instance declarations: -------------------------------------------------------------
---------------------------------------------------------------------------------------
-- | show the matrix in a nice table like representation
instance (Show t) => Show (Matrix t) where
	show m@(M listLines) = 
		concat $ intersperse "\n" $ elems $ fmap (prettyShow " | " ((fromIntegral maxLength)%1) 0 ) $ listLines
			where
				maxLength = Fold.maximum $ fmap (length . show) m
				prettyShow = Pretty.showContainer "" "" " " " " Pretty.LeftJust
-- |enables mapping a function over every element in the matrix
instance Functor Matrix where
	fmap f (M listLines) = M $ fmap (fmap f) listLines
-- |enables to fold a matrix into a single value
instance Foldable Matrix where
	foldMap toMonoid (M l) = foldMap (foldMap toMonoid) l
{-instance Traversable Matrix where
	--traverse :: Applicative f => (a -> f b) -> Matrix a -> f (Matrix b)
	traverse f (M listLines) = traverse (mUnsafe . traverse f) listLines
	-}
-- |a matix of functions can be applied on a matrix of values
instance Applicative Matrix where
	--pure :: a -> Matrix a
	pure val = mUnsafe [[ val ]]
	--(<*>) :: Matrix (a->b) -> Matrix a -> Matrix b
	matrF <*> matrVal = mUnsafe $
		[ [ (mGet (indexRow,indexCol) matrF $ (mGet (indexRow,indexCol) matrVal)) | indexCol <- (mGetAllIndexCol matrVal) ] | indexRow <- (mGetAllIndexRow matrVal)]

-- |fmap allows to map a function over a 'Foldable'
-- but the function does not know the position of the element it is applied on.
-- 'mapWithIndex' solves this problem
mapWithIndex :: (MatrIndex -> a -> b) -> Matrix a -> Matrix b
mapWithIndex f mat = mUnsafe $
	[ [ (f (indexRow,indexCol) (mGet (indexRow,indexCol) mat)) | indexCol <- (mGetAllIndexCol mat) ] | indexRow <- (mGetAllIndexRow mat)]

-- |creates a matrix from a list of lines. The result is packed into Maybe, because the input might be invalid
m :: [[t]] -> Maybe (Matrix t)
m listLines = if (isValid listLines)
	then Just $ M $ arrayFromList height $ map (arrayFromList width) listLines
	else Nothing
	where
		isValid listLines = foldl (\x y -> x && (length y==width)) True listLines
		height = length listLines
		width = length $ listLines !! 0

-- |creates a matrix from a list of lists. If the input is invalid, 'error' is called
mUnsafe :: [[t]] -> Matrix t
mUnsafe listLines = fromMaybe (error "failed to create Matrix from list") $ m listLines

-- |create a square matrix
mSqr :: [[t]] -> Maybe (Matrix t)
mSqr = m
	-- to do: check if input makes up a valid square matrix

-- |retrieve the height of a matrix, that is the number of lines it consists of
mGetHeight :: Matrix t -> Height
mGetHeight (M listLines) = (+1) $ snd $ bounds $ listLines
-- |retrieve the width of a matrix, that is the number of columns it consists of
mGetWidth :: Matrix t -> Width
mGetWidth m@(M listLines) = if (mGetHeight m > 0) then ((+1) $ snd $ bounds $ listLines ! 0) else 0


-- |retrieve the element by its index from the matrix
mGet :: MatrIndex -> Matrix t -> t
mGet index (M listLines) = (listLines ! row) ! col
	where
		row = fst index; col = snd index

-- |returns an element, packed in the 'WithOriginMatr' Monad
mGetWithOrigin :: MatrIndex -> Matrix t -> (t,MatrIndex)
mGetWithOrigin index matr = (mGet index matr, index)

-- |returns an element, packed in the 'WithOriginMatr' Monad
mGetWithLog :: MatrIndex -> Matrix t -> LogOrigin t
mGetWithLog index matr = do
	tell $ Log [(ValO index)]
	return $ val
		where val = mGet index matr
mGetAllIndexRow matr = [0..(mGetHeight matr -1)]
mGetAllIndexCol matr = [0..(mGetWidth matr -1)]
mGetAllIndex matr = [(row,col) | row <- mGetAllIndexRow matr, col <- mGetAllIndexCol matr ]

-- |set the element at a specific index inside a matrix
mSet :: MatrIndex -> t -> Matrix t -> Matrix t
mSet index val matr = mapWithIndex maybeSet matr
	where
		maybeSet index' val' = if index' == index then val else val'


---------------------------------------------------------------------------------------
-- Monads  ----------------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- |This Monad enables you to make calculations based on values that possibly come from a matrix,
-- while logging where the values come from
-- Example:
--
-- >>> let test = mUnsafe [[1,2],[3,4]] -- creating a test matrix
--
-- >>> runWriter $ mGetWithOrigin (0,1) test
-- (2,(0,1))
--
-- first entry is the value, second is the log of this value (in fact packed into 'Origin'):
-- 
-- >>> :t runWriter $ mGetWithOrigin (0,1) ma
-- :: (Integer, Log (Origin MatrIndex))

type LogOrigin t = Writer (Log (Origin MatrIndex)) t
type WithLog t = Writer (Log (LogVal t)) t

-- |defines a log, which is considered a sequence of any type.
-- if printed it looks like this:
--
--	thing0 -> thing1 -> thing2 -> ...
{-type Log logType = [logType]
showLog :: (Show logType) => Log logType -> String
showLog listOfEntries = foldl conc "" $ map show listOfEntries 
	where
		conc "" y = y
		conc x y = x ++ " -> " ++ y-}
newtype Log logType = Log { getLog :: [logType] }
	deriving(Monoid)
instance (Show logType) => Show (Log logType) where
	show (Log list) = foldl conc "" $ map show list
		where
			conc "" y = y
			conc x y = x ++ " -> " ++ y

-- |The encodes that the origin might be given:
data Origin t =
	ValO t -- ^ origin
	| NilO -- ^ origin not known
	deriving(Show)
{-instance (Show t) => Show (Origin t) where
	show NilO = ""
	show (ValO val) = show val-}

data LogVal t = NilL | ValL t | Fun String
instance (Show t) => Show (LogVal t) where
	show NilL = ""
	show (ValL val) = show val
	show (Fun str) = str


---------------------------------------------------------------------------------------
-- internal functions -----------------------------------------------------------------
---------------------------------------------------------------------------------------
type Length = Int
arrayFromList :: Length -> [a] -> Array Length a
arrayFromList length list = listArray (0,(length-1)) list
