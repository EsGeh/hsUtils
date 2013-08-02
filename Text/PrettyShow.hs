module Text.PrettyShow where

import Data.Foldable as Fold
import Data.Maybe
import Data.Monoid
import Data.Ratio


type Text = String
type Width = Int
type Height = Int
type Size = (Width,Height)

{-data TextBlock = TB {
	text :: Text,
	size :: Size
}

toTextBlock :: Text -> TextBlock

runTextBlock :: TextBlock -> Ellipse -> Text
runTextBlock tb ellipse =  -}

--import CalcWithStatus
--import Prelude hiding(LeftJust,RightJust)

--showContainer :: (Foldable cont, Functor cont, Show e) => cont e -> String
{--
show :: (Functor cont, Foldable cont, Show e) => String -> Length -> Length -> cont e -> String
show = showContainer "" "" " " " " LeftJust 
--}

--showNice separator orient c = showContainer " " " " " " " " orient separator 0 0 c 

--addPos c = Fold.foldl merge mempty c
	--where merge list r = list `mappend` [(r,length list)]

--showContainer :: (Functor c, Foldable c, Show e) => Border -> Border -> Tile -> Tile -> Orient -> String -> Rational -> Int -> c e -> String
{--
	calls show on the elements and concats the result, intersected by "separator".
	depending on a constraints, the visualized elements are filled using tileR/-L.
	constraints:	
		elementLength
			is the maximum of "elemLenMin" and the maximal length of all visualized elements
			all elements which have a smaller length are filled.
		total length:
			if totalLength is 0:
				the #elements x elementLength + (#elements -1) * (length separator)
			else:
				if the (minTotalLength > the formula above) the remaining space is divided between the elements. If the result of the division is not an integer, the rest is unequally divided from left to right.
--}
showContainer borderL borderR tileL tileR orient separator elemLenMin minTotalLength c = fst . fromJust $ Fold.foldl conc Nothing $ fmap Just strings
	where
		conc l r = if (isJust l && isJust r)
			then let (x,ind)=fromJust l; y= fromJust r in Just (concStrings (x) (fill' (ind+1) y), ind+1)
			else Just (fill' 0 (fromJust r),0)
		concStrings x y = x ++ separator ++ y
		fill' n str = fill borderL borderR tileL tileR orient (lengthOfNthElement n) str -- str++show n
		lengthOfNthElement n = floor (elementLength*((n+1)%1)) - floor (elementLength*(n%1))
		elementLength = Prelude.maximum [elementLengthShouldBe,maxLength,elemLenMin]
		maxLength = toRational $ Fold.maximum $ fmap length strings
		elementLengthShouldBe = (toRational $ minTotalLength - length separator * (elementCount-1)) / elementCount
		elementCount :: Num a => a
		elementCount = getSum $ Fold.foldMap (\_-> Sum 1) c
		strings = fmap show c

{--
showContainer borderL borderR tileL tileR orient separator elemLenMin totalLength c = fromJust $ Fold.foldl concStrings Nothing $ fmap Just strings
	where
		strings = fmap Prelude.show c
		maxLength = toRational $ Fold.maximum $ fmap length strings
		concStrings l r = if (isJust l && isJust r)
			then do
				l' <- l
				r' <- r
				return $ fill' l' ++ separator ++ fill' r'
			else l `mappend` r
		fill' = fill borderL borderR tileL tileR orient elementLength 
		elementLength = Prelude.maximum [elementLengthShouldBe,maxLength,elemLenMin]
		elementLengthShouldBe = (toRational $ totalLength - length separator * (elementCount-1)) / elementCount
		elementCount :: Num a => a
		elementCount = getSum $ Fold.foldMap (\_-> Sum 1) c
--}

-- if string is shorter than the given Length, it is filled
-- else : the sting is returned without a change
fill :: Border -> Border -> Tile -> Tile -> Orient -> Length -> String -> String
fill borderL borderR tileL tileR orient len str = leftFill ++ str ++ rightFill
	where
		lnLeftJustFill = case orient of 
			LeftJust -> 0
			RightJust -> spaceToFill
			MidJust -> ceiling $ (toRational spaceToFill) / 2
		lnRightJustFill = case orient of
			LeftJust -> spaceToFill
			RightJust -> 0
			MidJust -> floor $ (toRational spaceToFill) / 2
		spaceToFill = if (len > length str) then len - (length str) else 0
		leftFill = take lnLeftJustFill (borderL ++ cycle tileL);
		rightFill = reverse (take lnRightJustFill (reverse borderR ++ cycle (reverse tileR))) 

data Orient = LeftJust | MidJust | RightJust

type Length = Int
type Tile = String
type Border = String
