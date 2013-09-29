{-| notes about "Text.PrettyShow"

Introduction:

This module proposes a way how to define RenderMethods, for serializing data into a 2 dimensional representation, while giving you specific guarantees on the size the representation has.
It does not define any RenderMethods at all, but ways to combine RenderMethods into new ones (see chapter 2.). For making actually use of this module you need some /basic render methods/, e.g. "Text.TextBlock" defines some. You can then use this module to create more complex RenderMethods out of existin ones.

TODO: example

\1. Render methods

* What is a render method? It calculates a representation from some data.

* The representation should be an instance of the class 'Monoid2D', so it has some size (see 'm2size'). The size of the representation should not be arbitrary, so a 'RenderMethod' takes the desired size as parameter. (see 'RenderMethod' again)

* Beware, 'RenderMethods' are packed into an algebraic type, so you have to \"run\" them, using 'runRenderMeth', before you can apply them.

* So any 'RenderMethod' should fulfill the following law:
	TODO
	Example: TODO

If you have some basic render functions, you can use render function combinators to create new Rendermethods to create new ones:

\2. render function combinators

Now about representations:

* A representation should be an instance of 'Monoid2D', so it can be concatenated horizontally ('|||') and vertically ('===')

* If a render function fullfills the law mentioned above, new renderFunctions can be created of them (see 'lr','ud', ...)
All these combinators should guarantee, that the result of their application is

-}

module Text.PrettyShow (
	-- * data types and type classes
	-- ** basic
	Monoid2D(..),
	RenderMethod(..),
	FillFunction,
	-- ** distance partition functions
	DivDistance2, DivDistance,
	-- ** bundle of settings to increase lazyness
	RenderCombParam(..),
	-- ** simple type synonyms
	{-Area,-}Size,Pos,
	-- --* elementary render functions
	--renderNothing,
	-- * render function combinators
	-- ** render 2 things
	lr, ud,
	-- ** render many things
	vertical, horizontal,
	-- ** render many things in a specific way
	horizontalWith,
	-- * space division functions
	div2FromRatio,div2ConstAndRest,div2RestAndConst,
	divEqually, divAllConstThenCut,
	
	intersectedDistances
) where

import Util.Vector2D

import Test.QuickCheck

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Data.Ratio
import Data.Tuple(swap)
import Prelude hiding (lines)
import qualified Prelude as P

import Debug.Trace


{-type Text = String
type Width = Int
type Height = Int
type Size = (Width,Height)-}

type Pos a = Vec a
type Size a = Vec a
posX = vecX
posY = vecY
sizeX = vecX
sizeY = vecY

--type Area a = (Pos a, Size a)
size = snd
pos = fst

{- |this class is used for results of a render function. It represents things that

* have size

* can be concatenated in two ways. (|||) is for horizontal, (===) for vertical concatenation
-}
class Monoid2D a where
	-- |concatenate horizontal ( "Block | Block")
	--
	-- see 'Text.PrettyShow.Properties.prop_m2Hori'
	(|||) :: a -> a -> a 
	-- |concatenate vertically ( "Block / Block")
	--
	-- see 'Text.PrettyShow.Properties.prop_m2Vert'
	(===) :: a -> a -> a
	m2size :: a -> Size Int
	-- |neutral element for (|||) and (===). should have size (0,0)
	--
	-- see 'Text.PrettyShow.Properties.prop_m2Empty'
	m2empty :: a

-- |this type represents a method (let us call it a function) to create some data of type t, that fills a frame of some given "size". /Usually 'dest' should be an instance of "Monoid2"/
data RenderMethod src dest = RenderMeth {
	runRenderMeth :: Size Int -> src -> dest,
	minSize :: src -> Int
}

-- |if there are 2 functions that create a 'Monoid2D' of the same type, the results can be combined generically
{-(^===) :: (Monoid2D repr) => RenderMethod src repr -> RenderMethod src repr -> RenderMethod src repr
l ^=== r = lr 0.5 l r-}

type Count = Int

-- |given a distance return a partition of this distance into two distances
-- this is basically the same as the type ('DivDistance' 2), but safer, because count is fix at compile time.
type DivDistance2 = Int -> (Int,Int)
{- |given a distance partition it into \"Count\" parts

be pieces = divFunc count dist, then:

count > 0 && dist >= 0 =>

* sum div == dist

* part <- pieces => part >= 0 && part <= dist
-}
type DivDistance = Count -> Int -> [Int]

-- |takes a piece from a distance given
type PieceFromDist = Int -> Int

div2FromRatio r = divDistFunc2FromPieceF (pieceFromRatio r)
div2ConstAndRest c = divDistFunc2FromPieceF (pieceConst c)
div2RestAndConst c = divDistFunc2FromPieceF (pieceConstFromRight c)

pieceFromRatio :: (RealFrac r) => r -> PieceFromDist
pieceFromRatio ratio dist = ceiling $ fromIntegral dist * ratio

pieceConst :: Int -> PieceFromDist
pieceConst const dist = const 

pieceConstFromRight const dist = dist - pieceConst const dist

divDistFunc2FromPieceF :: PieceFromDist -> DivDistance2
divDistFunc2FromPieceF fDist dist = (l, dist - l)
	where l = fDist dist


-- |divide a distance into a number of distances
-- precondition: count > 0 (calls exception, if not fulfilled)
divEqually :: DivDistance
divEqually count dist = case count of
	0 -> error "distance cannot be divided by 0 elements!"
	1 -> [dist]
	_ -> oneElLength : divEqually (count-1) (dist - oneElLength)
		where
			oneElLength = ceiling (fromIntegral dist/fromIntegral count)

-- |divide a distance into a number of distances
-- precondition: count > 0 (calls exception, if not fulfilled)
-- precondiction: constElSize >= 0
divAllConstThenCut :: Int -> DivDistance
divAllConstThenCut constElSize count dist = case count of
	0 -> error "distance cannot be divided by 0 elements!"
	1 -> [newDist]
	_ -> (if constElSize <= dist then constElSize else newDist) :
		divAllConstThenCut constElSize (count-1) (dist - constElSize)
	where newDist = max 0 dist

{-partitionDist :: DivDistance -> Count -> Int -> [Int]
partitionDist fdist count dist = l ++ [dist - sum l]
	where l = fdist count dist-}



{-horizontal :: (Monoid2D repr) => [RenderMethod src repr] -> RenderMethod [src] repr
horizontal = horizontal_ (|||)


vertical :: (Monoid2D repr) => [RenderMethod src repr] -> RenderMethod [src] repr 
vertical renderMethods = case renderMethods of
	[] -> RenderMeth $ \size src -> m2empty
	(fstRenderMethod:otherRenderMethods) ->
		let
			fRenderFst = runRenderMeth fstRenderMethod
		in
			RenderMeth $ \size srcList -> case srcList of
				[] -> m2empty
				(fstSrc:restSrc) -> fRenderFst oneElSize fstSrc === (runRenderMeth $ vertical otherRenderMethods) (size |-| (0, vecY oneElSize)) restSrc
					where
						oneElSize = vecMap ceiling $ vecMap fromIntegral size |/| (1, fromIntegral $ minLength renderMethods srcList)
						minLength list otherList = length $ zip list otherList
				
horizontalConstWidth:: (Monoid2D repr) => Width -> [RenderMethod src repr] -> RenderMethod [src] repr
horizontalConstWidth elementWidth renderList = case renderList of
	[] -> renderNothing
	(renderF: restRenderList) ->
		RenderMeth $ \size srcList -> case srcList of
			[] -> m2empty
			(src: restSrcList) = -}
 
-- | given a tuple of two things (srcL,srcR) this method applies the first RenderMethod to the left one, and the 
-- second to the right one, while dividing the width depending on the divF function,
-- then concatenates both results horizontally using ('|||')
lr :: (Monoid2D repr) => DivDistance2 -> RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr
lr divF l r = combine2 sizeDiv (|||) l r
	where
		sizeDiv (w,h) = ((wL, h), (wR, h))
			where (wL,wR) = divF w
	
-- | given a tuple of two things (srcL,srcR) this method applies the first RenderMethod to the left one, and the 
-- second to the right one, while dividing the height depending on the divF function
-- then concatenates both results horizontally using ('===')
ud :: (Monoid2D repr) => DivDistance2 -> RenderMethod srcU repr -> RenderMethod srcD repr -> RenderMethod (srcU,srcD) repr
ud divF l r = combine2 sizeDiv (===) l r
	where
		sizeDiv (w,h) = ((w, hU), (w, hD))
			where (hU,hD) = divF w

type CombineRepr2 repr = repr -> repr -> repr
type CombineMethods2 repr srcL srcR = RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr

combine2 :: (Monoid2D repr) => (Size Int -> (Size Int,Size Int)) -> CombineRepr2 repr -> RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr
combine2 sizeF combineRepr methL methR = RenderMeth{
	runRenderMeth = newRenderMeth,
	minSize = \(valL,valR) -> ((minSize methL) valL + (minSize methR) valR ) }
	where
		(lf,rf) = (runRenderMeth methL, runRenderMeth methR)
		newRenderMeth = \size (lsrc,rsrc) ->
			let (sizeL,sizeR) = sizeF size
			in
				lf sizeL lsrc `combineRepr` rf sizeR rsrc

{-horizontalWith :: (Monoid2D repr) => (Size Int -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
horizontalWith middle = horizontal_ concWithGap
	where
		concWithGap l r = l ||| middle size ||| r
			where size = (1, max (vecY $ m2size l) (vecY $ m2size r))
-}

printVal str x = traceShow (str ++ show x) x

{-
type DivDistance2Types l r = [Either l r] -> Int -> [Int]

divGrowingGaps :: Int -> DivDistance -> DivDistance2Types
divGrowingGaps constElSize divF inputList width = if firstRight <= constElSize then 
	where
		(firstRight : restRight) = divF countRight width
-}
{-
divGrowingGaps constElSize divF inputList width = if restSize <= 0
	then intersect (divF countRight width) (repeat 0)
	else intersect (divF countRight widthElements) (divFLeft countLeft restSize)
	
	)
	where
		countRight = length $ filter isRight inputList
		countLeft = length $ filter isLeft inputList
		widthElements = (length inputList) * constElSize
		restSize = width - widthElements
		-}

horizontalWith :: (Monoid2D repr) => FillFunction repr -> RenderCombParam repr -> [RenderMethod src repr] -> RenderMethod [src] repr
horizontalWith middleF renderCombP renderList = RenderMeth {
	runRenderMeth = renderF,
	minSize = newMinSize }
	where
		--renderF :: Size Int -> [src] -> repr
		renderF size listSrc = (runRenderMeth renderMethWithSeperators) size (addSeperators listSrc)
		newMinSize listSrc = (minSize renderMethWithSeperators) (addSeperators listSrc) --sum $ minSize <$> renderList <*> valList

		--addSeperators :: [src] -> [Either () src]
		addSeperators listSrc = intersperse (Left ()) (map Right listSrc)

		--renderMethWithSeperators :: RenderMethod [Either () src] repr
		renderMethWithSeperators = horizontal renderCombP{ divF=(intersectedDistances (divF renderCombP) 1) } (newRenderList renderList)
		--newRenderList :: [RenderMethod src repr] -> [RenderMethod (Either () src) repr]
		newRenderList remainingMeths = case remainingMeths of
			[] -> []
			(fstMeth:otherMeths) -> (RenderMeth { runRenderMeth = firstRenderM, minSize = firstMinSize }) : newRenderList otherMeths where
				firstRenderM = \size src -> case src of
					Left _ -> middleF size
					Right val -> (runRenderMeth fstMeth) size val 
				firstMinSize = \src -> case src of
					Left _ -> 0
					Right val -> (minSize fstMeth) val

intersectedDistances divF separatorLength count dist = intersperse separatorLength distances
	where
		distances = divF countElements (dist - countSeparators*separatorLength) 
		countElements = floor $ (fromIntegral $ count+1)/2
		countSeparators = countElements - 1
	
{-
divF' count w = intersectedDistances
	where
		intersectedDistances = intersperse 1 distances
		-}

-- | this method applies the list of 'RenderMethod's to a list of things ([src])
-- while dividing the width depending on the divF function,
-- then concatenates the results horizontally using (|||)
-- the number of representations is
--
-- > min (length renderList) (length srcList)
horizontal :: (Monoid2D repr) => RenderCombParam repr -> [RenderMethod src repr] -> RenderMethod [src] repr
horizontal RenderCombP{fillF=fillF, divF= divF} renderList = combine fillF divF' (|||) renderList
	 where
	 	divF' count (w,h) = zip distances (repeat h)
			where distances = divF count w
vertical :: (Monoid2D repr) => RenderCombParam repr -> [RenderMethod src repr] -> RenderMethod [src] repr
vertical RenderCombP{fillF=fillF, divF= divF} renderList = combine fillF divF' (===) renderList
	 where
	 	divF' count (w,h) = zip (repeat w) distances 
			where distances = divF count h

data RenderCombParam repr = RenderCombP{
	divF :: Count -> Int -> [Int], -- space division function
	fillF :: Size Int -> repr -- function to call if there is nothing to render
}

type FillFunction repr = Size Int -> repr

--rndrCombPStd = RenderCombP{ divF = divEqually, fillF

combine :: (Monoid2D repr) => FillFunction repr -> (Count -> Size Int -> [Size Int]) -> (repr -> repr -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
combine fillF divF conc renderList = RenderMeth { runRenderMeth= newRenderMeth, minSize = \valList -> sum $ minSize <$> renderList <*> valList } where
	newRenderMeth = \size srcList ->
		let
			count = {-printVal "combine count: " $-} saveMinimumLength renderList srcList
		in
			case count of
				0 -> fillF size
				_ -> (runRenderMeth $ combine_ (divF count size) conc renderList) size srcList
		where
			saveMinimumLength l r = length $ zip l r

-- precond: listSize = maximum (length renderList, length srcList)
combine_ :: (Monoid2D repr) => [Size Int] -> (repr -> repr -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
combine_ listSize conc renderList = {-trace "calling combine_" $-} if count == 0 then (error "nothing to print") else
	case renderList of
		(fstRender:restRender) -> RenderMeth{ runRenderMeth=newRenderMeth, minSize = newMinSize } where
			newRenderMeth = \size srcList -> case srcList of
				[] -> error "to few sources"
				(fstSrc:restSrc) -> if count == 1
					then ((runRenderMeth $ fstRender) (head listSize) fstSrc)
					else ((runRenderMeth $ fstRender) (head listSize) fstSrc) `conc`
						((runRenderMeth $ combine_ (tail listSize) conc restRender) size restSrc)
			newMinSize = \srcList -> case srcList of
				[] -> error "to few sources"
				(fstSrc:restSrc) -> if count == 1
					then ((minSize $ fstRender) fstSrc)
					else ((minSize $ fstRender) fstSrc) +
						((minSize $ combine_ (tail listSize) conc restRender) restSrc)
	where count = {-printVal "count: " $-} length listSize

renderNothing :: (Monoid2D repr) => RenderMethod src repr
renderNothing = RenderMeth { runRenderMeth = \size src -> m2empty, minSize = \src -> 0 }

