module Text.PrettyShow (
	-- * data types and type classes
	Monoid2D(..),
	RenderMethod(..),
	DivDistance2, DivDistance,
	Area,Size,Pos,
	-- * elementary render functions
	renderNothing,
	-- * render function combinators
	lr, ud,
	vertical, horizontal,
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

type Area a = (Pos a, Size a)
size = snd
pos = fst

-- |things that can be concatenated in two ways. (|||) is for horizontal, (===) for vertical concatenation
class Monoid2D a where
	(|||) :: a -> a -> a -- concatenate horizontal ( "Block | Block")
	(===) :: a -> a -> a -- concatenate vertically ( "Block / Block")
	m2size :: a -> Size Int
	m2empty :: a

-- |this type represents a method (let us call it a function) to create some data of type t, that fills a "frame" of some given size
data RenderMethod src dest = RenderMeth {
	runRenderMeth :: Size Int -> src -> dest
}

-- |if there are 2 functions that create a 'Monoid2D' of the same type, the results can be combined generically
{-(^===) :: (Monoid2D repr) => RenderMethod src repr -> RenderMethod src repr -> RenderMethod src repr
l ^=== r = lr 0.5 l r-}

type Count = Int

-- |given a distance return the first part of this distance
type DivDistance2 = Int -> Int
-- |given a distance partition it into Count parts
type DivDistance = Count -> Int -> [Int]

div2FromRatio :: (RealFrac r) => r -> DivDistance2
div2FromRatio ratio dist = ceiling $ fromIntegral dist * ratio

div2ConstAndRest :: Int -> DivDistance2
div2ConstAndRest const dist = const 

div2RestAndConst const dist = dist - div2ConstAndRest const dist

partitionDist2 :: DivDistance2 -> Int -> (Int,Int)
partitionDist2 fdist dist = (l, dist - l)
	where l = fdist dist


divEqually :: Count -> Int -> [Int]
divEqually count length = case count of
	0 -> error "distance cannot be divided by 0 elements!"
	1 -> [length]
	_ -> oneElLength : divEqually (count-1) (length - oneElLength)
		where
			oneElLength = ceiling (fromIntegral length/fromIntegral count)

-- precondiction: constElSize >= 0
divAllConstThenCut :: Int -> Count -> Int -> [Int]
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
 
lr :: (Monoid2D repr) => DivDistance2 -> RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr
lr spacing l r = combine2 sizeDiv (|||) l r
	where
		sizeDiv (w,h) = ((wL, h), (wR, h))
			where (wL,wR) = partitionDist2 spacing w
	
ud :: (Monoid2D repr) => DivDistance2 -> RenderMethod srcU repr -> RenderMethod srcD repr -> RenderMethod (srcU,srcD) repr
ud spacing l r = combine2 sizeDiv (===) l r
	where
		sizeDiv (w,h) = ((w, hU), (w, hD))
			where (hU,hD) = partitionDist2 spacing w


type CombineRepr2 repr = repr -> repr -> repr
type CombineMethods2 repr srcL srcR = RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr

combine2 :: (Monoid2D repr) => (Size Int -> (Size Int,Size Int)) -> CombineRepr2 repr -> RenderMethod srcL repr -> RenderMethod srcR repr -> RenderMethod (srcL,srcR) repr
combine2 sizeF combineRepr methL methR = let
	(lf,rf) = (runRenderMeth methL, runRenderMeth methR)
	in
	RenderMeth $ \size (lsrc,rsrc) ->
		let
			(sizeL,sizeR) = sizeF size
		in
			lf sizeL lsrc `combineRepr` rf sizeR rsrc

{-horizontalWith :: (Monoid2D repr) => (Size Int -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
horizontalWith middle = horizontal_ concWithGap
	where
		concWithGap l r = l ||| middle size ||| r
			where size = (1, max (vecY $ m2size l) (vecY $ m2size r))
-}

printVal str x = traceShow (str ++ show x) x

horizontalWith :: (Monoid2D repr) => (Size Int -> repr) -> (Count -> Int -> [Int])-> [RenderMethod src repr] -> RenderMethod [src] repr
horizontalWith middleF divF renderList = RenderMeth $ renderM
	where
		--renderM :: Size Int -> [src] -> repr
		renderM size listSrc = (runRenderMeth newRenderMeth) size newListSrc
			where
				--newListSrc :: [Either () src]
				newListSrc = intersperse (Left ()) (map Right listSrc)
		--newRenderMeth :: RenderMethod [Either () src] repr
		newRenderMeth = horizontal (intersectedDistances divF 1) (newRenderList renderList)
		--newRenderList :: [RenderMethod src repr] -> [RenderMethod (Either () src) repr]
		newRenderList remainingMeths = case remainingMeths of
			[] -> []
			(fstMeth:otherMeths) -> (RenderMeth $ \size src -> case src of
				Left _ -> middleF size
				Right val -> (runRenderMeth fstMeth) size val) : newRenderList otherMeths

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

horizontal divF renderList = combine divF' (|||) renderList
	 where
	 	divF' count (w,h) = zip distances (repeat h)
			where distances = divF count w
vertical divF renderList = combine divF' (===) renderList
	 where
	 	divF' count (w,h) = zip (repeat w) distances 
			where distances = divF count h


combine :: (Monoid2D repr) => (Count -> Size Int -> [Size Int]) -> (repr -> repr -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
combine divF conc renderList = RenderMeth $ \size srcList ->
	let
		count = {-printVal "combine count: " $-} saveMinimumLength renderList srcList
	in 
		--divF count size
		(runRenderMeth $ combine_ (divF count size) conc renderList) size srcList
	where
		saveMinimumLength l r = length $ zip l r

-- precond: listSize = maximum (length renderList, length srcList)
combine_ :: (Monoid2D repr) => [Size Int] -> (repr -> repr -> repr) -> [RenderMethod src repr] -> RenderMethod [src] repr
combine_ listSize conc renderList = {-trace "calling combine_" $-} if count == 0 then (error "nothing to print") else
	case renderList of
		(fstRender:restRender) -> RenderMeth $ \size srcList -> case srcList of
			[] -> error "to few sources"
			(fstSrc:restSrc) -> if count == 1
				then ((runRenderMeth $ fstRender) (head listSize) fstSrc)
				else ((runRenderMeth $ fstRender) (head listSize) fstSrc) `conc`
					((runRenderMeth $ combine_ (tail listSize) conc restRender) size restSrc)
	where count = {-printVal "count: " $-} length listSize

renderNothing :: (Monoid2D repr) => RenderMethod src repr
renderNothing = RenderMeth $ \size src -> m2empty

