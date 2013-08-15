module Text.PrettyShow (
	-- * data types and type classes
	Monoid2D(..),
	RenderMethod(..),
	SpaceDivide,
	Area,Size,Pos,
	-- * render function combinators
	lr, ud,
	vertical, horizontal,
	-- * space division functions
	horiSpaceDiv, vertSpaceDiv
) where

import Util.Vector2D

import Data.Maybe
import Data.Monoid
import Data.Ratio
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
	(|||) :: a -> a -> a -- |concatenate horizontal ( "Block | Block")
	(===) :: a -> a -> a -- |concatenate vertically ( "Block / Block")
	m2size :: a -> Size Int
	m2empty :: a

-- |this type represents a method (let us call it a function) to create some data of type t, that fills a "frame" of some given size
data RenderMethod src dest = RenderMeth {
	runRenderMeth :: Size Int -> src -> dest
}

-- |if there are 2 functions that create a 'Monoid2D' of the same type, the results can be combined generically
{-(^===) :: (Monoid2D repr) => RenderMethod src repr -> RenderMethod src repr -> RenderMethod src repr
l ^=== r = lr 0.5 l r-}

type SpaceDivide = Size Int -> (Size Int, Size Int)

horiSpaceDiv ratio size = (sizeL,sizeR)
	where
		sizeL = vecMap ceiling $ vecMap fromIntegral size |*| (ratio,1)
		sizeR = size |-| (vecX sizeL,0)
vertSpaceDiv ratio size = (sizeU,sizeD)
	where
		sizeU = vecMap ceiling $ vecMap fromIntegral size |*| (1,ratio)
		sizeD = size |-| (0,vecY sizeU)


horizontal :: (Monoid2D repr) => [RenderMethod src repr] -> RenderMethod [src] repr
horizontal renderList = case renderList of
	[] -> RenderMeth $ \size src -> m2empty
	(fstRender:restRender) -> 
		let
			fRenderFst = runRenderMeth fstRender
		in
			RenderMeth $ \size srcList -> case srcList of
				[] -> m2empty
				(fstSrc:restSrc) -> fRenderFst oneElSize fstSrc ||| (runRenderMeth $ horizontal restRender) (size |-| (vecX oneElSize,0)) restSrc
					where
						oneElSize = vecMap ceiling $ vecMap fromIntegral size |/| (fromIntegral $ minLength renderList srcList, 1)
						minLength list otherList = length $ zip list otherList

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
				
 
lr :: (Monoid2D repr) => SpaceDivide -> RenderMethod src repr -> RenderMethod src repr -> RenderMethod (src,src) repr
lr spacing l r = let
	(lf,rf) = (runRenderMeth l, runRenderMeth r)
	in
	RenderMeth $ \size (lsrc,rsrc) ->
		let
			(sizeL,sizeR) = spacing size
		in
			lf sizeL lsrc ||| rf sizeR rsrc

ud :: (Monoid2D repr) => SpaceDivide -> RenderMethod src repr -> RenderMethod src repr -> RenderMethod (src,src) repr
ud spacing u d = let
	(uf,df) = (runRenderMeth u, runRenderMeth d)
	in
	RenderMeth $ \size (usrc,dsrc) ->
		let
			(sizeU,sizeD) = spacing size
		in
			uf sizeU usrc === df sizeD dsrc


{-lr :: (Monoid2D repr) => Rational -> RenderMethod src repr -> RenderMethod src repr -> RenderMethod src repr
lr ratio l r = let
	lf = runRenderMeth l
	rf = runRenderMeth r
	in
	RenderMeth $ \size' ->
		let
			sizeL = vecMap fromIntegral size' |*| (ratio,1)
			sizeR = vecMap fromIntegral size' |-| (vecX sizeL,0)
		in
			lf (vecMap floor sizeL) ||| rf (vecMap ceiling sizeR)
			-}

{-ud :: (Monoid2D repr) => Rational -> RenderMethod src repr -> RenderMethod src repr -> RenderMethod src repr
ud ratio u d = let
	uf = runRenderMeth u
	df = runRenderMeth d
	in
	RenderMeth $ \size ->
		let
			sizeU = vecMap fromIntegral size |*| (1,ratio)
			sizeD = vecMap fromIntegral size |-| (0,vecY sizeU)
		in
			uf (vecMap floor sizeU) === df (vecMap ceiling sizeD)
-}


--infixl ^===
