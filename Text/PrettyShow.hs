module Text.PrettyShow where {-(
	Monoid2D,
	Area,Size,Pos
) where-}

import Util.Vector2D

import Data.Maybe
import Data.Monoid
import Data.Ratio
import Control.Monad.State
import Prelude hiding (lines)
import qualified Prelude as P


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
	msize :: a -> Size Int
	mempty :: a

-- |this type represents a method (let us call it a function) to create some data of type t, that fills a "frame" of some given size
data RenderMethod t = RenderMeth {
	runRenderMeth :: Size Int -> t
}

-- |if there are 2 functions that create a 'Monoid2D' of the same type, the results can be combined generically
(^===) :: (Monoid2D repr) => RenderMethod repr -> RenderMethod repr -> RenderMethod repr
l ^=== r = lr 0.5 l r

type SpaceDivide = Size Int -> (Size Int, Size Int)

spaceDiv ratio size = (sizeL,sizeR)
	where
		sizeL = vecMap ceiling $ vecMap fromIntegral size |*| (ratio,1)
		sizeR = size |-| (vecX sizeL,0)

lrCombinator :: (Monoid2D repr) => SpaceDivide -> RenderMethod repr -> RenderMethod repr -> RenderMethod repr
lrCombinator spacing l r = let
	lf = runRenderMeth l
	rf = runRenderMeth r
	in
	RenderMeth $ \size ->
		let
			(sizeL,sizeR) = spacing size
		in
			lf (sizeL) ||| rf (sizeR)


lr :: (Monoid2D repr) => Rational -> RenderMethod repr -> RenderMethod repr -> RenderMethod repr
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

ud :: (Monoid2D repr) => Rational -> RenderMethod repr -> RenderMethod repr -> RenderMethod repr
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



infixl ^===
