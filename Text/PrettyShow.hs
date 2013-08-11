module Text.PrettyShow(
	Area,Size,Pos,size,pos
) where

import Vector2D

import Data.Maybe
import Data.Monoid
import Data.Ratio
import Control.Monad.State


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
	(|||) :: a -> a -> a
	(===) :: a -> a -> a

-- |this type represents a method (let us call it a function) to create some data of type t, that fills a "frame" of some given size
data RenderMethod t = RenderMeth {
	runRenderMeth :: Size Int -> t
}

-- |if there are 2 functions that create a 'Monoid2D' of the same type, the results can be combined generically
(^===) :: (Monoid2D repr) => RenderMethod repr -> RenderMethod repr -> RenderMethod repr
l ^=== r = lr 0.5 l r

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

ud :: Rational -> RenderMethod TextBlock -> RenderMethod TextBlock -> RenderMethod TextBlock
ud ratio u d = let
	uf = runRenderMeth u
	df = runRenderMeth d
	in
	RenderMeth $ \size ->
		let
			sizeU = vecMap fromIntegral size |*| (1,ratio)
			sizeD = vecMap fromIntegral size |-| (0,vecY sizeU)
		in
			uf (vecMap floor sizeU) `udText` df (vecMap ceiling sizeD)

-- |ignores the given size, and renders taking the size needed
justBlock :: String -> RenderMethod TextBlock
justBlock str = RenderMeth $ \size -> textBlock str

-- |just forces something into the given size, cut if too big
force :: String -> RenderMethod TextBlock
force str = RenderMeth $ \size -> textBlockTrunc size str

type Ellipse = String

{-forceWithEllipse :: String -> Ellipse -> Block TextBlock
forceWithEllipse str ell = Block $ \size -> (textBlockTrunc (size-length ell) str)-}


test = lr 0.2 (force "Hallo\nWelt") (justBlock "bli\nbla\nblubb")


--Block num t -> Block num t -> Block num t
infixl ^===


-- |represents text in a way that also encodes how it is divided into lines
-- imagine it as representing the text as a list of lines
data TextBlock = TextBlock {
	lines :: [String]
}
instance Monoid2D TextBlock where
	l ||| r = lrText l r
	u === d = udText u d
instance Show TextBlock where
	show = fromTextBlock -- (TextBlock lines) = fromTextBlock lines

textBlock = normalize . TextBlock . Prelude.lines
textBlockTruncWE size ellAtLineEnding ellAtTextEnding str = textMap (linesToSizeWE size ellAtLineEnding ellAtTextEnding) $ textBlock str
textBlockTrunc size str = textMap (linesToSize size) $ textBlock str

textBlockAutoNewLineWE size ellAtLineEnding ellAtTextEnding str = textMap (linesToSizeWE size ellAtLineEnding ellAtTextEnding . textAutoNewLine size "" ) $ textBlock str
--textBlock size str = autoNewLine size 
fromTextBlock (TextBlock lines) = unlines $ linesHomWidth lines


lrText :: TextBlock -> TextBlock -> TextBlock
lrText (TextBlock lLines) (TextBlock rLines) = TextBlock $
	zipWith (++) (linesHomWidth lSameHeight) rSameHeight
	where
		lSameHeight = linesSameHeight maxHeight lLines
		rSameHeight = linesSameHeight maxHeight rLines
		maxHeight = max (length lLines) (length rLines)
udText :: TextBlock -> TextBlock -> TextBlock
udText (TextBlock lLines) (TextBlock rLines) = TextBlock $ linesHomWidth $
	lLines ++ rLines 

normalize = textMap linesHomWidth
textMap f (TextBlock lines) = TextBlock $ f lines

-- lists of line: 
linesHomWidth :: [String] -> [String]
linesHomWidth lines = linesSameWidth (maximum $ map length lines) lines

linesToSize size = linesSameHeight (vecY size) . linesSameWidth (vecX size)
linesToSizeWE size ellAtLineEnding ellAtTextEnding = linesSameHeightWE (vecY size) ellAtTextEnding . linesSameWidthWE (vecX size) ellAtLineEnding

textAutoNewLine size ell = divNice . join . map words -- . Prelude.lines
	where
		divNice :: [String] -> [String]
		divNice words = snd $ snd $ runState (divNiceM words) $ ((0,0), [])

		divNiceM :: [String] -> State (Pos Int,[String]) [String]
		divNiceM words = return words >>= \remaining -> do
			if (not . null) remaining
				then fillLine size remaining >>= divNiceM 
				else return remaining

runFirst st = (runState st) ((0,0), [])
test' = fillLine (10,10)
param = ["Hallo","Welt","Bli","Bla","Blubb"]


-- remaining -> State( \(pos,yetParsed) -> (remaining, (pos,yetParsed)) )
fillLine :: Size Int -> [String] -> State (Pos Int, [String]) [String]
fillLine size (nextWord:remainingWords) = state $ \(pos, yetParsed') ->
	let
		yetParsed = if null yetParsed' then [""] else yetParsed'
		lastLinePlusNextWord = last yetParsed ++ nextWord
		
	in 
		if (length lastLinePlusNextWord <= vecX size || last yetParsed == []) -- line not yet full
		then ( remainingWords,
			(pos |+| (length nextWord,0), init yetParsed ++ [(last yetParsed ++ nextWord)] ) )
		else ( remainingWords,
			((length nextWord, vecY pos +1 ), yetParsed ++ [nextWord]) )
fillLine size [] = state $ \(pos, yetParsed) -> ([], (pos,yetParsed))

linesSameWidthWE width ell = map manipulateLine
	where
		manipulateLine line = if length line <= width
			then take width $ line ++ (repeat ' ')
			else (take (width-length ell) line) ++ ell

linesSameHeightWE height ell lines =
	if length lines <= height
	then linesSameHeight height lines
	else take (height-1) (linesSameHeight height lines) ++ [ell]

linesSameHeight :: Int -> [String] -> [String]
linesSameHeight height lines' = take height $ lines' ++ (repeat "")

linesSameWidth :: Int -> [String] -> [String]
linesSameWidth width = map fillWithSpaces
	where
		fillWithSpaces line = take width $ line ++  (repeat ' ')

--
{-showContainer borderL borderR tileL tileR orient separator elemLenMin minTotalLength c = fst . fromJust $ Fold.foldl conc Nothing $ fmap Just strings
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
type Border = String-}
