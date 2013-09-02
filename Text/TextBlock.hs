{-|This module mainly gives you two things:

1. the data type 'TextBlock', that is used to represent blocks of text that fit into a specific size.

2. some basic render functions that render into 'TextBlock's (see "Text.TextBlock.RenderMethods")

usually you don't want to create textBlocks by hand, but use the submodule "Text.TextBlock.RenderMethods" instead, and combine new methods from them using the combinators defined in "Text.PrettyShow".
-}
module Text.TextBlock(
	module Text.PrettyShow,
	-- * data types
	TextBlock(),
	-- ** nice aliases
	Ellipsis,Line,
	-- * pseudo constructors
	textBlock,textBlockTrunc,textBlockTruncWE,textBlockAutoNewLineWE,
	filledBlock,
	{--- * basic render methods
	force,forceWE,divToLinesWE,
	justBlock,-}
	-- * standard combinator param for Textblocks:
	combPStd, 
) where

import Text.PrettyShow
import Text.TextBlockInternal
--import Text.TextBlock.RenderMethods
import Util.Vector2D

import Test.QuickCheck

import Data.List hiding(lines)
import Control.Monad.State

import Prelude hiding (lines)
import qualified Prelude as P


-- |represents text in a way that also encodes how it is divided into lines
-- any text t is guaranteed to fit into the rectangle of 'msize' t
data TextBlock = TextBlock {
	lines :: [String]
}
-- |this enables construction of 'RenderMethod's that create TextBlocks (see module 'Text.PrettyShow')
instance Monoid2D TextBlock where
	l ||| r = lrText l r
	u === d = udText u d
	m2size text = case (lines text) of
		[] -> (0,0)
		list -> (length (head list), length list)
	m2empty = textBlock ""
instance Show TextBlock where
	show = fromTextBlock -- (TextBlock lines) = fromTextBlock lines



{-testRenderMeth = ud (vertSpaceDiv 0.5) force force 
testRenderMeth2 = lr (horiSpaceDiv 0.5) justBlock justBlock 
testRenderMeth3 = lr (horiSpaceDiv 0.5) justBlock force -}

--testHorizontal = horizontal (repeat force)

combPStd = RenderCombP{ divF=divEqually, fillF= filledBlock " " }


-- |fills a block using 'str' as a tile. The result should have the 'size' given as second parameter
filledBlock :: String -> Size Int -> TextBlock
filledBlock str (width,height) = TextBlock $ take height $ map (take width) $ repeat (concat $ repeat str)


---------------------------------------------------------------------------
-- functions on text blocks:
---------------------------------------------------------------------------

{-prop_textBlock_width text=
	-- all lines should be as long as the longest one:
	prop_allLinesSameLength 
	{-(and $ (map (==longestLine) . map length) textBlockLines)
	where
		longestLine = maximum $ map length $ textBlockLines
		textBlockLines = P.lines $ show $ textBlock text-}
prop_textBlockTrunc text size = (vecY size >=0 && vecX size >=0) `implies`
	prop_textBlock_width text && (m2size $ textBlockTrunc size text) == size-}
prop_m2size text = 
	(m2size $ textBlock text) == ((saveMax . map length . P.lines) text, (length . P.lines) text)
	where
		saveMax list = case list of
			[] -> 0
			_ -> maximum list

-- |divide a string onto lines using the newline character (\'\n\') as seperator
textBlock = normalize . TextBlock . P.lines
-- |same as 'textBlockTrunc', but also inserts ellipsis for a line that is too long, or in the last line, if the result would be too high
textBlockTruncWE ellAtLineEnding ellAtTextEnding size str = textMap (linesSetSizeWE ellAtLineEnding ellAtTextEnding size) $ textBlock str
-- |same as 'textBlock', but cuts the result to make it fit into the 'size'
textBlockTrunc size str = textMap (linesSetSize size) $ textBlock str

-- |behaves as follows: first concatenates the 'str' in a way as if the whole text has been written into one line (todo: example). Then divide the result to make it fit into the given 'size', splitting the text if a line is full.
textBlockAutoNewLineWE size ell str = textMap (linesAutoNewLineWE size ell ) $ textBlock str
--textBlock size str = autoNewLine size 
fromTextBlock (TextBlock lines) = unlines $ linesHomWidth lines


lrText :: TextBlock -> TextBlock -> TextBlock
lrText (TextBlock lLines) (TextBlock rLines) = TextBlock $
	zipWith (++) (linesHomWidth lSameHeight) rSameHeight
	where
		lSameHeight = linesSetHeight maxHeight lLines
		rSameHeight = linesSetHeight maxHeight rLines
		maxHeight = max (length lLines) (length rLines)
udText :: TextBlock -> TextBlock -> TextBlock
udText (TextBlock lLines) (TextBlock rLines) = TextBlock $ linesHomWidth $
	lLines ++ rLines 

normalize = textMap linesHomWidth
textMap f (TextBlock lines) = TextBlock $ f lines





{-forceWithEllipsis :: String -> Ellipsis -> Block TextBlock
forceWithEllipsis str ell = Block $ \size -> (textBlockTrunc (size-length ell) str)-}



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
