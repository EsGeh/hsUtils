module Text.TextBlock(
	module Text.PrettyShow,
	-- * data types
	TextBlock(),
	-- ** nice aliases
	Ellipsis,Line,
	-- * standard combinator param for Textblocks:
	combPStd, 
	-- * pseudo constructors
	textBlock,textBlockTrunc,textBlockTruncWE,textBlockAutoNewLineWE,
	filledBlock,
	-- * basic render methods
	force,forceWE,divToLinesWE,
	justBlock
) where

import Text.PrettyShow
import Text.TextBlockInternal
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

---------------------------------------------------------------------------
-- RenderMethods using TextBlocks:
---------------------------------------------------------------------------

-- |ignores the given size, and renders taking the size needed
justBlock :: (Show a) => RenderMethod a TextBlock
justBlock = RenderMeth $ \size val -> textBlock (show val)

-- |just forces something into the given size, cut if too big
force :: (Show a) => RenderMethod a TextBlock
force = RenderMeth $ \size val -> textBlockTrunc size (show val)

-- |freely divide into lines, cut using ellipsis, if too big:
divToLinesWE :: (Show a) => Ellipsis -> RenderMethod a TextBlock
divToLinesWE ell = RenderMeth $ \size val -> textBlockAutoNewLineWE size ell (show val)

-- |just forces something into the given size, cut if too big, print ellipsis
forceWE :: (Show a) => Ellipsis -> Ellipsis -> RenderMethod a TextBlock
forceWE ellAtLineEnding ellAtLastLine= RenderMeth $ \size val -> textBlockTruncWE ellAtLineEnding ellAtLastLine size (show val)


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

textBlock = normalize . TextBlock . P.lines
textBlockTruncWE ellAtLineEnding ellAtTextEnding size str = textMap (linesSetSizeWE ellAtLineEnding ellAtTextEnding size) $ textBlock str
textBlockTrunc size str = textMap (linesSetSize size) $ textBlock str

textBlockAutoNewLineWE size ell str = textMap (linesAutoNewLineWE size ell ) $ textBlock str
{-textBlockAutoNewLineWE size ellAtLineEnding ellAtTextEnding str = textMap (linesSetSizeWE size ellAtLineEnding ellAtTextEnding . linesAutoNewLineCareful size "" ) $ textBlock str-}
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
