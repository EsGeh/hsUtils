module Text.TextBlock(
	-- * data types
	TextBlock(),
	-- ** nice aliases
	Ellipse,Line,
	-- * pseudo constructors
	textBlock,textBlockTrunc,textBlockTruncWE,textBlockAutoNewLineWE,
	force,justBlock
)where

import Text.PrettyShow
import Util.Vector2D


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


testRenderMeth = lr (spaceDiv 0.5) force force 
testRenderMeth2 = lr (spaceDiv 0.5) justBlock justBlock 
testRenderMeth3 = lr (spaceDiv 0.5) justBlock force 
text1 = "Hallo\nWelt\nDu\nIdiot"
text2 = "Bli\nBla\nBlubb"

testHorizontal = horizontal (repeat force)

---------------------------------------------------------------------------
-- RenderMethods using TextBlocks:
---------------------------------------------------------------------------

-- |ignores the given size, and renders taking the size needed
justBlock :: RenderMethod String TextBlock
justBlock = RenderMeth $ \size str -> textBlock str

-- |just forces something into the given size, cut if too big
force :: RenderMethod String TextBlock
force = RenderMeth $ \size str -> textBlockTrunc size str


---------------------------------------------------------------------------
-- functions on text blocks:
---------------------------------------------------------------------------

type Ellipse = String
type Line = String

textBlock = normalize . TextBlock . P.lines
textBlockTruncWE size ellAtLineEnding ellAtTextEnding str = textMap (linesSetSizeWE size ellAtLineEnding ellAtTextEnding) $ textBlock str
textBlockTrunc size str = textMap (linesSetSize size) $ textBlock str

textBlockAutoNewLineWE size ellAtLineEnding ellAtTextEnding str = textMap (linesSetSizeWE size ellAtLineEnding ellAtTextEnding . linesAutoNewLine size "" ) $ textBlock str
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


---------------------------------------------------------------------------
-- functions on lists of lines
---------------------------------------------------------------------------
linesHomWidth :: [String] -> [String]
linesHomWidth lines = linesSetWidth (maximum $ map length lines) lines


linesAutoNewLine :: Size Int -> Ellipse -> [Line] -> [Line]
linesAutoNewLine size ell = divNice . join . map words -- . Prelude.lines
	where
		divNice :: [String] -> [String]
		divNice words = snd $ snd $ runState (divNiceM words) $ ((0,0), [])

		divNiceM :: [String] -> State (Pos Int,[String]) [String]
		divNiceM words = return words >>= \remaining -> do
			if (not . null) remaining
				then fillLine size remaining >>= divNiceM 
				else return remaining


-- the suffix "WE" is for "with ellipse"

linesSetSize size = linesSetHeight (vecY size) . linesSetWidth (vecX size)
linesSetSizeWE size ellAtLineEnding ellAtTextEnding = linesSetHeightWE (vecY size) ellAtTextEnding . linesSetWidthWE (vecX size) ellAtLineEnding

linesSetWidthWE width ell = map manipulateLine
	where
		manipulateLine line = if length line <= width
			then take width $ line ++ (repeat ' ')
			else (take (width-length ell) line) ++ ell

linesSetHeightWE height ell lines =
	if length lines <= height
	then linesSetHeight height lines
	else take (height-1) (linesSetHeight height lines) ++ [ell]

linesSetHeight :: Int -> [String] -> [String]
linesSetHeight height lines' = take height $ lines' ++ (repeat "")

linesSetWidth :: Int -> [String] -> [String]
linesSetWidth width = map fillWithSpaces
	where
		fillWithSpaces line = take width $ line ++  (repeat ' ')


-- internal helper functions:

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



{-forceWithEllipse :: String -> Ellipse -> Block TextBlock
forceWithEllipse str ell = Block $ \size -> (textBlockTrunc (size-length ell) str)-}



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
