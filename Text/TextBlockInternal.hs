module Text.TextBlockInternal(
	--Pos,Size,
	Ellipsis,
	Line,
	linesAutoNewLineWE,
	linesSetWidth,linesSetHeight, linesSetSize,
	linesSetWidthWE, linesSetSizeWE, linesSetHeightWE,
	linesHomWidth,

	linesConcAll,
) where

import Text.PrettyShow(Pos,Size)

import Control.Monad(join)
import Control.Monad.State
import Util.Vector2D
import Data.List hiding(lines)

{-type Pos t = Vec t
type Size t = Vec t-}
type Ellipsis = String
type Line = String


---------------------------------------------------------------------------
-- functions on lists of lines
---------------------------------------------------------------------------
linesHomWidth :: String -> [String] -> [String]
linesHomWidth fillPattern lines = linesSetWidth fillPattern (maximum $ map length lines) lines

 -- experimental!!
linesAutoNewLineCareful :: Size Int -> Ellipsis -> [Line] -> [Line]
linesAutoNewLineCareful size ell = divNice . join . map words -- . Prelude.lines
	where
		divNice :: [String] -> [String]
		divNice words = snd $ snd $ runState (divNiceM words) $ ((0,0), [])

		divNiceM :: [String] -> State (Pos Int,[String]) [String]
		divNiceM words = return words >>= \remaining -> do
			if (not . null) remaining
				then fillLine size remaining >>= divNiceM 
				else return remaining

-- experimental!! 
linesAutoNewLineWE :: String -> Size Int -> Ellipsis -> [Line] -> [Line]
linesAutoNewLineWE fillPattern size ell =
	linesSetSize fillPattern size . -- if the result is too small, force it to have the given size
	divNice ell size . -- divide the whole text into the size
	linesConcAll -- concatenate all lines into one string


-- divide a string to lines, so that the result fits in the given size
-- use ellipsis, it the string does not fit into the size
divNice :: Ellipsis -> Size Int -> String -> [Line]
divNice ell size str = fst $ divNice_ [] str
	where
		divNice_ :: [Line] -> String -> ([Line],String)
		divNice_ yetParsed str = (newParsed,newRest)
			where
				(newParsed,newRest) =
					if null str
					then (yetParsed,str)
					else
						if length yetParsed >= (vecY size)
						then (appendEllipse (vecX size) ell yetParsed, str)
						else (\(lastLine,newRest) -> divNice_ (yetParsed ++ [lastLine]) newRest) $ appendAsMuchAsPossibleToLine (vecX size) "" str


-- returns all lines concatenated into one string
linesConcAll :: [Line] -> String
linesConcAll lines = case lines of
	[] -> ""
	_  -> foldr1 concLines lines
concLines l r = if l=="" 
	then if r==""
		then ""
		else r
	else dropWhileEnd (==' ') l ++ " " ++ dropWhile (==' ') r

appendEllipse :: Int -> Ellipsis -> [Line] -> [Line]
appendEllipse width ell yetParsed = case yetParsed of
	[] -> []
	_  -> init yetParsed ++ [appendEllipsisToLine width (last yetParsed) ell]
-- |
appendAsMuchAsPossibleToLine :: Int -> Line -> String -> (Line,String)
appendAsMuchAsPossibleToLine width line append = splitAt width (line ++ append)

-- | guaranties: result will have length == width, 
-- result will end with the ellipse (if length ell <= width)
appendEllipsisToLine width line ell = take width $ beginning ++ ell
	where
		beginning = take (width-length ell) $ line ++ repeat ' '

prop_appendEllipsisToLine width line ell =
	let line' = appendEllipsisToLine width line ell
	in
		length line' == width

-- the suffix "WE" is for "with ellipsis"

linesSetSize fillPattern size = linesSetWidth fillPattern (vecX size) . linesSetHeight (vecY size) 
linesSetSizeWE fillPattern ellAtLineEnding ellAtTextEnding size = linesSetWidthWE fillPattern ellAtLineEnding (vecX size) . linesSetHeightWE ellAtTextEnding (vecY size) 


linesSetWidthWE fillPattern ell width = map manipulateLine
	where
		manipulateLine line = if length line <= width
			then take width $ line ++ concat (repeat fillPattern)
			else take width $ (take (width-length ell) line) ++ ell

linesSetHeightWE ell height lines =
	if length lines <= height
	then linesSetHeight height lines
	else take height $ take (height-1) (linesSetHeight height lines) ++ [ell]

linesSetHeight :: Int -> [String] -> [String]
linesSetHeight height lines' = take height $ lines' ++ (repeat "")


linesSetWidth :: String -> Int -> [String] -> [String]
linesSetWidth fillPattern width = map fillWithPattern
	where
		fillWithPattern line = take width $ line ++ concat (repeat fillPattern)



-- internal helper functions:

stateLoop :: (Monad m) => (m a -> Bool) -> (a -> m a) -> m a -> m a
stateLoop cond mf s = if cond s
	then stateLoop cond mf (s >>= mf)
	else s

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
