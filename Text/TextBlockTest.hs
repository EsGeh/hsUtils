import Text.TextBlockInternal
import Text.TextBlock
import Util.Vector2D
import Test.QuickCheck

prop_force size text = size == (m2size $ (runRenderMeth force) size (textBlock text))


-- test TextBlockInternal:

{-prop_linesAutoNewLine size ell lines =
	let lines' = linesAutoNewLine size ell lines
	in
		if lines' `isPrefixOf` unlines lines -}
prop_linesAutoNewLine size ell lines =
	let lines' = linesAutoNewLineWE size ell lines
	in
		isValidSize size `implies` (size == linesGetSize lines')
prop_linesSetSizeWE ellW ellH size lines =
	let lines' = linesSetSizeWE ellW ellH size lines
	in
		isValidSize size `implies` ((size == linesGetSize lines') &&
		linesHaveSameLength lines')

prop_linesSetSize size lines =
	let lines' = linesSetSize size lines
	in
		isValidSize size `implies` ((size == linesGetSize lines') &&
		linesHaveSameLength lines')


prop_linesSetHeightWE ell height lines = (height >= 0) `implies` 
	((vecY $ linesGetSize (linesSetHeightWE ell height lines))== height)
prop_linesSetWidthWE ell width lines =
	let lines' = (linesSetWidthWE ell width lines)
	in
		(width >= 0) `implies`
		((vecX $ linesGetSize lines')== width &&
		linesHaveSameLength lines')

prop_linesSetHeight height lines = (height >= 0) `implies` 
	(height == (length $ linesSetHeight height lines))
prop_linesSetWidth width lines =
	let lines' = linesSetWidth width lines
	in
		(width >= 0) `implies`
		((case lines' of
			[] -> True -- an empty block has any width!
			_  -> (vecX $ linesGetSize lines') == width) &&
		linesHaveSameLength lines')
--
implies a b = not a || b
isValidSize (x,y) = x >= 0 && y >= 0 &&
	if 0 `elem` [x,y]
	then x == 0 && y == 0
	else True

linesGetSize lines = case lines of
	[] -> (0,0)
	_ -> (maximum $ map length lines, length lines)

-- size == (longestLine, number of lines)
--linesHaveSize size lines = 
linesHaveSameLength lines = and $ map ((==maxLength) . length) lines
	where
		maxLength = case lines of
			[] -> 0
			_ -> maximum $ map length $ lines
