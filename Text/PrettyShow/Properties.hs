module Text.PrettyShow.Properties(
	prop_m2Hori, prop_m2Vert, prop_m2Empty,
	prop_div,
	prop_div2,
	prop_divEqually, prop_divAllConstThenCut,
	prop_div2FromRatio, prop_div2ConstAndRest, prop_div2RestAndConst,
) where
import Text.PrettyShow
import Text.TextBlock
import Text.TextBlock.RenderMethods
import Test.QuickCheck

import Util.Vector2D

text1 = "Hallo\nWelt\nDu\nIdiot"
text2 = "Bli\nBla\nBlubb"

fillSpaces = filledBlock " "

test_horizontalWith =
	let render = horizontalWith (filledBlock "|") combPStd (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (4,4) [textBlock text2,textBlock text1]
test_horizontal =
	let render = horizontal combPStd (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (8,4) [textBlock text2,textBlock text1]
test_vertical =
	let render = vertical combPStd (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (8,4) [textBlock text2,textBlock text1]


test_lr = let render = lr (div2ConstAndRest 3) (forceWE ".." "..") (forceWE ".." "..")
	in
		(runRenderMeth render) (20,3) (textBlock text1,textBlock text2)

-- |law: (m2size l) |+| (m2size r) == m2size ( l ||| r)
prop_m2Hori l r = (m2size l) |+| (m2size r) == m2size (l ||| r)
-- |law: (m2size l) |+| (m2size r) == m2size ( l === r)
prop_m2Vert u d = (m2size u) |+| (m2size d) == m2size (u === d)

-- |law: m2empty should be the neutral element
prop_m2Empty a =
	m2size (a ||| m2empty) == m2size a &&
	m2size (a === m2empty) == m2size a


prop_div2FromRatio :: Float -> Int -> Bool
prop_div2FromRatio ratio = prop_div2 (div2FromRatio ratio)
prop_div2ConstAndRest const = prop_div2 (div2ConstAndRest const)
prop_div2RestAndConst const = prop_div2  (div2RestAndConst const)


prop_div2 divFunc = prop_div (\count dist -> (\(l,r)-> [l,r]) $ divFunc dist) 2

{-prop_divDist divFunc count dist = and $ [
	-- number of parts should be equal to count-1
	length partition == count - 1,
	-- sum should be less or equal the dist
	sum partition <= dist,
	-- all parts should be in [0..dist]
	and [ part >=0 && part <= dist
		| part <- partition ] ]
	where partition = divFunc count dist-}

{-prop_spacePartition2 divFunc dist = (l + r == dist)
	where (l,r) = (partitionDist2 divFunc) dist-}
prop_divAllConstThenCut const count dist = (const >=0) `implies` prop_div (divAllConstThenCut const ) count dist
prop_divEqually = prop_div divEqually

prop_div divFunc count dist =
	let div = divFunc count dist
	in
		(count > 0 && dist>=0 ) `implies`
		and [
			sum div == dist,
			and $ do
				part <- div
				return $ part >=0 && part <=dist ]

implies a b = not a || b

{-prop_renderNothing :: Size Int -> TextBlock -> Bool
prop_renderNothing size src = 
	(0,0) == (m2size $ (runRenderMeth renderNothing) size src)
		where types = src :: TextBlock-}
