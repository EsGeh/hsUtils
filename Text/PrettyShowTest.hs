import Text.PrettyShow
import Text.TextBlock
import Test.QuickCheck

text1 = "Hallo\nWelt\nDu\nIdiot"
text2 = "Bli\nBla\nBlubb"

test_horizontalWith =
	let render = horizontalWith (filledBlock "|") divEqually (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (1,4) [textBlock text2,textBlock text1]
test_horizontal =
	let render = horizontal divEqually (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (8,4) [textBlock text2,textBlock text1]
test_vertical =
	let render = vertical divEqually (repeat $ forceWE ".." "..")
	in
		(runRenderMeth render) (8,4) [textBlock text2,textBlock text1]


test_lr = let render = lr (div2ConstAndRest 3) (forceWE ".." "..") (forceWE ".." "..")
	in
		(runRenderMeth render) (20,3) (textBlock text1,textBlock text2)

prop_divDist2 divFunc dist = (divFunc dist >= 0) && (divFunc dist <= dist)

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
prop_divAllConstThenCut const count dist = (const >=0) `implies` prop_div (divAllConstThenCut constthe first ) count dist
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
