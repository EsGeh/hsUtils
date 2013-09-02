module Text.Tests where

import Text.PrettyShow.Properties

import Test.QuickCheck


tests = [
	("prop_div2 with (div2FromRatio ratio)", quickCheck prop_div2FromRatio),
	("prop_div2 with (div2ConstAndRest const)", quickCheck prop_div2ConstAndRest),
	("prop_div2 with (div2RestAndConst const)", quickCheck prop_div2RestAndConst),
	("prop_div with prop_divAllConstThenCut", quickCheck prop_divAllConstThenCut),
	("prop_div with prop_divEqually", quickCheck prop_divEqually)]
