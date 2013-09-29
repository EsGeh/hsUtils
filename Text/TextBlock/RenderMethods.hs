-- | basic renderMethods using Textblocks
module Text.TextBlock.RenderMethods(
	force,forceWE,autoNewLineWE,
	justBlock,
) where

import Text.TextBlock
import Util.Vector2D


---------------------------------------------------------------------------
-- RenderMethods using TextBlocks:
---------------------------------------------------------------------------

-- |ignores the given size, and renders taking the size needed
justBlock :: (Show a) => RenderMethod a TextBlock
justBlock = RenderMeth {
	runRenderMeth = \size val -> block val,
	minSize = \val -> (vecX . m2size . block $ val) * (vecY . m2size . block $ val) }
		 where block = textBlock . show

-- |just forces something into the given size, cut if too big
force :: (Show a) => RenderMethod a TextBlock
force = RenderMeth { 
	runRenderMeth = \size val -> textBlockTrunc size (show val),
	minSize = length . show
}

-- |freely divide into lines, cut using ellipsis, if too big:
autoNewLineWE :: (Show a) => Ellipsis -> RenderMethod a TextBlock
autoNewLineWE ell = RenderMeth {
	 runRenderMeth = \size val -> textBlockAutoNewLineWE size ell (show val),
	 minSize = length . show
}

-- |just forces something into the given size, cut if too big, print ellipsis
forceWE :: (Show a) => Ellipsis -> Ellipsis -> RenderMethod a TextBlock
forceWE ellAtLineEnding ellAtLastLine= RenderMeth{
	runRenderMeth = \size val -> textBlockTruncWE ellAtLineEnding ellAtLastLine size (show val),
	minSize = length . show
}
