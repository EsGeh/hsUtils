-- | basic renderMethods using Textblocks
module Text.TextBlock.RenderMethods(
	force,forceWE,divToLinesWE,
	justBlock,
) where

import Text.TextBlock


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
