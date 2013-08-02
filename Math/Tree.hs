module Math.Tree where

import qualified Text.PrettyShow as Pretty
import Data.Ratio
import qualified Data.Foldable as Fold


type Tree t = Node t
--data Node t = Leaf t | Node t [Node t]
data Node t = Node {
	value :: t,
	children :: [Node t]
}

{-- node :: t -> [t] -> Node t
node value sublist = Node value (map node --}
leaf :: t -> Node t 
leaf value = Node value []

node :: t -> [Node t] -> Node t
node val list = Node val list

unfoldTree :: (a -> (t,[a])) -> a -> Tree t
unfoldTree f start = Node { value = v, children = map (unfoldTree f) c }
	where
		(v, c) = f start

addChild :: Node t -> Node t -> Node t
addChild child node = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = child : (children node)
delChildFromIndex :: Node t -> Int -> Node t
delChildFromIndex node index = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = take index (children node) ++ drop (index+1) (children node)

mapOverChildren :: (Node t -> Node t) -> Node t -> Node t
mapOverChildren f = applyOnChildren $ map f

applyOnChildren :: ([Node t] -> [Node t]) -> Node t -> Node t
applyOnChildren f node = Node oldVal newChildren
	where
		oldVal = value node
		newChildren = f $ children node


--this makes it possible to map over a tree:
instance Functor Node where
	fmap f (Node value []) = leaf (f value)
	fmap f (Node params (children)) = Node (f params) (map (fmap f) children)
{--
instance Fold.Foldable Node where
	foldMap toMonoid (Node params list)= 
--}


testTree = node 0 [ leaf 1, leaf 2, leaf 3 ]
testTree2 = node 0 [ node 1 [leaf 1.1, leaf 1.2, leaf 1.3], leaf 2, leaf 3 ]
	

--this method should give a nice text serialisation of the tree:
pShow width (Node params list) =
	(prettyFill width $ show params)
		++ (if length list > 0 then "\n" else "")
		++ subNodes
			where
				subNodes = if length list > 0 then unlines lines' else ""
				lines' = [ concat (map (getLine currentLine) subNodes') | currentLine <- [0..(deepestSubNode-1)]]
					 
				deepestSubNode = maximum $ map length subNodes'
				subNodes' = map (lines . pShow subWidth) list
				getLine n lines 
					| (n < length lines) = lines !! n
					| otherwise = ""
				prettyFill = Pretty.fill "[" "]" " " " " Pretty.MidJust
				subWidth = floor $ (width%1) / ((length list) %1)

instance (Show t) => Show (Node t) where
	show (Node params []) = show params 
	show (Node params children) = show params ++ showChildren children ++ "\n"
		where
			showChildren :: (Show t) => [Node t] -> String
			showChildren list = "-> [" ++ showChildren' list ++ "]"
				where
					showChildren' [] = ""
					showChildren' (node:[]) = show node
					showChildren' (node:rest) = show node ++ "," ++ showChildren' rest
