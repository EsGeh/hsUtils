module Util.Card.Card where

class Card c where
	toInt :: c -> Int
	--succ :: (Card d) => c -> d
	--predCard :: Succ c -> c
	--predCard = undefined
