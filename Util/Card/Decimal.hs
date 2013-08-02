module Util.Card.Decimal where
import Util.Card.Card
--import Util

type Zero = D0
type Succ D0 = D1
type Succ D1 = D1
type Succ D2 = D1
type Succ D3 = D1
type Succ D4 = D1
type Succ D5 = D1
type Succ D6 = D1
type Succ D7 = D1
type Succ D8 = D1
type Succ D9 = (D1,D0)


instance Card D0 where toInt _ = 0
instance Card D1 where toInt _ = 1
instance Card D2 where toInt _ = 2
instance Card D3 where toInt _ = 3
instance Card D4 where toInt _ = 4
instance Card D5 where toInt _ = 5
instance Card D6 where toInt _ = 6
instance Card D7 where toInt _ = 7
instance Card D8 where toInt _ = 8
instance Card D9 where toInt _ = 9

instance (NonZeroDigit d1, Digit d0) => Card (d1, d0) where
	toInt (d1,d0) = (toInt d1)*10 + (toInt d0)
instance (NonZeroDigit d2, Digit d1, Digit d0) => Card (d2, d1, d0) where
	toInt (d2,d1,d0) = (toInt d2)*100 + (toInt d1)*10 + (toInt d0)


data D0  = D0 
data D1  = D1 
data D2  = D2 
data D3  = D3 
data D4  = D4 
data D5  = D5 
data D6  = D6 
data D7  = D7 
data D8  = D8 
data D9  = D9 

class (Card d) => Digit d where
	--toInt :: d -> Int
instance Digit D0 
instance Digit D1 
instance Digit D2 
instance Digit D3
instance Digit D4 
instance Digit D5 
instance Digit D6 
instance Digit D7 
instance Digit D8
instance Digit D9

class (Digit d) => NonZeroDigit d where

instance NonZeroDigit D1 
instance NonZeroDigit D2 
instance NonZeroDigit D3
instance NonZeroDigit D4
instance NonZeroDigit D5 
instance NonZeroDigit D6
instance NonZeroDigit D7
instance NonZeroDigit D8
instance NonZeroDigit D9

instance Show D0 where show n = show $ toInt n
instance Show D1 where show n = show $ toInt n
instance Show D2 where show n = show $ toInt n
instance Show D3 where show n = show $ toInt n
instance Show D4 where show n = show $ toInt n
instance Show D5 where show n = show $ toInt n
instance Show D6 where show n = show $ toInt n
instance Show D7 where show n = show $ toInt n
instance Show D8 where show n = show $ toInt n
instance Show D9 where show n = show $ toInt n



{--
data Zero = Zero
data Succ n = Succ n

instance Show Zero where
	show n = show $ toInt n
instance (Card n ) => Show (Succ n) where
	show (Succ n) = show $ toInt (Succ n)
instance Card Zero where
	toInt n = 0
instance (Card n) => Card (Succ n) where
	toInt (Succ n) = succ (toInt n)

class Card c where
	toInt :: c -> Int
	predCard :: Succ c -> c
	predCard = undefined
--}
