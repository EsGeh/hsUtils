{--

 module not working! (it won't, due to design errors)


--}
module Math.CalcWithStatus(WithRest,runWithRest,rasterize,mulWithRest) where

import Control.Monad.State

{--
multWithRest :: (Integral n, RealFrac r) => r -> n -> WithRest r n
multWithRest mul x = state $ \r -> let val = (fromIntegral x)*(mul+r) in properFraction val
--}

mulWithRest times val = do
	rasterize $ times*val

addWithRest :: (RealFrac r,Integral n) => WithRest r n -> WithRest r n -> WithRest r n
addWithRest lwr rwr = do
	l <- lwr
	r <- rwr
	return $ l+r

rasterize :: (RealFrac r,Integral n) => r -> WithRest r n
rasterize r = WithRest $ properFraction r

{--
diffWithRest :: r -> r -> WithRest r n
diffWithRest a b =
	let 
		(aVal,aRest) = runWithRest a 0
		(bVal,bRest) = runWithRest a aRest
	in
		state $ \r -> 
--}

newtype WithRest r n = WithRest { runWithRest :: (n,r) }
instance (RealFrac r) => Monad (WithRest r) where
	return x = WithRest $ (x,0)
	--(>>=) :: WithRest r a -> (a -> WithRest r b) -> WithRest r b
	x >>= f =
		let (val,rest) = runWithRest x
		in calcNewWithRest (f val) rest
			where
				calcNewWithRest x rest = WithRest$ (xVal+restsX,restsRest)
					where
						(restsX,restsRest)= rasterize (xRest+rest)
						(xVal,xRest) = (runWithRest x)

--addWR :: WithRest r n -> WithRest r n -> WithRest r n

makeProper :: (RealFrac r,Integral n) => WithRest r n -> WithRest r n
makeProper wr = 
	WithRest $ (val+addToVal,newRest)
		where
			(val,rest) = runWithRest wr
			(addToVal,newRest) = properFraction rest
		
{--
type WithRest r n = State r n
runWithRest = runState
--}

--newtype (Integral n,Fractional r) => WithRest r n = WithRest { runWithRest :: (n,r) }
