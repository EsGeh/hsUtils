import qualified Text.Tests as Text
import qualified Math.Tests as Math

import Text.Printf


main = mapM_ performTest tests
	where
		performTest :: (String, IO ()) -> IO ()
		performTest (s,a) = (printf "%-25s " s) >> a 

tests :: [(String, IO ())]
tests = Text.tests ++ Math.tests
