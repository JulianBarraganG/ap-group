import qualified APL.Tests
import Test.Tasty (defaultMain, localOption)
import Test.Tasty.QuickCheck (QuickCheckReplay (..), testProperties)

-- The seed is fixed so that a run of the test suite is reproducible: you and we
-- see the same counterexamples.
main :: IO ()
main =
  defaultMain
    . localOption (QuickCheckReplay (Just 20261005))
    $ testProperties "APL properties" APL.Tests.properties
