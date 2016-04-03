import Addition

import Test.Hspec
import Test.QuickCheck

-- This gives an example of how to use HSpec and QuickCheck for testing
-- HSpec has QuickCheck integration built in!
main :: IO ()
main = do
  hspec $ do
    describe "PreludeAddition" $ do
        it "1 + 1 is greater than 1" $
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $
            2 + 2 `shouldBe` 4
        it "x + 1 is always greater than x" $
            property $ \x -> x + 1 > (x :: Int)
    describe "MyAddition" $ do
        it "15 divided by 3 is 5" $
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remained 2" $
            dividedBy 22 5 `shouldBe` (4, 2)

  -- QuickCheck by itself, as opposed to within HSpec:
  runQuickCheckStandalone

  -- Some stuff with Generators:
  putStrLn "Example of sample from a QuickCheck generator:"
  sample' oneThroughThree >>= mapM_ print
  putStrLn "Example tuple generation:"
  sample (genTuple :: Gen (Int, Float))
  putStrLn "Example Maybe Int generation:"
  sample (genMaybe' :: Gen (Maybe Int))
  putStrLn "Done!"

runQuickCheckStandalone :: IO ()
runQuickCheckStandalone = quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


-- A number of generator examples:
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

-- Generate maybe values, with 3x liklihood of getting a Just a
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]
