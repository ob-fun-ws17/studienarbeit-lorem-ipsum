{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec where

import Lib --(getSubTupelOfTuple, makeListOfTuple, makeCharTupleListTuple)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  --describe "getSubTupelOfTuple" $ do
  describe "getSubTupelOfTuple :: (Char, (Char, Float)) -> (Char, Float)" $ do
    it "calculates the getSubTupelOfTuple of ('A', ('B', 3.1415))" $
      getSubTupelOfTuple ('A', ('B', 3.1415)) `shouldBe` ('B', 3.1415)
    it "calculates the getSubTupelOfTuple of arbitrary first char" $
      property $ \(fstc :: Char) -> getSubTupelOfTuple (fstc, ('B', 3.1415)) == ('B', 3.1415)
    it "calculates the getSubTupelOfTuple of arbitrary second char" $
      property $ \(sndc :: Char) -> getSubTupelOfTuple ('A', (sndc, 3.1415)) == (sndc, 3.1415)
    it "calculates the getSubTupelOfTuple of arbitrary float" $
      property $ \(f :: Float) -> getSubTupelOfTuple ('A', ('B', f)) == ('B', f)

  describe "makeListOfTuple :: [(Char, (Char, Float))] -> [(Char, Float)]" $ do
    it "calculates the makeListOfTuple of [('A', ('B', 3.1415))]" $
      makeListOfTuple [('A', ('B', 3.1415))] `shouldBe` [('B', 3.1415)]
    it "calculates the makeListOfTuple of [('A', ('B', 3.1415)), ('A', ('C', 3.14))]" $
      makeListOfTuple [('A', ('B', 3.1415)), ('A', ('C', 3.14))] `shouldBe` [('B', 3.1415), ('C', 3.14)]

  describe "kummulateAddNumber :: (Char, Float) -> Float -> (Char, Float)" $ do
    it "calculates a float to the second part of a ('B', 3.1415)" $
      kummulateAddNumber ('B', 3.1415) 1.8585 `shouldBe` ('B', 5.0)
    it "calculates the kummulateAddNumber of arbitrary float" $
      property $ \(f :: Float) -> kummulateAddNumber ('B', 2.2) f == ('B', 2.2 + f)

  describe "kummulate :: [(Char, Float)] -> [(Char, Float)]" $ do
    it "calculates the float to the second part of a tuple from the second part (float) of its predecessor" $
      kummulate [('B', 3.1415), ('C', 3.14), ('D', 3.1)] `shouldBe` [('B', 3.1415), ('C', 6.2815), ('D', 9.3815)]

  describe "makeCharTupleListTuple :: [(Char, (Char, Float))] -> (Char, [(Char, Float)])" $ do
    it "calculates the makeCharTupleListTuple of [('A', ('B', 3.1415))]" $
      makeCharTupleListTuple [('A', ('B', 3.1415))] `shouldBe` ('A', [('B', 3.1415)])
    it "calculates the makeCharTupleListTuple of [('A', ('B', 3.1415)), ('A', ('C', 3.14))]" $
      makeCharTupleListTuple [('A', ('B', 3.1415)), ('A', ('C', 3.14))] `shouldBe` ('A', [('B', 3.1415), ('C', 6.2815)])
    it "calculates the makeCharTupleListTuple of [('A', ('B', 3.1415)), ('A', ('C', 3.14)), ('A', ('D', 3.1))]" $
      makeCharTupleListTuple [('A', ('B', 3.1415)), ('A', ('C', 3.14)), ('A', ('D', 3.1))] `shouldBe` ('A', [('B', 3.1415), ('C', 6.2815), ('D', 9.3815)])
