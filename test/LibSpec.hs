{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec where

import Lib --(getSubTupelOfTuple, makeListOfTuple, makeCharTupleListTuple)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do

  let alpha_A1 = ('A', 0.1)
  let alpha_B2 = ('B', 0.2)
  let alpha_C3 = ('C', 0.3)
  let alpha_D4 = ('D', 0.4)
  let alpha_B5 = ('B', 0.5)
  let alpha_C6 = ('C', 0.6)
  let alpha_D7 = ('D', 0.7)
  let alpha_A8 = ('A', 0.8)
  let alpha_D9 = ('D', 0.9)

  let alpha_AA1 = ('A', alpha_A1)
  let alpha_AC6 = ('A', alpha_C6)
  let alpha_AD9 = ('A', alpha_D9)

  let alpha_BD4 = ('B', alpha_D4)
  let alpha_BB5 = ('B', alpha_B5)
  let alpha_BC6 = ('B', alpha_C6)

  let alpha_CC1 = ('C', alpha_C3)
  let alpha_CB5 = ('C', alpha_B5)
  let alpha_CD9 = ('C', alpha_D9)

  let alpha_DD4 = ('D', alpha_D4)
  let alpha_DB5 = ('D', alpha_B5)
  let alpha_DC6 = ('D', alpha_C6)

  let alpha_AList = [alpha_AA1, alpha_AC6, alpha_AD9]
  let alpha_BList = [alpha_BD4, alpha_BB5, alpha_BC6]
  let alpha_CList = [alpha_CC1, alpha_CB5, alpha_CD9]
  let alpha_DList = [alpha_DD4, alpha_DB5, alpha_DC6]

  let alpha_List = [alpha_AList, alpha_BList, alpha_CList, alpha_DList]

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
