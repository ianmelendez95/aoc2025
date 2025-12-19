module Test.GEOS (test) where 

import GEOS

import Test.Hspec
  ( Expectation,
    SpecWith (..),
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
    shouldMatchList,
    shouldNotBe,
    shouldSatisfy,
    xdescribe,
    xit,
  )
import Data.Text qualified as T

outer_poly :: T.Text
outer_poly = 
  "POLYGON ((\
  \  7 1,\
  \  11 1,\
  \  11 7,\
  \  9 7,\
  \  9 5,\
  \  2 5,\
  \  2 3,\
  \  7 3,\
  \  7 1\
  \))\
  \"

inside_poly_coords :: [(Int, Int)]
inside_poly_coords = 
  [ (7, 3),
    (7, 1),
    (11, 1),
    (11, 3),
    (7, 3)
  ]

inside_poly :: T.Text
inside_poly = 
  "POLYGON((\
  \ 7 3,\
  \ 7 1,\
  \ 11 1,\
  \ 11 3,\
  \ 7 3\
  \ ))"


test :: SpecWith ()
test = do
  describe "geosContains" $ do 
    it "contains" $ do 
      let does_contain = geosContains outer_poly inside_poly
      does_contain `shouldBe` True

  describe "wktPolygon" $ do 
    it "converts inside_poly" $ do
      let inside_poly_wkt = wktPolygon inside_poly_coords
      inside_poly_wkt `shouldBe` inside_poly
      

