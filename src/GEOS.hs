{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module GEOS
  ( geosContains,
    wktPolygon
  )where 

import Foreign.C.Types (CChar (..))
import Foreign.C.String (CString)

import Data.Text qualified as T
import Data.Text.Foreign qualified as TF

import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import capi "geos_bind.h hgeos_contains" c_hgeosContains :: CString -> CString -> IO CChar

wktPolygon :: [(Int, Int)] -> T.Text
wktPolygon coords = "POLYGON(( " <> T.intercalate ", " (map (\(x, y) -> T.show x <> " " <> T.show y) (closeCoordLoop coords)) <> " ))"

closeCoordLoop :: [(Int, Int)] -> [(Int, Int)]
closeCoordLoop [] = []
closeCoordLoop (c:cs) = c : go cs 
  where 
    go [] = []
    go [c'] = 
      if c == c'
        then [c'] -- already closed
        else [c', c] -- close off with the last one
    go (c' : cs') = c' : go cs'

geosContains :: T.Text -> T.Text -> Bool
geosContains wkt_a wkt_b = unsafeDupablePerformIO (geosContainsIO wkt_a wkt_b) 

geosContainsIO :: T.Text -> T.Text -> IO Bool
geosContainsIO wkt_a wkt_b = 
  TF.withCString wkt_a $ \wkt_a_cstr -> do
    TF.withCString wkt_b $ \wkt_b_cstr -> do
      c_char <- c_hgeosContains wkt_a_cstr wkt_b_cstr
      case fromIntegral c_char :: Int of 
        0 -> pure False
        1 -> pure True
        err -> fail $ "GEOS failed with code: " ++ show err
