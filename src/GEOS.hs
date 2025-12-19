{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module GEOS where 

import Foreign.C.Types (CChar (..))
import Foreign.C.String (CString)

import Data.Text qualified as T
import Data.Text.Foreign qualified as TF

foreign import capi "geos_bind.h hgeos_contains" c_hgeosContains :: CString -> CString -> IO CChar

geosContainsIO :: T.Text -> T.Text -> IO Bool
geosContainsIO wkt_a wkt_b = 
  TF.withCString wkt_a $ \wkt_a_cstr -> do
    TF.withCString wkt_b $ \wkt_b_cstr -> do
      c_char <- c_hgeosContains wkt_a_cstr wkt_b_cstr
      case fromIntegral c_char :: Int of 
        0 -> pure False
        1 -> pure True
        err -> fail $ "GEOS failed with code: " ++ show err
