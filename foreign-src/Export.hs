module Export ()  where

import           Control.Monad           ((<=<))
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Text               as T
import           Foreign.C.String
import           Language.Dickinson.File

-- | It is the responsibility of the callee to free the string returned.
run_dickinson :: CString -> IO CString
run_dickinson = newCString <=< fmap T.unpack . fromInput . BSL.fromStrict <=< BS.packCString

fromInput :: BSL.ByteString -> IO T.Text
fromInput = pipelineBSL [] "(input)"

foreign export ccall run_dickinson :: CString -> IO CString
