module REPL.Save ( decodeReplSt
                 , encodeReplSt
                 ) where

import qualified Codec.Compression.Zstd.Lazy as Zstd
import           Data.Binary                 (Binary, Get, Put, get, put)
import           Data.Binary.Get             (runGet)
import           Data.Binary.Put             (runPut)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Semigroup              ((<>))
import           Language.Dickinson.Eval

getReplState :: Binary a => [Double] -> Get (EvalSt a)
getReplState ds =
    EvalSt ds
        <$> get
        <*> get
        <*> get
        <*> get
        <*> get

putReplState :: Binary a => EvalSt a -> Put
putReplState (EvalSt _ be rs t lSt ty) =
       put be
    <> put rs
    <> put t
    <> put lSt
    <> put ty

decodeReplSt :: Binary a => [Double] -> BSL.ByteString -> EvalSt a
decodeReplSt ds = runGet (getReplState ds) . Zstd.decompress

encodeReplSt :: Binary a => EvalSt a -> BSL.ByteString
encodeReplSt = Zstd.compress 3 . runPut . putReplState
