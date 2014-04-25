{-# LANGUAGE TemplateHaskell #-}

module Data.Arib.CRC.Internal where

import Language.Haskell.TH
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.List
import Data.Word
import qualified Data.Vector.Unboxed as U

crc32Table :: U.Vector Word32
crc32Table = U.fromList $( listE $
    map (\i -> litE . integerL . fromIntegral $ foldl' (\c _ -> 
        if testBit c 31
        then 0x04C11DB7 `xor` shiftL c 1
        else shiftL c 1
    ) (shiftL i 24) [0..7::Int]) [0..255::Word32]
    )
{-# INLINE crc32Table #-}

calc32 :: Word32 -> S.ByteString -> Word32
calc32 = S.foldl' (\c i -> (crc32Table U.! (fromIntegral $ fromIntegral (shiftR c 24) `xor` i)) `xor` shiftL c 8)
{-# INLINE calc32 #-}

class CRC32 a where
  -- | Calc Mpeg2 CRC32.
  crc32 :: a -> Word32

instance CRC32 L.ByteString where
    crc32 = L.foldlChunks calc32 0xFFFFFFFF
    {-# INLINE crc32 #-}

instance CRC32 S.ByteString where
    crc32 = calc32 0xFFFFFFFF
    {-# INLINE crc32 #-}
