{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Arib.String.Internal.Charset where

import qualified Data.ByteString as S
import Data.Arib.String.Internal.TH

kanji :: Dict
kanji = $(mkCharsetFunction "/Users/philopon/mysrc/Haskell/arib/data/Kanji.txt")

pickDict :: Dict -> Int -> S.ByteString
pickDict (Dict elen b) i =
    let (u,l) = i `quotRem` 0x100
        ix    = 94 * (u - 0x21) + (l - 0x21)
        len   = fromIntegral $ b `S.index` (fromIntegral $ ix * elen) :: Int
    in S.take (fromIntegral len) $ S.drop (fromIntegral $ ix * elen + 1) b
{-# INLINE pickDict #-}

