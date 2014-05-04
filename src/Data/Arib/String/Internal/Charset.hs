{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Arib.String.Internal.Charset where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Encoding as T
import Data.Arib.String.Internal.Types
import Data.Arib.String.Internal.TH
import Data.Word
import Data.Monoid

dict1 :: Dict -> Word8 -> B.Builder
dict1 (Dict elen b) w =
    let ix  = fromIntegral w - 0x21
        len = fromIntegral $ b `S.index` (ix * elen) :: Int
    in B.byteString $ S.take len $ S.drop (ix * elen + 1) b
{-# INLINE dict1 #-}

dict2 :: Dict -> Word8 -> Word8 -> B.Builder
dict2 (Dict elen b) u l =
    let ix  = 94 * (fromIntegral u - 0x21) + (fromIntegral l - 0x21)
        len = fromIntegral $ b `S.index` (ix * elen) :: Int
    in B.byteString $ S.take len $ S.drop (ix * elen + 1) b
{-# INLINE dict2 #-}

eisuuDict, hiraganaDict, katakanaDict :: Dict
eisuuDict    = $(mkCharset1 (SC.unpack . T.encodeUtf8) "aribdata/Eisuu.txt")
hiraganaDict = $(mkCharset1 (SC.unpack . T.encodeUtf8) "aribdata/Hiragana.txt")
katakanaDict = $(mkCharset1 (SC.unpack . T.encodeUtf8) "aribdata/Katakana.txt")

utf8Config :: AribConfig B.Builder
utf8Config 
    = AribConfig
        { kanji       = dict2 $(mkCharset2 (SC.unpack . T.encodeUtf8) "aribdata/Kanji.txt") 
        , eisuu       = dict1 eisuuDict
        , hiragana    = dict1 hiraganaDict
        , katakana    = dict1 katakanaDict
        , mosaicA     = \_ -> mempty
        , mosaicB     = \_ -> mempty
        , mosaicC     = \_ -> mempty
        , mosaicD     = \_ -> mempty
        , eisuuP      = dict1 eisuuDict
        , hiraganaP   = dict1 hiraganaDict
        , katakanaP   = dict1 katakanaDict
        , jisKatakana = dict1 $(mkCharset1 (SC.unpack . T.encodeUtf8) "aribdata/JisKatakana.txt")
        , jisKanji1   = dict2 $(mkCharset2 (SC.unpack . T.encodeUtf8) "aribdata/JisKanji1.txt")
        , jisKanji2   = dict2 $(mkCharset2 (SC.unpack . T.encodeUtf8) "aribdata/JisKanji2.txt")
        , additional  = dict2 $(mkCharset2 (SC.unpack . T.encodeUtf8) "aribdata/Arib.txt")

        , drcs0       = \_ _ -> mempty
        , drcs1       = \_   -> mempty
        , drcs2       = \_   -> mempty
        , drcs3       = \_   -> mempty
        , drcs4       = \_   -> mempty
        , drcs5       = \_   -> mempty
        , drcs6       = \_   -> mempty
        , drcs7       = \_   -> mempty
        , drcs8       = \_   -> mempty
        , drcs9       = \_   -> mempty
        , drcs10      = \_   -> mempty
        , drcs11      = \_   -> mempty
        , drcs12      = \_   -> mempty
        , drcs13      = \_   -> mempty
        , drcs14      = \_   -> mempty
        , drcs15      = \_   -> mempty

        , control     = \_ _ _ -> mempty
        }


