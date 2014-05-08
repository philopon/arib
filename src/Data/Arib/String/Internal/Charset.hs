{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Arib.String.Internal.Charset where

import Data.Arib.String.Internal.Types
import Data.Arib.String.Internal.TH
import Data.Word
import Data.Monoid
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Internal.Builder as B
import qualified Data.Text.Array as TA
import qualified Data.Vector.Storable as VS

eisuuDict, hiraganaDict, katakanaDict :: Simple
eisuuDict    = $(mkCharset charset1 "aribdata/Eisuu.txt")
hiraganaDict = $(mkCharset charset1 "aribdata/Hiragana.txt")
katakanaDict = $(mkCharset charset1 "aribdata/Katakana.txt")

v2b :: VS.Vector Word16 -> B.Builder
v2b v = B.writeN (VS.length v) $ \p i ->
    VS.foldM'_ (\j a -> TA.unsafeWrite p j a >> return (succ j)) i v

simple1 :: Simple -> Word8 -> B.Builder
simple1 (Simple (Dict elen b)) w = 
    let ix = fromIntegral w - 0x21
    in v2b . VS.take elen $ VS.drop (ix * elen) b

dict2 :: Dict -> Word8 -> Word8 -> B.Builder
dict2 (Dict elen b) u l =
    let ix  = 94 * (fromIntegral u - 0x21) + (fromIntegral l - 0x21)
        len = fromIntegral $ b VS.! (ix * elen) :: Int
    in v2b . VS.take len $ VS.drop (ix * elen + 1) b
{-# INLINE dict2 #-}

textConfig :: AribConfig B.Builder
textConfig = AribConfig
    { kanji       = dict2 $(mkCharset charset2 "aribdata/Kanji.txt")
    , eisuu       = simple1 eisuuDict
    , hiragana    = simple1 hiraganaDict
    , katakana    = simple1 katakanaDict
    , mosaicA     = \_ -> mempty
    , mosaicB     = \_ -> mempty
    , mosaicC     = \_ -> mempty
    , mosaicD     = \_ -> mempty
    , eisuuP      = simple1 eisuuDict
    , hiraganaP   = simple1 hiraganaDict
    , katakanaP   = simple1 katakanaDict
    , jisKatakana = simple1 $(mkCharset charset1 "aribdata/JisKatakana.txt")
    , jisKanji1   = dict2   $(mkCharset charset2 "aribdata/JisKanji1.txt")
    , jisKanji2   = dict2   $(mkCharset charset2 "aribdata/JisKanji2.txt")
    , additional  = dict2   $(mkCharset charset2 "aribdata/Arib.txt")

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
