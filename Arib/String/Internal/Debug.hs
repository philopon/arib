module Arib.String.Internal.Debug where

import Data.Word
import Arib.String.Internal.Types
import Numeric

data DebugChar
    = Char1 String Word8
    | Char2 String Word8 Word8
    | EOF

instance Show DebugChar where
    show (Char1 d a)   = d ++ ':': showHex a []
    show (Char2 d a b) = d ++ ':': showHex a (showHex b [])
    show EOF           = "EOF"

debugConfig :: AribConfig DebugChar
debugConfig = AribConfig
    { kanji = Char2 "kanji"
    , eisuu = Char1 "eisuu"
    , hiragana = Char1 "hiragana"
    , katakana = Char1 "katakana"
    , mosaicA  = Char1 "mosaicA"
    , mosaicB  = Char1 "mosaicB"
    , mosaicC  = Char1 "mosaicC"
    , mosaicD  = Char1 "mosaicD"
    , eisuuP   = Char1 "eisuuP"
    , hiraganaP = Char1 "hiraganaP"
    , katakanaP = Char1 "katakanaP"
    , jisKatakana = Char1 "katakanaP"
    , jisKanji1  = Char2 "jisKanji1"
    , jisKanji2  = Char2 "jisKanji2"
    , additional = Char2 "additional"
    , drcs0 = Char2 "drcs0"
    , drcs1 = Char1 "drcs1"
    , drcs2 = Char1 "drcs2"
    , drcs3 = Char1 "drcs3"
    , drcs4 = Char1 "drcs4"
    , drcs5 = Char1 "drcs5"
    , drcs6 = Char1 "drcs6"
    , drcs7 = Char1 "drcs7"
    , drcs8 = Char1 "drcs8"
    , drcs9 = Char1 "drcs9"
    , drcs10 = Char1 "drcs10"
    , drcs11 = Char1 "drcs11"
    , drcs12 = Char1 "drcs12"
    , drcs13 = Char1 "drcs13"
    , drcs14 = Char1 "drcs14"
    , drcs15 = Char1 "drcs15"
    , macro  = Char1 "macro"
    }

debugState :: AribState DebugChar
debugState = initialState debugConfig
