module Data.Arib.String.Internal.Debug where

import Data.Word
import Data.Arib.String.Internal.Types
import Numeric

data DebugChar
    = Char1 String Word8
    | Char2 String Word8 Word8
    | Control_  Word8 [Word8]
    | CSI_  Word8 [Word8]

instance Show DebugChar where
    show (Char1 d a)   = d ++ ':': showHex a []
    show (Char2 d a b) = d ++ ':': showHex a (showHex b [])
    show (Control_ h ws)   = "Ctl" ++ showHex h (show ws)
    show (CSI_ h ws)   = "CSI" ++ showHex h (show ws)

controlF :: Control -> Word8 -> [Word8] -> [DebugChar]
controlF Control w ws = [Control_ w ws]
controlF CSI     w ws = [CSI_     w ws]

char1 :: String -> Word8 -> [DebugChar]
char2 :: String -> Word8 -> Word8 -> [DebugChar]
char1 s a   = [Char1 s a]
char2 s a b = [Char2 s a b]

debugConfig :: AribConfig [DebugChar]
debugConfig = AribConfig
    { kanji = char2 "kanji"
    , eisuu = char1 "eisuu"
    , hiragana = char1 "hiragana"
    , katakana = char1 "katakana"
    , mosaicA  = char1 "mosaicA"
    , mosaicB  = char1 "mosaicB"
    , mosaicC  = char1 "mosaicC"
    , mosaicD  = char1 "mosaicD"
    , eisuuP   = char1 "eisuuP"
    , hiraganaP = char1 "hiraganaP"
    , katakanaP = char1 "katakanaP"
    , jisKatakana = char1 "katakanaP"
    , jisKanji1  = char2 "jisKanji1"
    , jisKanji2  = char2 "jisKanji2"
    , additional = char2 "additional"
    , drcs0 = char2 "drcs0"
    , drcs1 = char1 "drcs1"
    , drcs2 = char1 "drcs2"
    , drcs3 = char1 "drcs3"
    , drcs4 = char1 "drcs4"
    , drcs5 = char1 "drcs5"
    , drcs6 = char1 "drcs6"
    , drcs7 = char1 "drcs7"
    , drcs8 = char1 "drcs8"
    , drcs9 = char1 "drcs9"
    , drcs10 = char1 "drcs10"
    , drcs11 = char1 "drcs11"
    , drcs12 = char1 "drcs12"
    , drcs13 = char1 "drcs13"
    , drcs14 = char1 "drcs14"
    , drcs15 = char1 "drcs15"
    , control = controlF
    }

debugState :: AribState [DebugChar]
debugState = initialState debugConfig
