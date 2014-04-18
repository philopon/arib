module Data.Arib.String.Internal.Types where

import Data.Word
import Data.Map.Strict
import Data.Arib.String.Internal.Common

data GetChar a 
    = GetChar1 (Word8 -> a)
    | GetChar2 (Word8 -> Word8 -> a)

type GSet1 a = Word8 -> a
type GSet2 a = Word8 -> Word8 -> a
type DRCS1 a = Word8 -> a
type DRCS2 a = Word8 -> Word8 -> a

data AribConfig a
    = AribConfig
        { kanji       :: GSet2 a
        , eisuu       :: GSet1 a
        , hiragana    :: GSet1 a
        , katakana    :: GSet1 a
        , mosaicA     :: GSet1 a
        , mosaicB     :: GSet1 a
        , mosaicC     :: GSet1 a
        , mosaicD     :: GSet1 a
        , eisuuP      :: GSet1 a
        , hiraganaP   :: GSet1 a
        , katakanaP   :: GSet1 a
        , jisKatakana :: GSet1 a
        , jisKanji1   :: GSet2 a
        , jisKanji2   :: GSet2 a
        , additional  :: GSet2 a
        , drcs0       :: DRCS2 a
        , drcs1       :: DRCS1 a
        , drcs2       :: DRCS1 a
        , drcs3       :: DRCS1 a
        , drcs4       :: DRCS1 a
        , drcs5       :: DRCS1 a
        , drcs6       :: DRCS1 a
        , drcs7       :: DRCS1 a
        , drcs8       :: DRCS1 a
        , drcs9       :: DRCS1 a
        , drcs10      :: DRCS1 a
        , drcs11      :: DRCS1 a
        , drcs12      :: DRCS1 a
        , drcs13      :: DRCS1 a
        , drcs14      :: DRCS1 a
        , drcs15      :: DRCS1 a
        , macro       :: DRCS1 a
        }

data AribState a
    = AribState
        { getGl :: AribState a -> GetChar a
        , getGr :: AribState a -> GetChar a
        , g0 :: GetChar a
        , g1 :: GetChar a
        , g2 :: GetChar a
        , g3 :: GetChar a
        , macros :: Map Word8 [Word8]
        }

glTo, grTo :: (AribState a -> GetChar a) -> AribState a -> AribState a
glTo c s = s { getGl = c }
grTo c s = s { getGr = c }

g0To, g1To, g2To, g3To :: GetChar a -> AribState a -> AribState a
g0To g s = debug "  G0 ->" s { g0 = g }
g1To g s = debug "  G1 ->" s { g1 = g }
g2To g s = debug "  G2 ->" s { g2 = g }
g3To g s = debug "  G3 ->" s { g3 = g }

gl, gr :: AribState a -> GetChar a
gl st = getGl st st
gr st = getGr st st

defaultMacro :: Map Word8 [Word8]
defaultMacro = fromList
    [ (0x60, [esc, 0x24, f1, esc, 0x29,        f2, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x61, [esc, 0x24, f1, esc, 0x29,        f4, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x62, [esc, 0x24, f1, esc, 0x29, 0x20, f10, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x63, [esc, 0x28, f5, esc, 0x29,        f7, esc, 0x2A, f8, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x64, [esc, 0x28, f5, esc, 0x29,        f6, esc, 0x2A, f8, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x65, [esc, 0x28, f5, esc, 0x29, 0x20, f10, esc, 0x2A, f8, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])

    , (0x66, [esc, 0x28, 0x20, f10, esc, 0x29, 0x20, f11, esc, 0x2A, 0x20, f12, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x67, [esc, 0x28, 0x20, f13, esc, 0x29, 0x20, f14, esc, 0x2A, 0x20, f15, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x68, [esc, 0x28, 0x20, f16, esc, 0x29, 0x20, f17, esc, 0x2A, 0x20, f18, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x69, [esc, 0x28, 0x20, f19, esc, 0x29, 0x20, f20, esc, 0x2A, 0x20, f21, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x6A, [esc, 0x28, 0x20, f22, esc, 0x29, 0x20, f23, esc, 0x2A, 0x20, f24, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])

    , (0x6B, [esc, 0x24, f1, esc, 0x29, 0x20, f11, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x6C, [esc, 0x24, f1, esc, 0x29, 0x20, f12, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x6D, [esc, 0x24, f1, esc, 0x29, 0x20, f13, esc, 0x2A, f3, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])

    , (0x6E, [esc, 0x28, f4, esc, 0x29,        f3, esc, 0x2A, f2, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    , (0x6F, [esc, 0x28, f2, esc, 0x29, f5, esc, 0x2A, 0x20, f10, esc, 0x2B, 0x20, f9, ls0, esc, 0x7D])
    ]
  where
    esc = 0x1B
    ls0 = 0x0F

    f1  = 0x42 -- kanji
    f2  = 0x4A -- Eisuu
    f3  = 0x30 -- Hiragana
    f4  = 0x31 -- Katakana
    f5  = 0x32 -- Mosaic A
    f6  = 0x33 -- Mosaic B
    f7  = 0x34 -- Mosaic C
    f8  = 0x35 -- Mosaic D
    f9  = 0x70 -- Macro

    f10 = 0x40 
    f11 = 0x41
    f12 = 0x42
    f13 = 0x43
    f14 = 0x44
    f15 = 0x45
    f16 = 0x46
    f17 = 0x47
    f18 = 0x48
    f19 = 0x49
    f20 = 0x4A
    f21 = 0x4B
    f22 = 0x4C
    f23 = 0x4D
    f24 = 0x4E




initialState :: AribConfig a -> AribState a
initialState c =
    AribState g0 g2 
    (GetChar2 $ kanji c) (GetChar1 $ eisuu c)
    (GetChar1 $ hiragana c) (GetChar1 $ macro c) defaultMacro

data Control
    = NUL
    | BEL
    | APB
    | APF
    | APD
    | APU
    | CS
    | APR
    | PAPF Word8
    | CAN
    | APS Word8 Word8
    | RS
    | US
    | SP

    | DEL
    | BKF
    | GRF
    | YLF
    | BLF
    | MGF
    | CNF
    | WHF
    | SSZ
    | MSZ
    | NSZ
    | SZX Word8
    | COL Word8 (Maybe Word8)
    | FLC Word8
    | CDC Word8 (Maybe Word8)
    | POL Word8
    | WMM Word8
    | HLC Word8
    | RPC Word8
    | SPL
    | STL
    | CSI Word8 [Word8]
    | TIME Word8 Word8
    | C10_0
    | C15_15
    deriving (Show, Eq)


-- MACRO 0x40 MC body MACRO 0x4F
