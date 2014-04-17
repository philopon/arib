{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedLists #-}

import Language.Haskell.TH
import Data.Word
import qualified Data.HashMap.Strict as H
import Data.List
import Numeric
import qualified Data.Vector.Unboxed
import Control.Applicative

readData :: String -> H.HashMap Word16 (Word32, Word32)
readData = H.fromList . map readDatum . lines
  where
    readDatum :: String -> (Word16, (Word32, Word32))
    readDatum str = case readHex str of
        [(k,o1)] -> case readHex $ tail o1 of
            [(v1,[])]   -> (k, (toEnum v1, maxBound))
            [(v1,_:o2)] -> case readHex o2 of
                [(v2,_)] -> (k, (toEnum v1,toEnum v2))
                _ -> error "cannot read: str. must be \"hex hex\""
            _ -> error "cannot read: str. must be \"hex hex\""
        _ -> error "cannot read: str. must be \"hex hex\""

mkList dict =  [minBound .. maxBound]

-- 初期状態
-- G0 Kanji
-- G1 Eisuu
-- G2 Hira
-- G3 Macro -- Kata?
--
-- GL LS0  (G0)
-- GR LS2R (G2)

-- http://www35.atwiki.jp/tvrock/m/pages/26.html?guid=on
-- 1B  7C   C9 E9 DE A2 F3 B3 21 3C EB 21 56 3F 65 38
-- ESC LS3R ド ラ マ ア ン コ ー    ル 「    水

-- GR LS3R (G3)

