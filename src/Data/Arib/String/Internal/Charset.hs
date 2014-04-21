{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Arib.String.Internal.Charset where

import qualified Data.Text.Lazy.Builder as B
import Data.Primitive.Array
import System.IO.Unsafe
import Data.Arib.String.Internal.TH

kanji :: Dict
kanji = $(mkCharsetFunction "/Users/philopon/mysrc/Haskell/arib/data/Kanji.txt")

