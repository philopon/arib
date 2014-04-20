{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module Data.Arib.String.Internal.Charset where

import qualified Data.Text.Lazy.Builder as B
import Data.Primitive.Array
import System.IO.Unsafe
import Data.Arib.String.Internal.TH

kanji :: Array B.Builder
kanji = unsafePerformIO $(mkCharsetFunction "/Users/philopon/mysrc/Haskell/arib/data/Kanji.txt")

