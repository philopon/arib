{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Arib.PSI.Descriptor.Content where

import Data.Word
import Data.Typeable

import Data.Arib.PSI.Descriptor.Internal.TH (mkGenre)

mkGenre "aribdata/genre.txt"

data Content = Content
    { genre      :: Genre
    , userNibble :: Word8
    } deriving (Show, Read, Eq, Ord, Typeable)


