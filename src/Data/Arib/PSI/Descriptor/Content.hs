{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Arib.PSI.Descriptor.Content
    ( 
      Content(..)
    -- * Genre
    , Genre(..)
    -- * sub genre
    , News(..), Sports(..), Information(..), Drama(..), Music(..)
    , Variety(..), Movie(..), Anime(..), Documentary(..)
    , Performance(..), Hobby(..), Welfare(..), Extension(..)
    , prettySubGenre
    ) where

import Data.Word
import Data.Typeable

import Data.Arib.PSI.Descriptor.Internal.TH (mkGenre)

mkGenre "aribdata/genre.txt"

data Content = Content
    { genre      :: Genre
    , userNibble :: {-#UNPACK#-}!Word8
    } deriving (Show, Read, Eq, Ord, Typeable)

