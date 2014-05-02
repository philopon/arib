{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.PAT.Internal where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Bits
import Data.Binary.Get
import Data.Tagged

import Data.Arib.PSI.Internal.Common

data PAT 
    = PAT
        { patPsiHeader  :: {-#UNPACK#-}!PSIHeader
        , programPidMap :: ![(Int, Int)]
        } deriving (Show, Eq, Typeable)

instance HasPSIHeader PAT where
    header = header . patPsiHeader
    {-# INLINE header #-}

pat :: PSITag PAT
pat = Tagged (== 0)

instance PSI PAT where
    getPSI _ = {-# SCC "pat" #-} runPsi getF
      where
        getF h = PAT h . flip ($) [] <$> foldM (\f _ -> do
            genre <- getWord16be
            pid   <- fromIntegral . (0x1FFF .&.) <$> getWord16be
            return $ if genre == 0
                     then f
                     else f . (:) (fromIntegral genre, pid)
            ) id [1 .. ((sectionLength h - 9) `quot` 4)]

    {-# INLINE getPSI #-}
