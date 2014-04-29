{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal.PAT where

import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Bits
import Data.Binary.Get

import Data.Arib.PSI.Internal.Common

data PAT 
    = PAT
        { patPsiHeader  :: {-#UNPACK#-}!PSIHeader
        , programPidMap :: ![(Int, Int)]
        } deriving(Show, Typeable)

instance PSI PAT where
    header = header . patPsiHeader
    {-# INLINE header #-}

pat :: PSIFunc PAT
pat 0 = {-# SCC "pat" #-} runPsi getF
  where
    getF h = PAT h <$> foldM (\m _ -> do
        genre <- getWord16be
        pid   <- fromIntegral . (0x1FFF .&.) <$> getWord16be
        return $ if genre == 0
                 then m
                 else (fromIntegral genre, pid) : m
        ) [] [1 .. ((sectionLength h - 9) `quot` 4)]

pat _ = const []
{-# INLINE pat #-}

