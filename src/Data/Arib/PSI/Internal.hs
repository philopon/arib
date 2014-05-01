{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal where

import Control.Monad

import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Typeable
import Data.Tagged

import Data.Arib.PESPSI
import Data.Arib.PSI.Internal.Common

data Wrapper = forall a. (Typeable a, Show a) => Wrap a
deriving instance Show Wrapper

unWrap :: PSI a => PSITag a -> Wrapper -> Maybe a
unWrap _ (Wrap a) = cast a
{-# INLINE unWrap #-}

data PSIs = forall a. PSI a => PSITag a :-> PSIs
          | END 
infixr :->
infixr -|

(-|) :: (PSI a, PSI b) => PSITag a -> PSITag b -> PSIs
a -| b = a :-> b :-> END
{-# INLINE (-|) #-}

-- | parse multiple PSI.
--
-- @
-- ex :: Monad m => Conduit (PESPSI L.ByteString) m Wrapper
-- ex = multiPSI (eit :-> pat -| raw)
-- @
--
-- ex conduit parse EIT(programId \`elem\` [0x12, 0x26, 0x27]) and PAT(programId == 0x00),
-- then packet which has other programId, pass through.
--
-- all elements wrapped around by 'Wrapper', thus you should unWrap to use it.
--
-- @
-- Just h <- sourceTs file $$ concatTsPackets_ =$ ex =$ CL.head
-- let _ = h            :: Wrapper
--     _ = unWrap eit h :: Maybe EIT
-- @
--
multiPSI :: Monad m => PSIs -> Conduit (PESPSI L.ByteString) m Wrapper
multiPSI = awaitForever . go
  where
    go END        _ = return ()
    go (t :-> ts) r 
        | untag t (pesPsiProgramId r) = case getPSI t (pesPsiPayload r) of
            [] -> return ()
            m  -> mapM_ (yield . Wrap) m
        | otherwise = go ts r
{-# INLINE multiPSI #-}

singlePSI :: (Monad m, PSI a) => PSITag a -> Conduit (PESPSI L.ByteString) m a
singlePSI t = awaitForever $ \r -> when (untag t $ pesPsiProgramId r) $
    case getPSI t (pesPsiPayload r) of
        [] -> return ()
        m  -> mapM_ yield m
{-# INLINE singlePSI #-}

