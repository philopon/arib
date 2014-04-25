{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Arib.PSI.Internal where

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Typeable

import Data.Arib.PESPSI
import Data.Arib.PSI.Internal.Common

raw :: PSIFunc L.ByteString
raw _ = (:[])

data Wrapper = forall a. (Typeable a, Show a) => Wrap a
deriving instance Show Wrapper

type WrappedFunc  = PESPSI L.ByteString -> [Wrapper]

wrap :: (Typeable a, Show a) => (PSIFunc a) -> WrappedFunc
wrap f p@PESPSI{..} = (\b -> Wrap $ p {pesPsiPayload = b}) <$> f pesPsiProgramId pesPsiPayload 

data WrappedFuncs = forall a. (Typeable a, Show a) => (PSIFunc a) :-> WrappedFuncs
                  | END 
infixr :->
infixr -|

(-|) :: (Typeable a, Typeable b, Show a, Show b) => PSIFunc a -> PSIFunc b -> WrappedFuncs
a -| b = a :-> b :-> END

multiPSI :: Monad m => WrappedFuncs -> Conduit (PESPSI L.ByteString) m Wrapper
multiPSI = awaitForever . go
  where
    go END        _ = return ()
    go (f :-> fs) r = case f (pesPsiProgramId r) (pesPsiPayload r) of
        [] -> go fs r
        m  -> mapM_ (yield . Wrap) m

singlePSI :: Monad m => PSIFunc a -> Conduit (PESPSI L.ByteString) m a
singlePSI f = awaitForever $ \r ->
    case f (pesPsiProgramId r) (pesPsiPayload r) of
        [] -> return ()
        m  -> mapM_ yield m
