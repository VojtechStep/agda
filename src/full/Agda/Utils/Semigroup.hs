-- | Some semigroup instances used in several places

module Agda.Utils.Semigroup where

import Control.Applicative (liftA2)

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State  (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Trans.Maybe (MaybeT)

import qualified Agda.Utils.StrictState as Strict
import qualified Agda.Utils.StrictWriter as Strict

instance (Monad m, Semigroup doc)       => Semigroup (ExceptT e m doc) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Monad m, Semigroup doc)       => Semigroup (MaybeT m doc) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Applicative m, Semigroup doc) => Semigroup (ReaderT s m doc) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Monad m, Semigroup doc)       => Semigroup (StateT s m doc)  where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Monad m, Semigroup doc, Monoid w) => Semigroup (WriterT w m doc)  where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Monad m, Semigroup doc)      => Semigroup (Strict.StateT s m doc) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)

instance (Monad m, Semigroup doc, Monoid w) => Semigroup (Strict.WriterT w m doc) where
  {-# INLINE (<>) #-}
  (<>) = liftA2 (<>)
