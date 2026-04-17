{-# OPTIONS_GHC -Wunused-imports #-}

module Agda.TypeChecking.Monad.MetaVars where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Identity ( IdentityT )
import Control.Monad.Trans.Maybe    ( MaybeT )

import Agda.Syntax.Common (InteractionId, MetaId)
import Agda.TypeChecking.Monad.Base
import qualified Agda.Utils.StrictReader as Strict
import qualified Agda.Utils.StrictState as Strict
import qualified Agda.Utils.StrictWriter as Strict

class (MonadTCEnv m, ReadTCState m) => MonadInteractionPoints m where
  freshInteractionId :: m InteractionId
  modifyInteractionPoints :: (InteractionPoints -> InteractionPoints) -> m ()

  default freshInteractionId
    :: (MonadTrans t, MonadInteractionPoints n, t n ~ m)
    => m InteractionId
  freshInteractionId = lift freshInteractionId

  default modifyInteractionPoints
    :: (MonadTrans t, MonadInteractionPoints n, t n ~ m)
    => (InteractionPoints -> InteractionPoints) -> m ()
  modifyInteractionPoints = lift . modifyInteractionPoints

instance MonadInteractionPoints m => MonadInteractionPoints (IdentityT m)
instance MonadInteractionPoints m => MonadInteractionPoints (MaybeT m)
instance MonadInteractionPoints m => MonadInteractionPoints (ReaderT r m)
instance MonadInteractionPoints m => MonadInteractionPoints (StateT s m)
instance (MonadInteractionPoints m, Monoid w) => MonadInteractionPoints (WriterT w m)
instance MonadInteractionPoints m => MonadInteractionPoints (Strict.ReaderT s m)
instance MonadInteractionPoints m => MonadInteractionPoints (Strict.StateT s m)
instance (MonadInteractionPoints m, Monoid w) => MonadInteractionPoints (Strict.WriterT w m)

instance MonadInteractionPoints TCM

isInteractionMeta :: ReadTCState m => MetaId -> m (Maybe InteractionId)
