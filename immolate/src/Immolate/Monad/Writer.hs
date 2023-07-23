module Immolate.Monad.Writer where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Writer.CPS as Transformers
import Data.ByteString.Builder (Builder)

newtype WriterT o m a = WriterT (Transformers.WriterT o m a)
  deriving ( Functor, Applicative, Monad
           , MonadTrans, MonadFix
           )

instance (Monoid o, Monad m, Semigroup a) => Semigroup (WriterT o m a) where
  (<>) = liftA2 (<>)

instance (Monoid o, Monad m, Monoid a) => Monoid (WriterT o m a) where
  mempty = pure mempty
  mappend = (<>)

runWriterT :: Monoid o => WriterT o m a -> m (a, o)
runWriterT (WriterT a) = Transformers.runWriterT a

hoistWriterT :: (Monoid o, Monad n) => (forall x . m x -> n x) -> WriterT o m a -> WriterT o n a
hoistWriterT nt (WriterT a) = WriterT (Transformers.mapWriterT nt a)

tell :: (Monoid o, Monad m) => o -> WriterT o m ()
tell = WriterT . Transformers.tell
{-# INLINE tell #-}
{-# SPECIALIZE tell :: Monad m => Builder -> WriterT Builder m () #-}

censor :: (Monoid o, Monad m) => (o -> o) -> WriterT o m a -> WriterT o m a
censor f (WriterT a) = WriterT (Transformers.censor f a)