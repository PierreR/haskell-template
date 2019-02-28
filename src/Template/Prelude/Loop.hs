{-|
Imperative style loop that can be exited.

The module relies on 'asum' to exit the loop.
`asum`` will strive for the first non empty value
that's why returning one with pure would break the loop

The same effect can be achieved with 'ExceptT' and 'forever'
The downside of 'ExceptT' is a Right value that will never happen
and a return 'Left' value.
-}
module Template.Prelude.Loop
where

import           Control.Monad.Trans.Maybe
import           Protolude                 as Exports hiding (break, die, (%))
import           Data.Maybe                            (fromJust)

continue :: MonadIO m => MaybeT m a
continue = empty

break :: MonadIO m => a -> MaybeT m a
break = pure

-- | loop n times with the ability to break the loop
loopN :: MonadIO m => Int -> MaybeT m a -> m a
loopN n = fmap fromJust . runMaybeT . asum . replicate n

-- As a note, compare this with forever ... which would have the opposite behavior
-- `runMaybeT . forever` would break whenever empty is encountered.

-- we are using 'fromJust' because the loop should never return Nothing

-- | loop n times with the ability to break the loop
loop :: MonadIO m => MaybeT m a -> m a
loop  = fmap fromJust . runMaybeT . asum . repeat
