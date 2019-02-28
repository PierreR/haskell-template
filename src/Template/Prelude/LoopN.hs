{-|
This code is not enough generic to be useful.

One issue is that a function `loop` would have to be declare in a separate module.
This is because the meaning of continue/break would be completely backward.

A second issue is the return value that an user might need to customize.
-}
module Template.Prelude.LoopN
where

import           Control.Monad.Trans.Maybe
import           Protolude                 as Exports hiding (break, die, (%))

continue :: MonadIO m => MaybeT m ()
continue = empty

-- asum will strive for the first non empty value
-- that's why returning one would break the loop
-- As a note, compare this with forever ... which would have the opposite behavior
-- `runMaybeT . forever` would break whenever empty is encountered.
break :: MonadIO m => MaybeT m ()
break = pure ()

-- | loop n times with the ability to break the loop
loopN :: MonadIO m => Int -> MaybeT m () -> m (Maybe ())
loopN n = runMaybeT . asum . replicate n
