module Fail where

import Base

data Fail m a = Fail

makeSem ''Fail

-- | Run a 'Fail' effect purely.
runFail' ::
  Sem '[Fail] a ->
  Maybe a
runFail' = run . runFail
{-# INLINE runFail' #-}

-- | Run a 'Fail' effect purely.
runFail ::
  Sem (Fail ': r) a ->
  Sem r (Maybe a)
runFail = fmap (^? _Right) . runError @() . reinterpret (\Fail -> throw ())
{-# INLINE runFail #-}

-- | Run a 'Fail' effect purely with a default value.
runFailDefault ::
  a ->
  Sem (Fail ': r) a ->
  Sem r a
runFailDefault defaultVal = fmap (fromMaybe defaultVal . (^? _Right)) . runError @() . reinterpret (\Fail -> throw ())
{-# INLINE runFailDefault #-}

ignoreFail ::
  Sem (Fail ': r) a ->
  Sem r ()
ignoreFail = void . runFail
{-# INLINE ignoreFail #-}

failWhen :: (Member Fail r) => Bool -> Sem r ()
failWhen c = when c fail
{-# INLINE failWhen #-}

failUnlessM :: (Member Fail r) => Sem r Bool -> Sem r ()
failUnlessM c = unlessM c fail
{-# INLINE failUnlessM #-}

failUnless :: (Member Fail r) => Bool -> Sem r ()
failUnless c = unless c fail
{-# INLINE failUnless #-}

failMaybe :: (Member Fail r) => Maybe a -> Sem r a
failMaybe = \case
  Nothing -> fail
  Just x -> return x
{-# INLINE failMaybe #-}
