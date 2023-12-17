module Value where

import Base

newtype Value =
  ValueNat Natural
  deriving stock (Show)

incr :: Value -> Value
incr = \case
  ValueNat n -> ValueNat (succ n)

decr :: Value -> Value
decr = \case
  ValueNat n -> ValueNat (pred n)
