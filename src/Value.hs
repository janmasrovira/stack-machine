module Value where

import Base

data Value =
  ValueNat Natural
  | ValueInstruction Natural
  deriving stock (Show)

incr :: Value -> Value
incr = \case
  ValueNat n -> ValueNat (succ n)
  _ -> impossible

decr :: Value -> Value
decr = \case
  ValueNat n -> ValueNat (pred n)
  _ -> impossible
