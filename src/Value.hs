module Value where

import Base

newtype Value =
  ValueNat {_valueNat :: Natural}
  deriving stock (Show)

makeLenses ''Value

incr :: Value -> Value
incr = \case
  ValueNat n -> ValueNat (succ n)

decr :: Value -> Value
decr = \case
  ValueNat n -> ValueNat (pred n)

encodeBool :: Bool -> Value
encodeBool = \case
  True -> ValueNat 0
  False -> ValueNat 1

decodeBool :: Value -> Bool
decodeBool (ValueNat n ) = case n of
  0 -> True
  1 -> False
  _ -> error "bad bool encoding"
