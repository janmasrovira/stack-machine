module Extra (module Extra,
    module Base,
    module Fail,
    ) where

import Base
import Fail

subtract :: (Members '[Fail] r) => Natural -> Natural -> Sem r Natural
subtract a b
  | a >= b = return (a - b)
  | otherwise = fail

naturalPred :: (Members '[Fail] r) => Natural -> Sem r Natural
naturalPred a = subtract a 1
