module Path where

import Base

data Direction
  = L
  | R
  deriving stock (Eq, Show)

type Path = [Direction]
