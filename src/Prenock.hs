module Prenock where

import Base
import Path

type Prenock = [Instruction]

type ProgramLoc = Natural

data Op
  = Add
  | Mult
  deriving stock (Show)

data StackOp
  = StackPush Natural
  | StackPop
  deriving stock (Show)

-- data BranchOp
--   = BranchGoto ProgramLoc
--   | BranchCond ProgramLoc ProgramLoc
--   deriving stock (Show)

data MemoryOp
  = MemoryRead
  | MemoryWrite
  deriving stock (Show)

data Instruction
  = InstructionOp Op
  | InstructionStack StackOp
  --- | InstructionBranch BranchOp
  --- | InstructionMemory MemoryOp
  deriving stock (Show)

newtype EncodedPath = EncodedPath
  { _encodedPath :: Natural
  }

data Compiler m a where
  Push :: Natural -> Compiler m ()
  BinOp :: Op -> Compiler m ()
  -- Alloc :: Compiler m Pointer
  -- stack [pointer, ..] => [*pointer, ..]
  -- Read :: Compiler m ()
  -- Write :: Compiler m Pointer

data StackId
  = StackMemory
  | StackMain
  | StackProgram
  deriving stock (Enum, Bounded)

makeSem ''Compiler
makeLenses ''EncodedPath

instance Semigroup EncodedPath where
  a <> b = encodePath (decodePath a <> decodePath b)

instance Monoid EncodedPath where
  mempty = encodePath []

decodePath :: EncodedPath -> Path
decodePath = undefined

encodePath :: Path -> EncodedPath
encodePath = undefined

readPath :: (Members '[Compiler] r) => Path -> Sem r ()
readPath p = do
  push (encodePath p ^. encodedPath)
  read

writePath :: (Members '[Compiler] r) => Path -> Sem r ()
writePath p = do
  push (encodePath p ^. encodedPath)
  write

writeConst :: (Members '[Compiler] r) => Path -> Natural -> Sem r ()
writeConst p c = push c >> writePath p

initializeStacks :: (Members '[Compiler] r) => Sem r ()
initializeStacks = forM_ allElements $ \sid -> do
  writeConst (stackRoot sid) 0

stackId :: StackId -> Natural
stackId = fromIntegral . fromEnum

rootPath :: Path
rootPath = []

getStackSize :: (Members '[Compiler] r) => StackId -> Sem r ()
getStackSize s = readPath (stackRoot s)

getNumStacks :: (Members '[Compiler] r) => Sem r ()
getNumStacks = readPath rootPath

stackRoot :: StackId -> Path
stackRoot n = rootPath ++ replicate (succ (stackId n)) L

relativeToStack :: StackId -> Path -> Path
relativeToStack sid p = stackRoot sid ++ [R] ++ p
