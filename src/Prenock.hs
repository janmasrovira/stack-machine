module Prenock where

import Base
import Path
import Stacks (Stacks)
import Stacks qualified
import Value

type Prenock = [Instruction]

type ProgramLoc = Natural

data Op
  = Incr
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

runNock :: [Instruction] -> IO (Either Text Natural)
runNock = fmap (fmap (^. valueNat)) . runM . runError . Stacks.runStacks . runProgram
  where
    runProgram :: forall r'. (Members '[Stacks] r') => [Instruction] -> Sem r' Value
    runProgram instructions = do
      valueStackId <- Stacks.newStack
      instructionsStackId <- Stacks.newStack
      pcStackId <- Stacks.newStack
      firstInstructionPath <- Stacks.stackBottomPath instructionsStackId
      Stacks.pushStack pcStackId (ValueNat (encodePath firstInstructionPath ^. encodedPath))
      pcPath <- Stacks.stackBottomPath pcStackId
      forM_ instructions $ \i -> Stacks.pushStack instructionsStackId (encodeInstruction i)
      let incrementPc :: Sem r' ()
          incrementPc = do
            ValueNat pc :: Value <- Stacks.readValue pcPath
            pc' :: Path <- Stacks.nextPath (decodePath (EncodedPath pc))
            Stacks.popStack pcStackId
            void (Stacks.pushStack pcStackId (ValueNat (encodePath pc' ^. encodedPath)))

          interpretLoop :: Sem r' ()
          interpretLoop = do
            ValueNat pc :: Value <- Stacks.readValue pcPath
            let curPc = decodePath (EncodedPath pc)
            whenM (decodeBool <$> Stacks.pathExists curPc) $ do
              i <- decodeInstruction <$> Stacks.readValue curPc
              case i of
                InstructionOp Incr -> do
                  v <- Stacks.popStack valueStackId
                  void (Stacks.pushStack valueStackId (incr v))
                InstructionStack sop -> case sop of
                  StackPop -> void (Stacks.popStack valueStackId)
                  StackPush n -> void (Stacks.pushStack valueStackId (ValueNat n))
              incrementPc
              interpretLoop
      interpretLoop
      Stacks.popStack valueStackId

prog :: [Instruction]
prog = [
  InstructionStack (StackPush 13),
  InstructionOp Incr,
  InstructionStack (StackPush 50),
  InstructionStack StackPop
       ]

instance Semigroup EncodedPath where
  a <> b = encodePath (decodePath a <> decodePath b)

instance Monoid EncodedPath where
  mempty = encodePath []

magicBase :: Natural
magicBase = 2 ^ (16 :: Natural)

popOpCode :: Natural
popOpCode = magicBase

incrOpCode :: Natural
incrOpCode = magicBase + 1

pushOpCode :: Natural
pushOpCode = magicBase + 2

encodeInstruction :: Instruction -> Value
encodeInstruction = ValueNat . \case
  InstructionOp Incr -> incrOpCode
  InstructionStack s -> case s of
    StackPop -> popOpCode
    StackPush n -> pushOpCode + n

decodeInstruction :: Value -> Instruction
decodeInstruction (ValueNat n)
 | n == popOpCode = InstructionStack StackPop
 | n == incrOpCode = InstructionOp Incr
 | n >= pushOpCode = InstructionStack (StackPush (n - pushOpCode))
 | otherwise = error "bad encoding of instruction"

decodePath :: EncodedPath -> Path
decodePath ep = run (execOutputList (go (ep ^. encodedPath)))
  where
    go :: Natural -> Sem (Output Direction ': r) ()
    go = \case
      0 -> error "encoded path cannot be 0"
      1 -> return ()
      x
        | even x -> do
            go (x `div` 2)
            output L
        | otherwise -> do
            go ((x - 1) `div` 2)
            output R

encodePath :: Path -> EncodedPath
encodePath = EncodedPath . foldl' step 1
  where
    step :: Natural -> Direction -> Natural
    step n = \case
      R -> 2 * n + 1
      L -> 2 * n

readPath :: (Members '[Compiler] r) => Path -> Sem r ()
readPath p = do
  push (encodePath p ^. encodedPath)
  undefined
  -- read

writePath :: (Members '[Compiler] r) => Path -> Sem r ()
writePath p = do
  push (encodePath p ^. encodedPath)
  undefined
  -- write

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
