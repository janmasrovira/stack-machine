module Prenock where

import Base
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty
import Stacks (Stacks)
import Stacks qualified
import Value

type Prenock = [Instruction]

type ProgramLoc = Natural

data Op
  = Incr
  deriving stock (Show)

data StackName
  = ValueStack
  | CallStack
  | TempStack
  | FunctionsLibrary
  | StandardLibrary
  deriving stock (Enum, Bounded, Eq, Show)

numStacks :: (Integral a) => a
numStacks = fromIntegral (length (allElements @StackName))

data StackOp
  = StackPush Natural
  | StackPop
  deriving stock (Show)

data BranchCondIte = BranchCondIte
  { _branchCondIteTrue :: [Instruction],
    _branchCondIteFalse :: [Instruction]
  }
  deriving stock (Show)

newtype BranchOp
  = BranchCond BranchCondIte
  deriving stock (Show)

data MemoryOp
  = MemoryRead
  | MemoryWrite
  deriving stock (Show)

data Instruction
  = InstructionOp Op
  | InstructionStack StackOp
  | InstructionBranch BranchOp
  --- | InstructionMemory MemoryOp
  deriving stock (Show)

data Compiler m a where
  Push :: StackName -> Natural -> Compiler m ()
  Pop :: StackName -> Compiler m ()
  Increment :: StackName -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()

-- | The path to the head of a named stack
stackPath :: StackName -> Path
stackPath s = replicate (fromIntegral (fromEnum s)) R ++ finalPath
  where
    finalPath :: Path
    finalPath
      | fromEnum s == numStacks - 1 = []
      | otherwise = [L]

-- | Construct a path rooted at he head of a named stack
pathInStack :: StackName -> Path -> Path
pathInStack s p = stackPath s ++ p

-- BinOp :: Op -> Compiler m ()

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

seqTerms :: [Term Natural] -> Term Natural
seqTerms = foldl' step (OpAddress # emptyPath) . reverse
  where
    step :: Term Natural -> Term Natural -> Term Natural
    step acc t = OpSequence # t # acc

runCompiledNock :: (MonadIO m) => Sem '[Compiler] () -> m ()
runCompiledNock s = do
  let t = run (execCompiler s)
      stack = foldr1 (#) (reverse (replicate numStacks (toNock (0 :: Natural))))
      evalT =
        run
          . runError @(ErrNockNatural Natural)
          . runError @NockEvalError
          $ eval stack t
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (show e)
      Right res -> putStrLn (ppTrace res)

execCompiler :: Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

serializeCompiler :: Sem '[Compiler] () -> [Instruction]
serializeCompiler = run . execOutputList . reInstruction

reInstruction :: Sem (Compiler ': r) a -> Sem (Output Instruction ': r) a
reInstruction = reinterpretH $ \case
  Push _ n -> output (InstructionStack (StackPush n)) >>= pureT
  Pop _ -> output (InstructionStack StackPop) >>= pureT
  Increment _ -> output (InstructionOp Incr) >>= pureT

re :: Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  Push s n -> output (pushOnStack s (OpQuote # n)) >>= pureT
  Pop s -> output (popStack s) >>= pureT
  Increment s ->
    output
      ( replaceOnStack
          s
          (OpInc # (OpAddress # pathInStack s [L]))
      )
      >>= pureT
  Branch t f -> do
    termT <- runT t >>= raise . execCompiler . (pop ValueStack >>)
    termF <- runT f >>= raise . execCompiler . (pop ValueStack >>)
    output (OpIf # (OpAddress # pathInStack ValueStack [L]) # termT # termF) >>= pureT
  where
    pushOnStack :: StackName -> Term Natural -> Term Natural
    pushOnStack s t = OpPush # t # topStack s

    replaceOnStack :: StackName -> Term Natural -> Term Natural
    replaceOnStack s t = OpPush # t # replaceTopStack s

    popStack :: StackName -> Term Natural
    popStack sn =
      foldr1
        (#)
        [ let p = stackPath s
              a
                | sn == s = p ++ [R]
                | otherwise = p
           in OpAddress # a
          | s <- allElements
        ]

    -- Reconstruct the value-stack / call-stack cell by moving the global head to the
    -- respective stack head.
    topStack :: StackName -> Term Natural
    topStack sn =
      foldr1
        (#)
        [ let p = OpAddress # (R : stackPath s)
            in if
                | sn == s -> (OpAddress # [L]) # p
                | otherwise -> p
          | s <- allElements
        ]

    replaceTopStack :: StackName -> Term Natural
    replaceTopStack sn =
      foldr1
        (#)
        [ let p = R : stackPath s
           in if
                | sn == s -> (OpAddress # [L]) # (OpAddress # (p ++ [R]))
                | otherwise -> OpAddress # p
          | s <- allElements
        ]

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
            pc' :: Path <- Stacks.nextPath (decodePath' (EncodedPath pc))
            Stacks.popStack pcStackId
            void (Stacks.pushStack pcStackId (ValueNat (encodePath pc' ^. encodedPath)))

          interpretLoop :: Sem r' ()
          interpretLoop = do
            ValueNat pc :: Value <- Stacks.readValue pcPath
            let curPc = decodePath' (EncodedPath pc)
            whenM (decodeBool <$> Stacks.pathExists curPc) $ do
              i <- decodeInstruction <$> Stacks.readValue curPc
              case i of
                InstructionOp Incr -> do
                  v <- Stacks.popStack valueStackId
                  void (Stacks.pushStack valueStackId (incr v))
                InstructionStack sop -> case sop of
                  StackPop -> void (Stacks.popStack valueStackId)
                  StackPush n -> void (Stacks.pushStack valueStackId (ValueNat n))
                InstructionBranch b -> undefined
              incrementPc
              interpretLoop
      interpretLoop
      Stacks.popStack valueStackId

prog1 :: Sem '[Compiler] ()
prog1 = do
  push ValueStack 13
  push TempStack 20
  push CallStack 666
  increment CallStack
  push ValueStack 13
  increment CallStack
  pop ValueStack
  push ValueStack 50
  push CallStack 33
  push CallStack 44
  pop CallStack
  pop ValueStack
  increment CallStack
  push ValueStack 0
  branch (push ValueStack 99) (push ValueStack 77)

prog :: [Instruction]
prog = serializeCompiler prog1

instance Semigroup EncodedPath where
  a <> b = encodePath (decodePath' a <> decodePath' b)

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
encodeInstruction =
  ValueNat . \case
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

readPath :: (Members '[Compiler] r) => Path -> Sem r ()
readPath p = do
  undefined

-- read

writePath :: (Members '[Compiler] r) => Path -> Sem r ()
writePath p = do
  undefined

-- write

writeConst :: (Members '[Compiler] r) => Path -> Natural -> Sem r ()
writeConst p c = undefined

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
