module Prenock where

import Base
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty

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
  = StackPush (Term Natural)
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
  Push :: StackName -> Term Natural -> Compiler m ()
  Pop :: StackName -> Compiler m ()
  Call :: Text -> Natural -> Compiler m ()
  Increment :: StackName -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()

-- | The path to the head of a named stack
stackPath :: StackName -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInPath :: Path -> Natural -> Path
indexInPath p idx = p ++ indexStack idx

indexInStack :: StackName -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

--- [ code [0 0] ]
--- [8 [9 ADDR STDLIB_ADDR] [9 2 [10 [6 ARGS] [0 2]]]]

-- [push
--   [quote [[suc [@ RL]] 123 0]]
--   [call L @ L]
-- ]

-- [[[suc [@ RL]] 123 0] [value-stack call-stack....]  ]

-- [call L [quote [[suc [@ RL]] 123 0]]]

-- [call L [@ (stackPath FunctionLibrary PATH_TO_FUNCTION)]]

-- [value-stack call-stack]

-- [push
--   [quote [[@ RL] 123 0]]
--   [@ S]
-- ]

-- Proposal:

--   [stuff, arg1, ...., argn, f] => [stuff, arg1, ... argn, return] => [stuff, arg1]

--   1. Copy the value stack to the head of the call stack
--   2. Pop the arguments from the stack in 1.
--   2. Push the function to value stack
--   3. Pop/Write the arguments into the function
--   4. Evaluate the function
--   5. Push the head of the value stack to the stack in 1.
--   6. Replace the value stack with the stack in 1
--   7. Pop the call stack

funIncrement :: Term Natural
funIncrement = (OpInc # (OpAddress # [R, L])) # ((0 :: Natural) # (0 :: Natural))

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

-- initStacks :: [(Text, Term Natural)] -> HashMap Text Path

makeList :: [Term Natural] -> Term Natural
makeList ts = foldr1 (#) (ts ++ [TermAtom nockNil])

remakeList :: [Term Natural] -> Term Natural
remakeList ts = foldr1 (#) (ts ++ [OpQuote # nockNil'])

nockNil' :: Term Natural
nockNil' = TermAtom nockNil

initStack :: [Term Natural] -> Term Natural
initStack defs = makeList (initSubStack <$> allElements)
  where
    initSubStack :: StackName -> Term Natural
    initSubStack = \case
      ValueStack -> nockNil'
      CallStack -> nockNil'
      TempStack -> nockNil'
      StandardLibrary -> nockNil'
      FunctionsLibrary -> makeList defs

functions :: [(Text, Term Natural)]
functions = [("increment", funIncrement), ("lala", toNock (1 :: Natural))]

functionLocations :: HashMap Text Path
functionLocations =
  hashMap
    [ (n, pathInStack FunctionsLibrary (replicate i R))
      | (i, (n, _)) <- zip [0 ..] functions
    ]

runCompiledNock :: (MonadIO m) => Sem '[Compiler, Reader (HashMap Text Path)] () -> m ()
runCompiledNock s = do
  let t =
        run
          . runReader functionLocations
          . execCompiler
          $ s
      stack = initStack (snd <$> functions)
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

execCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
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

re :: (Member (Reader (HashMap Text Path)) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  Push s n -> outputT (pushOnStack s (OpQuote # n))
  Pop s -> outputT (popStack 1 s)
  Call funName funArgsNum -> do
    funPath <- fromJust <$> asks @(HashMap Text Path) (^. at funName)
    -- outputT (pushOnStack CallStack (OpAddress # pathInStack ValueStack (replicate funArgsNum R)))
    outputT (pushOnStack ValueStack
              (OpAddress # funPath)
             --(OpQuote #

              -- (OpReplace # (([R, L] # (OpAddress # pathInStack ValueStack [L])) # (OpAddress # funPath)))

             --)

            )
    -- outputT (pushOnStack ValueStack (OpReplace # (([R, L] # (OpAddress # pathInStack ValueStack [L])) # OpAddress # funPath)))


    ----- stack: [[h stuff] .... [.... [code args]] ...]
    ----


    --- [code [h args]]
    -- outputT (pushOnStack ValueStack (OpAddress # funPath))

  Increment s ->
    outputT
      ( replaceOnStack
          s
          (OpInc # (OpAddress # pathInStack s [L]))
      )
  Branch t f -> do
    termT <- runT t >>= raise . execCompiler . (pop ValueStack >>)
    termF <- runT f >>= raise . execCompiler . (pop ValueStack >>)
    outputT (OpIf # (OpAddress # pathInStack ValueStack [L]) # termT # termF)
  where
    outputT :: (Functor f, Member (Output (Term Natural)) r) => Term Natural -> Sem (WithTactics e f m r) (f ())
    outputT = pureT <=< output

    pushOnStack :: StackName -> Term Natural -> Term Natural
    pushOnStack s t = OpPush # t # topStack s

    replaceOnStack :: StackName -> Term Natural -> Term Natural
    replaceOnStack s t = OpPush # t # replaceTopStack s

    popStack :: Natural -> StackName -> Term Natural
    popStack n sn =
      remakeList
        [ let p = stackPath s
              a
                | sn == s = p ++ replicate n R
                | otherwise = p
           in OpAddress # a
          | s <- allElements
        ]

    -- Reconstruct the value-stack / call-stack cell by moving the global head to the
    -- respective stack head.
    --- [h [s1 s1 s3 nil]]
    --- [ s1 .. [h si] ... sn nil]
    topStack :: StackName -> Term Natural
    topStack sn =
      remakeList
        [ let p = OpAddress # (R : stackPath s)
           in if
                | sn == s -> (OpAddress # [L]) # p
                | otherwise -> p
          | s <- allElements
        ]

    replaceTopStack :: StackName -> Term Natural
    replaceTopStack sn =
      remakeList
        [ let p = R : stackPath s
           in if
                | sn == s -> (OpAddress # [L]) # (OpAddress # (p ++ [R]))
                | otherwise -> OpAddress # p
          | s <- allElements
        ]

-- runNock :: [Instruction] -> IO (Either Text Natural)
-- runNock = fmap (fmap (^. valueNat)) . runM . runError . Stacks.runStacks . runProgram
--   where
--     runProgram :: forall r'. (Members '[Stacks] r') => [Instruction] -> Sem r' Value
--     runProgram instructions = do
--       valueStackId <- Stacks.newStack
--       instructionsStackId <- Stacks.newStack
--       pcStackId <- Stacks.newStack
--       firstInstructionPath <- Stacks.stackBottomPath instructionsStackId
--       Stacks.pushStack pcStackId (ValueNat (encodePath firstInstructionPath ^. encodedPath))
--       pcPath <- Stacks.stackBottomPath pcStackId
--       forM_ instructions $ \i -> Stacks.pushStack instructionsStackId (encodeInstruction i)
--       let incrementPc :: Sem r' ()
--           incrementPc = do
--             ValueNat pc :: Value <- Stacks.readValue pcPath
--             pc' :: Path <- Stacks.nextPath (decodePath' (EncodedPath pc))
--             Stacks.popStack pcStackId
--             void (Stacks.pushStack pcStackId (ValueNat (encodePath pc' ^. encodedPath)))

--           interpretLoop :: Sem r' ()
--           interpretLoop = do
--             ValueNat pc :: Value <- Stacks.readValue pcPath
--             let curPc = decodePath' (EncodedPath pc)
--             whenM (decodeBool <$> Stacks.pathExists curPc) $ do
--               i <- decodeInstruction <$> Stacks.readValue curPc
--               case i of
--                 InstructionOp Incr -> do
--                   v <- Stacks.popStack valueStackId
--                   void (Stacks.pushStack valueStackId (incr v))
--                 InstructionStack sop -> case sop of
--                   StackPop -> void (Stacks.popStack valueStackId)
--                   StackPush n -> void (Stacks.pushStack valueStackId (ValueNat n))
--                 InstructionBranch b -> undefined
--               incrementPc
--               interpretLoop
--       interpretLoop
--       Stacks.popStack valueStackId

pushNat :: (Member Compiler r) => StackName -> Natural -> Sem r ()
pushNat s n = push s (toNock n)

prog1 :: Sem '[Compiler, Reader (HashMap Text Path)] ()
prog1 = do
  pushNat ValueStack 111
  pushNat ValueStack 222
  pushNat ValueStack 333
  --call "increment" 1
  -- push FunctionsLibrary funIncrement
  -- pushNat ValueStack 13
  -- pushNat TempStack 20
  -- pushNat CallStack 666
  -- increment CallStack
  -- pushNat ValueStack 13
  -- increment CallStack
  -- pop ValueStack
  -- pushNat ValueStack 50
  -- pushNat CallStack 33
  -- pushNat CallStack 44
  -- pop CallStack
  -- pop ValueStack
  -- increment CallStack
  pushNat ValueStack 0
  branch (pushNat ValueStack 99) (pushNat ValueStack 77)

initProg :: Sem '[Compiler] ()
initProg = do
  push FunctionsLibrary funIncrement

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

-- encodeInstruction :: Instruction -> Value
-- encodeInstruction =
--   ValueNat . \case
--     InstructionOp Incr -> incrOpCode
--     InstructionStack s -> case s of
--       StackPop -> popOpCode
--       StackPush n -> pushOpCode + n

-- decodeInstruction :: Value -> Instruction
-- decodeInstruction (ValueNat n)
--   | n == popOpCode = InstructionStack StackPop
--   | n == incrOpCode = InstructionOp Incr
--   | n >= pushOpCode = InstructionStack (StackPush (n - pushOpCode))
--   | otherwise = error "bad encoding of instruction"

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
