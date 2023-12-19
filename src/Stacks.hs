module Stacks
  ( module Stacks,
    module Value,
  )
where

import Base
import BinTree hiding (traceTree)
import BinTree qualified
import Value

type StackId = Natural

{-
The binary tree encoding of the stacks machine has the following shape:
                         numStacks
                      /            \
                  stack1Size       empty
                  /         \
            stack2Size     bottomStack1
             /     \        /   \
           ..      ..    empty   bottomStack1 - 1
                                   \
                                    ..
                                     \
                                      topStack1

progStack:
    progCounter
    /        \
 empty       instruction1
              \
               instruction2
-}
data Stacks m a where
  NewStack :: Stacks m StackId
  PopStack :: StackId -> Stacks m Value
  PushStack :: StackId -> Value -> Stacks m Path
  PathExists :: Path -> Stacks m Value
  -- | Returns the path to the next free node of a stack
  StackBottomPath :: StackId -> Stacks m Path
  ReadValue :: Path -> Stacks m Value
  NextPath :: Path -> Stacks m Path
  TraceTree :: Stacks m ()

makeSem ''Stacks

re :: (Members '[Error Text] r) => Sem (Stacks ': r) a -> Sem (BinTree Value ': r) a
re = reinterpret $ \case
  NewStack -> newStack'
  PopStack i -> popStack' i
  PathExists i -> pathExists' i
  PushStack i v -> pushStack' i v
  StackBottomPath i -> return (bottomStackPath i)
  ReadValue p -> readValue' p
  NextPath p -> nextPath' p
  TraceTree -> BinTree.traceTree (show @Value)

readValue' :: (Members '[Error Text, BinTree Value] r) => Path -> Sem r Value
readValue' = getNode

nextPath' :: (Monad m) => Path -> m Path
nextPath' p = return (p ++ [R])

pathExists' :: (Members '[BinTree Value] r) => Path -> Sem r Value
pathExists' t = encodeBool . isJust <$> lookupNode @Value t

pushStack' :: (Members '[Error Text, BinTree Value] r) => StackId -> Value -> Sem r Path
pushStack' s t = do
  pos <- nextFreePath s
  setNode_ pos t
  overNode (stackRoot s) incr
  return pos

popStack' :: (Members '[Error Text, BinTree Value] r) => StackId -> Sem r Value
popStack' s = do
  tp <- topPath s
  res <- fromMaybeM (throw @Text "pop invalid path") (popNode tp)
  overNode (stackRoot s) decr
  return res

nextFreePath :: (Members '[Error Text, BinTree Value] r) => StackId -> Sem r Path
nextFreePath s = do
  n <- getStackSize s
  return (relativeToStackNat s n)

topPath :: (Members '[Error Text, BinTree Value] r) => StackId -> Sem r Path
topPath s = do
  n <- getStackSize s
  when (n == 0) (throw @Text "empty stack")
  return (relativeToStackNat s (pred n))

relativeToStackNat :: StackId -> Natural -> Path
relativeToStackNat sid p = relativeToStack sid (replicate p R)

bottomStackPath :: StackId -> Path
bottomStackPath sid = stackRoot sid ++ [R]

relativeToStack :: StackId -> Path -> Path
relativeToStack sid p = bottomStackPath sid ++ p

newStack' :: (Members '[Error Text, BinTree Value] r) => Sem r StackId
newStack' = do
  num <- getNumStacks
  setNode_ rootPath (ValueNat (succ num))
  let newStackRoot = stackRoot num
  setNode_ newStackRoot (ValueNat 0)
  return num

getNat :: (Members '[Error Text, BinTree Value] r) => Path -> Sem r Natural
getNat p = do
  v <- getNode p
  case v of
    ValueNat n -> return n

getStackSize :: (Members '[Error Text, BinTree Value] r) => StackId -> Sem r Natural
getStackSize = getNat . stackRoot

getNumStacks :: (Members '[Error Text, BinTree Value] r) => Sem r Natural
getNumStacks = getNat rootPath

stackRoot :: StackId -> Path
stackRoot n = replicate (succ n) L

runStacks :: (Members '[Error Text, Embed IO] r) => Sem (Stacks ': r) a -> Sem r a
runStacks m = evalBinTree $ do
  setNode_ rootPath (ValueNat 0)
  re m

ex :: IO ()
ex = runM . runTextErrorIO . runStacks $ x
  where
    x :: Sem '[Stacks, Error Text, Embed IO] ()
    x = do
      s1 <- newStack
      s2 <- newStack
      pushStack s1 (ValueNat 101)
      pushStack s1 (ValueNat 102)
      v3 <- pushStack s1 (ValueNat 103)
      v4 <- nextPath v3
      pushStack s1 (ValueNat 104)
      pushStack s2 (ValueNat 201)
      pushStack s2 (ValueNat 202)
      v <- popStack s2
      putStrLn ("pop s2 = " <> show v)
      putStrLn ("v3 = " <> show v3)
      traceTree
      v3' <- readValue v3
      v4' <- readValue v4
      putStrLn ("*v3 = " <> show v3')
      putStrLn ("*v4 = " <> show v4')
      putStrLn "end"
