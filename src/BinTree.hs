module BinTree
  ( module Value,
    BinTree,
    Direction (..),
    Path,
    lookupNode,
    getNode,
    adjustNode,
    setNode,
    popNode,
    overNode,
    setNode_,
    rootPath,
    runBinTree,
    evalBinTree,
    traceTree,
  )
where

import Base
import Data.Tree qualified as Tree
import Value

data Direction
  = L
  | R
  deriving stock (Eq, Show)

type Path = [Direction]

data NodeTree = NodeTree
  { _treeLeft :: Tree,
    _treeValue :: Value,
    _treeRight :: Tree
  }

-- | for internal use
data Tree
  = TreeLeaf
  | TreeNode NodeTree

data BinTree m a where
  LookupNode :: Path -> BinTree m (Maybe Value)
  -- | Setting a node to Nothing will delete the entire subtree at that position
  AdjustNode :: Path -> (Maybe Value -> Maybe Value) -> BinTree m (Maybe Value)
  TraceTree :: BinTree m ()

makeSem ''BinTree
makeLenses ''NodeTree

evalBinTree :: (Members '[Error Text, Embed IO] r) => Sem (BinTree ': r) a -> Sem r a
evalBinTree = evalState TreeLeaf . re

runBinTree :: (Members '[Error Text, Embed IO] r) => Sem (BinTree ': r) a -> Sem r (Tree, a)
runBinTree = runState TreeLeaf . re

re :: (Members '[Error Text, Embed IO] r) => Sem (BinTree ': r) a -> Sem (State Tree ': r) a
re = reinterpret $ \case
  LookupNode p -> lookupNode' p
  AdjustNode p v -> adjustNode' p v
  TraceTree -> traceTree'

setNode_ :: (Members '[Error Text, BinTree] r) => Path -> Value -> Sem r ()
setNode_ p = void . setNode p

getNode :: (Members '[Error Text, BinTree] r) => Path -> Sem r Value
getNode = fromMaybeM (throw @Text "empty lookup") . lookupNode

rootPath :: Path
rootPath = []

traceTree' :: (Members '[State Tree, Embed IO] r) => Sem r ()
traceTree' =
  get >>= \case
    TreeLeaf -> putStrLn "<empty tree>"
    TreeNode n -> putStrLn (pack (Tree.drawTree (unpack . show <$> goNode n)))
  where
    goNode :: NodeTree -> Tree.Tree Value
    goNode n = Tree.Node (n ^. treeValue) (mapMaybe toTree [n ^. treeLeft, n ^. treeRight])

    toTree :: Tree -> Maybe (Tree.Tree Value)
    toTree = \case
      TreeLeaf -> Nothing
      TreeNode n -> Just (goNode n)

lookupNode' :: (Members '[Error Text, State Tree] r) => Path -> Sem r (Maybe Value)
lookupNode' p = do
  t <- get
  return (go t p)
  where
    go :: Tree -> Path -> Maybe Value
    go = \case
      TreeLeaf -> const Nothing
      TreeNode n -> \case
        [] -> Just (n ^. treeValue)
        d : ds ->
          let lr = case d of
                L -> treeLeft
                R -> treeRight
           in go (n ^. lr) ds

singletonTree :: Value -> Tree
singletonTree v =
  TreeNode
    NodeTree
      { _treeLeft = TreeLeaf,
        _treeRight = TreeLeaf,
        _treeValue = v
      }

popNode :: forall r. (Members '[Error Text, BinTree] r) => Path -> Sem r (Maybe Value)
popNode p = adjustNode p (const Nothing)

overNode :: forall r. (Members '[Error Text, BinTree] r) => Path -> (Value -> Value) -> Sem r ()
overNode p v = void (adjustNode p (fmap v))

setNode :: forall r. (Members '[Error Text, BinTree] r) => Path -> Value -> Sem r (Maybe Value)
setNode p v = adjustNode p (const (Just v))

adjustNode' :: forall r. (Members '[Error Text, State Tree] r) => Path -> (Maybe Value -> Maybe Value) -> Sem r (Maybe Value)
adjustNode' p newVal = do
  t0 <- get
  (newT, oldVal) <- go t0 p
  put newT
  return oldVal
  where
    go :: (Members '[Error Text] r') => Tree -> Path -> Sem r' (Tree, Maybe Value)
    go t = \case
      [] -> return $ case t of
        TreeLeaf -> (maybe TreeLeaf singletonTree (newVal Nothing), Nothing)
        TreeNode n ->
          let n' = case newVal (Just (n ^. treeValue)) of
                Nothing -> TreeLeaf
                Just val' -> TreeNode (set treeValue val' n)
           in (n', Just (n ^. treeValue))
      dir : dirs -> case t of
        TreeLeaf -> throw @Text "invalid position"
        TreeNode d -> case dir of
          L -> do
            (l', oldVal) <- go (d ^. treeLeft) dirs
            return (TreeNode (set treeLeft l' d), oldVal)
          R -> do
            (r', oldVal) <- go (d ^. treeRight) dirs
            return (TreeNode (set treeRight r' d), oldVal)
