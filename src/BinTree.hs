module BinTree
  ( module Value,
    module Path,
    BinTree,
    Direction (..),
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
import Path

data NodeTree a = NodeTree
  { _treeLeft :: Tree a,
    _treeValue :: a,
    _treeRight :: Tree a
  }

-- | for internal use
data Tree a
  = TreeLeaf
  | TreeNode (NodeTree a)

data BinTree t m a where
  LookupNode :: Path -> BinTree t m (Maybe t)
  -- | Setting a node to Nothing will delete the entire subtree at that position
  AdjustNode :: Path -> (Maybe t -> Maybe t) -> BinTree t m (Maybe t)
  TraceTree :: (t -> Text) -> BinTree t m ()

makeSem ''BinTree
makeLenses ''NodeTree

instance Functor Tree where
  fmap f = \case
    TreeLeaf -> TreeLeaf
    TreeNode n -> TreeNode (f <$> n)

instance Functor NodeTree where
  fmap f n =
    NodeTree
      { _treeValue = f (n ^. treeValue),
        _treeLeft = f <$> n ^. treeLeft,
        _treeRight = f <$> n ^. treeRight
      }

evalBinTree :: (Members '[Error Text, Embed IO] r) => Sem (BinTree t ': r) a -> Sem r a
evalBinTree = evalState TreeLeaf . re

runBinTreeWith :: (Members '[Error Text, Embed IO] r) => Tree t -> Sem (BinTree t ': r) a -> Sem r (Tree t, a)
runBinTreeWith ini = runState ini . re

runBinTree :: (Members '[Error Text, Embed IO] r) => Sem (BinTree t ': r) a -> Sem r (Tree t, a)
runBinTree = runBinTreeWith TreeLeaf

re :: (Members '[Error Text, Embed IO] r) => Sem (BinTree t ': r) a -> Sem (State (Tree t) ': r) a
re = reinterpret $ \case
  LookupNode p -> lookupNode' p
  AdjustNode p v -> adjustNode' p v
  TraceTree f -> traceTree' f

setNode_ :: (Members '[Error Text, BinTree t] r) => Path -> t -> Sem r ()
setNode_ p = void . setNode p

getNode :: (Members '[Error Text, BinTree t] r) => Path -> Sem r t
getNode = fromMaybeM (throw @Text "empty lookup") . lookupNode

rootPath :: Path
rootPath = []

traceTree' :: forall r t. (Members '[State (Tree t), Embed IO] r) => (t -> Text) -> Sem r ()
traceTree' shw =
  get >>= \case
    TreeLeaf -> putStrLn "<empty tree>"
    TreeNode n -> putStrLn (pack (Tree.drawTree (unpack <$> goNode (shw <$> n))))
  where
    goNode :: NodeTree Text -> Tree.Tree Text
    goNode n = Tree.Node (n ^. treeValue) $ case (n ^. treeLeft, n ^. treeRight) of
      (TreeLeaf, TreeLeaf) -> []
      (TreeNode l, TreeLeaf) -> [goNode l, Tree.Node "<right>" []]
      (TreeLeaf, TreeNode r) -> [Tree.Node "<left>" [], goNode r]
      (TreeNode l, TreeNode r) -> [goNode l, goNode r]

lookupNode' :: forall t r. (Members '[Error Text, State (Tree t)] r) => Path -> Sem r (Maybe t)
lookupNode' p = do
  t <- get
  return (go t p)
  where
    go :: Tree t -> Path -> Maybe t
    go = \case
      TreeLeaf -> const Nothing
      TreeNode n -> \case
        [] -> Just (n ^. treeValue)
        d : ds ->
          let lr = case d of
                L -> treeLeft
                R -> treeRight
           in go (n ^. lr) ds

singletonTree :: t -> Tree t
singletonTree v =
  TreeNode
    NodeTree
      { _treeLeft = TreeLeaf,
        _treeRight = TreeLeaf,
        _treeValue = v
      }

popNode :: forall r t. (Members '[Error Text, BinTree t] r) => Path -> Sem r (Maybe t)
popNode p = adjustNode p (const Nothing)

overNode :: forall r t. (Members '[Error Text, BinTree t] r) => Path -> (t -> t) -> Sem r ()
overNode p v = void (adjustNode p (fmap v))

setNode :: forall r t. (Members '[Error Text, BinTree t] r) => Path -> t -> Sem r (Maybe t)
setNode p v = adjustNode p (const (Just v))

adjustNode' :: forall r t. (Members '[Error Text, State (Tree t)] r) => Path -> (Maybe t -> Maybe t) -> Sem r (Maybe t)
adjustNode' p newVal = do
  t0 <- get
  (newT, oldVal) <- go t0 p
  put newT
  return oldVal
  where
    go :: (Members '[Error Text] r') => Tree t -> Path -> Sem r' (Tree t, Maybe t)
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
