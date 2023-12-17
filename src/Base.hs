module Base (module Base,
    module Control.Applicative,
    module Control.Monad.Combinators,
    module Control.Monad.Extra,
    module Control.Monad.Fix,
    module Control.Monad.Trans.Class,
    module Data.Bifunctor,
    module Data.Bitraversable,
    module Data.Bool,
    module Data.Char,
    module Data.Either.Extra,
    module Data.Eq,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.Graph,
    module Data.Hashable,
    module Data.Int,
    module Data.IntMap.Strict,
    module Data.IntSet,
    module Data.List.Extra,
    module Data.List.NonEmpty.Extra,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.Monoid,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Set,
    module Data.String,
    module Data.Text,
    module Data.Text.Encoding,
    module Data.Text.IO,
    module Data.Traversable,
    module Data.Tuple.Extra,
    module Data.Typeable,
    module Data.Void,
    module Data.Word,
    module GHC.Enum,
    module GHC.Generics,
    module GHC.Num,
    module GHC.Real,
    module Lens.Micro.Platform,
    module Polysemy,
    module Polysemy.Embed,
    module Polysemy.Error,
    module Polysemy.Fixpoint,
    module Polysemy.Output,
    module Polysemy.Resource,
    module Polysemy.Scoped,
    module Polysemy.State,
    module Polysemy.Reader,
    module Polysemy.Tagged,
    module Prelude,
    module Safe,
    module Safe.Exact,
    module Safe.Foldable,
    module System.Environment,
    module System.IO,
    module Text.Megaparsec,
    module Text.Show,
    Data,
    HashMap,
    HashSet,
    IsString (..),
    Alternative (..),
    MonadIO (..),
    type (~),
            ) where

import Control.Applicative hiding (many, some)
import Control.Monad.Extra hiding (fail, mconcatMapM, whileJustM)
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor hiding (first, second)
import Data.Bitraversable
import Data.Bool
import Data.Char
import Data.Data
import Data.Either.Extra
import Data.Eq
import Data.Foldable hiding (minimum, minimumBy)
import Data.Function
import Data.Functor
import Data.Graph (Graph, SCC (..), Vertex, stronglyConnComp)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import Data.Int
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.List.Extra hiding (allSame, groupSortOn, head, last, mconcatMap, lines, unlines, replicate)
import Data.List.NonEmpty.Extra ( NonEmpty (..), appendList, head, last, maximum1, maximumOn1, minimum1, minimumOn1, nonEmpty, prependList, some1, (|:),)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup (Semigroup, Min(..), Max(..), sconcat, (<>))
import Data.Set (Set)
import Data.String hiding (lines, unlines)
import Data.Text (Text, pack, strip, unpack, lines, unlines)
import Data.Text.Encoding
import Data.Text.IO hiding (appendFile, readFile, writeFile, putStrLn)
import Data.Text.IO qualified as Text
import Data.Traversable
import Data.Tuple.Extra hiding (both)
import Data.Type.Equality (type (~))
import Data.Typeable hiding (TyCon)
import Data.Void
import Data.Word
import GHC.Enum
import GHC.Err qualified as Err
import GHC.Generics (Generic)
import GHC.Num hiding (subtract)
import GHC.Real
import GHC.Stack.Types
import Lens.Micro.Platform
import Polysemy
import Polysemy.Embed
import Polysemy.Error hiding (fromEither, try)
import Control.Monad.Combinators
import Polysemy.Fixpoint
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Scoped
import Polysemy.State
import Polysemy.Reader
import Polysemy.Tagged hiding (tag)
import Prelude (Double)
import Safe (headMay)
import Safe.Exact
import Safe.Foldable
import System.Environment
import System.IO (IO)
import Text.Megaparsec hiding (ParsecT, State, runParser, runParserT, parse)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Megaparsec.Char qualified as Char
import Text.Show (Show)
import Text.Show qualified as Show
import Debug.Trace qualified as T
import System.Exit (exitFailure)

allElements :: (Bounded a, Enum a) => [a]
allElements = [minBound .. maxBound]

fromMaybe' :: Maybe a -> a
fromMaybe' = fromMaybe impossible

{-# DEPRECATED undefined "undefined" #-}
undefined :: (HasCallStack) => a
undefined = Err.error "undefined"

hashMapBy :: Hashable k => (v -> k) -> [v] -> HashMap k v
hashMapBy f vs = HashMap.fromList [ (f v, v) | v <- vs]

hashMap :: Hashable a => [(a, b)] -> HashMap a b
hashMap = HashMap.fromList

hashSet :: Hashable a => [a] -> HashSet a
hashSet = HashSet.fromList

intMap :: [(Int, a)] -> IntMap a
intMap = IntMap.fromList

error :: (HasCallStack) => Text -> a
error = Err.error . unpack

-- | Used to indicate impossible corner cases.
impossible :: (HasCallStack) => a
impossible = Err.error "impossible"

nonEmpty' :: [a] -> NonEmpty a
nonEmpty' = fromJust . nonEmpty

putStrLn :: (MonadIO m) => Text -> m ()
putStrLn = liftIO . Text.putStrLn

print :: (MonadIO m, Show a) => a -> m ()
print = putStrLn . pack . Show.show

show :: Show a => a -> Text
show = pack . Show.show

runParserT :: Monad m => ParsecT m a -> Text -> m a
runParserT p txt = do
  r <- P.runParserT p "<txt>" txt
  case r of
    Left _ -> error "parser error"
    Right a -> return a

parse :: Parsec Void Text a -> Text -> a
parse p txt = case P.runParser p "<txt>" txt of
  Left _ -> error "parser error"
  Right a -> a

parseOnly :: Parsec Void Text a -> Text -> a
parseOnly p = parse (p <* eof)

type Parser a = forall e m. MonadParsec e Text m => m a
type ParsecT = P.ParsecT Void Text

space :: MonadParsec e Text m => m ()
space = P.space Char.space1 empty empty

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = P.lexeme space

natural :: Parser Natural
natural = lexeme P.decimal

readNatural :: Text -> Natural
readNatural = parseOnly natural

symbol :: MonadParsec e Text m => Text -> m ()
symbol = void . P.symbol space

trace :: Text -> a -> a
trace msg = T.trace (unpack msg)
{-# WARNING trace "Using trace" #-}

traceShowId :: (Show a) => a -> a
traceShowId a = T.trace (Show.show a) a
{-# WARNING traceShowId "Using traceShowId" #-}

traceM :: (Applicative f) => Text -> f ()
traceM msg = T.trace (unpack msg) (pure ())
{-# WARNING traceM "Using traceM" #-}

execOutputList :: Sem (Output o ': r) a -> Sem r [o]
execOutputList = fmap fst . runOutputList

execOutputSemigroup :: Semigroup o => o -> Sem (Output o ': r) a -> Sem r o
execOutputSemigroup o m = do
  l <- execOutputList m
  return (sconcat (o :| l))

execOutputSemigroup' :: Semigroup o => Sem (Output o ': r) a -> Sem r o
execOutputSemigroup' = fmap (sconcat . nonEmpty') . execOutputList

modifyM :: Members '[State s] r => (s -> Sem r s) -> Sem r ()
modifyM m = get >>= m >>= put

replicate :: forall a. Natural -> a -> [a]
replicate = \case
  0 -> const []
  m -> \a -> a : replicate (pred m) a

runTextErrorIO :: Members '[Embed IO] r => Sem (Error Text ': r) a -> Sem r a
runTextErrorIO m = do
  res <- runError m
  case res of
    Left msg -> embed $ do
      putStrLn msg
      exitFailure
    Right r -> return r
