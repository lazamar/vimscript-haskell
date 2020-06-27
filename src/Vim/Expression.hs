{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vim.Expression where

import Control.Monad.Writer.Lazy (WriterT, tell, MonadWriter, runWriterT)
import Control.Monad.Reader (ReaderT, MonadReader, local, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.State.Lazy (MonadState, StateT, runStateT, withStateT, evalStateT)
import Data.Traversable (for)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity, runIdentity)

-- | A Vimscript expression
data Expr a
    = Const a
    | Var Scope String
    | App (Func a) [Argument]

-- | Scope of a variable
data Scope
    = Argument
    | Window
    | Buffer
    | Tab
    | VimSpecial

-- | A function reference
data Func a = Func String

-- | A function argument of any type
data Argument where
    Arg :: forall a. Expr a -> Argument

data Statement
    = DefineFunc String [String] [Statement]
    | DefineVar String Argument
    | Call String [Argument]
    | Return Argument

-- Evaluation

data FunState = FunState
    { varCount :: Int -- ^ Variables declared
    , funCount :: Int -- ^ Functions declared
    }

newtype Depth = Depth Int
    deriving (Show)
    deriving newtype (Eq, Ord, Num, Enum)

newtype Vim a = Vim
    ( WriterT [Statement] -- ^ Our vim statements up to now
    ( ReaderT Depth       -- ^ How many functions we are
    ( StateT  FunState    -- ^ Modifiable function-specific data
    ( Identity
    ))) a )
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadWriter [Statement]
        , MonadReader Depth
        , MonadState FunState
        )

class Monad m => MonadVim m where
    statement   :: Statement -> m ()
    makeVarName :: Scope -> m String
    makeFunName :: m String
    getDepth    :: m Depth
    eval        :: Depth -> m a -> (a, [Statement])

-- instance MonadVim Vim where
--     statement  = tell . pure
--     getEnv  = ask
--     eval depth
--         = runIdentity
--         . flip evalStateT emptyFunState
--         . flip runReaderT depth
--         . runWriterT
--         where
--             emptyFunState = FunState 0 0


-- | Infinite list of unique argument names
-- argNames :: MonadVim m => m [String]
-- argNames = do
--     Env{depth} <- getEnv
--     let d = show depth
--     for [1..] $ \n -> return $ "arg_" <> d <> "_" <> show n

define :: forall m a b. MonadVim m => (Expr a -> m (Expr b)) -> m (Expr a -> Expr b)
define f = do
    argName <- makeVarName Argument
    fname   <- makeFunName
    depth   <- getDepth
    let body = f (Var Argument argName)
        (result, statements) = eval (succ depth) body

    statement $ DefineFunc fname [argName] $ statements ++ [Return $ Arg result]
    return $ \a -> App (Func fname) [Arg a]
