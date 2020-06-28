{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Vim.Expression where

import Control.Monad.Writer.Lazy (WriterT, tell, MonadWriter, runWriterT)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, state)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (intercalate)

-- | A Vimscript expression
data Expr a
    = LInt Int
    | LStr String
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
    ( WriterT [Statement] --  ^ Our vim statements up to now
    ( ReaderT Depth       --  ^ How many functions we are
    ( StateT  FunState    --  ^ Modifiable function-specific data
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

instance MonadVim Vim where
    statement = tell . pure
    makeVarName _ = do
        Depth d <- getDepth
        state $ \s ->
            let count = varCount s
                vname = concat ["var_", show d, "_", show count]
            in
            (vname , s { varCount = succ count })
    makeFunName = do
        Depth d <- getDepth
        state $ \s ->
            let count = funCount s
                vname = concat ["Fun_", show d, "_", show count]
            in
            (vname , s { funCount = succ count })
    getDepth = ask
    eval depth (Vim vim)
        = runIdentity
        . flip evalStateT (FunState 0 0)
        . flip runReaderT depth
        . runWriterT
        $ vim


class Monad m => MonadVim m where
    statement   :: Statement -> m ()
    makeVarName :: Scope -> m String
    makeFunName :: m String
    getDepth    :: m Depth
    eval        :: Depth -> m a -> (a, [Statement])

-- Combinators

str :: String -> Expr String
str = LStr

int :: Int -> Expr Int
int = LInt

define1 :: MonadVim m
    => (Expr a -> m (Expr b))
    -> m (Expr a -> Expr b)
define1 f = do
    argName <- makeVarName Argument
    fname   <- makeFunName
    depth   <- getDepth
    let body = f (Var Argument argName)
        (result, statements) = eval (succ depth) body

    statement $ DefineFunc fname [argName] $ statements ++ [Return $ Arg result]
    return $ \a -> App (Func fname) [Arg a]

define2 :: MonadVim m
    => (Expr a -> Expr b -> m (Expr c))
    -> m (Expr a -> Expr b -> Expr c)
define2 f = do
    argName1 <- makeVarName Argument
    argName2 <- makeVarName Argument
    fname <- makeFunName
    depth <- getDepth
    let body = f (Var Argument argName1)
                 (Var Argument argName2)
        (result, statements) = eval (succ depth) body
    statement $ DefineFunc fname [argName1, argName2] $ statements ++ [Return $ Arg result]
    return $ \a b -> App (Func fname) [Arg a, Arg b]

define3 :: MonadVim m
    => (Expr a -> Expr b -> Expr c -> m (Expr d))
    -> m (Expr a -> Expr b -> Expr c -> Expr d)
define3 f = do
    argName1 <- makeVarName Argument
    argName2 <- makeVarName Argument
    argName3 <- makeVarName Argument
    fname <- makeFunName
    depth <- getDepth
    let body = f (Var Argument argName1)
                 (Var Argument argName2)
                 (Var Argument argName3)
        (result, statements) = eval (succ depth) body
    statement
        $ DefineFunc fname [argName1, argName2, argName3]
        $ statements ++ [Return $ Arg result]
    return $ \a b c -> App (Func fname) [Arg a, Arg b, Arg c]

-- Stdlib

-- | Print something
echo :: MonadVim m => Expr String -> m ()
echo = statement . Call "echo" . pure . Arg

-- | Call a function only for its effects.
call :: MonadVim m => Expr a -> m ()
call e = case e of
    App _ _ -> statement $ Call "call" [Arg e]
    _ -> error "Executing a `call` statement on something that is not a function"

-- Code Generation

gen :: Depth -> Statement -> [String]
gen d = \case
    DefineFunc name args body ->
        concat
            [ ["function! " <> name <> "(" <> intercalate ", " args <> ")"]
            , map indent . concatMap (gen (succ d)) $ body
            , ["endfunction", "",""] -- Two lines after function definition
            ]
    Return arg->
        pure $ "return " <> genE arg

    Call fun args ->
        pure $ unwords $ fun : map genE args

genE :: Argument -> String
genE (Arg e) = case e of
    LInt v -> show v
    LStr v -> "\"" <> v <> "\""
    Var scope vname ->
        let scopeLetter = case scope of
                Argument   -> "a"
                Window     -> "w"
                Buffer     -> "b"
                Tab        -> "t"
                VimSpecial -> "v"
        in
        scopeLetter <> ":" <> vname
    App (Func "+") [a, b] -> unwords [genE a, "+", genE b]
    App (Func "-") [a, b] -> unwords [genE a, "+", genE b]
    App (Func fun) args -> fun <> parenthesize (map genE args)

indent :: String -> String
indent = ("    " <>)

parenthesize :: [String] -> String
parenthesize vals = "(" <> intercalate "," vals <> ")"





