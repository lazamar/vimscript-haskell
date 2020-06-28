{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Vim.Expression where

import qualified Prelude as P

import Prelude hiding (Ord(..), Eq(..))
import Control.Monad.Writer.Lazy (WriterT, tell, MonadWriter, runWriterT, pass)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT, local)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, state, get, put)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, runExcept)
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (intercalate)
import Data.String (IsString(..))
import Numeric.Natural (Natural)

-- | A Vimscript expression
data Expr a where
    LInt    :: Int              -> Expr Int
    LStr    :: String           -> Expr String
    LBool   :: Bool             -> Expr Bool
    LList   :: [Expr a]         -> Expr [Expr a]
    Var     :: Scope -> String  -> Expr a
    App     :: String -> [Arg]  -> Expr a

instance Show (Expr a) where
    show = \case
        LInt v -> "LInt " <> show v
        LStr v -> "LStr " <> show v
        LBool v -> "LBool " <> show v
        LList exprs -> "LList (" <> show exprs <> ")"
        Var scope name -> unwords ["Var", show scope , show name]
        App fun args -> unwords ["App", show fun, show args]

-- | Scope of a variable
data Scope
    = Argument
    | Window
    | Buffer
    | Tab
    | VimSpecial
    deriving  (Show)

-- | A function argument of any type
data Arg where
    A :: forall a. Expr a -> Arg

instance Show Arg where
    show (A expr) = "A (" <> show expr <> ")"

data Statement
    = DefineFunc String [String] [Statement]
    | Call String [Arg]
    | Return Arg
    deriving (Show)

--------------------------------------------------------------------------------
-- Evaluation

data Err
    = EvaluationError EvaluationError
    | CodeGenError CodeGenError
    | Unknown String
    deriving (Show)


data EvaluationError
    = NotFunction String Arg
    deriving (Show)

data FunState = FunState
    { varCount :: Int -- ^ Variables declared
    , funCount :: Int -- ^ Functions declared
    }

newtype Depth = Depth Int
    deriving (Show)
    deriving newtype (P.Eq, P.Ord, Num, Enum)

newtype Vim a = Vim
    ( ExceptT Err
    ( WriterT [Statement] --  ^ Our vim statements up to now
    ( ReaderT Depth       --  ^ How many functions we are
    ( StateT  FunState    --  ^ Modifiable function-specific data
    ( Identity
    )))) a )
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadError Err
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
    eval' (depth, fstate) f
        = localState fstate
        . local (const depth)
        . pass
        . fmap (\res -> (res, f res))
        where
            -- | Run action with modified state and restore initial state at the end.
            localState loc action = do
                s <- get
                put loc
                res <- action
                put s
                return res

class MonadError Err m => MonadVim m where
    statement   :: Statement -> m ()
    makeVarName :: Scope -> m String
    makeFunName :: m String
    getDepth    :: m Depth
    -- | Transform the definition of a Vimscript program
    eval'       :: (Depth, FunState) -> (a -> [Statement] -> [Statement]) -> m a -> m a

--------------------------------------------------------------------------------
-- Combinators

unaryOp :: String -> Expr a -> Expr b
unaryOp  name arg = App name [A arg]

binOp :: String -> Expr a -> Expr b -> Expr c
binOp name arg1 arg2 = App name [A arg1, A arg2]

class Boolean a where
    false :: a
    true  :: a
    (&&)  :: a -> a -> a
    (||)  :: a -> a -> a
    not   :: a -> a

instance Boolean (Expr Bool) where
    false = LBool False
    true  = LBool True
    (&&)  = binOp "&&"
    (||)  = binOp "||"
    not   = unaryOp "not"

class (Boolean a) => Cond a b | b -> a where
    cond :: a -> b -> b -> b

instance Cond (Expr Bool) (Expr a) where
    cond c a b = App "ternary" [A c, A a, A b]

class (Boolean b) => Eq a b | a -> b where
     (==) :: a -> a -> b

instance Eq (Expr a) (Expr Bool) where
    a == b = App "==#" [A a, A b]

-- TODO: Add LT and GT as optionally included module variables
class (Boolean b) => Ord a b | a -> b where
    (< ) :: a -> a -> b
    (<=) :: a -> a -> b
    (> ) :: a -> a -> b
    (>=) :: a -> a -> b
    max  :: a -> a -> a
    min  :: a -> a -> a

instance Num a => Ord (Expr a) (Expr Bool) where
    (< )    = binOp "<"
    (<=)    = binOp "<="
    (> )    = binOp ">"
    (>=)    = binOp ">="
    max a b = unaryOp "max" $ LList [a,b]
    min a b = unaryOp "min" $ LList [a,b]

instance Num (Expr Int) where
    x + y         = App "+" [A x, A y]
    x - y         = App "-" [A x, A y]
    x * y         = App "*" [A x, A y]
    negate x      = App "-1 *" [A x] -- TODO: Fix this hack
    abs    x      = App "abs"    [A x]
    signum x      = cond (x == 0) 0 (cond (x > 0) 1 (-1))
    fromInteger x = LInt (P.fromInteger x)

instance IsString (Expr String) where
    fromString = LStr

str :: String -> Expr String
str = LStr

int :: Int -> Expr Int
int = LInt

-- define1 :: MonadVim m
--     => (Expr a -> m (Expr b))
--     -> m (Expr a -> Expr b)
-- define1 f = do
--     argName <- makeVarName Argument
--     fname   <- makeFunName
--     depth   <- getDepth
--     let body = f (Var Argument argName)
--         (eResult, statements) = eval (succ depth) body
--     res <- either throwError return eResult
--     statement $ DefineFunc fname [argName] $ statements ++ [Return $ A res]
--     return $ \a -> App fname [A a]
--
-- define2 :: MonadVim m
--     => (Expr a -> Expr b -> m (Expr c))
--     -> m (Expr a -> Expr b -> Expr c)
-- define2 f = do
--     argName1 <- makeVarName Argument
--     argName2 <- makeVarName Argument
--     fname <- makeFunName
--     depth <- getDepth
--     let body = f (Var Argument argName1)
--                  (Var Argument argName2)
--         (eResult, statements) = eval (succ depth) body
--     res <- either throwError return eResult
--     statement $ DefineFunc fname [argName1, argName2] $ statements ++ [Return $ A res]
--     return $ \a b -> App fname [A a, A b]
--
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

        handleBody res statements =
            pure
                $ DefineFunc fname [argName1, argName2, argName3]
                $ statements ++ [Return $ A res] -- Add return statement

    _ <- eval' (succ depth, FunState 0 0) handleBody body
    return $ \a b c -> App fname [A a, A b, A c]

--------------------------------------------------------------------------------
-- Stdlib

-- | Print something
echo :: MonadVim m => Expr String -> m ()
echo = statement . Call "echo" . pure . A

-- | Call a function only for its effects.
call :: MonadVim m => Expr a -> m ()
call e = case e of
    App _ _ -> statement $ Call "call" [A e]
    _       -> throwError $ EvaluationError $ NotFunction "call" $ A e

--------------------------------------------------------------------------------
-- Code Generation

data CodeGenError
    = NumArgs
        String  -- ^ Function name
        Natural -- ^ Expected args
        Natural -- ^ Given args
    | TypeMismatch
        String  -- ^ Expected
        Arg     -- ^ Given
    deriving (Show)

newtype Code = Code { unCode :: String }

generateCode :: Vim () -> Either Err Code
generateCode (Vim vim)
    = fmap (Code . unlines . concat)
    . runExcept
    . traverse (gen 0)
    . snd
    . runIdentity
    . flip evalStateT (FunState 0 0)
    . flip runReaderT (Depth 0)
    . runWriterT
    . runExceptT
    $ vim

gen :: MonadError Err m => Depth -> Statement -> m [String]
gen d = \case
    DefineFunc name args body -> do
        statements <- traverse (gen $ succ d) body
        return $ concat
            [ ["function! " <> name <> "(" <> intercalate ", " args <> ")"]
            , map indent $ concat statements
            , ["endfunction", "",""] -- Two lines after function definition
            ]

    Return arg-> do
        res <- genE arg
        return [ "return " ++ res ]

    Call fun args -> do
        args' <- traverse genE args
        return $ [ unwords $ fun:args' ]

genE :: MonadError Err m => Arg -> m String
genE (A e) = case e of
    LInt v      -> return $ show v
    LStr v      -> return $ "\"" <> v <> "\""
    LBool v     -> return $ if v then "1" else "0"
    LList exs   -> do
        xs <- traverse (genE . A) exs
        return $ "[ " <> intercalate "," xs <> " ]"
    Var scope vname ->
        let scopeLetter = case scope of
                Argument   -> "a"
                Window     -> "w"
                Buffer     -> "b"
                Tab        -> "t"
                VimSpecial -> "v"
        in
        return $ scopeLetter <> ":" <> vname
    App "+"  [_,_] -> binInfix e
    App "-"  [_,_] -> binInfix e
    App "*"  [_,_] -> binInfix e
    App "&&" [_,_] -> binInfix e
    App "||" [_,_] -> binInfix e
    App "not" [a]  -> genE $ A $ App "ternary" [a, A $ LBool False, A $ LBool True]
    App "<"  [_,_] -> binInfix e
    App "<=" [_,_] -> binInfix e
    App ">"  [_,_] -> binInfix e
    App ">=" [_,_] -> binInfix e
    App "ternary" [c, a, b] -> do
        c' <- genE c
        a' <- genE a
        b' <- genE b
        return $ parens $ unwords [c', "?", a', ":", b']
    App fun args -> do
        args' <- traverse genE args
        return $ fun <> parenthesize args'
    where
        binInfix (App fun [a, b]) = do
            a' <- genE a
            b' <- genE b
            return $ unwords [a', fun, b']
        binInfix (App fun args  ) = throwError $ CodeGenError $ NumArgs fun 2 $ fromIntegral $ length args
        binInfix ex = throwError $ CodeGenError $ TypeMismatch "function application" $ A ex


indent :: String -> String
indent = ("    " <>)

parenthesize :: [String] -> String
parenthesize = parens . intercalate ","

parens :: String -> String
parens s = "(" <> s <> ")"

