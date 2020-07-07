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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Vim.Expression where

import qualified Prelude as P

import Prelude hiding (Ord(..), Eq(..), True, False)
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
    LInt    :: Int                      -> Expr Int
    LStr    :: String                   -> Expr String
    True    ::                             Expr Bool
    False   ::                             Expr Bool
    LList   :: [Expr a]                 -> Expr [Expr a]
    Var     :: Scope -> String          -> Expr a
    App     :: String -> [Arg]          -> Expr a
    Lam     :: (Expr a -> Expr b)       -> Expr (a -> b)
    App'    :: Expr (a -> b) -> Expr a  -> Expr b

instance Show (Expr a) where
    show = \case
        LInt v -> "LInt " <> show v
        LStr v -> "LStr " <> show v
        True   -> "True"
        False  -> "False"
        LList exprs -> "LList (" <> show exprs <> ")"
        Var scope name -> unwords ["Var", show scope , show name]
        App fun args -> unwords ["App", show fun, show args]
        Lam _ -> "Lam <function>"
        App' fun val ->  "App' (" <> show fun <> ") (" <> show val <> ")"

-- | Scope of a variable
data Scope
    = Argument
    | Window
    | Buffer
    | Tab
    | VimSpecial
    | Lambda
    deriving  (Show)

-- | A function argument of any type
data Arg where
    A :: Expr a -> Arg

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
    ( WriterT [Statement] -- Our vim statements up to now
    ( ReaderT Depth       -- How many functions we are
    ( StateT  FunState    -- Modifiable function-specific data
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
    false = False
    true  = True
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

-------------------------------------------------------------------------------
-- Function definition

lam :: (Expr a -> Expr b) -> Expr (a -> b)
lam = Lam

app :: Expr (a -> b) -> Expr a -> Expr b
app = App'

-- | Make type inference work
type Fun function params return =
    ( function ~ Function params return
    , params   ~ Params function
    , return   ~ Return function
    )

type family Function params return where
    Function ()        return = return
    Function (x -> xs) return = x -> Function xs return

-- | Capture only the function parameters, replacing the return type for unit.
type family Params f where
    Params (a -> b) = a -> Params b
    Params _        = ()

type family Return f where
    Return (a -> b) = Return b
    Return a        = a

-- | A function that returns actions in the Vim monad
class MonadVim m => VimFunction m f where
    saturate :: [String] -> f -> m ([String], Return f)

instance (MonadVim m, VimFunction m g) => VimFunction m (Expr a -> g) where
    saturate args f = do
        vname <- makeVarName Argument
        let g = f $ Var Argument vname
        saturate (vname:args) g

instance MonadVim m => VimFunction m (Vim a) where
    saturate args val = return (reverse args, val)

-- | A Function that can be called using the Expr type.
-- Creates a Haskell function that once saturated will produce
-- an invokation of the equivalent Vim function.
class VimFunctionExpression a where
    produce :: String -> [Arg] -> a

instance VimFunctionExpression p => VimFunctionExpression (Expr a -> p) where
    produce fname args = \a -> produce fname (A a : args)

instance VimFunctionExpression (Expr a) where
    produce fname args = App fname $ reverse args

-- | Define a vim function
-- Accepts functions of any arity > 1
--
--          add <- define $ \a b -> a + b
--          echo $ add 1 2
--
define ::
    ( VimFunction m f           -- f is a sequence of statements
    , Fun f xs (m (Expr a))     -- f returns an (m (Expr a))
    , Fun g ys (Expr a)         -- g returns an expression of same time as in f
    , Params f ~ Params g       -- f and g require the same parameters
    , VimFunctionExpression g   -- We can express g as an expression
    ) => f -> m g
define f = do
    -- Create all the arguments to the function as variables and apply them.
    -- We get back the body of code that was generated and the name of the
    -- variables that were used as arguments.
    (args, body) <- saturate [] f
    fname <- makeFunName
    depth <- getDepth
    let handleBody res statements =
            pure
                $ DefineFunc fname args
                $ statements ++ [Return $ A res] -- Add return statement
    _ <- eval' (succ depth, FunState 0 0) handleBody body
    return $ produce fname []

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
            , ["endfunction"] -- Two lines after function definition
            ]

    Return arg-> do
        res <- genE d arg
        return [ "return " ++ res ]

    Call fun args -> do
        args' <- traverse (genE d) args
        return $ [ unwords $ fun:args' ]

genE :: MonadError Err m => Depth -> Arg -> m String
genE d (A e) = case e of
    LInt v      -> return $ show v
    LStr v      -> return $ show v
    True        -> return "1"
    False       -> return "0"
    LList exs   -> do
        xs <- traverse (genE d . A) exs
        return $ "[ " <> intercalate "," xs <> " ]"
    Var scope vname ->
        let scopeLetter = case scope of
                Argument   -> Just "a"
                Window     -> Just "w"
                Buffer     -> Just "b"
                Tab        -> Just "t"
                VimSpecial -> Just "v"
                Lambda     -> Nothing
        in
        return $ maybe  vname (<> (":" <> vname)) $ scopeLetter
    App "+"  [_,_] -> binInfix e
    App "-"  [_,_] -> binInfix e
    App "*"  [_,_] -> binInfix e
    App "&&" [_,_] -> binInfix e
    App "||" [_,_] -> binInfix e
    App "not" [a]  -> genE d $ A $ App "ternary" [a, A False, A True]
    App "<"  [_,_] -> binInfix e
    App "<=" [_,_] -> binInfix e
    App ">"  [_,_] -> binInfix e
    App ">=" [_,_] -> binInfix e
    App "==#" [_,_] -> binInfix e
    App "ternary" [c, a, b] -> do
        c' <- genE d c
        a' <- genE d a
        b' <- genE d b
        return $ parens $ unwords [c', "?", a', ":", b']
    App fun args -> do
        args' <- traverse (genE d) args
        return $ fun <> parenthesize args'
    Lam fun -> do
        let Depth depth = d
            vname = "lambdavar" <> show depth
        body <- genE (Depth $ depth + 1) $ A $ fun $ Var Lambda vname
        return $ unwords ["{", vname, "->", body, "}"]

    App' exfun exval -> do
        fun <- genE d $ A $ exfun
        var <- genE d $ A $ exval
        return $ parenthesize [fun] <> parenthesize [var]
    where
        binInfix (App fun [a, b]) = do
            a' <- genE d a
            b' <- genE d b
            return $ unwords [a', fun, b']
        binInfix (App fun args  ) = throwError $ CodeGenError $ NumArgs fun 2 $ fromIntegral $ length args
        binInfix ex = throwError $ CodeGenError $ TypeMismatch "function application" $ A ex


indent :: String -> String
indent = ("    " <>)

parenthesize :: [String] -> String
parenthesize = parens . intercalate ","

parens :: String -> String
parens s = "(" <> s <> ")"

