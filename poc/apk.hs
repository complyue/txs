
{-# LANGUAGE 
  ViewPatterns,
  KindSignatures,
  TypeOperators, 
  DataKinds, 
  FlexibleInstances, 
  FlexibleContexts, 
  PatternSynonyms, 
  ConstraintKinds, 
  ScopedTypeVariables, 

  BangPatterns
#-}

module Main where

import           Prelude

import           GHC.TypeLits                   ( Symbol
                                                , KnownSymbol
                                                , symbolVal
                                                )
import           Data.Kind                      ( Type )
import           Data.Maybe
import           Data.Proxy
import           Data.Dynamic


-- artifacts for named arguments

newtype NamedArg (t :: Type) (name :: Symbol) = NamedArg t
type name !: t = NamedArg t name
type name ?: t = NamedArg (Maybe t) name

pattern Arg :: t -> name !: t
pattern Arg t = NamedArg t
{-# COMPLETE Arg #-}

arg ::  name !: t -> t 
arg (NamedArg a) = a

maybeArg :: name ?: t -> Maybe t
maybeArg (NamedArg !ma) = ma

argDef :: t -> name ?: t -> t
argDef !a (NamedArg !ma) = fromMaybe a ma


-- * minimum data structures as interface with scripting code

type AttrKey = String
data AttrVal = NilValue
  | IntValue !Integer
  | StrValue !String
  deriving (Eq, Ord, Typeable)
instance Show AttrVal where
  show NilValue      = "nil"
  show (IntValue !x) = show x
  show (StrValue !x) = show x


data ArgsPack = ArgsPack {
    positional'args :: [AttrVal]
  , keyword'args    :: [(AttrKey, AttrVal)]
  }
instance Semigroup ArgsPack where
  (ArgsPack p1 kw1) <> (ArgsPack p2 kw2) = ArgsPack (p1 ++ p2) (kw1 ++ kw2)
instance Monoid ArgsPack where
  mempty = ArgsPack [] []

takeKwArg
  :: AttrKey -> [(AttrKey, AttrVal)] -> (Maybe AttrVal, [(AttrKey, AttrVal)])
takeKwArg !k !kwargs = go [] kwargs
 where
  go
    :: [(AttrKey, AttrVal)]
    -> [(AttrKey, AttrVal)]
    -> (Maybe AttrVal, [(AttrKey, AttrVal)])
  go _      []                         = (Nothing, kwargs)
  go others (p@(!key, !val) : kwargs') = if key == k
    then (Just val, reverse others ++ kwargs')
    else go (p : others) kwargs'


type ContProc = (AttrVal -> IO ()) -> IO ()

-- | Haskell functions applicable with an apk
class Callable fn where
  call :: fn -> ArgsPack -> ContProc

-- instance for nullary functions, which is the base case
instance Callable ContProc where
  call !fn (ArgsPack !args !kwargs) exit =
    if null args && null kwargs then fn exit else error "extraneous args"

-- instance for repacking arg receiver
instance Callable fn' => Callable (ArgsPack -> fn') where
  call !fn !apk !exit = call (fn apk) (ArgsPack [] []) exit

-- instances for positional cases, i.e. non-last positional arg receivers

instance Callable fn' => Callable (AttrVal -> fn') where
  call !fn (ArgsPack (val : args) !kwargs) !exit =
    call (fn val) (ArgsPack args kwargs) exit
  call _ _ _ = error "missing positional args"

instance Callable fn' => Callable (Maybe AttrVal -> fn') where
  call !fn (ArgsPack [] !kwargs) !exit =
    call (fn Nothing) (ArgsPack [] kwargs) exit
  call !fn (ArgsPack (val : args) !kwargs) !exit =
    call (fn (Just val)) (ArgsPack args kwargs) exit

instance Callable fn' => Callable (String -> fn') where
  call !fn (ArgsPack (val : args) !kwargs) !exit = case val of
    StrValue !val' -> call (fn val') (ArgsPack args kwargs) exit
    _              -> error "arg type mismatch"
  call _ _ _ = error "missing positional args"

instance Callable fn' => Callable (Maybe String -> fn') where
  call !fn (ArgsPack [] !kwargs) !exit =
    call (fn Nothing) (ArgsPack [] kwargs) exit
  call !fn (ArgsPack (val : args) !kwargs) !exit = case val of
    StrValue !val' -> call (fn (Just val')) (ArgsPack args kwargs) exit
    _              -> error "arg type mismatch"

-- todo instances for receivers of positional arg of (Maybe) Integer
-- type, and other types covered by AttrVal

-- instances for keyword args

instance (KnownSymbol name, Callable fn') => Callable (NamedArg AttrVal name -> fn') where
  call !fn (ArgsPack !args !kwargs) !exit = case takeKwArg argName kwargs of
    (Just val, kwargs') ->
      call (fn (NamedArg val)) (ArgsPack args kwargs') exit
    (Nothing, _) -> error $ "missing kwarg: " <> argName
    where !argName = symbolVal (Proxy :: Proxy name)

instance (KnownSymbol name, Callable fn') => Callable (NamedArg (Maybe AttrVal) name -> fn') where
  call !fn (ArgsPack !args !kwargs) !exit = case takeKwArg argName kwargs of
    (maybeVal, kwargs') ->
      call (fn (NamedArg maybeVal)) (ArgsPack args kwargs') exit
    where !argName = symbolVal (Proxy :: Proxy name)

instance (KnownSymbol name, Callable fn') => Callable (NamedArg String name -> fn') where
  call !fn (ArgsPack !args !kwargs) !exit = case takeKwArg argName kwargs of
    (Just val, kwargs') -> case val of
      StrValue !val' -> call (fn (NamedArg val')) (ArgsPack args kwargs') exit
      _              -> error "arg type mismatch"
    (Nothing, _) -> error $ "missing kwarg: " <> argName
    where !argName = symbolVal (Proxy :: Proxy name)

instance (KnownSymbol name, Callable fn') => Callable (NamedArg (Maybe String) name -> fn') where
  call !fn (ArgsPack !args !kwargs) !exit = case takeKwArg argName kwargs of
    (Nothing, kwargs') ->
      call (fn (NamedArg Nothing)) (ArgsPack args kwargs') exit
    (Just !val, kwargs') -> case val of
      StrValue !val' ->
        call (fn (NamedArg (Just val'))) (ArgsPack args kwargs') exit
      _ -> error "arg type mismatch"
    where !argName = symbolVal (Proxy :: Proxy name)

-- todo instances for receivers of keyword arg of (Maybe) Integer
-- type, and other types covered by AttrVal


-- * functions to be callable from scripting code

-- | interfacing Haskell function meant to be easily called by scripting code
assert
  :: "expect" !: AttrVal
  -> "target" ?: AttrVal
  -> "message" ?: String
  -> (AttrVal -> IO ())
  -> IO ()
assert (Arg !expect) (maybeArg -> !maybeTarget) (argDef "sth ought to be" -> !message) !exit
  = case maybeTarget of
    Nothing -> case expect of
      NilValue    -> error $ "* assertion failed: " <> message
      IntValue 0  -> error $ "* assertion failed: " <> message
      StrValue "" -> error $ "* assertion failed: " <> message
      _           -> exit $ StrValue $ "* assertion passed: " <> message
    Just target -> if expect == target
      then exit $ StrValue $ "* assertion passed: " <> message
      else error $ "* assertion failed: " <> message


-- mockup & test out
main :: IO ()
main = call assert apk $ \ !result -> putStrLn $ "Got result: " <> show result

 where

  !apk = ArgsPack
    []
    [ ("message", StrValue "as good will")
    , ("target" , IntValue 333)
    , ("expect" , IntValue 333)
    ]
