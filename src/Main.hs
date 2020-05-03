{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}

module Main (main) where

import Control.Applicative (Alternative)
import Data.Foldable (asum, for_, toList, sequenceA_, traverse_)
import Domain.Interval (Interval, Z, (...), concrete, nez, zeq, zne)
import FD.Monad (run)
import Ref (MonadRef)
import Signal (MonadSignal)
import System.Exit (exitFailure)


main :: IO ()
main =
  case solutions of
    [] -> do
      putStrLn "No solutions found"
      exitFailure
    ss@(_:_:_) -> do
      putStrLn "Too many solutions found:"
      print ss
      exitFailure
    s:[] -> do
      putStrLn "Solution found:"
      print s


data Combo a
  = Combo{
    _1 :: a
  , _2 :: a
  , _3 :: a
  } deriving (
    Eq, Ord, Functor, Foldable, Traversable
  )

instance Applicative Combo where
  -- zippy
  Combo f g h <*> Combo a b c = Combo (f a) (g b) (h c)
  pure a = Combo a a a

instance Show a => Show (Combo a) where
  showsPrec p = showsPrec p . toList

options :: MonadSignal e m => m (Interval m)
options = do
  option <- 0...8
  option `nez` 5
  pure option

solutions :: [Combo Z]
solutions = run $ do
  -- digits
  a <- options
  b <- options
  c <- options

  -- solution variable
  let answer = Combo a b c

  -- constraints
  nothingCorrect             (Combo 7 3 8) answer
  oneCorrectAndWellPlaced    (Combo 6 8 2) answer
  oneCorrectButWronglyPlaced (Combo 6 1 4) answer
  oneCorrectButWronglyPlaced (Combo 7 8 0) answer
  twoCorrectButWronglyPlaced (Combo 2 0 6) answer

  traverse concrete answer

nothingCorrect :: MonadRef m => Combo Z -> Combo (Interval m) -> m ()
nothingCorrect wrongs answer =
  for_ wrongs $ \wrong ->
    traverse_ (zne wrong) answer

oneCorrectAndWellPlaced :: MonadRef m => Combo Z -> Combo (Interval m) -> m ()
oneCorrectAndWellPlaced (Combo a b c) answer =
  oneOf
  [ -- a is correct and well placed
    constrain (zeq a) (zne b) (zne c) answer
  , -- b is correct and well placed
    constrain (zne a) (zeq b) (zne c) answer
  , -- c is correct and well placed
    constrain (zne a) (zne b) (zeq c) answer
  ]

oneCorrectButWronglyPlaced :: MonadRef m => Combo Z -> Combo (Interval m) -> m ()
oneCorrectButWronglyPlaced (Combo a b c) answer =
  oneOf
  [
    oneOf -- a is correct but wrongly placed
    [ constrain (zne a) (zeq a) (zne a) answer
    , constrain (zne a) (zne a) (zeq a) answer
    ]
  , oneOf -- b is correct but wrongly placed
    [ constrain (zeq b) (zne b) (zne b) answer
    , constrain (zne b) (zne b) (zeq b) answer
    ]
  , oneOf -- c is correct but wrongly placed
    [ constrain (zeq c) (zne c) (zne c) answer
    , constrain (zne c) (zeq c) (zne c) answer
    ]
  ]

twoCorrectButWronglyPlaced :: MonadRef m => Combo Z -> Combo (Interval m) -> m ()
twoCorrectButWronglyPlaced (Combo a b c) answer =
  oneOf
  [ -- a b are correct but wrongly placed
    none c answer *>
    oneOf
    [ constrain (neither a b) (zeq a) (zeq b) answer
    , constrain (zeq b) (zeq a) (neither a b) answer
    , constrain (zeq b) (neither a b) (zeq a) answer
    ]
  , -- a c are correct but wrongly placed
    none b answer *>
    oneOf
    [ constrain (neither a c) (zeq c) (zeq a) answer
    , constrain (zeq c) (zeq a) (neither a c) answer
    , constrain (zeq c) (neither a c) (zeq a) answer
    ]
  , -- b c are correct but wrongly placed
    none a answer *>
    oneOf
    [ constrain (zeq b) (zeq c) (neither b c) answer
    , constrain (zeq c) (neither b c) (zeq b) answer
    , constrain (neither b c) (zeq c) (zeq b) answer
    ]
  ]

constrain
  :: MonadRef m
  => (Interval m -> m a) -- a
  -> (Interval m -> m a) -- b
  -> (Interval m -> m a) -- c
  -> Combo (Interval m)
  -> m ()
constrain a b c answer =
  sequenceA_ (Combo a b c <*> answer)

none :: MonadRef m => Z -> Combo (Interval m) -> m ()
none z answer =
  for_ answer $ zne z

neither :: MonadRef m => Z -> Z -> Interval m -> m ()
neither z x digit = do
  digit `nez` z
  digit `nez` x

oneOf :: (Alternative m, Foldable f) => f (m a) -> m a
oneOf = asum
