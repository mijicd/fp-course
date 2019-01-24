{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

newtype Compose f g a = Compose (f (g a)) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) f (Compose fga) = Compose ((f <$>) <$> fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure

  (<*>) (Compose fgab) (Compose fga) = Compose $ lift2 (<*>) fgab fga

-- monad can't be implemented unless given concrete f and g
