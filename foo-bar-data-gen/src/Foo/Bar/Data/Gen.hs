{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Foo.Bar.Data.Gen
  ( module X,
  )
where

import Foo.Bar.Data.Thing.Gen as X ()
