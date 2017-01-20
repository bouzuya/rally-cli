module Fetch (HTTP, fetch) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Foreign (Foreign)
import Data.Options (Options, options)
import Fetch.Options (FetchOptions, defaults)
import Prelude (Unit, ($), (<>), (<<<))

foreign import data HTTP :: !

foreign import fetchImpl ::
  forall eff.
    Foreign
    -> (String -> Eff (http :: HTTP | eff) Unit)
    -> (Error -> Eff (http :: HTTP | eff) Unit)
    -> (Eff (http :: HTTP | eff) Unit)

fetch :: forall eff. Options FetchOptions -> Aff (http :: HTTP | eff) String
fetch opts = makeAff \ng ok ->
  fetchImpl (options $ defaults <> opts) ok ng
