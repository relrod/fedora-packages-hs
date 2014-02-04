-----------------------------------------------------------------------------
-- |
-- Copyright : (c) 2014 Ricky Elrod
-- License : BSD3
--
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Provides a Haskell interface to the
-- <https://apps.fedoraproject.org/packages Fedora Packages> API.
--
-- This module simply re-exports all public modules within the fedora-packages
-- project.
--
-- Depending on usecase, it might be easiest to import this and Lens, as below:
--
-- > import Fedora.Packages
-- > import Fedora.Packages.Lens
-- > import Control.Lens
--
-- But keep in mind that this will clutter your namespace pretty badly, and
-- might not be ideal for all situations.
--
-- If you don't want to use lenses (but you should because they are awesome),
-- you can do this instead:
--
-- > import qualified Fedora.Packages as FedPackages
--
-- ...and then access everything using understored record-style getters, e.g.
-- @_packageName theBuild@.
--
-- Or you can import specific pieces of @Fedora.Packages.\<whatever\>@ instead
-- of importing @Fedora.Packages@ which re-exports everything.
--
-- If you do that, however, you'll want to import @Fedora.Packages.API@ and
-- @Fedora.Packages.Config@ as well, so that you have the necessary data types
-- for interacting with the API.
--
-- TL;DR: This is written to be very modular, and you are free to use it however
-- it best fits in with the rest of your project. Have fun!
----------------------------------------------------------------------------

module Fedora.Packages
  ( module F
  ) where

import Fedora.Packages.API as F
import Fedora.Packages.Bugs as F
import Fedora.Packages.Builds as F
import Fedora.Packages.Config as F
import Fedora.Packages.Releases as F
import Fedora.Packages.Search as F
