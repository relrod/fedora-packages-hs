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
----------------------------------------------------------------------------

module Fedora.Packages
  ( module F
  ) where

import Fedora.Packages.Config as F
import Fedora.Packages.Search as F
