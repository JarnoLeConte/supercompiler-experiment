-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM (
  -- * Syntax
  Term

  -- * Parser
  , parser                      -- :: Component String Term
) where

import CCO.HM.Base      (Var, Term (..))
import CCO.HM.Parser    (parser)
