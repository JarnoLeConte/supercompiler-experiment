-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Base
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

module CCO.HM.Base (
  module CCO.HM.HS.Data,
  module CCO.HM.HS.Base,
  module CCO.HM.HS.Supercompile
) where


import CCO.HM.HS.Data
import CCO.HM.HS.Base
import CCO.HM.HS.Supercompile (supercompile)

import CCO.Printing               (Printable (pp))
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>))
import Data.Map as M (empty)

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Term where
  fromTree (Term _ t) = T.App "Term" [fromTree t]
  toTree = parseTree [app "Term" (Term (-1) <$> arg)]

instance Tree UTerm where
  -- fromTree (Nat x)       = T.App "Nat"   [fromTree x]
  fromTree (Var x)       = T.App "Var"   [fromTree x]
  -- fromTree (Chr x)       = T.App "Chr"   [fromTree x]
  fromTree (Lam x t1)    = T.App "Lam"   [fromTree x, fromTree t1]
  fromTree (App t1 t2)   = T.App "App"   [fromTree t1, fromTree t2]
  -- fromTree (Let x t1 t2) = T.App "Let"   [fromTree x, fromTree t1, fromTree t2]
  fromTree (Let bs t)    = T.App "Let"   [fromTree bs, fromTree t]
  -- fromTree (Prim x args) = T.App "Prim"  [fromTree x, fromTree args]
  fromTree (Con x args)  = T.App "Con"  [fromTree x, fromTree args]
  fromTree (Case x alts)  = T.App "Case" [fromTree x, fromTree alts]

  toTree = parseTree [
                     --   app "Nat"   (Nat   <$> arg                )
                       app "Var"   (Var   <$> arg                )
                     -- , app "Chr"   (Chr   <$> arg                )
                     , app "Lam"   (Lam   <$> arg <*> arg        )
                     , app "App"   (App   <$> arg <*> arg        )
                     -- , app "Let"   (Let   <$> arg <*> arg <*> arg)
                     , app "Let"   (Let <$> arg <*> arg      )
                     -- , app "Prim"  (Prim  <$> arg <*> arg        )
                     , app "Con"   (Con  <$> arg <*> arg        )
                     , app "Case"  (Case <$> arg <*> arg        )
                     ]

instance Tree Alt where
  fromTree (Alt p t) = T.App "Alt" [fromTree p, fromTree t]
  toTree = parseTree [app "Alt" (Alt <$> arg <*> arg)]

instance Tree Bind where
  fromTree (Bind x e) = T.App "Bind" [fromTree x, fromTree e]
  toTree = parseTree [app "Bind" (Bind <$> arg <*> arg)]



