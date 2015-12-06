-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Parser (
    -- * Parser
    parser    -- :: Component String Term
  , pTerm
) where

import CCO.HM.Base                     (Var, Term (..), UTerm (..), Alt (..), Bind (..))
import CCO.HM.Lexer                    (Token, lexer, keyword, constr, var, nat, label, spec, chr)
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative
import Data.Map as M (empty)

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------


-- A 'Component' for parsing terms.
parser :: Component String Term
parser = C.parser lexer (pTerm <* eof)

tg :: UTerm -> Term
tg = Term 0

tag :: TokenParser UTerm -> TokenParser Term
tag = fmap tg


pTerm, pTerm' :: TokenParser Term
pTerm = tag pUTerm
pTerm' = tag pUTerm'

pUTerm, pUTerm' :: TokenParser UTerm
pUTerm = pCon <|> pApps <|> pLam <|> pLet <|> pCase <|> pUTerm'
pUTerm' = pVar <|> pCon0 <|> spec '(' *> pUTerm <* spec ')'

pApps :: TokenParser UTerm
pApps = foldl (App . tg) <$> pUTerm' <*> some var

pApp :: TokenParser UTerm
pApp = App <$> pTerm' <*> var

pLam :: TokenParser UTerm
pLam = Lam <$ spec '\\' <*> var <* spec '.' <*> pTerm

pVar :: TokenParser UTerm
pVar = Var <$> var

pCon :: TokenParser UTerm
pCon = Con <$> constr <*> many var

pCon0 :: TokenParser UTerm
pCon0 = (\c -> Con c []) <$> constr

pCase :: TokenParser UTerm
pCase = Case <$  keyword "case" <*> pTerm <* keyword "of" <*> some pAlt <* keyword "end"

pLet :: TokenParser UTerm
pLet = Let <$ keyword "let" <*> some pBind <* keyword "in" <*> pTerm

pAlt :: TokenParser Alt
pAlt = Alt <$> tag pCon <* keyword "->" <*> pTerm

pBind :: TokenParser Bind
pBind = Bind <$> var <* spec '=' <*> pTerm




