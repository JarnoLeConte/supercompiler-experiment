import CCO.Component    (Component, component, printer, ioWrap)
import CCO.HM.Base
import CCO.HM           hiding (parser)
import CCO.Tree         (ATerm, fromTree, toTree)
import Control.Arrow    (arr, (>>>))
import Debug.Trace
import qualified Data.Map as M

import CCO.Lexing
import CCO.HM.Lexer                    (Token(Keyword), lexer, keyword, constr, var, nat, label, spec, chr)

import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative
import CCO.HM.Parser (pTerm)

import CCO.HM.HS.Match (match)
import CCO.HM.HS.Rename (renameTerm)
import CCO.HM.HS.Evaluate (normalize, rebuild)



main = ioWrap $ parser
            >>> arr runMatch
            >>> arr fromTree
            >>> printer

type TokenParser = Parser Token

-- A 'Component' for parsing terms.
parser :: Component String (Term, Term)
parser = C.parser lexer (pMatch <* eof)

pMatch :: TokenParser (Term, Term)
pMatch = (,) <$
  (keyword "match")
  <* spec '(' <*> pTerm <* spec ')'
  <* spec '(' <*> pTerm <* spec ')'

lMatch :: Lexer Token
lMatch = fmap Keyword $ string "match"

runMatch :: (Term, Term) -> Term
runMatch (e1, e2) = case match s1 s2 of
  Just rn -> trace (show rn) $
             Term 0 $ Let [Bind "m1" (renameTerm rn (rebuild s1))
                          ,Bind "m2" (rebuild s2)]
                          (Term 0 (Con "EQ" ["m1", "m2"]))
  _ -> error $ "no match"
  where s1 = normalize $ State M.empty [] e1 M.empty
        s2 = normalize $ State M.empty [] e2 M.empty




