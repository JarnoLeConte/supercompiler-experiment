-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.HM.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for a simple, implicitly typed functional language.
--
-------------------------------------------------------------------------------

module CCO.HM.Lexer (
    -- * Tokens
    Token(..)      -- abstract, instance: Symbol

    -- * Lexer
  , lexer      -- :: Lexer Token

    -- * Token parser
  , keyword    -- :: String -> Parser Token String
  , constr     -- :: Parser Token String
  , var        -- :: Parser Token String
  , nat        -- :: Parser Token String
  , label      -- :: Parser Token String
  , spec       -- :: Char -> Parser Token Char
  , chr        -- :: Parser Token String
) where

import CCO.HM.Base    (Var)
import CCO.Lexing hiding (satisfy)
import CCO.Parsing    (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of tokens.
data Token
  = Keyword  { fromKeyword :: String }    -- ^ Keyword.
  | Constr   { fromConstr  :: String }    -- ^ Constructor.
  | Var      { fromVar     :: Var    }    -- ^ Variable.
  | Nat      { fromNat     :: Int    }    -- ^ Nat/Int.
  | Label    { fromLabel   :: String }    -- ^ Name written between quotes.
  | Spec     { fromSpec    :: Char   }    -- ^ Special character.
  | Chr      { fromChr     :: Char    }    -- ^ character.


instance Symbol Token where
  describe (Keyword _)  lexeme = "keyword "     ++ lexeme
  describe (Nat _)      lexeme = "integer "     ++ lexeme
  describe (Constr _)   lexeme = "constructor " ++ lexeme
  describe (Var _)      lexeme = "variable "    ++ lexeme
  describe (Label _)    lexeme = "label "       ++ lexeme
  describe (Chr _)      lexeme = "character "   ++ lexeme
  describe (Spec _)     lexeme =                   lexeme

-- | Retrieves whether a 'Token' is a 'Keyword'.
isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

-- | Retrieves whether a 'Token' is a 'Nat'.
isNat :: Token -> Bool
isNat (Nat _) = True
isNat _       = False

-- | Retrieves whether a 'Token' is a 'Constr'.
isConstr :: Token -> Bool
isConstr (Constr _) = True
isConstr _          = False

-- | Retrieves whether a 'Token' is a 'Var'.
isVar :: Token -> Bool
isVar (Var _) = True
isVar _       = False

-- | Retrieves whether a 'Token' is a 'Label'.
isLabel :: Token -> Bool
isLabel (Label _) = True
isLabel _         = False

-- | Retrieves whether a 'Token' is a 'Spec'.
isSpec :: Token -> Bool
isSpec (Spec _) = True
isSpec _        = False

-- | Retrieves whether a 'Token' is a 'Chr'.
isChr :: Token -> Bool
isChr (Chr _) = True
isChr _       = False


-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer Token
layout_ = ignore (some (anyCharFrom " \n\t"))

-- | A 'Lexer' that recognises 'Keyword' tokens.
keyword_ :: Lexer Token
keyword_ = fmap Keyword $
  string "in" <|> string "let" <|> string "letrec" <|> string "ni" <|> string "prim" <|> string "->" <|> string "case" <|> string ";" <|> string "of" <|> string "end"
  <|> string "match"

-- | A 'Lexer' that recognises 'Constr' tokens.
constr_ :: Lexer Token
constr_ = fmap Constr $ (:) <$> upper <*> many (alpha <|> digit <|> char '_' <|> char '\'')

-- | A 'Lexer' that recognises 'Var' tokens.
var_ :: Lexer Token
var_ = fmap Var $ (:) <$> lower <*> many (alpha <|> digit <|> char '_' <|> char '\'')

-- | A 'Lexer' that recognises 'Nat' tokens.
nat_ :: Lexer Token
nat_ = (Nat . read) <$> some digit

-- | A 'Lexer' that recognises 'Label' tokens.
label_ :: Lexer Token
label_ = Label <$ char '"' <*> some alpha <* char '"'

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "()=\\."

-- | A 'Lexer' that recognises 'Chr' tokens.
chr_ :: Lexer Token
chr_ = Chr <$ char '\'' <*> alpha <* char '\''

-- | The 'Lexer' for the language.
lexer :: Lexer Token
lexer = layout_ <|> keyword_ <|> constr_ <|> var_ <|> nat_ <|> label_ <|> spec_ <|> chr_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises a specified keyword.
keyword :: String -> Parser Token String
keyword key = fromKeyword <$>
              satisfy (\tok -> isKeyword tok && fromKeyword tok == key) <!>
              "keyword " ++ key

-- | A 'Parser' that recognises constructors.
constr :: Parser Token Var
constr = fromConstr <$> satisfy isConstr <!> "constructor"

-- | A 'Parser' that recognises variables.
var :: Parser Token Var
var = fromVar <$> satisfy isVar <!> "variable"

-- | A 'Parser' that recognises numbers.
nat :: Parser Token Int
nat = fromNat <$> satisfy isNat <!> "nat"

-- | A 'Parser' that recognises labels.
label :: Parser Token String
label = fromLabel <$> satisfy isLabel <!> "label"

-- | A 'Parser' that recognises characters.
chr :: Parser Token Char
chr = fromChr <$> satisfy isChr <!> "character"

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c = fromSpec <$>
         satisfy (\tok -> isSpec tok && fromSpec tok == c) <!>
         [c]
