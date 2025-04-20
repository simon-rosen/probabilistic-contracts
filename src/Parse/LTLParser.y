{
module Parse.LTLParser
  ( ltlParser
  , parseLTL
  ) where

import qualified Parse.Lexer as L
import Specs.LTL (Formula (..))

}

%name ltlParser
%tokentype { L.Token }
%error { parseError }

%token
  '('             { L.LPar }
  ')'             { L.RPar }
  id              { L.Ident $$ }
  '!'             { L.Not }
  '&'             { L.And }
  '|'             { L.Or }
  '->'            { L.Implies }
  'G'             { L.Globally }
  'F'             { L.Future }
  'X'             { L.Next }
  'U'             { L.Until }

%right '->'
%left '|' '&'
%left 'U'
%left '!' 'G' 'F' 'X'
%%

Phi   : '(' Phi ')'         { $2 }
      | id                  { Atom $1 }
      | '!' Phi             { Not $2 }
      | Phi '&' Phi         { And $1 $3 }
      | Phi '|' Phi         { Or $1 $3 }
      | Phi '->' Phi        { Implies $1 $3 }
      | 'G' Phi             { Globally $2 }
      | 'F' Phi             { Future $2 }
      | 'X' Phi             { Next $2 }
      | Phi 'U' Phi         { Until $1 $3 }


{

parseError :: [L.Token] -> a
parseError _ = error "Parse error"

-- | utility function to parse an LTL formula from a string
parseLTL :: String -> Formula
parseLTL = ltlParser . L.alexScanTokens

}

