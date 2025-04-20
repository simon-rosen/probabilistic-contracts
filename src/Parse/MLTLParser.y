{
module Parse.MLTLParser
  ( mltlParser
  , parseMLTL
  ) where

import qualified Parse.Lexer as L
import Specs.MLTL (Formula (..))

}

%name mltlParser
%tokentype { L.Token }
%error { parseError }

%token
  '('             { L.LPar }
  ')'             { L.RPar }
  '['             { L.LBrack }
  ']'             { L.RBrack }
  ','             { L.Comma }
  int             { L.IntLit $$ }
  id              { L.Ident $$ }
  '!'             { L.Not }
  '&'             { L.And }
  '|'             { L.Or }
  '->'            { L.Implies }
  'G'             { L.Globally }
  'F'             { L.Future }
  'U'             { L.Until }

%right '->'
%left '|' '&'
%left 'U'
%left '!' 'G' 'F' 'X'
%%

Phi   : '(' Phi ')'                             { $2 }
      | id                                      { Atom $1 }
      | '!' Phi                                 { Not $2 }
      | Phi '&' Phi                             { And $1 $3 }
      | Phi '|' Phi                             { Or $1 $3 }
      | Phi '->' Phi                            { Implies $1 $3 }
      | 'G' '[' int ',' int ']' Phi             { Globally ($3, $5) $7 }
      | 'F' '[' int ',' int ']' Phi             { Future ($3, $5) $7 }
      | Phi 'U' '[' int ',' int ']' Phi         { Until ($4, $6) $1 $8 }

{

parseError :: [L.Token] -> a
parseError _ = error "Parse error"

-- | utility function to parse a MLTL formula from a string
parseMLTL :: String -> Formula
parseMLTL = mltlParser . L.alexScanTokens

}
