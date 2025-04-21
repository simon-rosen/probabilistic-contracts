{
module Parse.LTLParser
  ( ltlParser
  , parseLTL
  , ltlProbContractParser
  , parseLTLProbContract
  , ltlRefinementProblemParser
  , parseLTLRefinementProblem
  ) where

import qualified Parse.Lexer as L
import Specs.LTL (Formula (..))
import Contracts.Probabilistic (ProbContract (..), RefinementProblem (..))
import qualified Math

}

%name ltlParser Phi
%name ltlProbContractParser PC
%name ltlRefinementProblemParser Problem
%tokentype { L.Token }
%error { parseError }

%token
  -- ltl
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
  -- contract
  'P'             { L.Prob }
  ','             { L.Comma }
  '<'             { L.Less }
  '<='            { L.Leq }
  '>'             { L.Greater }
  '>='            { L.Geq }
  -- problem
  '[='            { L.Refines }
  '||'            { L.Composed }
  double          { L.DoubleLit $$ }

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
      ;

PC : 'P' '(' Phi ',' Phi ')' Cmp double { ProbContract (Math.Probability ($3, $5) $7 $8) }
   ;

Cmp : '<'  { Math.Less }
    | '<=' { Math.Leq }
    | '>'  { Math.Greater }
    | '>=' { Math.Geq }
    ;


-- from https://serokell.io/blog/parsing-with-happy
sepBy_rev(p, sep)
  : p                       { [$1] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }
  ;

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }
  ;

Problem : PC '[=' sepBy(PC,'||') { RefinementProblem { systemContract = $1, componentContracts = $3} }
        ;
{

parseError :: [L.Token] -> a
parseError ts = error $ "Parse error:" <> show ts

-- | utility function to parse an LTL formula from a string
parseLTL :: String -> Formula
parseLTL = ltlParser . L.alexScanTokens

-- | utility function to parse a probabilistic contract specified with ltl from a string
parseLTLProbContract :: String -> ProbContract Formula
parseLTLProbContract = ltlProbContractParser . L.alexScanTokens

-- | utility function to parse a RefinementProblem of contracts specified with LTL from a string
parseLTLRefinementProblem :: String -> RefinementProblem Formula
parseLTLRefinementProblem = ltlRefinementProblemParser . L.alexScanTokens
}
