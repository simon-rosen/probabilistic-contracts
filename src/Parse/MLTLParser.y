{
module Parse.MLTLParser
  ( mltlParser
  , parseMLTL
  , mltlProbContractParser
  , parseMLTLProbContract
  , mltlRefinementProblemParser
  , parseMLTLRefinementProblem
  ) where

import qualified Parse.Lexer as L
import Specs.MLTL (Formula (..))
import Contracts.Probabilistic (ProbContract (..), RefinementProblem (..))
import qualified Math

}

%name mltlParser Phi
%name mltlProbContractParser PC
%name mltlRefinementProblemParser Problem
%tokentype { L.Token }
%error { parseError }

%token
  -- contract
  'P'             { L.Prob }
  '<'             { L.Less }
  '<='            { L.Leq }
  '>'             { L.Greater }
  '>='            { L.Geq }
  double          { L.DoubleLit $$ }
  -- mltl
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
  'top'           { L.Top }
  'bottom'        { L.Bottom }
  'G'             { L.Globally }
  'F'             { L.Future }
  'U'             { L.Until }
  -- problem
  '[='            { L.Refines }
  '||'            { L.Composed }

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
      | 'top'                                   { Top }
      | 'bottom'                                { Bottom }
      | 'G' '[' int ',' int ']' Phi             { Globally ($3, $5) $7 }
      | 'F' '[' int ',' int ']' Phi             { Future ($3, $5) $7 }
      | Phi 'U' '[' int ',' int ']' Phi         { Until ($4, $6) $1 $8 }
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
parseError _ = error "Parse error"

-- | utility function to parse a MLTL formula from a string
parseMLTL :: String -> Formula
parseMLTL = mltlParser . L.alexScanTokens

-- | utility function to parse a probabilistic contract specified with ltl from a string
parseMLTLProbContract :: String -> ProbContract Formula
parseMLTLProbContract = mltlProbContractParser . L.alexScanTokens


-- | utility function to parse a RefinementProblem of contracts specified with LTL from a string
parseMLTLRefinementProblem :: String -> RefinementProblem Formula
parseMLTLRefinementProblem = mltlRefinementProblemParser . L.alexScanTokens
}
