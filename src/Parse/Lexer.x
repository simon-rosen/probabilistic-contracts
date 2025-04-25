{
module Parse.Lexer 
  ( Token (..)
  , alexScanTokens
  ) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "#".*                          ; -- "#" starts a comment that continues until the end of the line
  "("                            { \s -> LPar }
  ")"                            { \s -> RPar }
  "["                            { \s -> LBrack }
  "]"                            { \s -> RBrack }
  ","                            { \s -> Comma }
  "!"                            { \s -> Not }
  "&"                            { \s -> And }
  "|"                            { \s -> Or }
  "->"                           { \s -> Implies }
  "G"                            { \s -> Globally }
  "F"                            { \s -> Future }
  "X"                            { \s -> Next }
  "U"                            { \s -> Until }
  "P"                            { \s -> Prob }
  "<"                            { \s -> Less }
  "<="                           { \s -> Leq }
  ">"                            { \s -> Greater }
  ">="                           { \s -> Geq }
  "]="                           { \s -> Refines }
  "||"                           { \s -> Composed }
  (0|[1-9][0-9]*) "." ([0-9]+) ("e-" $digit+)? { \s -> DoubleLit (read s) }
  (0|[1-9][0-9]*)              { \s -> IntLit (read s) }
  $alpha [$alpha $digit]*  { \s -> Ident s } -- an identifier (propositional literal in this program) must start with a alhanumeric and can then contain both alhanumerics and numbers

{
-- Each action has type :: String -> Token

-- The token type:
data Token 
  = LPar
  | RPar
  | LBrack
  | RBrack
  | Comma
  | Ident String
  | IntLit Int
  | DoubleLit Double
  | Not
  | And
  | Or
  | Implies
  | Globally
  | Future
  | Next
  | Until
  | Prob
  | Less
  | Leq
  | Greater
  | Geq
  | Refines
  | Composed
  deriving (Eq, Show)

}
