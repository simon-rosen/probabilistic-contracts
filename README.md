# probabilistic-contracts
This project will implement a refinement verification algorithm for
probabilistic contracts, described in
https://link.springer.com/chapter/10.1007/978-3-031-35257-7_6.

## Project overview

### steps
1. Identify variables that should equal 0 and variables that should be greater
   or equal to 0. (Language emptiness check / satisfiability check on the
   specifications)
2. Create the system of linear inequalities.
3. Solve the equation system.

#### step 1 - language emptiness / satisfiability
Step 1 is done by checking language emptiness / satisfiability of each 
combination of specifications or their complement, 
ex: A0 & G0 & A1^C & G1^C & ... & An & Gn. There is 2^(2n) such combinations.
The language emptiness / satisfiability checks can and should be ofloaded to an
external tool.

#### step 2 - creating the equation system
Each variable z0,  ..., z(2^(2n) - 1) can be associated with a specific
combination of specs by letting the binary representation of its number
represent what specs are complemented and not for this variable, ex: 
z13 = 001101_2 would mean that this variable represents the combination of specs
A0^C & G0^C & A1 & G1 & A2^C & G2.

When creating the equations representing the probability of each contract we can
use this meaning behind the variables to identify all variables that contain (Ai
& Gi) or (Ai) by using bitwise masking.

#### step 3 - solve equation system
When the equation system is ready it can be solved by an external tool. For
example, one can represent it in smt2 format and send it to z3.

### modularity of this implementation
The steps above illustrate that an implementation of this algorithm can benefit
from using external tools for solving both step 1 and step 3. Relying on
external tools for these steps makes the implementation very modular: it would
be possible to add new specification languages by making small changes to the
code and using new tools that operate on these languages.

If one wants to add support to something like timed automata later on, the
simplest solution would be to create a separate program that can manipulate
timed automata and use this tool as an external dependency for this program.

## Dependencies
It will rely quite heavily on external tools for solving satisfiability of
different logics.

* SAT-solver for propositional logic: [kissat](https://github.com/arminbiere/kissat)
* SAT-solvers for LTL: [BLACK - a symbolic solver based on SAT-solvers for propositional logic](https://www.black-sat.org/en/stable/), [Spot - a non-symbolic solver based on automata](https://spot.lre.epita.fr/), [Altaa - a hybrid approach](https://github.com/lijwen2748/aalta)
* SMT-solver: [Z3](https://github.com/Z3Prover/z3)
* PSAT-solver: ?

## syntax of the contracts

The tool is meant to be used to solve refinement problems for probabilistic
contracts specified with LTL and MLTL.

## How to use the tool
### solving refinement
Here are some examples on how the tool is expected to be used from the command
line.

Solve a refinement problem (for contracts specified with LTL) written in a file
```bash
probabilistic-contracts solve --lang LTL --file example.pc
```

Solve a refinement problem (for contracts specified with MLTL), where the
problem is supplied as an argument 
```bash
probabilistic-contracts solve --lang MLTL --problem 'P(((p4) -> (G[2,2](F[1,6](p3)))) & (G[9,9](p5)), G[9,10](F[9,9]((!((p3) & (p4))) -> (p5)))) > 0.4786220769399635 ]= P(F[4,9](G[8,10]((p5) & (!((p2) -> (p3))))), ((p4) | (F[5,10](p4))) -> ((p1) & (!(p3)))) <= 0.8571528545768496 || P((p3) & (F[10,10](!(!((p5) -> (p1))))), G[6,8](G[1,8](G[1,6]((p5) U[1,2](!(p3)))))) >= 0.2564396157936457 || P(!((G[9,9](p4)) | (!((p2) U[4,6](p4)))), ((p2) -> ((G[5,6](p1)) U[2,5](G[9,9](p4)))) | (p3)) <= 0.14063748298715617'
```
*OBS*: use single quotes '' instead of double quotes "" around the problem to avoid weird bash bugs.


### generating testcases
The tool can also generate random problem instances.

