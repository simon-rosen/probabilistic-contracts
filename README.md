# probabilistic-contracts
This project will implement a refinement verification algorithm for
probabilistic contracts, described in
https://link.springer.com/chapter/10.1007/978-3-031-35257-7_6.

## How to install the tool

The tool itself should be easy to install. It is written in Haskell and uses
[stack](https://docs.haskellstack.org/en/stable/) as the build system so make
sure you have this installed. Then do the following:

1. Clone this repository
2. `cd` into it and type `stack build` to compile the project.
3. After compiling, type `stack install` to move the executable file into the
   directory that stack think is suitable. On a linux system the file will be
   placed under `~/.local/bin` so make sure that this direcroty is included in
   your `$PATH`.
4. The executable will be called `probabilistic-contracts-exe` so try to invoke
   the tool by entering this in the terminal. 

But, in order for the tool to work we also have to install a couple of
dependencies...

### Dependencies
* SAT-solvers for LTL: [BLACK - a symbolic solver based on SAT-solvers for propositional logic](https://www.black-sat.org/en/stable/), [Spot - among other things this tool can solve LTL formulas by converting them to omega-automata and checking language emptiness](https://spot.lre.epita.fr/), [Altaa - a very good LTL solver](https://github.com/lijwen2748/aalta)
* SMT-solver: [Z3](https://github.com/Z3Prover/z3)
* A tool for converting MLTL formulas to LTL: [MLTLConvertor](https://github.com/lijwen2748/mltlsat)
* (Also sqlite3 is needed for the benchmarking)

If you are interested in undertstanding how these external tool are used, then
in short:

* The LTL solvers are used to solve satisfiability on an exponential amount of LTL formulas 
  ( exponential in the number of components).
* A system of linear equations are built up based on the result of the previous
  step, and this system is then solved using the LRA solver of z3.
* (In order to also support MLTL, the MLTLConvertor is used to convert MLTL
  formulas to LTL so that the same solvers can be used to support also MLTL
  formulas)

#### More on installing the dependencies
Make sure all of these tools are in your `$PATH` after having installed them.

##### Black 
Black is easy to install and has good documentation on how to do so [here](https://www.black-sat.org/en/stable/installation.html).

I would recommend to install z3 first because Black uses it as a dependecy.

##### Aalta 
Aalta is also easy to install. The documentation on how to do this is on its
[github page](https://github.com/lijwen2748/aalta).

##### Spot
Spot can be a bit more complicated to install, and it is importand that the
python bindings are installed. However, spot is a really well documented tool
and installation instructions for multiple systems is located [here](https://spot.lre.epita.fr/install.html).

I installed spot by downloading the tarball for spot-2.13 and then
```bash
cd spot-2.13
./configure
make
sudo make install
```

And I had to add a path to my python search path.

###### spot_ltl_sat.py
In order to solve LTL formulas with spot I wrote a little python script that
uses the spot api to first convert the formulas to automata, and then checks
emptiness on the automata. This script is located in my project
`scripts/spot_ltl_sat.py`. Install this script in your `$PATH`:
* first make it executable `chmod +x scripts/spot_ltl_sat.py`
* then copy it into a folder that is on your `$PATH`, for example `cp
  scripts/spot_ltl_sat.py ~/.local/bin`

If python is called python3 on your system you may have to change the first line
in the script to `#!/usr/bin/python3` instead of `#!/user/bin/python`.

##### MLTLConvertor
This tool was part of a larger set of tools used for an article on MLTL
satisfiability testing. The whole project is located
[here](https://github.com/lijwen2748/mltlsat), but to compile just the
convertor:
* clone the larger project
* go to the `translator/src` folder and type `make run`
* copy the `MLTLConvertor` binary into a folder that is in your `$PATH`, for
  example `cp MLTLConvertor ~/.local/bin`

##### z3
z3 should be the simplest external tool to install, either clone the [github repository](https://github.com/Z3Prover/z3)
or download it with your package manager.

## How to use the tool
### Solving refinement
The main use of this tool is for verifying refinement of probabilistic contracts
specified with LTL or MLTL. View how to do this by typing the command:

```bash
probabilistic-contracts-exe -h
```

#### Syntax

#### Some examples
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


### Generating testcases
The tool can also generate random problem instances. View how to do this by
entering the command:

```bash
probabilistic-contracts-exe generate -h
```

### Benchmarking the algorithm
The tool can also be used to benchmark the algorithm on random instances. View
how to do this by entering the command:

```bash
probabilistic-contracts-exe benchmark -h
```

### Warning

This tool manually stops each external tool by calling for example `pkill -9 -f
z3`,  so if you are running any other instances of these solvers they would also
be shut down. The reason I did it like this was because I realized rather late
that lazy IO in haskell was problematic (sometimes solvers would not get shut
down, or I read from them after they were shut down)...

This could of course be fixed, but the `pkill` solution worked for my needs and
was really simple to implement.
