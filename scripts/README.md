# scripts

Some tools would benefit from scripting and these are put here.

One example is Spot, which when used from the comman line would involve piping
the output from `ltl2tgba` (constructs an automaton) to `autfilt` (among other
things, check if the automaton is empty), I think this would introduce
unnecesary overhead which is why using the python bindings to write a script
that does the full satisfiability check makes sense.

## setting up spot

1. install spot in the way proposed at their [website](https://spot.lre.epita.fr/install.html)
2. if necesary, add the directory where the python bindings were installed to
   your `$PYTHONPATH` (otherwise python will not find this module)
