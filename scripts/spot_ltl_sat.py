#!/usr/bin/python

import spot
import sys

def is_satisfiable(ltl_formula: str) -> bool:
    # parse the ltl formula
    formula = spot.formula(ltl_formula)
    
    # convert into a buchi automaton
    aut = spot.translate(formula, "buchi")

    # Check for emptiness of the automaton, empty -> non-satisfiable
    return not aut.is_empty()

if __name__ == "__main__":
    # read the formula from stdin
    ltl = sys.stdin.read()
    # check sat with spot
    if is_satisfiable(ltl):
        print("sat")
    else:
        print("unsat")
