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
    if len(sys.argv) != 2:
        print("Usage: python check_sat.py '<ltl_formula>'")
        sys.exit(1)

    ltl = sys.argv[1]
    if is_satisfiable(ltl):
        print("sat")
    else:
        print("unsat")

