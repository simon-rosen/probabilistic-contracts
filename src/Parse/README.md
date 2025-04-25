# about the parsers

I have written two parsers, one for LTL and one for MLTL. These parsers can
parse formulas, probabilistic contracts, and full refinement problems of
probabilistic contracts for these two temporal logics.

These parsers parse directly into the datatype I have for representing LTL and
MLTL formulas. Doing it this way allowed me to not mess around with ASTs and
typechecking (for example we would not want to parse contracts that use a mix of
LTL and MLTL), and in lack of time I think this was reasonable.

But if I would add any more specification languages I think it would be worth it
to spend some time to improve the parsing and do it using ASTs and typechecking.
