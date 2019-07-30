Here is the implementation of the SAT Solving ALgorithm in racket(DrRacket) and using it to solve N-queens problem.

SAT-Solving Algorithm:
Here is a summary of the SAT solving method called the Davis-Putnam-LogemannLoveland (DPLL) procedure. Given a formula F, call the procedure being described
below as dpll(F). Here is a summary of the procedure:

1. Unit propagation: If the formula contains a unit clause (a clause consisting of
a single literal), drop all clauses containing this literal and drop the negation of the
literal from the remaining clauses. Applying this step on (¬x1∨¬x3)∧(x1∨¬x2)∧x3
because of x3 gives ¬x1 ∧ (x1 ∨ ¬x2).
2. Literal Elimination: Identify a literal that has the same polarity in every clause
that it appears in. Eliminate all such clauses. Applying this on (¬x1 ∨ ¬x2)∧(x1 ∨
¬x2) ∧ (¬x1 ∨ ¬x3) because of x2 gives (¬x1 ∨ ¬x3).
3. At this point if the formula has no clauses then return true. Else if it has an empty
clause, then return false.
4. The previous steps can be considered to be the base steps of the DPLL procedure.
(a) Now choose any literal, l, set its value to true. Remove all clauses containing
l and remove the negation of the literal from all clauses. Call the resulting
formula F'. If the result of dpll(F') is true, return true.
(b) Set the value of the same literal l to false. Remove all clauses containing the
negation of the literal l and remove l itself from all clauses containing it. Call
the resulting formula F'. Return dpll(F').
