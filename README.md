# TermEnumerator

Only 21 lines of code to generate a list with all untyped lambda terms (with repetitions)

A few steps:
  1. take a function that, for a list of terms of at most n variables, creates a new list of terms by turning the leaves (vars) of the original list into connectives (appl, abst) in the output list.
  2. take a function that iteratively concatenates the outputs of 1, starting from the list of all n variables, these are all terms with at most n variables.
  3. concate those final lists for 1, 2, ... variables, this are all terms of all amounts of variables
  4. print all terms from 3. using a diagonal (cantor) traversal, to let all elements of all infinite lists to eventually appear.
