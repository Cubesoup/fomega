How to use:

1. Install 'stack' (https://docs.haskellstack.org/en/stable/README/)
2. In the directory where the repository lives, run
   > stack build --only-dependencies
3. Now, run
   > stack ghci --ghci-options -XQuasiQuotes src/Examples.hs

You can now get the mutual recursion out of signatures sig :: AlgSignature with
  ghci> runExample <example>

Some examples to try are "fgh","treeforest","onlyList", "multi", and "(generate (veryDense 3))". See also the function "generate" in Examples.hs.
You can parse declarations of algebraic types using the quasiquoter, as in
  ghci> list_decl = algDecl [declExp| all a::*. 1 + ((list a) * a)  |]

