{-# LANGUAGE QuasiQuotes #-}
module Examples where

import Solver
import AlgTypes

-- (use the runExample :: AlgSignature -> IO () from AlgTypes)

--------------
-- Examples --
--------------
-- parser has a bunch of shift/reduce conflicts, so these might not actually be the thing I was intending due to lack
-- of brackets. Luckily, the demutualizer only requires substitution, so it works anyway. 

list :: Decl AlgType
list = algDecl [declExp| list = all x . 1 + (list x) * x |]  

binarytree :: Decl AlgType
binarytree = algDecl [declExp| binarytree = all a . 1 + (binaryTree a) * a * (binaryTree a)|]

rosetree :: Decl AlgType
rosetree = algDecl [declExp| rosetree = all a . 1 * (list (rosetree a)) |]

tree :: Decl AlgType
tree = algDecl [declExp| tree = all a . 1 + (a * (forest a)) |]

forest :: Decl AlgType
forest = algDecl [declExp| forest = all a . 1 + ((tree a) * (forest a)) |]

typeF :: Decl AlgType
typeF = algDecl [declExp| f = (all a . 1 + ((f a) * (h a)))|]

typeG :: Decl AlgType
typeG = algDecl [declExp| g = (all a . ((h a) * (g a)) + 1)|]

typeH :: Decl AlgType
typeH = algDecl [declExp| h = (all a . (f a) * (g a))|]

treeForest :: AlgSignature
treeForest = algSignature [tree,forest]

fgh :: AlgSignature
fgh = algSignature [typeF,typeG,typeH]

onlyList :: AlgSignature
onlyList = algSignature [list]

multi :: AlgSignature
multi = algSignature [typeF,typeG,typeH,tree,forest,binarytree,list,rosetree]


-------------------------
-- Generating Examples --
-------------------------

{- We may think of a system of fixed point equations (corresponding to some mutually recursive types)

t1 = T1(t1,...,tn)
...
tn = Tn(t1,...,tn)

in terms of a "density matrix"

t1,1 t1,2 ... t1,n
t2,1 t2,2 ... t2,n
...
tn,1 tn,2 ... tn,n

where ti,j \in Nat is the number of times tj appears in Ti(t1,...,tn).

We can generate a system of mutually recursive types with any given density matrix.

Useful for testing!
-}

type Density = [[Int]] -- really "nxn matrices of Nats". use responsibly

generate :: Density -> AlgSignature
generate rows = algSignature $
                  map (\(i,t) -> Decl ("t" ++ (show i)) t)
                      (zip [1..] (map (generateOne 1) rows))
  where
  generateOne :: Int -> [Int] -> AlgType
  generateOne i [] = Zero
  generateOne i (n:ns) = Sum (n `times` i) (generateOne (succ i) ns)
  times :: Int -> Int -> AlgType
  times 0 i = One
  times k i = Prod (Var ("t" ++ (show i))) (times (pred k) i)

-- I don't think we need to be too worried about density matrices with
-- entries that aren't all 0 or 1.

-- in which the nxn matrix is composed entirely of '1's.
veryDense :: Int -> Density
veryDense x = (replicate x (replicate x 1))
                                  

