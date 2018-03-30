{-# LANGUAGE QuasiQuotes #-}

-- Major TODO: get rid of shift/reduce conflicts, (some) type normalization, kind inference

module Stuff where

import Parser
import qualified Data.Set as S

----------------------------------------
-- Some Examples of Type Declarations --
----------------------------------------

--list :: Decl AlgType
--list = decl [declExp| list = all a:* . 1 + (a * (list a)) |]

binarytree :: Decl AlgType
binarytree = decl [declExp| binarytree = all a : * . 1 + ((binarytree a) * a * (binarytree a)) |]

rosetree :: Decl AlgType
rosetree = decl [declExp| rosetree = all a : * . a * (list (rosetree a)) |]

-- these seem to be the canonical example of mutually recursive datatypes
  
tree :: Decl AlgType
tree = decl [declExp| tree = all a : * . 1 + ( a * forest a ) |]

forest :: Decl AlgType
forest = decl [declExp| forest = all a : * . 1 + ( (tree a) * (forest a) )  |]

-- and this is my "hairy example", worked out by hand. I think it's a good test case.

typeF :: Decl AlgType
typeF = decl [declExp| f = (all a:* . 1 + ((f a) * (h a))) |]

typeG :: Decl AlgType
typeG = decl [declExp| g = (all a:* . ((h a) * (g a)) + 1) |]

typeH :: Decl AlgType
 typeH = decl [declExp| h = (all a:* . (f a) * (g a))|]

treeForest = S.fromList [tree,forest]

fgh = S.fromList [typeF,typeG,typeH]

onlyList = S.singleton (list)

multi = S.fromList [typeF,typeG,typeH,tree,forest,binarytree,list,rosetree]


-------------------------------------------------  
-- Identifying Mutually Recursive Declarations --
-------------------------------------------------
  
-- list the free names in a type
free :: Ty -> S.Set String
free TUnit = S.empty
free (TVar (Ident name)) = S.singleton name
free (TProd t1 t2) = S.union (free t1) (free t2)
free (TSum t1 t2)  = S.union (free t1) (free t2)
free (TApp t1 t2)  = S.union (free t1) (free t2)
free (TAbs (Ident bound) _ t) = S.delete bound (free t)
free (TAll (Ident bound) _ t) = S.delete bound (free t)
free (TMu  (Ident bound) t) = S.delete bound (free t)

-- Problem: names in signature must be unique for this to work, we don't currently check this.
type Signature = S.Set Decl

prettySignature :: Signature -> IO ()
prettySignature sig = mapM_ (putStrLn . prettyDecl) (S.toList sig)

prettyTyExp :: TyExp -> String
prettyTyExp TUnit = "1"
prettyTyExp (TVar (Ident x)) = x
prettyTyExp (TProd t1 t2) = (prettyTyExp t1) ++ " * " ++ (prettyTyExp t2)
prettyTyExp (TSum  t1 t2) = (prettyTyExp t1) ++ " + " ++ (prettyTyExp t2)
prettyTyExp (TApp  t1 t2) = (prettyTyExp t1) ++ " " ++ (prettyTyExp t2)
prettyTyExp (TAll (Ident x) k t) = "(all " ++ x ++ ":" ++ (prettyKind k) ++ " . " ++ (prettyTyExp t) ++ ")"
prettyTyExp (TAbs (Ident x) k t) = "(lam " ++ x ++ ":" ++ (prettyKind k) ++ " . " ++ (prettyTyExp t) ++")"

prettyKindExp :: KindExp -> String
prettyKind KStar = "*"
prettyKind (KArr k1 k2) = (prettyKind k1) ++ "->" ++ (prettyKind k2)

prettyDecl :: Decl -> String
prettyDecl (TyDecl (Ident x) t) = x ++ " = " ++ (prettyTy t)

-- 'directReferences sig d'  is the set of declarations directly referred to by name in the definition of d.
directReferences :: Signature -> Decl -> S.Set Decl
directReferences sig (TyDecl _ t) = S.filter referredTo sig
  where
  referredTo :: Decl -> Bool
  referredTo (TyDecl (Ident name) _) = S.member name (free t)

-- If aDb means "a directly references b", then this is like the transitive closure.
-- i.e., aRb in case a references b. Then aDb => aRb, and (aRb & bRC) => aRc.
references :: Signature -> Decl -> S.Set Decl
references sig d = fix (references' sig) (S.singleton d)
  where
  references' :: Signature -> S.Set Decl -> S.Set Decl
  references' sig rs = S.foldr S.union
                               rs
                               (S.map (\d -> directReferences sig d) rs)

-- attempt to construct a fixed point of f by repeatedly applying it to x.
-- needless to say, this is exceedingly partial, by we'll use it responsibly.
fix :: (Eq a) => (a -> a) -> a -> a
fix f x = if f x == x then x else fix f (f x)

-- now, two types in a signature are mutually recursive in case they reference one another.
mutuallyRecursive :: Signature -> Decl -> Decl -> Bool
mutuallyRecursive sig d1 d2 = S.member d1 (references sig d2) && S.member d2 (references sig d1)

-- splitting a signature up into its component mutually recursive bits.
partitionMRec :: Signature -> [[Decl]] 
partitionMRec sig =
  if S.null sig then []
    else (S.toList mrec) : (partitionMRec rest)
  where
  decl = S.elemAt 0 sig
  (mrec,rest) = S.partition (\x -> mutuallyRecursive sig decl x) sig

----------------------------------------------------------------
-- Constructing Fixed Point Equations from a Type Declaration --
----------------------------------------------------------------

-- sub x t a is a[t/x], the result of substituting t for free occurrences of x in a.
sub :: String -> Ty -> Ty -> Ty
sub x t a = subExcept S.empty x t a
  where
  subExcept :: S.Set String -> String -> Ty -> Ty -> Ty
  subExcept bound x t a =
    if S.member x bound then a
    else case a of
           TUnit -> TUnit
           (TVar (Ident y)) -> if x == y then t else a
           (TProd t1 t2) -> TProd (subExcept bound x t t1) (subExcept bound x t t2)
           (TSum  t1 t2) -> TSum  (subExcept bound x t t1) (subExcept bound x t t2)
           (TApp  t1 t2) -> TApp  (subExcept bound x t t1) (subExcept bound x t t2)
           (TAbs (Ident y) k t') -> TAbs (Ident y) k (subExcept (S.insert y bound) x t t')
           (TAll (Ident y) k t') -> TAll (Ident y) k (subExcept (S.insert y bound) x t t')
           (TMu  (Ident y) t') -> TMu  (Ident y) (subExcept (S.insert y bound) x t t')

-- the type of fixed point equations.
type FPEquation = (String,[Ty] -> Ty)

--  for t1,...,tn (ti = Ti) -> (i , [t1,...,tn] \mapsto Ti)
fpEquations :: [Decl] -> [FPEquation]
fpEquations  ds = let allDeclNames :: [String]
                      allDeclNames = map (\(TyDecl (Ident x) _) -> x) ds
                  in
                      map (fpEquation allDeclNames) ds

fpEquation :: [String] -> Decl -> FPEquation
fpEquation names (TyDecl (Ident x) t) = 
  (x , (\subs -> foldr (\(x,y) a -> sub x y a) t (zip names subs)))

----------------------------------------------
-- Solving Systems of Fixed Point Equations --
----------------------------------------------

-- The symmetic form of the Bekic identity.
solveTwo :: (FPEquation,FPEquation) -> (Decl,Decl)
solveTwo ((t1,bigT1),(t2,bigT2)) = (d1,d2)
  where
  d1 :: Decl
  d1 = TyDecl (Ident t1)
              (TMu (Ident t1)
                   (bigT1 [(TVar (Ident t1))
                          ,(TMu (Ident t2)
                                (bigT2 [(TVar (Ident t1))
                                       ,(TVar (Ident t2))]))]))
  d2 :: Decl
  d2 = TyDecl (Ident t2)
              (TMu (Ident t2)
                   (bigT2 [(TMu (Ident t1)
                                (bigT1 [(TVar (Ident t1))
                                       ,(TVar (Ident t2))]))
                          ,(TVar (Ident t2))]))

-- N-ary version of the symmetric form of the Bekic identity. (This is tricky!)
solve :: [FPEquation] -> [Decl]
solve []  = []
solve [(t,bigT)] = [TyDecl (Ident t) (TMu (Ident t) (bigT [(TVar (Ident t))]))]
solve [e1,e2] = (\(a,b) -> [a,b]) (solveTwo (e1,e2))
solve es = map (uncurry solveOne) (zip [1..] es) 
  where
  solveOne :: Int -> FPEquation -> Decl
  solveOne i (ti,bigTi) =
    let subs :: [Ty]
        subs = map (\(TyDecl _ x) -> x)
                   (solve (map (fixArg i (TVar (Ident ti)))
                               (exclude ti)))
    in
      TyDecl (Ident ti) (TMu (Ident ti) (bigTi subs))
                        
  exclude :: String -> [FPEquation]
  exclude ti = filter (\(x,xT) -> x /= ti) es


-- fix the value of the nth argument of a term function.
fixArg :: Int -> Ty -> FPEquation -> FPEquation
fixArg n x (t,bigT) = ( t
                      , bigT . (\tys -> (take n tys) ++ [x] ++ (drop n tys)) )

-----------------------------------------------------------------
-- Finally, Getting Rid of the Mutual Recursion in a Signature --
-----------------------------------------------------------------
                      

-- this soon! (TODO)
inlineReferences :: Signature -> Signature
inlineReferences sig = undefined

-- some examples:


           
