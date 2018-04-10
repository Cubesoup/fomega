* Term Expansion When Removing Mutual Recursion


A system of N mutually recursive types t1,t2,...,tN can be thought of as a system of fixed point equations

t1 = T1(t1,t2,...,tN)

t2 = T2(t1,t2,...,tN)

...

tN = TN(t1,t2,...,tN)

and from such a system of fixed point equations we can construct an NxN matrix

t1,1 t1,2 ... t1,N

t2,1 t2,2 ... t2,N

...

tN,1 tN,2 ... tN,N

where ti,j is 1 in case tj occurs in T1(t1,t2,...,tN), and is 0 otherwise. Conversely, from such a matrix, we can construct a system of N types that would yield the matrix. For example, if the matrix is

1 1 0 1

0 1 0 0

0 1 1 0

1 0 1 1

then the system 

t1 = t1 + t2 + 1  + t4

t2 = 1  + t2 + 1  + 1

t3 = 1  + t2 + t3 + 1

t4 = t1 + 1  + t3 + t4

yields that matrix. The main thing determining how large the representation of a system of types becomes when mutual recursion is removed seems to be this matrix. 

** Deterministic Tests

Individual tests have been plotted as points, with the X coordinate being the size (in number of constructors required to represent) of the system of mutually recursive equations under consideration, and the Y coordinate being the size of the system once mutuall recursion has been removed. 

The worst case considered is the case in which the matrix consists entirely of 1. Plotting the result of removing mutual recursion from the system corresponding to this matrix of size 1..10 yields: 

![worst-case](/results/worst-case.png)

The shape of the graph tells us two things. First, the worst case for a system of 10 types is gigantic (about size 1.9*10^8, with initial size ~180). Second, the function this graph plots is growing absurdly fast (the little dip is caused by the line being a spline. The function never decreases).

The worst case probably never occurs in practice. I've certainly never seen a system of 10 mutually recursive types in which each type directly references each other type in the wild. It also makes sense to consider the system in which ti refers directly to ti+1, and tN refers to t1, creating a cycle. This is one of the sparser systems in which every type refers to every other type. Plotting this system for (N in [1..20]) types yields:

![small-nontrivial](/results/small-nontrivial.png)

It looks like a polynomial function. This isn't terrible.

As a sort of control, I've also plotted the system where each type refers only to itself (no mutual recursion) for (N in [1..20]) types:

![trivial](/results/trivial.png)

It seems to be growing linearly, as expected. (It isn't quite Y=X, but it's very, very close). 

** Random Tests

Given p a value between 0 and 1, we can easily generate an NxN matrix in which each entry is 1 with probability p, and is 0 with probability (p-1). (p is now something like the "density" of the generated matrix). For p in [10,20,30,..,90], I've used such matrices to generate 100 systems of N types for N in [1..9], and plotted the result as before. 

While the curve looks more or less the same across all densities tested (becoming slightly steeper at higher density), and the X-scale remains the same, the Y-scale grows very quickly. That a randomly generated system (density 50) with initial size ~150 has size ~500000 is worrying. 

![100 random systems of size 1..9 with density 10](/results/density-10.png)

![100 random systems of size 1..9 with density 20](/results/density-20.png)

![100 random systems of size 1..9 with density 30](/results/density-30.png)

![100 random systems of size 1..9 with density 40](/results/density-40.png)

![100 random systems of size 1..9 with density 50](/results/density-50.png)

![100 random systems of size 1..9 with density 60](/results/density-60.png)

![100 random systems of size 1..9 with density 70](/results/density-70.png)

![100 random systems of size 1..9 with density 80](/results/density-80.png)

![100 random systems of size 1..9 with density 90](/results/density-90.png)
