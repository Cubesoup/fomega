* Term Expansion When Removing Mutual Recursion

Individual tests have been plotted as points, with the X coordinate being the size (in number of constructors required to represent) of the system of mutually recursive equations under consideration, and the Y coordinate being the size of the system once mutuall recursion has been removed. 

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

** Random Tests

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
