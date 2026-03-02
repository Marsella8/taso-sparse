Ok so the IR is currently cooked. Right now we represent it as a tree of expressions. Problems:
1. Some expressions have common subexpressions..
Consider:
(x, y tensors, s1, s2 scalar)
a = x @ y
b = a * s1
c = b * s2
d = b + c

with the current IR, we would represent the output d as:

d = (x @ y) * s1 + (x @ y) * s2

i.e. we have x @ y in 2 different places, even thoguh it's the same expression.

2. We might have multiple outputs. 

e.g.

src:

x = A@B
y = A@C

dst:

z = concat(B, C)
w = A@z
x' = split0(w)
y' = split1(w)

This is more self explanatory, right now we have Eq(a, b), but dont really have a way to express this mapping. One way is to just have a list of Eq instead, but idk its sus, since idk how to use this nicely when doing pattern matching (bc basically has to be such that all the eq are matched at the same time, and that the way you match one is dependent on hwo you match the others).

3. Uhh some other issues, like right now there is a awkward split between what values are just variables and which are concretized.

Also need to figure out how invertible the TASO substitutions are, i beleive that if you take them in the reverse direction, some of them generate fresh vairbales which might be cooked.



UPDATE:

ok I fixed this stuff, now rewrites look as follows:

tranpose(transpose(x)) -> x

becomes:

(rewrite 
    (graph 
        (asst (tensor s0) (transpose (tensor x))) 
        (asst (tensor s1) (transpose (tensor s0)))) 
    (graph) 
    (bimap ((tensor x) (tensor x))) 
    (bimap ((tensor s1) (tensor x)))
)

so:
First graph has 2 nodes: s0 = transpose(x) and s1 = transpose(s0)
Second graph is empty (since we are not binding any new variables)
Then the maps are: x -> x and s1 -> x (note that in this second graph, x is both input and output)


TODO:

change so that IR:
- has a 0 input operator which instantiates an input tensor
- 

## Week 8 Work Logs.

Weird substitution:

(substitution 
    (graph 
        (asst (tensor s0) (const-iconv (kernel2d k)))) 
        (asst (tensor out) (pool2d-avg (kernel2d k) (stride2d 1 1) same (tensor s0))) 
    (graph 
        (asst (tensor out) (const-pool (kernel2d k)))
    ) 
    (bimap) 
    (bimap ((tensor out) (tensor out)))
)

note that this substitution operates entirely on the constants (so like not tensors, meaning that as you can see from `(bimap)` it does not take any inputs). Also this substitution is the only one that was NOT present in the original paper, so we can discard it?

THere is this weird asymmetry between:
- All the exprs are tensors, but
- When matching, we have to map each variable to each other variable.

So like why say for something like Scalar, don't we also need to store the entire expression and instead just have it as a (possibly nested) arguent to one of the tensor exprs? Because there is never a case in which a scalar that is not a var is shared by 2 expressions, so we dont really need to store it as a DAG: storing it as a tree suffices.