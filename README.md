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