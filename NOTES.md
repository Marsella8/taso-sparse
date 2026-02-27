## Pietro

So TASO right now does:
Have a set of manually generated axioms (which we sanity check by verifying with symbolic execution over small tensors)
Generate all graphs up to a certain size, compute fingerprints, and from this get a set of candidate substitutions.
Use an SMT solver to verify that these substitutions are correct by entailment of the axioms.
    (plus some other stuff like deduplication etc...)
Instead you propose:

For the same set of axioms, show that using these as axioms as rewrites (bidirectionally, plus some ways to do parameter instantiation for specific matrices), should ideally allow us to derive all the substitutions that TASO discovered.
This proves that TASO searching for substitutions with fuzzing is superfluous (though, as Zhihao had told Colin, part of the reason for the fuzzing is to discover the axioms themselves). But I would argue that you can still have the fuzzing, but this is only used to discover new axioms, and that once we have a large enough set of axioms, we can find (an unlimited amount) of substitutions. So we can then do bounded closure, and get all the possible substitutions under some max size (this max size should be larger than the TASO one, since with TASO we were enumerating all graphs up to a certain size, but they are not guaranteed to have a valid corresponding substitution, unlike here where for a given base graph each new graph we are generating is valid substitution for that graph). We then do deduplication of all these substitutions the same way TASO does, and this is our new set of hopefully much larger substitutions, which we can them use normally (I guess why not apply directly the axioms as substitutions and skip the "generating more complex substitutions" entirely? Suppose it would be to slow, since the axioms are too atomic of a rewrite). Interesting extension once this is done is to find a sparsifier of this substitution space, where we take only a small subset of these substitutions we found and show that any substitution in the original set is the composition of at most, say 3, compositions in our reduced set.


## Colin

I won't have time to run over to CoDa, but I went through your (i.e.,
Pietro's) proposal and left some comments. Overall I agree with most of
it (except one place you lost me in a rather long sentence), I just
would make sure to clarify a bunch of the intermediate steps--right now
I think your current paragraph is contingent on a couple
details/hypotheses that, while definitely possible, we'd want to verify
first.

> For the same set of axioms, show that using these as axioms as
> rewrites (bidirectionally, plus some ways to do parameter
> instantiation for specific matrices), should ideally allow us to
> derive all the substitutions that TASO discovered.

I think an important detail to keep in mind here is that the ability to
(efficiently) represent a solution space through graph substitutions is
not necessarily a given. When we model a search space, we usually
represent it as a set of opaque points with moves between these points.

For TASO, of course, our points are operator graphs, but there's nothing
in the base model of the search space that mandates any topological
aspect to the set of moves available: it would be perfectly fine to have
a move (say, fuse two convolutions) available only if some completely
irrelevant condition is true (say, the number of nodes in the whole
graph is odd). The fact that we can encode the set of possible moves
entirely through a small set of graph substitutions gives substantial
structure to the solution space: if a subgraph matches the pattern of
the graph substitution, then the move is allowed regardless of the
structure of the rest of the graph.

As such, when we're considering equivalence between different search
spaces (say, S and S'), there are a couple different notions to
consider:

(1) Given a graph G and a move sequence r = (r_1, ..., r_n) in S,
    there exists a move sequence r' = (r_1', ..., r_m') in S' such that
    r(G) = r'(G).
(2) Given a move sequence r = (r_1, ..., r_n) in S, there exists a move
    sequence r' = (r_1', ..., r_m') in S' such that for any graph G,
    r(G) = r'(G).
(3) Assuming that the move set of S can be encoded in a set of graph
    substitutions D, and that the move set of S' can be encoded in a
    different set of graph substitutions D', then given a graph
    substitution in D, there exists a "composition" (for some notion of
    composition) of substitutions in D' equal to D.

Note that (3) => (2) => (1), but that it's not trivially true that the
implication goes the other way.

> This proves that TASO searching for substitutions with fuzzing is
> superfluous

The ability to replace TASO's substitution set with a reduced
substitution set only works if (3) holds I think. Note that I do agree
that it's likely that (3) holds, but I just want to emphasize that there
are a couple different notions of equivalence here that have impacts on
how they could/would get integrated into FlexFlow.

> (I guess why not apply directly the axioms as substitutions and skip
> the "generating more complex substitutions" entirely? Suppose it would
> be to slow, since the axioms are too atomic of a rewrite).

I wouldn't immediately throw out this option until we're sure it
actually slows things down, as it would substantially decrease the
complexity of the system. I think once we have shown that (3) holds for
the direct axiom->substitution representation of the space, that we'll
want to actually do a performance comparison between the two.

> (though, as Zhihao had told Colin, part of the reason for
> the fuzzing is to discover the axioms themselves).

Note that I don't buy this argument of Zhihao's, as assuming that the
axioms can be represented equivalently as graph substitutions, it seems
like you could use the same procedure TASO uses to identify candidate
substitutions to instead identify axioms--you'd just have to add some
logic to prune candidate axioms that are entailed by other candidate
axioms to make the axiom space minimal.

> But I would argue that you can still have the fuzzing, but this is
> only used to discover new axioms,

Yup!

> and that once we have a large enough set of axioms, we can find (an
> unlimited amount) of substitutions.

I suppose this is true, though I think it's only useful if we determine
that we actually need bigger substitutions to make rewriting efficient
(but I agree that in that case, yes, you could use the composition
procedure from (3) to generate these bigger substitutions from the
axiom set).

> So we can then do bounded closure, and get all the possible
> substitutions under some max size (this max size should be larger than
> the TASO one, since with TASO we were enumerating all graphs up to a
> certain size, but they are not guaranteed to have a valid
> corresponding substitution, unlike here where for a given base graph
> each new graph we are generating is valid substitution for that
> graph).

Tbh you lost me here.

> We then do deduplication of all these substitutions the same way TASO
> does, and this is our new set of hopefully much larger substitutions,
> which we can them use normally.

A procedure for more efficiently generating these compositions is I
suppose one possible outcome here, but I wouldn't overemphasize it--if
we can use a smaller substitution set to get the same results, that's
very much a win as it means we can simplify the system a bunch.

> Interesting extension once this is done is to find a sparsifier of
> this substitution space, where we take only a small subset of these
> substitutions we found and show that any substitution in the original
> set is the composition of at most, say 3, compositions in our reduced
> set.

Definitely. One other potential extension is if you can further shrink
the verification base (identify some specification of the properties of
the operators that lets you prove the axioms)?

