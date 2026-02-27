## Pietro

So TASO right now does:
Have a set of manually generated axioms (which we sanity check by verifying with symbolic execution over small tensors)
Generate all graphs up to a certain size, compute fingerprints, and from this get a set of candidate substitutions.
Use an SMT solver to verify that these substitutions are correct by entailment of the axioms.
    (plus some other stuff like deduplication etc...)
Instead you propose:

For the same set of axioms, show that using these as axioms as rewrites (bidirectionally, plus some ways to do parameter instantiation for specific matrices), should ideally allow us to derive all the substitutions that TASO discovered.
This proves that TASO searching for substitutions with fuzzing is superfluous (though, as Zhihao had told Colin, part of the reason for the fuzzing is to discover the axioms themselves). But I would argue that you can still have the fuzzing, but this is only used to discover new axioms, and that once we have a large enough set of axioms, we can find (an unlimited amount) of substitutions. So we can then do bounded closure, and get all the possible substitutions under some max size (this max size should be larger than the TASO one, since with TASO we were enumerating all graphs up to a certain size, but they are not guaranteed to have a valid corresponding substitution, unlike here where for a given base graph each new graph we are generating is valid substitution for that graph). We then do deduplication of all these substitutions the same way TASO does, and this is our new set of hopefully much larger substitutions, which we can them use normally (I guess why not apply directly the axioms as substitutions and skip the "generating more complex substitutions" entirely? Suppose it would be to slow, since the axioms are too atomic of a rewrite). Interesting extension once this is done is to find a sparsifier of this substitution space, where we take only a small subset of these substitutions we found and show that any substitution in the original set is the composition of at most, say 3, compositions in our reduced set.

## Proposal

CS 257 Project Proposal (Option 2)
Pietro Marsella (marsella@stanford.edu)
Bodo Wirth (bodow@stanford.edu)
Background
As machine learning models grow more complex and compute-intensive, manually designing parallelization
schedules has become increasingly difficult. Machine learning compilers address this by automatically improving model performance with minimal human intervention. A substantial part of this optimization is
algebraic substitution: given a computation graph, the compiler repeatedly applies semantics-preserving
algebraic rewrites to produce an equivalent graph with lower execution cost.
Historically, these substitutions were handcrafted by human engineers. Recent systems—such as TASO—aim
to generate and formally validate graph substitutions automatically. [1]
1. Establish a base set of operators.
2. Hand-design a set of axioms for these operators, expressed as first-order logic constraints.
3. Randomly generate the set of all graphs obtainable by composing these operators up to a chosen size
bound. Call this set G.
4. Fuzz each graph and partition G into equivalence classes according to a fingerprint of the graph’s
output behavior.
5. For each pair of graphs in the same equivalence class, use an SMT solver to formally prove (from the
axioms) that substituting one graph for the other is sound.
The resulting proven substitutions can then be plugged into any graph-substitution engine (e.g., greedy/beamsearch rewriting, e-graphs, . . . ) and used to optimize a given input graph.
Problem
We would like to better understand whether it is possible to fully recover all discovered substitutions by
defining a theory over the operators, casting it as a rewrite system, and performing bounded rewriting to
enumerate all graphs reachable within a fixed number of substitutions, up to a chosen size bound. We would
also employ an SMT solver to ensure that our rewrite system is sound. This would significantly speed up
the discovery of valid substitutions, since each additional rewrite would generate a valid substitution. We
believe this characterization of possible substitutions to be more elegant than discovering them with fuzzing.
Once we have this set of substitutions, a possible follow-up is to compute a sparsifier of this set, which
would allow us to find a much smaller subset S
′ ⊂ S while still ensuring that each substitution in S is the
composition of at most (say) two substitutions in S
′
.
Goals
Develop a rewrite system for the set of operators present in the TASO paper.
Recover all originally discovered substitutions in the original TASO paper (and hopefully many more), while
taking significantly less time.
Compute a sparsifier for this set.
Resources
No particular resources are needed. We could optionally use part of the TASO harness to optimize a model
under our expanded substitution set, though this is not the main focus of the project.
Timeline and task assignments
1. Project progress review (due Feb. 27, 2026): Extract TASO substitutions and formalize rewrite
rules, implement graph enumeration and bounded rewriting.
2. Before the final report and artifact: Evaluation plan and experimental write-up; Implement
sparsification and evaluate it.
References
[1] Zhihao Jia, Yuanhao He, Dongdong Li, et al. TASO: Optimizing Deep Learning Computation with
Automatic Generation of Graph Substitutions. In Proceedings of the 27th ACM Symposium on Operating
Systems Principles (SOSP ’19), 2019.

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

