# Manual derivation: LHS → RHS for 10 failing substitutions

For each of 10 failing substitution indices we describe the LHS (src) and RHS (dst), then trace how our axioms/lemmas could be applied to go LHS → RHS (or why they cannot).

Notation: `conv` = Conv2D, `a0`/`a1` = axis 0/1, `inp` = Input.

---

## Index 0

**LHS:** Two outputs. Out1 = conv(1×3)(c2, c3), Out2 = conv(1×3)(c2, c6). Shared first input c2, two different second inputs.

**RHS:** Two outputs. Out1 = split0(axis1)(conv(1×3)(c3, concat(a0)(c5,c6))), Out2 = split1(axis1)(same conv). So one conv with concat(axis0) on the weight side, then split on axis 1 to get two outputs.

**Manual derivation LHS → RHS:**

1. **Conceive LHS as one concat:** The two outputs can be seen as concat(axis1)(conv(c2,c3), conv(c2,c6)). We do not have an axiom that “two outputs = concat of two expressions” as a single node, so we must work from the RHS or from a version of LHS that already has a concat.

2. **RHS → LHS (reverse):** RHS has one conv with weight concat(a0)(c5,c6), and split0/split1 give two outputs. So the unsplit conv output is conv(c3, concat(c5,c6)).
   - **Axiom39 inverse** (in bwdSubs): conv(x, concat(axis0)(y,z)) → concat(axis1)(conv(x,y), conv(x,z)). Apply to conv(c3, concat(c5,c6)) → concat(axis1)(conv(c3,c5), conv(c3,c6)).
   - We now have one output that is concat(axis1)(A, B). To get two outputs A and B we need split0(axis1) and split1(axis1). **Axiom28/29**: split0(concat(a)(x,y))=x, split1(concat(a)(x,y))=y. So adding split0 and split1 on axis 1 to that concat yields the two convs. So RHS → LHS is: axiom39 inverse, then axiom28 and 29 (split of concat).

**Conclusion:** The derivation exists (RHS→LHS using axiom39 inverse then 28/29). LHS→RHS would be: first “merge” two outputs into one concat (we have no axiom that creates a concat from two separate outputs), then axiom39 forward. So **LHS→RHS is blocked** by the lack of an axiom “two outputs f, g → one output concat(axis1)(f, g)”. The matcher may also expect a single concat node that we never build from two separate outputs.

---

## Index 2

**LHS:** Same shape as index 0: two conv(1×3)s, shared first input (c2), different second inputs (c3, c6). Two outputs.

**RHS:** Same idea as index 0 but axis 0 instead of 1: split0(axis0)(conv(1×3)(c3, concat(a0)(c4,c5))), split1(axis0)(same). Concat and split on axis 0.

**Manual derivation:** Same as index 0. **Axiom39** handles axis1 concat of convs; here the RHS uses axis0. We have **lemmaConvConcatInput** for axis1: conv(x,z), conv(y,z) with concat(axis1)(s0,s1) → conv(concat(axis1)(x,y), z). So we batch the *first* input. For index 2 the batched side is the *second* (weight) input and the split is on axis 0. So we need either an axis0 variant of the “conv weight concat then split” rule or the same reasoning as index 0 with axis0. **Conclusion:** Same as index 0; axis0 variant of conv+concat+split may be missing or not applied.

---

## Index 5

**LHS:** Two conv(3×3)s with shared first input (c2), different second inputs (c3, c6). Two outputs.

**RHS:** split0(axis1)(conv(3×3)(c3, concat(a0)(c5,c6))), split1(axis1)(same). Same pattern as index 0 with 3×3 kernel.

**Manual derivation:** Identical to index 0 with kernel 3×3. **Conclusion:** Same as index 0; derivation RHS→LHS exists via axiom39 inverse + 28/29; LHS→RHS blocked by needing to form one concat from two outputs.

---

## Index 8

**LHS:** Two outputs: pool2dAvg(3×3)(c2), pool2dAvg(3×3)(c5). Two separate pools on two inputs.

**RHS:** split0(axis0)(pool2dAvg(3×3)(concat(a0)(c4,c5))), split1(axis0)(same). One pool on concat(axis0)(c4,c5), then split on axis 0.

**Manual derivation LHS → RHS:**

- We have **lemmaPool2dAvgConcatAxis0**: pool2dAvg(x), pool2dAvg(y) with out = concat(axis0)(s0,s1) → pool2dAvg(concat(axis0)(x,y)). So two pools with concat(axis0) on outputs → one pool on concat(axis0)(x,y).
- So we need LHS to be “concat(axis0)(pool(c2), pool(c5))” and then apply the lemma to get pool(concat(c2,c5)), then split0/split1 to get two outputs. So LHS→RHS: (1) Form one output concat(axis0)(pool(c2), pool(c5)) — we have no axiom that “two outputs → one concat output”. (2) Lemma forward: pool(concat(c2,c5)). (3) Split: split0/split1 to get two outputs. So again **blocked** by the need to turn two outputs into one concat. RHS→LHS: we have one output split0(pool(concat)), split1(pool(concat)). The “merged” output is pool(concat(c4,c5)). Lemma inverse: pool(concat(x,y)) → concat(axis0)(pool(x), pool(y)). So we get one output concat(axis0)(pool(c4), pool(c5)). Then we need to “split” that into two outputs — again we need split0(concat)=first, split1(concat)=second (axiom28/29 style but for axis0). We have axiom28: split0(concat(a)(x,y))=x for any axis. So RHS→LHS is: lemma inverse, then axiom28 and 29. **Conclusion:** Same structural issue: LHS→RHS needs “two outputs → one concat”; RHS→LHS is derivable.

---

## Index 73

**LHS:** Two outputs. Out1 = MatMul(c2, c4) with c2 = Mul(c3, s20), c3 = Input, c4 = Input. Out2 = EwMul(c4, c2). So MatMul(Mul(In1,s), In2) and EwMul(In2, Mul(In1,s)).

**RHS:** Out1 = MatMul(c2, c3) with c2 = Input, c3 = Mul(c4, s20), c4 = Input. Out2 = EwMul(c2, c3). So MatMul(In1, Mul(In2,s)) and EwMul(In1, Mul(In2,s)). Same structure with the scalar Mul on the other MatMul input.

**Manual derivation:** This is “scalar Mul commutes from left to right argument of MatMul” (or vice versa). We have **axiom14a**: MatMul(x,y), out = Mul(matMul(x,y), w) → MatMul(x, Mul(y,w)) (mul on right). **axiom14b**: mul on left: MatMul(x,y) * w → MatMul(Mul(x,w), y). So LHS has MatMul(Mul(c3,s), c4) = MatMul(Mul(In1,s), In2). Axiom14b says MatMul(x,y)*w → MatMul(Mul(x,w), y). So if we had Mul(MatMul(c3,c4), s) we could get MatMul(Mul(c3,s), c4). So the inverse of 14b would take MatMul(Mul(c3,s), c4) → Mul(MatMul(c3,c4), s). So from LHS we could apply axiom14b *inverse*: MatMul(Mul(In1,s), In2) → Mul(MatMul(In1,In2), s). Then we’d have one output Mul(MatMul(In1,In2), s). To get RHS we need MatMul(In1, Mul(In2,s)). That’s axiom14a: Mul(MatMul(x,y), w) → MatMul(x, Mul(y,w)). So LHS → … → Mul(MatMul(In1,In2), s) → MatMul(In1, Mul(In2,s)) = RHS’s first output. But we also have a second output EwMul. So the graph has two outputs; the rewrite might be occurrence-local or we need to rewrite both. **Conclusion:** Axiom14b inverse then axiom14a could justify the MatMul part. The second output (EwMul) is the same structure with inputs swapped; we might need a similar scalar motion or the matcher may not apply because of the two-output structure.

---

## Index 106

**LHS:** One output: EwAdd(c2, c3) with c2 = Input, c3 = MatMul(c4, c2). So EwAdd(In1, MatMul(In2, In1)) = In1 + MatMul(In2, In1).

**RHS:** One output: MatMul(c2, c5) with c2 = EwAdd(c3, c4), c3 = Input, c4 = ConstImm, c5 = Input. So MatMul(In1 + ConstImm, In2).

**Manual derivation:** LHS is x + MatMul(y, x). RHS is MatMul(x + ConstImm, z). So we need to relate x + MatMul(y, x) to MatMul(x + 1, z). That is a non-trivial identity (e.g. when y is identity-like or under special conditions). We have **axiom15**: MatMul(x, EwAdd(y,z)) → EwAdd(MatMul(x,y), MatMul(x,z)). So MatMul(x, y+z) = MatMul(x,y)+MatMul(x,z). So RHS = MatMul(x+ConstImm, z). We don’t have an axiom that says x + MatMul(y,x) = MatMul(x+ConstImm, z) in general. **Conclusion:** No obvious axiom sequence; likely **missing axiom** or a composite identity we don’t encode.

---

## Index 147

**LHS:** Two outputs. Out1 = split0(axis0)(Transpose(Concat(axis1)(c4,c5))), Out2 = split1(axis0)(same). So Transpose(Concat(axis1)(In1, In2)), then split on axis 0.

**RHS:** Same DAG shape but **axis 0 and 1 swapped**: split0(axis1)(Transpose(Concat(axis0)(c4,c5))), split1(axis1)(same). So Transpose(Concat(axis0)(In1, In2)), then split on axis 1.

**Manual derivation:** So the only difference is axis literals: LHS uses (split axis 0, concat axis 1); RHS uses (split axis 1, concat axis 0). We have **axiom35**: Transpose(x), Transpose(y), out = Concat(axis1)(s0,s1) → Concat(axis0)(x,y), then Transpose. So transpose distributes over concat with axis swap. And **lemmaTransposeConcatAxis0**: Concat(axis0)(transpose(x), transpose(y)) → Transpose(Concat(axis1)(x,y)). So we have axis-swap lemmas for transpose+concat. We do **not** have an axiom that says “split0(axis0)(Transpose(Concat(axis1)(…))) ↔ split0(axis1)(Transpose(Concat(axis0)(…)))” (same structure, different axis constants). So this is **parameter sensitivity**: the rule is “axis swap” for this pattern. **Conclusion:** **Missing axiom**: same graph shape with axis 0↔1 swapped; need an axiom that swaps axis parameters in this split/transpose/concat pattern, or a parameter-agnostic formulation.

---

## Index 236

**LHS:** One output: Relu(EwAdd(c3, conv(3×3)(c3, c5))). So Relu(In1 + conv(In1, In2)).

**RHS:** One output: conv(3×3, ActRelu)(c2, c3) with c2 = Input, c3 = EwAdd(c4, c5), c4 = Input, c5 = ConstIConv(3×3). So Conv(ActRelu)(In1, In2 + ConstIConv).

**Manual derivation:** We have **axiom22**: conv(relu) → relu(conv) (Conv+Relu fusion: conv(actRelu)(x,y) = relu(conv(actNone)(x,y))). So relu(conv(x,y)) ↔ conv(relu)(x,y). LHS is relu(ewAdd(In1, conv(In1, In2))). RHS is conv(actRelu)(In1, EwAdd(In2, ConstIConv)). So RHS = relu(conv(In1, In2 + ConstIConv)). We have **axiom25**: conv(x, ConstIConv) → identity (output = x). So conv(In1, ConstIConv) is “enlarge” or identity-like. And **lemmaConstIConvLeft**: conv(ConstIConv, y) → enlarge(y). So In2 + ConstIConv in the second argument is unusual; we’d need to reason about conv(In1, EwAdd(In2, ConstIConv)). We don’t have an axiom that fuses relu(ewAdd(In1, conv(In1, In2))) to conv(actRelu)(In1, In2+ConstIConv). **Conclusion:** Relu–Conv fusion (axiom22) and ConstIConv lemmas don’t directly give this; likely **missing axiom** or a more complex fusion (add then conv with ConstIConv).

---

## Index 514

**LHS:** One output: conv(1×3, ActRelu)(c2, c5) with c2 = EwAdd(c3, c4), c3 = Input, c4 = Mul(c3, s20), c5 = Input. So conv(ActRelu)(In1 + Mul(In1,s), In2) = conv(ActRelu)(In1*(1+s), In2).

**RHS:** One output: conv(1×3, ActRelu)(c2, c3) with c2 = Input, c3 = EwAdd(c4, c5), c4 = Input, c5 = Mul(c4, s20). So conv(ActRelu)(In1, In2 + Mul(In2,s)) = conv(ActRelu)(In1, In2*(1+s)).

**Manual derivation:** So LHS has the scaled-add on the *first* conv input; RHS has it on the *second*. We have **axiom19**: conv(x, EwAdd(y,z)) → EwAdd(conv(x,y), conv(x,z)). So conv(x, y+z) = conv(x,y)+conv(x,z). So scaling/distribution on the second input is expressible. We don’t have an axiom that moves “EwAdd with Mul” from first to second conv input. So this would require something like “conv(EwAdd(In1, Mul(In1,s)), In2) ↔ conv(In1, EwAdd(In2, Mul(In2,s)))”, which is a non-trivial equality (and would depend on kernel shape). **Conclusion:** **Missing axiom**: scalar/add structure on first vs second conv input.

---

## Index 549

**LHS:** One output: EwMul(c2, c3) with c2 = Input, c3 = Mul(c4, s20), c4 = ConstPool(3×3). So EwMul(In, Mul(ConstPool, s)).

**RHS:** One output: Mul(c2, s20), c2 = Input. So Mul(In, s).

**Manual derivation:** So LHS is In * (ConstPool * s) and RHS is In * s. So the rule is “EwMul(In, Mul(ConstPool, s)) → Mul(In, s)” — i.e. drop the ConstPool when it’s scaled and elementwise-multiplied with input. We have **axiom24**: conv(x, ConstPool(k)) → pool2dAvg(k)(x). So ConstPool is related to pool. We don’t have an axiom that EwMul(Input, Mul(ConstPool, scalar)) simplifies to Mul(Input, scalar). **Conclusion:** **Missing axiom**: simplification of EwMul(Input, Mul(ConstPool, s)) to Mul(Input, s).

---

## Summary table

| Index | LHS → RHS possible with our axioms? | Main issue |
|-------|-------------------------------------|------------|
| 0     | No (RHS→LHS yes)                    | Need “two outputs → one concat” then axiom39; or matcher doesn’t build concat from two outputs. |
| 2     | No                                 | Same as 0; axis0 variant. |
| 5     | No                                 | Same as 0; 3×3 kernel. |
| 8     | No (RHS→LHS yes)                   | Same as 0; pool instead of conv; lemmaPool2dAvgConcatAxis0. |
| 73    | Plausible                           | Axiom14b inverse + 14a for MatMul; second output EwMul may block or need same motion. |
| 106   | No                                  | No axiom for x + MatMul(y,x) ↔ MatMul(x+ConstImm, z). |
| 147   | No                                  | Axis 0↔1 swap; same shape, different axis literals. |
| 236   | No                                  | Relu(EwAdd+conv) ↔ conv(ActRelu)(In1, EwAdd+ConstIConv) not covered. |
| 514   | No                                  | Moving EwAdd+Mul from first to second conv input. |
| 549   | No                                  | EwMul(In, Mul(ConstPool,s)) → Mul(In,s) not present. |

**Recurring themes:**

1. **Two outputs vs one concat** (0, 2, 5, 8): We have “concat of two ops → one op (then split)” (e.g. axiom39, lemmaPool2dAvgConcatAxis0) but no way to turn two separate outputs into one concat node, so the forward direction (LHS→RHS) is never triggered.
2. **Axis parameter swap** (147): Same graph shape with axis 0 and 1 swapped; we’d need an axis-swap axiom or parameter-agnostic matching.
3. **Missing equalities** (106, 236, 514, 549): Specific identities or simplifications not present in the current axiom set.
4. **Scalar/MatMul motion** (73): Partially covered by axiom14a/14b; may need occurrence-local or multi-output handling.
