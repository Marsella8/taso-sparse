# Trace findings: why the 53 failing substitutions don't match

We traced several failing substitution indices using `cabal run taso-sparse -- --trace N`.

## Summary

- **Match criterion**: The main program considers a substitution "matched" when the **intersection** of (graphs reachable from src) and (graphs reachable from dst) is non-empty, using **exact graph equality** (`Eq` on `Graph`).
- **Observation**: For all traced failures we also found **zero** graphs in either reachable set that are **isomorphic** to the other side. So the issue is not only "same structure, different names" — the search never reaches a graph with the same structure as the target.

## Index 0

- **src (canonical)**: Two Conv2D nodes (same kernel 1×3, stride, pad), each with two Inputs; two outputs.
- **dst (canonical)**: Split0(axis1) → Conv2D → Concat(axis0) with Inputs; different topology (split/concat around one conv).
- **Why it fails**: The transformation is "two convs with shared inputs → split / one conv / concat". The forward search from src never produces the dst graph. Either the axiom that performs this rewrite is not in `allSubs`, or it is never applied in the allowed depth/steps, or the matcher does not fire on the graphs we generate.

## Index 73

- **src**: MatMul(Mul(Input, scalar), Input), second output EwMul(Input, Mul(Input, scalar)).
- **dst**: MatMul(Input, Mul(Input, scalar)), second output EwMul(Input, Mul(Input, scalar)).
- Structurally symmetric (which input gets the Mul). Still **0** graphs in either set isomorphic to the other.
- **Why it fails**: Either the equivalence (moving the scalar Mul to the other input of MatMul) is not implemented as an axiom, or it is not reached within the current search limits.

## Index 147

- **src**: Split0(axis0), Split1(axis0), Concat(axis1) around Transpose(Concat(axis1)(Input, Input)).
- **dst**: Same DAG shape but **Split0(axis1), Split1(axis1), Concat(axis0)**.
- **Why it fails**: The two graphs differ only in **axis literals** (0 vs 1). So this is an "axis swap" rule: same structure, different axis parameters. We have no graph in either set that is equal or isomorphic to the other, so either we lack an axiom that swaps axes in this pattern, or the encoding of axes prevents the search from equating them.

## Index 549

- **src**: EwMul(Input, Mul(ConstPool(kernel), scalar)) — elementwise product of input with a scaled constant pool.
- **dst**: Mul(Input, scalar) — input scaled by scalar only.
- **Why it fails**: The rule simplifies "EwMul(Input, Mul(ConstPool, s))" to "Mul(Input, s)". That simplification is either not in `allSubs` or not applied within the search.

## Conclusions

1. **No isomorphism-only gap in the traced cases**: Using isomorphism instead of equality for the intersection would not fix these: we never see a graph in one set that is isomorphic to the other. So the 53 failures are not due to canonicalization/name differences alone.
2. **Likely causes**:
   - **Missing axioms**: Some TASO rules may not be implemented in `allSubs` (e.g. axis swap, scalar motion, or conv → split/concat).
   - **Search limits**: Depth 4 and 500 steps may be too low for some derivations.
   - **Matching/application**: Some axioms might not match or apply on the graphs we produce (e.g. parameter or shape conditions).

## How to trace more

Run:

```bash
cabal run taso-sparse -- --trace <index>
```

where `<index>` is one of the failed indices from `failure-report-indices.txt`. The trace prints exact vs isomorphism checks and the first few bindings of the canonical src and dst graphs.
