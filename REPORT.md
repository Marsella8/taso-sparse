# TASO Miss Taxonomy

Current baseline:

- `cabal run -- +RTS -M5G -N1 -RTS`: `420 / 568`
- remaining misses: `148`

Ordered by severity. Each item gives:

- exact affected count
- exact blocker
- minimal reproducer
- minimal fix

## 1. Local Rewriting Under Fanout

- count: `78`
- blocker:
  `src/Substitutions/Apply.hs:35-36` rejects a rewrite if deleting the matched image would leave any surviving context reference. This means rewrites are whole-image, not occurrence-local.
- minimal reproducer:
  existing test:
  `search: inverse double-transpose insertion should be occurrence-local under fanout`
  in `test/SearchSpec.hs`
- concrete reproducer shape:
  source:
  `x, s0 = transpose x, out = ewadd x s0`
  target:
  `x, s0 = transpose x, d0 = transpose x, d1 = transpose d0, out = ewadd d1 s0`
- minimal fix:
  occurrence-local rewriting in `Apply`, rather than deleting the entire matched image and rejecting if any sibling use survives

## 2. Post-Rewrite CSE

- count: `8`
- blocker:
  `src/Substitutions/Apply.hs:68-72` always freshens destination internals and never merges structurally identical nodes afterward.
- minimal reproducer:
  existing test:
  `search: distributing transpose and commuting relu should be able to reuse a shared transpose node`
  in `test/SearchSpec.hs`
- concrete reproducer shape:
  source:
  `s0 = relu x, s1 = ewadd x s0, out = transpose s1`
  target:
  `d0 = transpose x, d1 = relu d0, out = ewadd d0 d1`
- minimal fix:
  canonical CSE after each rewrite, or a normalization pass that merges duplicate equal nodes

## 3. Internal Multi-Output Matching

- count: `25`
- blocker:
  `src/Substitutions/Match.hs:58,72` only lets multi-output source outputs anchor at top-level graph outputs, not at internal tensors.
- minimal reproducer:
  existing tests:
  `search: concat followed by split0/split1 should be mutually reachable with two independent outputs`
  and
  `search: two relu outputs should be packageable into concat -> relu -> split`
  in `test/SearchSpec.hs`
- concrete reproducer shape:
  source:
  `x, y`
  target:
  `c0 = concat axis0 x y, o0 = split0 axis0 c0, o1 = split1 axis0 c0`
- minimal fix:
  a constrained internal multi-output matching mechanism that preserves which source outputs are externally visible.
  Important:
  the naive fix "let every multi-output source output anchor at any internal tensor" is wrong and overmatches badly.
  Any real fix must distinguish true internal packaged outputs from pass-through source outputs.

## 4. Hybrid `enlarge` Family

- count: `15`
- blocker:
  these are no longer plain “missing `1x3 -> 3x3` promotion” cases. They need one or more of:
  left-`ConstIConv` collapse, shared `enlarge(...)` reuse, or further DAG-sensitive packaging after promotion.
- minimal reproducer:
  minimal test to add:
  source:
  `s0 = conv2d 1x3 none x w, s1 = conv2d 1x3 relu x w, out = concat axis1 s0 s1`
  target:
  `d0 = enlarge 3x3 w, d1 = conv2d 3x3 none x d0, d2 = conv2d 3x3 relu x d0, out = concat axis1 d1 d2`
- minimal fix:
  not another generic `enlarge` axiom. The real fixes are:
  left-`ConstIConv` axioms plus CSE / occurrence-local rewriting

## 5. Grouped-Conv / Shape-Semantics Mismatch

- count: `8`
- blocker:
  TASO can emit substitutions whose validity depends on implicit group/shape behavior. The local IR has no explicit grouped-conv parameter and no shape side conditions.
- minimal reproducer:
  minimal test to add:
  source:
  `out = concat axis1 (conv x1 w1) (conv x2 w2)`
  target:
  `d0 = concat axis0 x1 x2, d1 = concat axis0 w1 w2, out = conv d0 d1`
- minimal fix:
  either model grouped convolution explicitly in the IR, or filter these substitutions out

## 6. Missing Left-`ConstIConv` Axioms

- count: `4`
- blocker:
  the current theory has the right-identity form but not the left-side collapse forms TASO uses.
- minimal reproducer:
  existing test:
  `search: conv2d(ConstIConv, y) should be reducible to enlarge(y)`
  in `test/SearchSpec.hs`
- concrete reproducer shape:
  source:
  `s0 = ConstIConv 3x3, out = conv2d 3x3 stride11 same none s0 y`
  target:
  `out = enlarge 3x3 y`
- minimal fix:
  add left-`ConstIConv` collapse axioms, starting with `actNone`, `stride11`, `padSame`, and then the lifted `relu` form

## 7. Missing `axis0` `pool2dAvg` Packaging

- count: `3`
- blocker:
  `axiom41` only packages `pool2dAvg` through `concat axis1`.
- minimal reproducer:
  minimal test to add:
  source:
  `s0 = pool2dAvg k s p x, s1 = pool2dAvg k s p y, out = concat axis0 s0 s1`
  target:
  `d0 = concat axis0 x y, out = pool2dAvg k s p d0`
- minimal fix:
  add the `axis0` companion of `axiom41`

## 8. Missing `pool2dAvg` Linearity Over `ewadd`

- count: `2`
- blocker:
  the current derivation route goes through `ConstPool`, which creates sharing problems. There is no direct linearity axiom.
- minimal reproducer:
  minimal test to add:
  source:
  `s0 = ewadd x y, out = pool2dAvg k s p s0`
  target:
  `d0 = pool2dAvg k s p x, d1 = pool2dAvg k s p y, out = ewadd d0 d1`
- minimal fix:
  add direct `pool2dAvg` distribution over `ewadd`

## 9. Budget-Only

- count: `2`
- blocker:
  the derivation exists, but not under the current `maxNumSteps` cap.
- minimal reproducer:
  targeted TASO substitutions:
  `193`, `484`
- minimal fix:
  increase search budget, but only after the structural blockers above are addressed

## 10. Missing Restricted `poolavg` / `conv` Commutation

- count: `1`
- blocker:
  one TASO rule needs a specific `poolavg` / `conv` commute fact that is not currently axiomatized.
- minimal reproducer:
  minimal test to add:
  source:
  `s0 = conv2d k1 s1 p1 none x w, out = pool2dAvg k2 s2 p2 s0`
  target:
  `d0 = pool2dAvg k2 s2 p2 x, out = conv2d k1 s1 p1 none d0 w`
  use the exact restricted parameter regime that appears in the TASO rule
- minimal fix:
  add the restricted commutation axiom, not a fully general one

## 12. `ConstPool` Artifact

- count: `1`
- blocker:
  this miss comes from TASO’s concrete constant implementation, not from a principled algebraic law that the local theory should learn.
- minimal reproducer:
  targeted TASO substitution:
  `543`
- minimal fix:
  do not add a general axiom for it
