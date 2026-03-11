# TASO substitution match failure report


## Summary

- **Total:** 568
- **Passed:** 515
- **Failed:** 53

## Failure details

| Index | src reachable | dst reachable | dst∈src? | src∈dst? | Likely cause |
|------:|------:|------:|:--------:|:--------:|------------|
| 0 | 13746 | 16785 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 2 | 13746 | 16772 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 5 | 13302 | 16658 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 6 | 13302 | 16665 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 8 | 10253 | 15397 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 9 | 10473 | 15216 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 10 | 10471 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 11 | 10471 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 12 | 10471 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 13 | 10471 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 14 | 13451 | 16739 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 15 | 13334 | 16754 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 16 | 10412 | 15451 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 17 | 10412 | 15453 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 18 | 10412 | 15451 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 19 | 10412 | 15453 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 34 | 15567 | 17212 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 36 | 15566 | 17189 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 55 | 15214 | 16784 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 56 | 15174 | 16763 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 73 | 17025 | 17020 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 74 | 17180 | 17180 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 82 | 13740 | 10471 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 83 | 13740 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 84 | 13740 | 15268 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 106 | 12034 | 11436 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 145 | 14863 | 10493 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 146 | 14862 | 10493 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 147 | 14862 | 14863 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 158 | 13974 | 14424 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 159 | 13974 | 14424 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 210 | 14439 | 14679 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 211 | 14439 | 14679 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 222 | 13511 | 14015 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 223 | 13511 | 14015 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 236 | 13671 | 10556 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 265 | 13948 | 14419 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 266 | 13948 | 14419 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 267 | 13669 | 10824 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 276 | 10977 | 11188 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 327 | 15106 | 10138 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 333 | 15156 | 15507 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 337 | 14765 | 13760 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 368 | 15145 | 15971 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 514 | 14264 | 14396 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 515 | 13889 | 14130 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 549 | 9873 | 8905 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 550 | 12061 | 8899 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 551 | 12299 | 10174 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 552 | 12424 | 10356 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 553 | 12100 | 8980 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 554 | 12276 | 10283 | False | False | No exact graph overlap (equality vs isomorphism?) |
| 560 | 12361 | 10283 | False | False | No exact graph overlap (equality vs isomorphism?) |

## Interpretation

- **|src→|** = number of graphs reachable from src in forward search
- **|dst→|** = number of graphs reachable from dst in reverse search
- **dst∈src?** = exact dst graph found in forward expansion
- **src∈dst?** = exact src graph found in reverse expansion
- A match requires some graph to appear in *both* reachable sets (exact equality).
- If both columns are 1, the axiom may not be in `allSubs` or the rule shape is not matched.
- If one side is large and the other is 1, one direction may need more depth/steps or a missing axiom.
