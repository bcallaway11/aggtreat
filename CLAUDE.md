# CLAUDE.md — aggtreat

**Package:** `aggtreat` | **Repo:** `bcallaway11/aggtreat`
**Paper:** Caetano, Caetano, Callaway, and Dyal (2025), "Causal Inference for Aggregated Treatment"
**Supplementary appendix:** https://tinyurl.com/AggTreatment

## Goals

1. General-purpose tools for diagnosing and addressing problems when a treatment variable
   aggregates multiple sub-treatments (incongruent comparisons, non-unique/negative weights,
   hard-to-interpret regression coefficients).
2. Exact replication of all paper results — every number, table, and figure in the paper
   must be reproducible via the vignette. This is a hard requirement.

---

## Main Functions

| Function | Paper Section | Purpose |
|---|---|---|
| `classify_pairs()` | §2.2, Def 2.1 | Classify (sd, sd-1) pairs as congruent (M+) or incongruent (M-) |
| `count_pairs()` | §3.4.1, Props C.5-C.6 | Count congruent/incongruent pairs for binary/trinary sub-treatments |
| `min_incongruent_weights()` | §3.4.3, Eq (3) | LP for minimally incongruent weights; uses `lpSolve` |
| `decompose_marginal()` | §3.2, Thm 3.1 | Decompose marginal effects into congruent/incongruent components |
| `diagnostics()` | §3.4, Prop 3.3 | Sub-treatment composition plot; Prop 3.3 pre-screening check |
| `estimate_aatt()` | §4.1, Thm 4.1 | AATT(d) = E[Y|D=d] - E[Y|D=0]; no sub-treatment data needed |
| `estimate_amatt_plus()` | §4.2, Eq (7) | AMATT+ with product weights; requires sub-treatment data |
| `regression_weights()` | §B.1, Prop B.1 | Yitzhaki-style decomposition of OLS coefficient |
| `aggtreat_analysis()` | — | Main user function; returns S3 object with print/summary/plot methods |

**Dependencies:** `lpSolve` (required), `ggplot2` (required). No hard `tidyverse` dependency.
**Style:** `snake_case` throughout. Internal helpers prefixed with `.`. Pure functions where possible.
**Docs:** Every exported function needs `@param`, `@return`, `@references`, `@examples` (roxygen2).

---

## Key Implementation Notes

- **Congruence (Def 2.1):** `sd` and `sd-1` are congruent iff `sd - sd-1` equals a unit vector
  `e_k` — exactly one element is 1, all others 0. Nothing else qualifies.
- **AATT vs AMATT+:** Sub-treatment data is *optional* for `estimate_aatt()` but *required* for
  `estimate_amatt_plus()`. Reflect this in function interfaces.
- **NA handling:** `AMATT+(d)` is NA when no congruent pairs exist (e.g., d=3.0 in application).
  The overall `E[AMATT+|D>0]` must exclude NA levels from both numerator and denominator.
- **Aggregated treatment is NOT causal.** Potential outcomes are defined at the sub-treatment
  level `Y(s)`. Documentation must not use causal language for the aggregated treatment.
- **Non-integer support:** Sub-treatments can be non-integer (e.g., half-hours) as long as they
  are discrete. Support is represented as a sorted numeric vector internally.

---

## Replication Targets (Paper Section 5)

Dataset: CDS/PSID, low-SES children, 2019 wave, n=214. Sub-treatments rounded to 0.5h.
D = Lessons + Sports + Volunteering + B&A School. Outcome = normalized noncognitive skill index.

**Table 2** (point estimates and bootstrap SEs, B=1000 — document seed used):

| Parameter | Estimate | SE |
|---|---|---|
| alpha1 | -0.061 | 0.042 |
| E[delta(D)|D>0] | -0.040 | 0.035 |
| E[AMATT+(D)|D>0] | -0.087 | 0.063 |
| E[AATT(D)|D>0] | -0.167 | 0.068 |
| E[AATT(D)/D|D>0] | -0.218 | 0.095 |

**Table 1** incongruent weight %: d=0.5→0%, d=1.0→0%, d=1.5→37.51%, d=2.0→33.75%,
d=2.5→40.00%, d=3.0→100%. Total pairs: 95 (19 congruent, 76 incongruent).

**Figure 3a** d-specific values: d=0.5: delta=-0.219/AMATT+=-0.219; d=1.0: -0.047/-0.047;
d=1.5: 0.197/0.095 (-51.48%); d=2.0: -0.083/-0.083; d=2.5: 0.103/-0.237 (-330.16%);
d=3.0: 0.013/NA.

---

## Workflow

### Memory
Read `MEMORY.md` at the start of every session. Update it when:
- A bug is found and fixed → `[LEARN:bug]`
- An R/package gotcha is discovered → `[LEARN:r-code]`
- A math-code discrepancy is resolved → `[LEARN:math]`
- A replication number is confirmed → `[LEARN:replication]`
- A design decision is made (note rejected alternatives too) → `[LEARN:design]`
- The critic agent flags a recurring issue → `[LEARN:critic]`

Append entries to the Corrections Log in `MEMORY.md` as:
`- [LEARN:tag] Description, specific enough to act on. Include file:line if relevant.`

### Slash Commands
| Command | What it does |
|---|---|
| `/check` | Runs `devtools::check()`; reports ERRORs/WARNINGs/NOTEs by severity; blocks on ERRORs |
| `/test [filter]` | Runs `devtools::test(filter=...)` or full suite; reports pass/fail/skip counts and failure details |
| `/document` | Runs `devtools::document()`; reports changed files in `man/`; flags missing roxygen2 fields |

### Agents
| Agent | Role |
|---|---|
| `r-pkg-critic` | Adversarial review of R functions against the paper; READ-ONLY; invoked after implementing any new function |
| `test-critic` | Adversarial review of test cases in `tests/testthat/`; READ-ONLY; iterates with `/test` up to 5 rounds |

**r-pkg-critic** checks: math-code alignment (congruence, LP constraints, AATT, product weights,
regression weights, NA handling), edge cases (K=1, d=1, d=N_bar, empty cells), roxygen2
completeness, and test coverage for the function under review.

**test-critic** checks: coverage of Example 2.1 (K=3 binary), uniform-probabilities case (p.16),
unavoidable-incongruency case (p.17), replication targets (Table 2), all edge cases, assertion
quality (`expect_equal` with tolerance, descriptive test names), and test independence. Issues a
findings report; main agent addresses Critical gaps and runs `/test`; repeat until APPROVED.
