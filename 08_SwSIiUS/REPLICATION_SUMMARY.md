# Replication Summary

## Overview

The R replication executes successfully from beginning to end and reproduces the main structure and substantive results of the authors’ Stata 17 analysis.

The final comparison covers the complete set of replicated `.tex` table outputs and both Figure 1 outputs.

Most reported results either match the original values to displayed rounding or differ only at approximately the 0.001 level. The remaining meaningful numerical differences are concentrated in a small number of post-double-selection LASSO specifications, particularly Table 7 and the spillover-only specifications in Table G5.

## Results that reproduce cleanly

The following outputs reproduce cleanly or effectively cleanly after the final code revisions:

- **Figure 1 — Manual desludging**
- **Figure 1 — Mechanized desludging**
- **Table 1 — Randomization balance**
- **Table 2 — Treated households**
- **Table 2 — Spillover households**
- **Table 2 — Full sample**
- **Table 3 — Multinomial / first encounter**
- **Table 8 — Social pressure**
- **Table 9 — Reciprocity**
- **Table G1 — Attrition**
- most **Table 5 — Social-network** specifications

Table 3 initially showed small odds-ratio differences, but these were resolved after correcting the conditional-logit implementation.

Table 1 and Table G1 also reproduce after aligning the absorbed fixed-effect and clustered small-sample inference behavior more closely with Stata.

## Remaining numerical differences

### Table 7 — Learning and coordination within networks

Table 7 contains the main remaining family of nontrivial numerical differences.

For several network specifications, the discrepancy is concentrated in the second outcome column, corresponding to **Used Subsidized Desludging**.

Examples from the final comparison include approximately:

- `near5inc`: `-0.048` versus `-0.046`
- `sanitation`: `0.028` versus `0.029`
- `tea`: `-0.049` versus `-0.046`
- `testknow`: `0.030` versus `0.026`

For these specifications, the other outcome columns and standard errors generally reproduce closely.

The `wealthy` and `leadhealth` specifications retain somewhat larger numerical differences across multiple outcomes. One notable example is the manual-desludging interaction in the `leadhealth` specification:

- original: approximately `-0.112`
- R replication: approximately `-0.127`

These differences are retained as documented Stata/R implementation differences rather than eliminated through specification-specific tuning.

### Table G5 — Impact on mechanized desludging price

The largest residual point-estimate differences occur in the **spillover-only (SO)** specifications of Table G5.

The final comparison reports approximately:

- cluster measure, SO sample: `0.274` versus `0.313`
- nearest-four measure, SO sample: `0.728` versus `0.979`

The corresponding **LS + SO** specifications reproduce correctly.

These estimates are relatively imprecise, and the remaining divergence is localized to the spillover-only estimation branch rather than the pooled price analysis.

## Small residual differences

Several remaining differences are at or near displayed-rounding scale and are not treated as substantive replication failures.

Examples include:

- Table G3: the `Past` standard error differs by approximately `0.001`;
- Table G4: three baseline-subsidy coefficients differ by approximately `0.001`;
- Table 5: one network coefficient differs by approximately `0.001`;
- Table 4: a small number of health-effect estimates differ by roughly `0.001–0.003`;
- Table 6: several first-column estimates differ by approximately `0.001`.

These are documented but were not pursued with further output-specific tuning.

## Table G2 — Ex-post power calculations

The remaining small differences in Table G2 primarily reflect upstream differences in the Table 7 estimates and standard errors.

Because the minimum detectable effects are calculated from those regression results, they should not be interpreted as a separate independent replication discrepancy.

## Main implementation differences

The original Stata analysis uses `dsregress` for post-double-selection inference with clustered standard errors.

The R implementation reproduces this logic with a custom post-double-selection workflow, including:

- selection from the outcome equation,
- selection from each treatment equation,
- union of selected controls,
- cluster-aware LASSO penalty construction,
- clustered final-stage regression,
- finite-sample corrections aligned as closely as practical with the Stata specifications.

The implementation was revised during validation to address several genuine translation issues:

1. dataset-specific cluster-ID variable names were resolved consistently;
2. conditional-logit estimation was corrected for Tables 3 and G3;
3. absorbed fixed-effect clustered inference was aligned more closely with Stata for Tables 1 and G1;
4. LASSO selection was made cluster-aware rather than clustering only the final regression;
5. Table 6 output ordering was corrected to match the original Stata order.

After these changes, the large majority of the original numerical differences disappeared.

Exact equivalence of every LASSO-selected control set is still not guaranteed because Stata 17 and the R implementation use different internal numerical routines.

## Sample sizes and outcome means

No systematic sample-size or outcome-mean discrepancies were found in the final comparison.

This is important because it indicates that the remaining differences are not driven by broad sample-selection mistakes or the use of different source datasets.

Instead, the residual differences are concentrated in specific estimation branches and high-dimensional selection specifications.

## Formatting differences

All original/R table pairs differ at the raw text level because the R replication intentionally uses a cleaner, standardized LaTeX format.

Common formatting differences include:

- compact R tables versus tab-delimited Stata output,
- descriptive labels versus variable-name labels in some rows,
- omission of Stata-specific `\sym` and `\rule` scaffolding,
- different `\cmidrule` and header structures,
- table fragments in the original Table 5/7 workflow versus standalone R tables,
- empty standard-error cells represented differently,
- thousands separators,
- column-number rows and multi-line headers.

These differences are cosmetic and should be separated from numerical comparison.

## Figures

Both Figure 1 outputs reproduce the substantive estimates and overall patterns.

The R and Stata figures differ in presentation, including panel layout and plotting style, but the replicated estimates are effectively aligned with the original figures after the final revisions.

## Interpretation of the remaining differences

The remaining discrepancies are not spread broadly across the replication.

They are concentrated mainly in:

- selected network-interaction regressions in Table 7, and
- spillover-only price regressions in Table G5.

The main treatment/spillover results, balance and attrition results, multinomial analysis, social-pressure analysis, reciprocity analysis, and principal figures reproduce cleanly.

Based on the final comparison, the residual numerical differences are not treated as changing the paper’s qualitative conclusions.

The replication therefore does **not** claim exact cell-for-cell equivalence with Stata. It claims substantive reproduction of the paper’s empirical findings with explicitly documented numerical differences in a limited set of specifications.

## Final assessment

The replication should be characterized as a **successful R translation with strong substantive reproduction and partial exact numerical reproduction**.

The appropriate stopping point is to freeze the current analysis code rather than continue modifying the implementation to force isolated cells to equal the published Stata outputs.

The recommended repository status is therefore:

1. retain the current R analysis code;
2. retain the final original-versus-R comparison results;
3. document the Table 7 and Table G5 residual differences;
4. distinguish cosmetic LaTeX differences from numerical differences;
5. treat the current workflow as the completed R replication.

Further modification would only be warranted if exact cell-for-cell Stata equivalence becomes an explicit project requirement.
