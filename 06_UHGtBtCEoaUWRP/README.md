# R Replication: Huguka Dukore

This repository provides an R implementation of the replication workflow for:

**“Using Household Grants to Benchmark the Cost Effectiveness of a USAID Workforce Readiness Program”**

## Running the replication

From the repository root, run:

```r
source("code/00_run_all.R")
```

The workflow starts from the authors’ cleaned analysis data and regenerates intermediate files and reported outputs.

## Repository structure

```text
code/
├── 00_run_all.R
├── 01_setup.R
├── 02_variable_lists.R
├── 03_helpers.R
├── 04_covariate_choice.R
├── 05_attrition.R
├── 06_balance.R
├── 07_hd_uptake.R
├── 08_itt.R
├── 09_itt_employment_deconstruction.R
├── 10_cost_equivalent.R
├── 11_cost_equivalent_linearity.R
├── 12_complementarities.R
├── 13_heterogeneity.R
├── 14_spillovers.R
└── 15_cash_accounting.R

data/
├── source/
│   ├── cleaned/
│   │   ├── HD_panel_clean.dta
│   │   └── HD_cost.xlsx
│   └── auxiliary/
│       └── kitchensink.csv
└── intermediate/
    └── HD_controls.csv

results/
├── tables/
└── figures/
```

## Data

Required source inputs:

- `data/source/cleaned/HD_panel_clean.dta`
- `data/source/cleaned/HD_cost.xlsx`
- `data/source/auxiliary/kitchensink.csv`

Generated intermediate file:

- `data/intermediate/HD_controls.csv`

Reported R outputs are prefixed with `R-`.

## Replication status

The R workflow executes successfully from beginning to end.

A comparison of the R outputs with the original Stata outputs finds:

- 9 tables reproduce estimates and standard errors to displayed rounding.
- `compliance_saturation` is a near-match, with one coefficient differing by approximately 0.02.
- All five heterogeneity specifications reproduce cleanly.
- Remaining numerical differences are concentrated in the cost-equivalence, complementarities, interference/spillover, saturation, transfer, and cash-accounting analyses.
- The largest differences occur for productive hours, monthly income, and productive assets.
- Standard errors generally reproduce more closely than point estimates.
- Cost-equivalence figures reproduce the substantive fitted-line, extrapolation, and estimated-GD-at-HD-cost construction, although graphical styling differs from Stata.

## Important implementation difference: covariate selection

The original Stata workflow performs covariate selection using `cvlasso` and `lasso2`, including Stata-specific partialling and cross-validation behavior.

The R implementation uses `glmnet` while reproducing the original workflow as closely as practical, including:

- sector-stratified cross-validation folds,
- the original random seed,
- integer attrition-weight expansion before cross-validation,
- unpenalized lagged outcomes, treatment indicators, and block indicators,
- screening of unusable and multicollinear candidate controls.

Because the optimization and cross-validation implementations are not identical, the selected control set and downstream estimates are not guaranteed to be numerically identical to the original Stata results.

## Multiple-testing adjustment

The R implementation reports raw p-values and sharpened q-values.

The adjusted values do not reproduce every bracketed adjusted value in the original tables exactly. This is retained as a documented replication difference rather than treated as an exact match.

## Output-format differences

The R tables use a standardized tidy output format and therefore do not attempt to reproduce every LaTeX feature of the original Stata tables.

Differences include:

- decimal precision,
- significance-star presentation,
- standard errors and p-values shown in separate columns,
- flatter row structures,
- reduced use of multirow/panel formatting,
- variable-name versus natural-language row labels in some outputs.

These presentation differences do not change the underlying regression specifications.

## Figures

The cost-equivalence figures reproduce the core substantive construction of the original figures:

- observed Control and HD outcomes,
- four GD treatment arms,
- the fitted GD cost-response line,
- the dashed extrapolation to the HD cost,
- the estimated GD outcome at the HD cost.

Remaining figure differences are primarily stylistic, including color, marker choice, panel arrangement, legends, and ggplot2 versus Stata defaults.

Interference and saturation plots reproduce the same underlying qualitative patterns, although some quantitative differences remain because of the corresponding regression-estimate drift.

## Validation

See `REPLICATION_SUMMARY.md` for the final validation summary and a concise description of the remaining differences between the original Stata outputs and the R replication.

## Overall assessment

The repository should be interpreted as a successful R translation of the original analysis workflow with strong agreement for a substantial subset of the reported results and documented residual numerical differences in several specifications.

It is not claimed to be an exact cell-for-cell numerical reproduction of every original Stata output.
