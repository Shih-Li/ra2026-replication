# R Replication: Spillovers without Social Interactions in Urban Sanitation

This repository provides an R implementation of the replication workflow for:

**“Spillovers without Social Interactions in Urban Sanitation”**  
Joshua Deutschmann, Molly Lipscomb, Laura Schechter, and Jessica Zhu

## Running the replication

From the paper-specific repository root, run:

```r
source("code/00_run_all.R")
```

The workflow starts from the authors’ cleaned analysis datasets and regenerates the reported tables and figures in R.

## Repository structure

```text
code/
├── 00_run_all.R
├── 01_setup.R
├── 02_variable_lists.R
├── 03_helpers.R
├── 04_table1_balance.R
├── 05_tableG1_attrition.R
├── 06_figure1_decision_spillovers.R
├── 07_table2_decision_spillovers.R
├── 08_table4_health.R
├── 09_table5_social_networks.R
├── 10_table6_learning_coordination.R
├── 11_table7_learning_networks.R
├── 12_table8_social_pressure.R
├── 13_tableG5_price.R
├── 14_table3_multinomial_first.R
├── 15_tableG3_multinomial_how.R
├── 16_table9_reciprocity.R
├── 17_tableG4_remember_neighbors.R
└── 18_tableG2_power.R

data/
└── source/
    └── cleaned/
        ├── FinalData_CompleteBLDecider.dta
        ├── FinalData_CompleteBLDecider_LASSO.dta
        ├── FinalData_CompleteBLDemo.dta
        ├── FinalData_DyadicData.dta
        ├── FinalData_Multi_First_4Outcome_LASSO.dta
        └── FinalData_Multi_How_5Outcome_LASSO.dta

results/
├── tables/
└── figures/
```

Generated R outputs are prefixed with `R-`.

## Data

The R workflow follows the authors’ official analysis workflow and begins from the cleaned, analysis-ready datasets supplied in the original replication package.

Required source inputs:

- `data/source/cleaned/FinalData_CompleteBLDecider.dta`
- `data/source/cleaned/FinalData_CompleteBLDecider_LASSO.dta`
- `data/source/cleaned/FinalData_CompleteBLDemo.dta`
- `data/source/cleaned/FinalData_DyadicData.dta`
- `data/source/cleaned/FinalData_Multi_First_4Outcome_LASSO.dta`
- `data/source/cleaned/FinalData_Multi_How_5Outcome_LASSO.dta`

The raw survey files and confidential GPS data are not required for this analysis-level R replication.

Original replication package:

- AEA Data and Code Repository / openICPSR project 181101
- https://doi.org/10.3886/ICPSR181101V1

## Main implementation notes

The original analysis was written for Stata 17 and makes extensive use of `dsregress`, clustered inference, absorbed fixed effects, and alternative-specific conditional logit models.

The R implementation reproduces the original workflow as closely as practical, including:

- clustered standard errors at the original cluster level,
- Stata-style finite-sample corrections for absorbed fixed-effect specifications,
- post-double-selection LASSO logic,
- cluster-aware LASSO selection,
- preservation of the original analysis samples,
- the original random seeds where relevant,
- conditional-logit / alternative-specific choice specifications,
- cross-script reuse of selected controls required by Tables 3 and G3,
- ex-post power calculations based on the replicated estimates and standard errors.

Because Stata and R use different internal implementations for some LASSO and estimation routines, exact cell-for-cell numerical equivalence is not guaranteed in every specification.

## Replication status

The R workflow executes successfully from beginning to end.

The final comparison with the authors’ Stata outputs finds that the main results reproduce very closely.

Clean or effectively clean results include:

- Figure 1, both panels/figures,
- Table 1,
- all three Table 2 output files,
- Table 3,
- Table 8,
- Table 9,
- Table G1,
- most Table 5 network specifications.

Remaining numerical differences are concentrated in:

- selected Table 7 learning/coordination-within-network specifications,
- the spillover-only columns of Table G5,
- a small number of rounding-level cells in Tables 4, 5, 6, G3, and G4,
- Table G2 values that mechanically inherit remaining Table 7 differences.

See `REPLICATION_SUMMARY.md` for the detailed assessment.

## Output-format differences

The R tables use a standardized LaTeX output format and do not attempt to reproduce every formatting feature of the original Stata `esttab` output.

Presentation differences can include:

- row and column labels,
- whitespace and tab alignment,
- multi-line headers,
- table fragments versus standalone tables,
- thousands separators,
- significance/standard-error formatting,
- LaTeX scaffolding such as `\sym`, `\rule`, and `\cmidrule`.

These presentation differences do not imply differences in the underlying statistical specification.

## Figures

The two Figure 1 outputs reproduce the substantive estimates and patterns of the original figures.

The R versions may differ cosmetically from Stata in:

- panel arrangement,
- axis styling,
- markers,
- fonts,
- spacing,
- other plotting defaults.

## Overall assessment

This repository should be interpreted as a **successful R translation with strong substantive replication and limited residual numerical differences in a small set of specifications**.

The principal empirical findings and qualitative conclusions are preserved. The remaining differences are documented rather than removed through output-specific or ad hoc tuning.

See `REPLICATION_SUMMARY.md` for the final validation summary.
