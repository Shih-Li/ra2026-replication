# Replication Summary

## Overview

The R replication executes successfully from beginning to end and reproduces the main structure of the authors’ Stata analysis.

Of the 26 directly comparable tables:

- **9 tables reproduce estimates and standard errors to rounding**.
- **1 additional table, `compliance_saturation`, is an effective near-match**.
- The remaining tables show varying degrees of numerical drift, concentrated in a limited set of analysis families and outcomes.

## Results that reproduce cleanly

The following tables match the original estimates and standard errors to displayed rounding:

- `balance`
- `HD_completion`
- `IntensiveTracking`
- `employment_breakdown`
- `heterogeneity_consump`
- `heterogeneity_employ`
- `heterogeneity_female`
- `heterogeneity_older`
- `heterogeneity_RA`

`compliance_saturation` is also effectively clean, with only one estimate differing by approximately 0.02.

## Main remaining numerical differences

Residual numerical differences are concentrated in:

- cost-equivalence regressions,
- cost-equivalence linearity tests,
- complementarities specifications,
- interference/spillover regressions,
- saturation-level regressions,
- ITT transfer regressions,
- cash-accounting results derived from those regressions.

The largest discrepancies occur for:

- productive hours,
- monthly income,
- productive assets.

The binary employment outcome and several index/baseline-covariate specifications generally reproduce more closely.

Standard errors tend to agree more closely than point estimates.

## Likely source of residual drift

An important remaining implementation difference is the covariate-selection procedure.

The original analysis uses Stata’s `cvlasso` and `lasso2` procedures, whereas the R implementation uses `glmnet`.

The R code reproduces the original logic as closely as practical, including:

- sector-stratified fold construction,
- the original random seed,
- integer attrition-weight expansion,
- multicollinearity screening,
- unpenalized lagged outcomes,
- treatment indicators,
- block indicators.

However, exact equivalence of the selected controls and downstream coefficients is not guaranteed because the Stata and R LASSO implementations differ.

## Heterogeneity results

All five heterogeneity specifications now reproduce cleanly:

- consumption,
- employment,
- female,
- older,
- risk aversion.

The earlier missing interaction terms in the employment and older specifications were corrected.

## Cash accounting

The cash-accounting table now matches the original table orientation and structure.

Some dollar values still differ because they are derived from upstream regression estimates that themselves show small numerical drift.

Cash transfers and control means reproduce closely, while derived income, consumption, asset, debt, and total-flow values inherit the remaining regression differences.

## Multiple-testing adjustment

The original bracketed adjusted values do not consistently equal the R replication’s `q.value`.

The R workflow therefore reproduces the raw inference layer more closely than the original multiple-testing adjustment.

This is retained as a documented difference.

## Figures

The cost-equivalence figures now reproduce the principal substantive features of the originals:

- observed Control and HD points,
- the four GD arms,
- the fitted GD cost-response relationship,
- the dashed extrapolation segment,
- the estimated GD outcome at the HD cost.

The replication correctly omits the Combined arm from these figures.

Remaining differences are mainly cosmetic:

- Stata versus ggplot2 styling,
- color versus black-and-white markers,
- legend formatting,
- panel arrangement.

The interference and saturation-density figures reproduce the same qualitative patterns, although their exact line positions reflect the remaining coefficient differences in the corresponding regression tables.

## Final assessment

The replication should be characterized as a **successful R translation with partial exact numerical reproduction**.

A substantial subset of the reported results reproduces to rounding, all major analysis components run successfully, and the remaining discrepancies are documented and concentrated in identifiable specifications and outcomes.

The remaining differences do not justify further ad hoc tuning of downstream regressions unless exact cell-for-cell Stata equivalence is explicitly required.

The recommended stopping point is therefore:

1. freeze the current analysis code,
2. retain the current comparison report,
3. document the remaining numerical and formatting differences,
4. treat the current repository as the completed R replication.
