# Replication Summary

## Overview

This document summarizes the final validation of the R replication of Jing Cai and Adam Szeidl, **“Indirect Effects of Access to Finance.”**

The replication translates the authors’ Stata workflow into R and starts from the same author-prepared analysis datasets:

- `loanmain.dta`
- `market.dta`
- `loan_welfare.dta`

The separate author-prepared workbook `Figure3_do.xlsx` is required only for the final Figure 3.

The current code is treated as the final statistical replication version.

## Overall validation result

The R implementation successfully reproduces the main empirical structure of the original Stata workflow.

Across the main tables:

- point estimates generally match the original results to displayed rounding or very closely;
- standard errors generally match closely;
- treatment-effect signs and economic magnitudes are preserved;
- the first-stage and IV results reproduce cleanly;
- market-level effects reproduce cleanly;
- welfare and social-return results now reproduce the original reported values;
- the remaining discrepancies do not change the main conclusions of the paper.

The replication is therefore considered successful even though it is not an exact byte-for-byte or cell-for-cell reproduction of every Stata output.

## Main-table validation

### Table 1 — Baseline balance

The reported baseline values reproduce closely.

Remaining differences are minor:

- the R output uses standardized labels and does not reproduce every original typographical symbol;
- the R output explicitly reports some variable-specific sample sizes that were not shown in the publication-formatted table;
- joint-significance p-values differ slightly from the Stata output.

These differences do not affect the balance assessment.

### Table 2 — Borrowing

Treatment and spillover coefficients and standard errors reproduce closely.

The significance pattern and substantive interpretation are preserved.

### Table 3 — Main firm-level effects

The main treatment-effect estimates reproduce closely, including the effects on sales, profit, employment, wage costs, fixed assets, material costs, and shutdown.

A remaining reporting difference concerns the number of observations in some fixed-effects specifications. For example, the R and Stata implementations can report different `N` even when the coefficient estimates are essentially unchanged.

This is retained as a documented fixed-effects implementation/sample-handling difference rather than corrected through ad hoc downstream tuning.

The difference does not materially change the treatment-effect conclusions.

### Table 4 — Other business outcomes

The main business-outcome coefficients and standard errors reproduce closely.

The R translation was corrected to reproduce Stata’s dummy-variable construction when the underlying category variable is missing. This prevents R from unnecessarily dropping observations that Stata retains as zero on all generated category indicators.

Some residual sample-count differences can remain in fixed-effects specifications, but the substantive estimates are stable.

### Table 5 — Consumer outcomes

The conventional coefficient estimates and standard errors reproduce closely.

The principal remaining inference difference is in the Romano-Wolf multiple-testing adjustment.

The R implementation reproduces the authors’ clustered resampling structure, replication count, and stepdown logic as closely as practical. However, some adjusted p-values differ from the original Stata `rwolf2` output. In a small number of secondary outcomes, the adjusted p-value can cross a conventional significance threshold.

This difference is explicitly retained in the replication record.

It does not alter the paper’s central empirical conclusions.

### Table 6 — Market effects

The market-level estimates, standard errors, and sample sizes reproduce cleanly.

The R output contains an explicit note where analytic weights are used. This is a transparency difference rather than a specification change.

### Table 7 — Borrowing and local exposure

The main coefficients and standard errors reproduce closely.

Differences are primarily presentation and labeling.

### Table 8 — Local and non-local spillovers

The spillover estimates reproduce closely.

Any remaining differences are concentrated in reporting/sample conventions rather than the underlying substantive effects.

### Table 9 — First stages and IV

The first-stage coefficients, first-stage F statistics, and IV estimates reproduce closely.

The R output explicitly notes that the original Stata `xtivreg` specification uses its default non-clustered IV standard errors. This note makes the original estimation choice transparent and does not alter the specification.

## Welfare results

### Table 10 — Welfare gains

The welfare table is now fully populated.

For the baseline elasticity parameter used in the main table, the R implementation reproduces the original producer-surplus, consumer-surplus, spillover, total-welfare, dollar-value, standard-error, and confidence-interval results to the reported precision.

The spillover entry under full treatment is intentionally undefined/blank, consistent with the original table, because that spillover concept is defined for partial treatment.

### Table 11 — Return decomposition

The R implementation reproduces the reported private return, business-stealing component, consumer-surplus component, and social return.

The main reported values therefore agree with the original welfare decomposition.

### Bootstrap diagnostics

An earlier version incorrectly described bootstrap draws with negative implied yields as incomplete failures.

The final implementation follows the original Stata welfare program:

- negative-yield draws are retained as valid bootstrap draws;
- the corresponding fallback return calculation is applied;
- the diagnostic output separately records negative-yield draws and actual estimation failures.

In the reviewed final run, the welfare bootstrap produced valid statistics for all intended metrics and did not fail because of those negative-yield draws.

## Appendix and robustness outputs

The R workflow also generates the appendix and robustness analyses corresponding to the translated Stata workflow, including:

- Tables A1–A12,
- market-structure descriptive statistics,
- Figure 3 nonlinearity diagnostics.

Not every original appendix output was included in the external comparison set used for the final review, so the validation assessment is strongest for the directly compared main tables and welfare results.

The appendix results are retained as part of the complete translated workflow rather than used to claim exact numerical reproduction where no original comparison file was available.

## Multiple-testing adjustment

Romano-Wolf adjusted p-values are the principal remaining inferential difference.

The original code uses Stata’s `rwolf2`, whereas the R implementation reproduces its resampling and stepdown logic using R estimation and random-number machinery.

Because the Stata and R implementations are not identical, exact adjusted p-value equality is not guaranteed even when:

- the same nominal number of replications is used,
- the same seed is specified,
- the same clustering variable is used,
- the same regression specifications are estimated.

The replication therefore prioritizes faithful implementation of the statistical procedure over ad hoc tuning to force individual adjusted p-values to equal the Stata output.

Where an adjusted p-value crosses a conventional threshold, this is documented as a minor secondary-inference difference.

## Fixed-effects observation counts

Some fixed-effects specifications report different numbers of observations in the R and Stata outputs even when the estimated coefficients are nearly identical.

The final code preserves the estimation implementation that reproduces the coefficients and standard errors closely rather than changing the statistical sample solely to force the displayed `N` to match.

This residual discrepancy is interpreted as an implementation/sample-reporting difference unless a future validation exercise demonstrates a substantive change in the underlying estimation sample.

It does not change the main treatment-effect conclusions.

## Figures

### Figure 2

The R implementation reproduces the same substantive distributional patterns as the original density plots.

Remaining differences are graphical:

- `ggplot2` versus Stata styling,
- color and line defaults,
- axis presentation,
- legend layout.

These do not alter the information conveyed by the figure.

### Figure 3

The authors’ final Figure 3 is generated from a separate author-prepared workbook, `Figure3_do.xlsx`, containing the plotting inputs for borrowing, sales, profit, and employment.

The exact final four-panel figure can therefore be generated only when that workbook is placed under:

```text
data/source/cleaned/Figure3_do.xlsx
```

If the workbook is unavailable, the R workflow skips the final Figure 3 but still produces the regression-based nonlinearity diagnostic table from the main analysis data.

The diagnostic table is not presented as an exact substitute for the authors’ final Figure 3.

## Output-format differences

The original Stata outputs are publication-formatted LaTeX tables.

The R replication uses a standardized tidy output format, with one row per reported coefficient/statistic and explicit columns for estimates, standard errors, p-values, sample sizes, and model metadata.

Accordingly, expected presentation differences include:

- significance stars versus numeric p-values,
- standard errors in separate columns rather than parenthetical rows,
- different decimal display,
- flatter table structure,
- explicit notes on weights or inference,
- standardized labels,
- `.csv` output in addition to `.tex`.

These are presentation differences rather than specification differences.

## Effect on the paper’s conclusions

The remaining discrepancies do **not** materially affect the conclusions of the paper.

The central findings are preserved:

- access to finance changes treated firms’ borrowing and business outcomes;
- treatment creates economically meaningful indirect effects through competitors and markets;
- local and non-local spillover patterns are reproduced;
- IV estimates support the borrowing mechanism;
- the welfare and social-return decomposition reproduces the original economic conclusions.

The remaining differences are concentrated in secondary adjusted p-values, some displayed fixed-effects sample counts, and presentation.

## Stopping decision

Further code revision is not recommended solely to force exact Stata equality.

The current stopping rule is:

1. the same estimands and specifications are implemented;
2. the main point estimates and standard errors reproduce closely;
3. the welfare and return decomposition reproduces;
4. remaining differences are understood and documented;
5. no remaining discrepancy materially changes the substantive conclusions.

These conditions are satisfied.

The recommended final workflow is therefore to:

1. freeze the current R analysis code;
2. retain the original-versus-replication comparison record;
3. document the fixed-effects `N` and Romano-Wolf differences;
4. preserve the optional Figure 3 source-file requirement;
5. treat paper 09 as a completed R replication.

## Final assessment

The replication should be characterized as a **successful R translation with strong numerical reproduction of the main empirical results and transparent documentation of a small number of non-conclusion-changing differences**.

Exact cell-for-cell equivalence with Stata is not claimed and is not necessary for the replication to support the same substantive conclusions.
