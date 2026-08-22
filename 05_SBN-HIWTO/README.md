# 05_SBN — Surviving Bad News: Health Information without Treatment Options

R replication of the supplied Stata workflow for **Alberto Ciancio, Fabrice Kämpfen, Hans-Peter Kohler, and Rebecca Thornton, “Surviving Bad News: Health Information without Treatment Options” (AER: Insights, 2025, 7(1): 1–18).**

This folder follows the `ra2026-replication` convention: executable scripts live under `code/`, source data under `data/source/`, R-generated analysis data under `data/processed/`, and generated tables and figures under `tables/` and `figures/`.

## 1. Replication target

The primary target is the **authors’ supplied Stata replication workflow**.

The authors’ master workflow:

1. constructs the mortality analysis dataset from the raw MLSFH data;
2. estimates the main survival regressions;
3. estimates the 2005 follow-up mechanism regressions;
4. estimates the instrumental-variable first stages and weak-instrument diagnostics; and
5. generates the appendix selection-robustness figures.

The R translation follows the same sequence and preserves the authors’ Stata script names wherever possible.

The purpose of this folder is to reproduce the supplied workflow in R, not to reverse-engineer the published numbers when a specialized Stata routine does not have an exact R equivalent.

## 2. Required source data

Place exactly these files before running:

```text
data/source/raw/
  rawdata.dta
  rawdata_2005followup.dta
```

`rawdata.dta` contains the 2004 MLSFH data and later vital-status information.

`rawdata_2005followup.dta` contains the early-2005 follow-up survey used for the mechanism analysis.

Do **not** copy the authors’ `hivtest_mortality.dta` into the source-data folder. The supplied Stata workflow creates that file in `test_mortality_prep.do`, so the R workflow reconstructs the corresponding analysis dataset.

The CSV copies supplied by the authors are not required when the `.dta` files are used as the canonical raw inputs.

## 3. Project structure

```text
05_SBN/
│  README.md
│  REPLICATION_SUMMARY.md
│
├─code/
│  config_stata.R
│  master.R
│  multiple_test.R
│  selection_robustness_2008.R
│  selection_robustness_2010.R
│  selection_robustness_2018.R
│  test_instruments.R
│  test_mortality_analysis.R
│  test_mortality_prep.R
│
├─data/
│  ├─source/
│  │  └─raw/
│  │     ├─rawdata.dta
│  │     └─rawdata_2005followup.dta
│  └─processed/
│
├─tables/
└─figures/
```

## 4. Run the replication

Run from the project root:

```r
source("code/master.R")
```

The master script runs the translated workflow in the same broad order as the Stata master file:

1. `test_mortality_prep.R`
2. `test_mortality_analysis.R`
3. `multiple_test.R`
4. `test_instruments.R`
5. `selection_robustness_2008.R`
6. `selection_robustness_2010.R`
7. `selection_robustness_2018.R`

The setup/helper functions are defined in `config_stata.R`.

The workflow requires the R packages used by the translated scripts, including `haven`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `sandwich`, `lmtest`, `MASS`, and `ivDiag`.

## 5. Generated files

The workflow generates:

```text
data/processed/hivtest_mortality.rds
```

as the R reconstruction of the authors’ analysis-ready mortality dataset.

It also recreates the paper outputs under:

```text
tables/
figures/
```

including the main tables, appendix tables, and Figures A.1–A.4.

Generated data are not source inputs and should not replace the raw-data preparation step.

## 6. Important translation details

### Data preparation

The R workflow starts from the same raw-data level as the authors’ Stata workflow. `test_mortality_prep.R` reconstructs the mortality analysis dataset from `rawdata.dta`.

The 2005 follow-up data are merged only for the mechanism analysis, following the merge logic in `multiple_test.do`.

### Survival IV regressions

The main survival regressions use two endogenous variables:

- `learnhivneg`
- `learnhivpos`

with instruments formed from the randomized incentive and distance variables and their HIV-status interactions.

The R implementation reproduces the clustered GMM/IV specification used in the Stata workflow. The main survival estimates and first stages reproduce the supplied Stata outputs very closely.

### Clustered standard errors

The authors cluster inference at `DK_village_number`.

The R workflow uses the corresponding clustered covariance calculations. Small differences of approximately 0.001 in a few standard errors can occur because of software-specific finite-sample and numerical conventions.

### 2005 follow-up mechanism regressions

Tables 3 and A.11–A.13 use the 2005 follow-up survey and Stata’s:

```text
ivregress gmm ..., wmatrix(cluster DK_village_number)
```

specification.

The R workflow reproduces the analysis samples and outcome summaries, but the final GMM-IV coefficients and standard errors do **not** reproduce the supplied Stata values closely enough to be considered validated.

These discrepancies are concentrated in the mechanism regressions and are especially large for the weakly identified HIV-positive endogenous variable.

This should be treated as an unresolved **R/Stata estimator-implementation difference**, not as evidence that the original paper is incorrect.

### Weak-instrument diagnostics

The authors use specialized Stata routines including:

```text
weakivtest
weakivtest2, tau(0.2)
```

The current R workflow does not reproduce these commands exactly.

Consequently:

- the R effective-F values should not be treated as exact replicas of the Stata `weakivtest` statistic;
- Cragg–Donald values are not yet reproduced in the R output;
- Lewis–Mertens diagnostics are not implemented as an exact R counterpart.

These diagnostics should be documented separately from the main IV coefficient replication.

### Selection-robustness figures

Figures A.2–A.4 are simulation-based attrition/selection robustness exercises.

The R translation reproduces the authors’ assignment logic, but R and Stata use different random-number generators. Exact point-for-point equality should therefore not be expected solely from matching numeric seeds.

### Figure appearance

The underlying analysis is the validation target. Differences in histogram scaling, axis ranges, tick formatting, labels, or other `ggplot2` versus Stata defaults are cosmetic unless they change the plotted quantities.

## 7. How to judge discrepancies

Use the supplied Stata workflow and supplied Stata-generated outputs as the primary benchmark.

Recommended validation order:

1. **Check samples and observation counts.** These should match for the same specification.
2. **Check descriptive statistics and outcome means.** These should match up to rounding.
3. **Check coefficients.** Deterministic survival-regression coefficients should match up to numerical precision.
4. **Check clustered standard errors.** Small software-specific rounding differences can be acceptable.
5. **Check first-stage estimates.** These should match when the specification and sample agree.
6. **Check specialized weak-IV diagnostics separately.** Do not substitute a different R F-statistic and label it as the Stata statistic.
7. **Check simulation-based figures separately.** Numerical RNG differences do not necessarily imply a substantive discrepancy.
8. **Do not attribute a discrepancy to the paper itself without first rerunning the authors’ original `.do` files in Stata.**

## 8. Current validation status

The R workflow runs end-to-end.

### Main results

The principal survival findings replicate strongly:

- Table 1 summary statistics: replicated;
- Table 2 second-stage survival estimates: replicated;
- Table 2 first stages: replicated;
- Table A.4 survival estimates and first stages: replicated;
- Tables A.6, A.7, A.9, and A.10: replicated or extremely close;
- selection/attrition analyses: replicated substantively.

### Remaining discrepancies

The unresolved differences are concentrated in three areas:

1. **Tables 3 and A.11–A.13:** 2005 follow-up mechanism GMM-IV estimates do not reproduce the Stata coefficients and standard errors.
2. **Table 2 / A.4 / A.7 weak-IV diagnostics:** the specialized Stata effective-F and related diagnostics are not reproduced exactly in R.
3. **Table A.5:** one extended-control balance specification retains a localized coefficient discrepancy.

These remaining issues are not scattered across the replication. The mortality/survival results that form the paper’s main empirical finding reproduce cleanly.

## 9. Interpretation

The current evidence supports a **partial but strong replication**.

The main mortality/survival results, first stages, descriptive statistics, and most robustness analyses reproduce successfully in R.

The unresolved items are concentrated in technically specialized Stata procedures and in the smaller 2005 follow-up mechanism GMM-IV analysis.

Because the original Stata source explicitly requests GMM-IV estimation for the mechanism regressions and specialized Stata commands for the weak-IV diagnostics, the remaining R discrepancies should be reported as **unresolved cross-software implementation differences** unless a direct rerun of the original Stata workflow shows that the authors’ own code fails to regenerate its supplied outputs.

## 10. Reproducibility principle

The objective of this folder is to reproduce the **authors’ supplied replication workflow in R as faithfully as possible**.

Do not tune the R code merely to force agreement with a published coefficient when doing so would depart from the authors’ documented specification.

Where an exact Stata routine cannot be reproduced in R, preserve the correct specification, document the remaining difference, and distinguish it from the results that do replicate.
