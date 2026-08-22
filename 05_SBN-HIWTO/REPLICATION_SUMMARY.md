# Replication Summary — Surviving Bad News: Health Information without Treatment Options

## Paper

**Alberto Ciancio, Fabrice Kämpfen, Hans-Peter Kohler, and Rebecca Thornton — “Surviving Bad News: Health Information without Treatment Options” (AER: Insights, 2025, 7(1): 1–18).**

## Replication scope

This R replication translates the authors’ supplied Stata workflow.

The workflow:

1. reconstructs the mortality analysis dataset from the raw MLSFH data;
2. reproduces the main mortality/survival analysis;
3. reproduces the 2005 follow-up mechanism analysis;
4. reproduces the first-stage and instrument-validity exercises;
5. translates the weak-instrument diagnostic workflow where possible; and
6. reproduces the appendix selection-robustness figures.

The authors’ `hivtest_mortality.dta` is treated as a generated analysis dataset rather than a required source input.

## Required data

```text
data/source/raw/
  rawdata.dta
  rawdata_2005followup.dta
```

The R workflow reconstructs:

```text
data/processed/hivtest_mortality.rds
```

from the raw MLSFH file.

## Primary validation benchmark

The correct benchmark for assessing the R translation is the **authors’ supplied Stata replication workflow and the supplied Stata-generated outputs**.

A difference between R and a specialized Stata routine is not, by itself, evidence that the paper is incorrect.

A claim that the original replication package is internally inconsistent would require a direct rerun of the authors’ original `.do` files in the required Stata environment.

## Overall result

The replication is best characterized as a **partial but strong replication**.

The paper’s principal mortality/survival results reproduce cleanly. The unresolved discrepancies are highly concentrated rather than widespread:

1. specialized weak-instrument diagnostics; and
2. GMM-IV mechanism regressions using the 2005 follow-up survey.

The data construction, descriptive statistics, mortality samples, survival coefficients, first-stage regressions, and most robustness analyses reproduce successfully.

## Table-by-table validation status

| Output | Result | Assessment |
|---|---|---|
| Table 1 | Summary statistics | **Replicated** |
| Table 2 top | Survival, second stage | **Replicated** |
| Table 2 bottom | First-stage regressions | **Replicated** |
| Table 2 test statistics | Weak-IV diagnostics | **Not reproduced exactly in R** |
| Table 3 | 2005 mechanism GMM-IV | **Not reproduced** |
| Table A.1 | Mechanism summary statistics | **Replicated** |
| Table A.2 | Instrument exogeneity | **Replicated** |
| Table A.3 | Reduced form | **Replicated; star convention differs** |
| Table A.4 top | Incentive-only survival IV | **Replicated** |
| Table A.4 bottom | Incentive-only first stage | **Replicated** |
| Table A.4 test statistics | Weak-IV diagnostics | **Not reproduced exactly in R** |
| Table A.5 | Extended-control exogeneity | **Near-match; localized discrepancy** |
| Table A.6 | Mortality, extended controls | **Near-exact / replicated** |
| Table A.7 | Separate regressions by HIV status | **Replicated / very close** |
| Table A.7 F-statistics | Weak-IV diagnostics | **Not reproduced exactly in R** |
| Table A.8 | First stage without cross-effects | **Replicated** |
| Table A.9 | Selection / attrition | **Replicated** |
| Table A.10 | Mortality with IPW | **Replicated / very close** |
| Table A.11 | Worry mechanisms | **Not reproduced** |
| Table A.12 | Economic-uncertainty mechanisms | **Not reproduced** |
| Table A.13 | Drinking mechanisms | **Not reproduced** |
| Figures A.1–A.4 | Appendix figures | **Substantive workflow reproduced; graphical differences remain** |

## Main survival results

The strongest result of the replication is that the paper’s main survival IV analysis is reproducible in R.

The following components match the supplied Stata output either exactly at the reported precision or with only negligible numerical differences:

- survival coefficients;
- first-stage coefficients;
- observation counts;
- most clustered standard errors;
- incentive-only IV robustness;
- extended-control mortality models;
- separate HIV-status regressions;
- selection/attrition specifications; and
- inverse-probability-weighted mortality estimates.

Later revisions to the R covariance calculations eliminated several earlier one-digit standard-error differences in Tables 2 and A.4.

This concentration of successful results provides strong evidence that the raw-data build, key treatment variables, HIV interactions, mortality outcomes, clustering identifier, and core IV design were translated correctly.

## Unresolved cluster 1 — weak-instrument diagnostics

The supplied Stata code uses specialized commands including:

```text
weakivtest
weakivtest2, tau(0.2)
```

The current R implementation does not reproduce these routines exactly.

### Effective F-statistics

The R values differ systematically from the Stata values.

For example, the Stata effective-F statistics in the main specification are substantially larger for the HIV-negative first stage and also differ materially for the weak HIV-positive first stage.

The discrepancy persists in the corresponding Table A.4 and A.7 diagnostics.

### Cragg–Donald and Lewis–Mertens statistics

The supplied Stata workflow records additional weak-IV diagnostics that do not currently have a like-for-like implementation in this R workflow.

These cells should therefore be treated as **not replicated**, rather than filled with a different R statistic under the same label.

### Interpretation

This is a diagnostic-implementation limitation, not a failure of the main IV coefficient replication.

The correct reporting approach is:

> The main first stages reproduce, but the specialized Stata weak-instrument diagnostics are not reproduced exactly in R.

## Unresolved cluster 2 — 2005 mechanism GMM-IV regressions

The 2005 follow-up mechanism analysis remains the principal substantive discrepancy.

The affected outputs are:

- Table 3;
- Table A.11;
- Table A.12; and
- Table A.13.

The R workflow reproduces the corresponding samples and descriptive outcome means, but the GMM-IV coefficients and standard errors differ substantially from the supplied Stata output.

The discrepancy is especially pronounced for the HIV-positive endogenous variable, whose instruments are much weaker than those for the HIV-negative endogenous variable.

The R standard errors remain much smaller than the Stata values for the weakly identified HIV-positive coefficients.

### What has been ruled out

Several potential translation problems were investigated:

- the correct two endogenous variables are used;
- the intended randomized incentive/distance instruments and HIV interactions are used;
- the 2005 follow-up merge follows the Stata master/using logic;
- Stata-style factor-variable base-category handling was corrected;
- Table 3 now reuses the same stored mechanism models as the corresponding appendix summary columns, eliminating earlier internal inconsistencies;
- the shared clustered-GMM helper reproduces the main survival IV results well.

Despite these corrections, the mechanism GMM estimates remain materially different from Stata.

### Interpretation

The supplied Stata source explicitly calls:

```text
ivregress gmm ..., wmatrix(cluster DK_village_number)
```

for these outcomes.

Therefore, the current evidence does **not** justify concluding that the authors ran OLS or that the published mechanism estimates are erroneous.

The appropriate conclusion is:

> The 2005 follow-up GMM-IV mechanism results are not reproduced by the current R implementation. The discrepancy appears specific to the cross-software implementation of the clustered GMM estimator in this smaller, weak-instrument sample.

A direct rerun of the original Stata code is required before attributing the discrepancy to the original replication package.

## Localized remaining discrepancy — Table A.5

Most of Table A.5 is close to the supplied Stata output.

One specification involving `schooling_ext` retains a localized difference in the coefficients on `incentive` and `incentive²`.

Because the discrepancy is isolated and the downstream mortality estimates remain very close, it is documented as a near-match rather than treated as evidence of a broader data-build failure.

## Figures

Figures A.1–A.4 are substantively reproduced, with presentation differences associated with the Stata-to-`ggplot2` translation.

Examples include:

- density versus frequency scaling in the histograms;
- different axis ranges or tick formatting;
- different legend wording;
- Unicode minus signs and number formatting; and
- Monte Carlo differences attributable to different R and Stata random-number generators.

These are not interpreted as substantive replication failures unless the underlying plotted values or simulation logic differ.

## Known R/Stata implementation differences

### Clustered covariance calculations

Minor standard-error differences can occur because Stata and R use different finite-sample and numerical conventions.

After aligning the covariance treatment more closely, the main Tables 2 and A.4 standard errors match the original output at reported precision.

### Significance stars

Some supplied Stata tables use different star thresholds from the default convention originally used in the R table writer.

Where coefficients and standard errors agree but a borderline star differs, this is a reporting convention rather than an estimation failure.

### Factor variables

Stata chooses factor-variable base categories from the estimation sample.

The R mechanism code was revised to reproduce this behavior rather than constructing a full dummy set before sample selection.

This corrected an internal Table 3 versus Table A.12 inconsistency but did not resolve the larger mechanism GMM discrepancy.

### Specialized weak-IV commands

`weakivtest` and `weakivtest2` should not be replaced by a different R statistic and presented under the same name.

Exact equality is not claimed for these diagnostics.

## Acceptance criteria

| Component | Expected agreement |
|---|---|
| Raw-data construction | Exact logic |
| Analysis sample for same model | Exact |
| Descriptive statistics | Exact up to rounding |
| Main survival coefficients | Exact / numerical precision |
| First-stage coefficients | Exact / numerical precision |
| Conventional clustered SEs | Exact or extremely close |
| Figure underlying quantities | Same substantive calculation |
| Figure appearance | Cosmetic differences acceptable |
| Stata `weakivtest` diagnostics | Not assumed equivalent to generic R diagnostics |
| 2005 clustered GMM mechanism estimates | Currently unresolved |

## Current conclusion

The R workflow runs end-to-end and successfully reproduces the central empirical findings of the paper.

The main survival effect, first stages, and most robustness analyses are reproducible.

The remaining failures are concentrated in:

- specialized weak-instrument statistics; and
- the 2005 follow-up GMM-IV mechanism regressions.

This pattern does **not** support a general conclusion that the original paper or data are wrong.

Instead, the evidence supports the following conclusion:

> The paper’s main survival findings are strongly replicated in R. Exact replication remains incomplete for specialized Stata weak-instrument diagnostics and for clustered GMM-IV mechanism regressions using the 2005 follow-up sample. These unresolved differences should be reported as cross-software implementation limitations unless a direct rerun of the authors’ original Stata workflow demonstrates an inconsistency in the original package itself.

## Overall assessment

**Partial but strong replication.**

The main empirical result is reproducible and the unresolved discrepancies are clearly localized and documented.

No further tuning of the R code solely to force agreement with the remaining Stata cells is recommended unless an exact estimator-level equivalence can be established.

A future validation step, if Stata is available, would be to rerun the authors’ original `master.do` and compare the regenerated Table 3 / A.11–A.13 and weak-IV diagnostic outputs directly with both the supplied `.tex` files and the R results.
