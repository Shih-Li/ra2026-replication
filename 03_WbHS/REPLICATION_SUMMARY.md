# Replication Summary — What's Behind Her Smile?

## Paper

**Gallego, Larroulet Philippi, and Repetto — “What’s Behind Her Smile? Health, Looks, and Self-Esteem” (AEJ: Applied, 2024).**

## Replication scope

This R replication translates the supplied Stata workflow rather than treating the authors’ generated analysis dataset as a source input.

The workflow:

1. reconstructs the analysis dataset from six anonymized WBHS raw files;
2. reproduces Tables 1–8;
3. reproduces Appendix Tables 1–5;
4. reproduces Figure 1; and
5. uses the separate CASEN employment `.dta` file directly for the CASEN figure panel.

The authors’ `main_dataset.dta` is therefore a generated/validation object rather than a required source file.

## Required data

```text
data/source/raw/
  random_anon.dta
  BL_anon.dta
  PICTURES_anon.dta
  EXAM_anon.dta
  FU1_anon.dta
  FU2_anon.dta

data/source/cleaned/
  Casen_employment_2009to2015.dta
```

`Casen_employment.xlsx` is not required by the supplied Stata execution path.

## Primary validation benchmark

The correct benchmark for assessing the R translation is the **supplied 2024 Stata replication workflow and its generated outputs**.

The 2018 J-PAL working paper is a secondary historical benchmark only. Differences between the working paper and the current replication package can reflect revisions made between working-paper and publication stages.

Accordingly:

> If the R output matches the supplied 2024 Stata output but differs from the 2018 working paper, the difference should be documented as a version change rather than “fixed” in the R code.

## Interpretation of the working-paper comparison

A comparison against the 2018 working paper identified several apparent differences, including sample sizes, balance-test p-values, selected treatment-effect estimates, and figure scaling.

These differences do **not**, by themselves, establish a failure of the R replication.

### Sample sizes

The R workflow should preserve the samples implied by the supplied raw data, merge logic, missing-value rules, outcome construction, and regression specification in the current Stata code.

The earlier working paper reports some different first-follow-up sample counts. Those historical counts should not be recovered by changing the R cleaning or estimation code unless the current Stata workflow itself produces them.

**Status:** report as a potential working-paper-to-publication revision; do not revise the R sample rules solely to match the working paper.

### Table 1 — descriptive statistics

The R descriptive statistics are generally very close to the working-paper values. Variable-specific sample sizes can differ because the Stata workflow computes summaries using available observations for each variable rather than imposing one universal complete-case sample.

The current replication-variable list also contains variables not shown in the older working-paper presentation.

**Status:** no code change indicated solely from the working-paper comparison.

### Table 2 — balance tests

The supplied Stata code estimates each balance regression with treatment status, randomization-strata fixed effects, and robust standard errors. The R workflow follows that specification.

Therefore, the noticeably different p-values relative to the 2018 working paper should not be “corrected” by switching the R code to an unadjusted t-test or a different specification.

**Status:** validate against current Stata output. If R and Stata agree, report the working-paper difference as a version difference.

### Table 3 — take-up of dental services

Most treatment-effect estimates are close to the working-paper values, while a few cells—such as second-follow-up prosthesis use among men—show larger differences.

The current Stata workflow constructs the prosthesis outcomes using the supplied follow-up variables and baseline-prosthesis adjustment before estimating treatment effects with the specified controls and strata.

**Status:** do not alter the R code to target the 2018 coefficient. Compare the R cell directly with the current Stata-generated Table 3.

### Table 4 — dental-health outcomes

The R estimates are generally close to the working-paper values, with some larger differences in subgroup coefficients such as cavities among men.

The R workflow follows the current Stata specification for cavities, need for treatment, subjective oral health, and smiling behavior.

**Status:** validate against current Stata-generated Table 4 before considering any code revision.

### Table 5 — psychological, social, economic, and health outcomes

The published replication workflow has a different organization from the older working-paper table. The current Stata code estimates self-esteem, employment, earnings, second-follow-up health outcomes, gender differences, and across-survey tests using the current control and strata specifications.

Working-paper differences in coefficients or sample sizes therefore cannot be interpreted as R errors without a current Stata comparison.

**Status:** no code revision based solely on the 2018 table.

### Tables 6–8

The table organization differs substantially from the 2018 working-paper numbering. The current replication package should determine the target content and specifications.

**Status:** compare R to current Stata outputs, not to historical table numbering.

### Figure 1

The R implementation follows the data and plotting ranges encoded in the supplied Stata figure script. Differences in visible tick marks, panel aspect ratios, or other graphics defaults are cosmetic when the plotted data agree.

**Status:** cosmetic differences are acceptable; validate plotted values rather than exact graphical rendering.

## Known R/Stata implementation differences

### Stata unique-prefix abbreviations

The Stata source sometimes refers to variables by a unique prefix. The R translation resolves those prefixes explicitly because R otherwise requires exact variable names.

### Table 1 Gender p-value

The Stata loop mechanically attempts to regress the Gender variable on itself for the Gender row. The R implementation leaves that one gender-difference p-value blank rather than fitting the degenerate regression.

This is an intentional cleanup and does not affect substantive results.

### Robust covariance warnings

HC1 robust covariance estimation can produce repeated leverage warnings when many strata indicators are included. Repeated copies of this diagnostic warning are suppressed in permutation loops; the HC1 calculation itself is unchanged.

### Romano–Wolf multiple-testing adjustment

This remains the principal validation caveat.

The Stata package uses `rwolf` with 200 repetitions, seed 456, randomization strata, controls, and baseline outcomes where applicable. The R workflow implements a stratified permutation stepdown analogue.

Because these are separate implementations, **exact equality of Romano–Wolf adjusted p-values is not assumed**. They should be validated separately from the deterministic OLS coefficients and conventional robust standard errors.

## Acceptance criteria

The replication should be assessed in the following order:

| Component | Expected agreement with current Stata |
|---|---|
| Analysis sample for the same model | Exact |
| Control means / descriptive statistics | Exact up to rounding |
| OLS treatment coefficients | Exact up to numerical precision |
| Conventional robust SEs | Very close / numerical precision |
| Figure underlying data | Exact up to numerical precision |
| Figure appearance | Cosmetic differences acceptable |
| Romano–Wolf adjusted p-values | Validate separately; small implementation/simulation differences possible |
| Bootstrap/IPW inference | Validate separately; simulation differences possible |

A meaningful mismatch in sample size, coefficient, or ordinary robust SE **between R and the supplied current Stata workflow** should trigger code review.

A mismatch **only between the current replication and the 2018 working paper** should normally be reported as a version difference unless additional evidence shows that the current Stata workflow is being mistranslated.

## Current conclusion

The end-to-end R workflow is operational and reproduces the structure of the supplied Stata replication: raw-data construction, main tables, appendix tables, and Figure 1.

The comparison with the 2018 working paper identifies useful historical differences but does not currently provide a basis for rewriting the substantive R specifications.

The next validation priority is therefore:

1. compare the R-generated tables directly with the outputs from the supplied 2024 Stata scripts;
2. investigate any deterministic differences in samples, coefficients, control means, or ordinary robust standard errors;
3. validate Romano–Wolf adjusted p-values separately because the R implementation is an analogue rather than the original Stata `rwolf` command; and
4. report residual 2018-vs-2024 differences as publication-stage revisions where the current Stata and R outputs agree.

## Overall assessment

**Do not revise the substantive R code simply to reproduce the 2018 working-paper values.**

The replication target is the authors’ current supplied workflow. Differences from the earlier working paper are reportable phenomena unless they also represent discrepancies between the R implementation and the current Stata output.
