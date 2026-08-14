# Replication Summary

## Paper

**Riley, Emma.**  
*Resisting Social Pressure in the Household Using Mobile Money: Experimental Evidence on Microenterprise Investment in Uganda.*

This repository provides an R implementation of the authors' original Stata replication workflow.

The R replication starts from the analysis-ready datasets distributed with the original replication package, consistent with the normal analysis path in the authors' workflow. The upstream Stata cleaning pipeline is therefore not reconstructed as part of the standard R replication.

---

## 1. Overall Replication Status

The R implementation successfully reproduces the authors' Stata results for the deterministic tables examined.

After validation and correction of several Stata-to-R implementation details, the R results now match the original output in:

- coefficient estimates;
- observation counts;
- heteroskedasticity-robust standard errors;
- R-squared values;
- control means;
- treatment-effect tests; and
- the full set of reported loan-use outcomes.

The main and appendix figures also reproduce the same underlying values and substantive patterns.

The remaining differences are primarily presentational rather than substantive. These include differences in table layout, graphical ordering, axis formatting, bar geometry, colors, themes, and other software-specific formatting choices.

Accordingly, the deterministic analyses examined can be considered an **essentially exact numerical replication** of the original Stata results.

Some caveats remain for software-sensitive or nonstandard procedures, particularly the causal-forest analysis and the two `multe` contamination-bias diagnostics described below.

---

## 2. Table Replication

### 2.1 Coefficient estimates

The treatment-effect point estimates in the R implementation match the original Stata results for the tables compared.

This includes the primary treatment effects as well as the appendix specifications examined during validation.

The matching estimates indicate that the R implementation reproduces the main empirical specifications, including:

- treatment definitions;
- outcome definitions;
- baseline controls;
- strata fixed effects;
- treatment interactions; and
- the principal regression samples.

---

### 2.2 Observation counts

Earlier versions of the R implementation used smaller estimation samples in a number of regressions.

This discrepancy has been resolved.

The observation counts now match the corresponding Stata regressions, including previously problematic cases such as:

- 2,639 observations;
- 2,642 observations;
- 1,613 observations in the spouse-presence analysis; and
- 964 observations in the subsequent-loan deposit analysis.

The main correction was to align the treatment of fixed-effect singleton groups with the behavior required to reproduce the original Stata `areg` specifications.

The R fixed-effect regressions therefore retain singleton fixed-effect groups rather than automatically removing them.

---

### 2.3 Robust standard errors

The robust standard errors now match the original Stata output for the specifications compared.

Earlier differences arose from implementation details associated with reproducing the Stata fixed-effect regressions and their variance calculations.

After aligning the fixed-effect sample handling and robust variance settings, the reported standard errors match the corresponding Stata values.

Because both coefficient estimates and standard errors now match, the associated inference is also reproduced.

---

### 2.4 R-squared

R-squared values now match the original Stata output for the regressions compared.

This includes the subsequent-loan deposit specifications that previously showed large discrepancies.

For example, the R implementation now reproduces values such as:

- 0.35;
- 0.78; and
- 0.85

in the relevant subsequent-loan specifications.

The earlier differences were associated with the smaller R estimation samples rather than a different substantive model specification.

---

## 3. Loan-Use Table

The original Stata analysis reports seven loan-use outcomes:

1. Business;
2. Sharing;
3. School;
4. Home;
5. Expenditure;
6. Saving; and
7. Loan repayment.

An earlier version of the R implementation produced only five of the seven columns because of a variable-name inconsistency between the Stata analysis code and the distributed analysis dataset.

The Stata code refers to:

```text
loan_use_exp
loan_use_sav