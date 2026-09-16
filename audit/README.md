# MIS Audit

This folder contains the cross-paper **Most Influential Set (MIS)** audit for the replication studies in this repository.

The audit asks a focused robustness question:

> **How sensitive is a selected reported coefficient to deleting a small set of observations?**

For each paper, the audit starts from a validated paper-specific estimation sample and coefficient, searches for deletion sets that move the target coefficient upward or downward, and then re-estimates each selected deletion set using the paper-specific estimator.

The audit is therefore an **observation-sensitivity audit of a selected estimand**, not a re-analysis of every result in a paper and not an audit of the experimental design as a whole.

---

## 1. Folder structure

```text
audit/
├── README.md
├── audit.Rproj
├── function/
│   ├── audit_engine.R
│   ├── audit_output.R
│   ├── audit_plot.R
│   ├── audit_validate.R
│   └── dinkelbach_topk.R
├── scripts/
│   ├── 01_RSPitHUMM.R
│   ├── 02_LMDIVUiSL.R
│   ├── 03_WbHS.R
│   ├── 04_SBN-HIWTO.R
│   ├── 05_TPCvIKT.R
│   ├── 06_UHGtBtCEoaUWRP.R
│   ├── 07_SwSIiUS.R
│   ├── 08_IEoAtF.R
│   ├── 09_StMEN.R
│   └── 10_TPoI.R
└── output/
    └── <paper-specific audit outputs>
```

The files in `function/` provide the shared MIS search, validation, output, and plotting machinery. Each file in `scripts/` adapts that machinery to one paper's validated specification.

---

## 2. Standard audit protocol

The paper-specific scripts follow the same broad protocol.

1. **Reproduce and validate the baseline estimand.**  
   The script checks the estimation sample and target coefficient against the corresponding replication benchmark before the MIS search runs.

2. **Construct an MIS-compatible linear representation.**  
   The shared search routine operates on an OLS representation. Fixed effects, weighted least squares, and selected nuisance specifications are represented in a way that preserves the target coefficient at baseline.

3. **Search over deletion sizes.**  
   The default protocol considers

   \[
   k = 1, \ldots, \lfloor 0.05N \rfloor,
   \]

   subject to any paper-specific feasibility restrictions.

4. **Search in both directions.**  
   For each `k`, the audit constructs candidate deletion sets that increase and decrease the target coefficient.

5. **Exact paper-estimator refit.**  
   Every selected deletion set is re-estimated using the paper-specific estimator. The reported coefficient path therefore comes from exact refits of feasible deletion sets, rather than from the search approximation alone.

6. **Save standardized outputs.**  
   The audit records the coefficient path, deleted observation identifiers, relative changes, nestedness information, and standardized figures/tables in `audit/output/<study_id>/`.

The shared search implemented in `function/audit_engine.R` uses a linear-fractional relaxation to identify influential candidate sets. Except in special designs where that relaxation is exact, the search should be interpreted as a computational procedure for finding highly influential feasible sets rather than as an exhaustive enumeration of all possible deletion sets.

---

## 3. What is being audited?

Each paper has **one explicitly selected target estimand** in the current audit. The target is usually a substantively important treatment, exposure, or policy coefficient, but it is not necessarily the only important coefficient in the paper.

| Paper | MIS target in this repository | Main scope note |
|---|---|---|
| **01 — Resisting Social Pressure in the Household Using Mobile Money** | Table 1 primary outcome `capital`; target `treatment3` (Mobile Disburse) | Audits the selected treatment coefficient in the paper's fixed-effects OLS specification. |
| **02 — Last-mile delivery increases vaccine uptake in Sierra Leone** | Table 1, column (1); outcome `vaccinated_endline`; target `treat_dtd` | Audits the selected treatment coefficient and retains the other treatment arm as a nuisance treatment regressor. |
| **03 — What's Behind Her Smile? Health, Looks, and Self-Esteem** | Table 5, first follow-up, full sample; outcome `sd_rosen1`; target `treatment` | Audits the treatment coefficient in the preferred linear specification with baseline controls and randomization-strata indicators. |
| **04 — Surviving Bad News: Health Information without Treatment Options** | Table 2 survival outcome; target `learnhivpos` | **Important:** the paper's preferred causal specifications are IV / efficient linear GMM. The current MIS engine is not an IV/GMM engine, so this script audits the reported **OLS companion specification**, not the preferred IV-GMM causal estimate. |
| **05 — Testing Paternalism: Cash versus In-Kind Transfers** | Aggregate food consumption; target contrast `ATT(In-kind) - ATT_EQ(Cash)` | Uses a reparameterized, validated **no-village-controls** specification. It should not be described as an audit of the unresolved controlled specification. |
| **06 — Using Household Grants to Benchmark the Cost Effectiveness of a USAID Workforce Readiness Program** | Main ITT; outcome `bn_employed`; target `treat_HD` | The paper estimator is fixed-weight WLS. The MIS search uses the exact `sqrt(w)` transformation, followed by exact weighted refits. |
| **07 — Spillovers without Social Interactions in Urban Sanitation** | Table 2 decision spillovers; treated-household sample; target `neighbor_high` | The baseline post-double-selection LASSO is reproduced once. Selected nuisance controls are then **frozen** during the deletion path; LASSO is not re-run after every deletion. |
| **08 — Indirect Effects of Access to Finance** | Table 3 main firm-level effects; outcome `lnpart5revenue`; target `inter4post` | Audits the competitor-treatment exposure coefficient in a firm fixed-effects specification, with exact paper-estimator refits. |
| **09 — Selecting the Most Effective Nudge** | Post-LASSO / pooled policy model for `shots_per_dollar`; selected pooled-policy target coefficient | The published model-selection and pooling workflow is reconstructed once and then **frozen**. Fixed-weight WLS is represented through the exact `sqrt(w)` transformation for the MIS search. |
| **10 — The Power of Information** | Table A5 linear-probability robustness model; outcome `support_priceB`; target `T1` | **Important:** the headline model is ordered logit. Because the shared MIS engine is linear, the audit uses the paper's reported **linear-probability robustness specification**, not the headline nonlinear estimator. |

These scope choices are documented at the top of the corresponding files in `audit/scripts/`.

---

## 4. Interpretation

The central object is the path

\[
\hat\beta_{(-S_k)},
\]

where `S_k` is a selected set of `k` deleted observations and `\hat\beta` is the target coefficient.

The audit is useful for questions such as:

- How much can the target coefficient move after deleting a very small number of observations?
- Is the baseline estimate relatively stable over the first 1%, 2%, or 5% of deletions?
- Is sensitivity concentrated in a small influential subset of the estimation sample?
- Do coefficient-increasing and coefficient-decreasing deletion paths behave differently?

The audit **does not by itself establish** that an estimate is correct or incorrect, causal or non-causal, replicated or non-replicated. It measures one specific dimension of empirical robustness: sensitivity of a selected coefficient to observation deletion.

Similarly, standard errors and clustering choices remain important for inference, but the MIS target here is the **point estimate**. In several paper-specific scripts, robust or clustered covariance estimation affects inference without changing the OLS point coefficient used in the deletion search.

---

## 5. Running the audit

The intended entry point is the audit R project.

1. Open:

```text
audit/audit.Rproj
```

2. Make sure the corresponding paper replication has already been run when the audit script depends on generated analysis-ready data or saved replication objects.

3. From the `audit/` project, source the desired paper script. For example:

```r
source("scripts/01_RSPitHUMM.R")
source("scripts/02_LMDIVUiSL.R")
source("scripts/03_WbHS.R")
```

and analogously for Papers 4–10.

Each script performs its own baseline checks before running the MIS audit. If the expected analysis-ready data, estimation sample, or benchmark coefficient does not match, the script is designed to stop rather than silently continue with a different estimand.

---

## 6. Important implementation choices

### Fixed effects

Where needed, fixed effects are represented explicitly for the MIS-compatible linear model. The paper-specific script verifies baseline coefficient equivalence before the deletion search.

### Weighted least squares

For fixed weights, the audit uses the exact transformation

\[
y^* = \sqrt{w}\,y, \qquad X^* = \sqrt{w}\,X,
\]

so deleting a transformed row corresponds to deleting the associated original observation while keeping the baseline weights fixed. Selected deletion sets are then checked with the original weighted estimator.

### Model selection

For Papers 7 and 9, model-selection steps are reconstructed on the validated full sample and then frozen. Re-running selection after every deletion would change the nuisance specification—and potentially the estimand—along the path, which is not the fixed-coefficient deletion problem targeted by the shared MIS routine.

### Nonlinear and IV/GMM estimators

The current shared MIS search is designed around a linear model representation. It should not be interpreted as a general-purpose deletion engine for arbitrary nonlinear, IV, or GMM estimators.

This is why:

- Paper 4 audits an OLS companion specification rather than the preferred IV/GMM estimate; and
- Paper 10 audits a reported linear-probability robustness specification rather than the headline ordered-logit model.

Any future extension to nonlinear or IV/GMM MIS analysis should be implemented and documented separately rather than treating the current linear audit as equivalent.

---

## 7. Reproducibility principle

A paper enters the MIS search only after the script has established that the audit representation matches the intended baseline estimand closely enough for that paper's validation standard.

The guiding rule is:

> **Validate first, perturb second.**

The purpose of the paper-specific setup code is to make clear exactly which sample, coefficient, controls, fixed effects, weights, and model-selection choices are being held fixed when observation sensitivity is evaluated.
