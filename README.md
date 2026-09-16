# RA2026 Replication

This repository contains **R-based replication workflows for ten published empirical studies**, together with a cross-paper **Most Influential Set (MIS) audit** of selected estimates.

The project has two related goals:

1. **Replication:** translate, reconstruct, and validate paper-specific empirical workflows in R using the authors' original replication materials.
2. **Robustness auditing:** evaluate how sensitive a selected coefficient from each study is to deleting small sets of observations.

Each paper is kept in its own project folder with paper-specific code, data instructions, outputs, and replication notes. The shared MIS audit lives in [`audit/`](audit/).

> The original replication packages linked below remain the reference source for the authors' original data and code. The paper-specific READMEs in this repository document exactly what is reproduced, translated, reconstructed, or intentionally left outside the R workflow.

---

## Repository structure

```text
ra2026-replication/
├── 01_RSPitHUMM/
├── 02_LMDIVUiSL/
├── 03_WbHS/
├── 04_SBN-HIWTO/
├── 05_TPCvIKT/
├── 06_UHGtBtCEoaUWRP/
├── 07_SwSIiUS/
├── 08_IEoAtF/
├── 9_StMEN/
├── 10_TPoI/
├── audit/
└── README.md
```

Paper folders generally contain their own R project, source/processed data structure, analysis code, outputs, and documentation. Because the original replication packages differ substantially across papers, the exact setup and execution instructions are intentionally documented within each paper folder rather than forced into one common workflow.

---

## Papers and original replication packages

| # | Repository folder | Paper | Published article | Original replication package |
|---:|---|---|---|---|
| 1 | [`01_RSPitHUMM`](01_RSPitHUMM/) | **Resisting Social Pressure in the Household Using Mobile Money: Experimental Evidence on Microenterprise Investment in Uganda** | [AEA](https://www.aeaweb.org/articles?id=10.1257/aer.20220717) | [openICPSR](https://www.openicpsr.org/openicpsr/project/194886/version/V2/view) |
| 2 | [`02_LMDIVUiSL`](02_LMDIVUiSL/) | **Last-mile delivery increases vaccine uptake in Sierra Leone** | [Nature](https://www.nature.com/articles/s41586-024-07158-w#data-availability) | [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRXF5Z) |
| 3 | [`03_WbHS`](03_WbHS/) | **What's Behind Her Smile? Health, Looks, and Self-Esteem** | [AEA](https://www.aeaweb.org/articles?id=10.1257/app.20210248) | [openICPSR](https://www.openicpsr.org/openicpsr/project/159261/version/V1/view) |
| 4 | [`04_SBN-HIWTO`](04_SBN-HIWTO/) | **Surviving Bad News: Health Information without Treatment Options** | [AEA](https://www.aeaweb.org/articles?id=10.1257/aeri.20240058) | [openICPSR](https://www.openicpsr.org/openicpsr/project/204883/version/V1/view) |
| 5 | [`05_TPCvIKT`](05_TPCvIKT/) | **Testing Paternalism: Cash versus In-Kind Transfers** | [AEA](https://www.aeaweb.org/articles?id=10.1257/app.6.2.195) | [openICPSR](https://www.openicpsr.org/openicpsr/project/113887/version/V2/view) |
| 6 | [`06_UHGtBtCEoaUWRP`](06_UHGtBtCEoaUWRP/) | **Using Household Grants to Benchmark the Cost Effectiveness of a USAID Workforce Readiness Program** | [ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0304387822000451#da1) | [OSF](https://osf.io/yrh4e/overview) |
| 7 | [`07_SwSIiUS`](07_SwSIiUS/) | **Spillovers without Social Interactions in Urban Sanitation** | [AEA](https://www.aeaweb.org/articles?id=10.1257/app.20220047) | [openICPSR](https://www.openicpsr.org/openicpsr/project/181101/version/V1/view) |
| 8 | [`08_IEoAtF`](08_IEoAtF/) | **Indirect Effects of Access to Finance** | [AEA](https://www.aeaweb.org/articles?id=10.1257/aer.20220711) | [openICPSR](https://www.openicpsr.org/openicpsr/project/197302/version/V1/view) |
| 9 | [`9_StMEN`](9_StMEN/) | **Selecting the Most Effective Nudge: Evidence From a Large-Scale Experiment on Immunization** | [Econometrica / Wiley](https://onlinelibrary.wiley.com/doi/full/10.3982/ECTA19739) | [Zenodo](https://zenodo.org/records/11392582) |
| 10 | [`10_TPoI`](10_TPoI/) | **The Power of Information: A Survey Experiment on Public Support for Electricity Price Compensation Schemes** | [ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0301421525002447#da0010) | [OSF](https://osf.io/5dfvg/overview) |

---

## Running a paper replication

The workflows are paper-specific. A reliable starting procedure is:

1. Choose a paper folder from the table above.
2. Read that folder's `README` before running code.
3. Obtain the required author-supplied data from the linked original replication package and place it in the location specified by the paper README.
4. Open the paper's `.Rproj` file when one is provided.
5. Install the R packages listed for that replication.
6. Run the paper's documented master/entry script.
7. Check the generated outputs and, where provided, the paper's replication summary for known discrepancies or software-sensitive results.

Several paper workflows use a master script of the form

```r
source("code/00_master.R")
```

but this should **not** be assumed for every folder. Follow the local README for the exact entry point, required inputs, switches, and expected outputs.

---

## Replication approach

The aim is substantive computational reproducibility rather than byte-for-byte reproduction of every original file.

Depending on the source package, a paper-specific workflow may translate Stata or other software into R, begin from author-supplied analysis-ready data rather than reconstructing an optional upstream cleaning pipeline, reproduce numerical tables while using different presentation code, or document procedures that are inherently software- or implementation-sensitive.

For that reason, the **paper-level README is the authoritative guide to the scope of each R replication**. Where a `REPLICATION_SUMMARY.md` is present, it provides a more detailed comparison of reproduced outputs and remaining differences.

---

## Most Influential Set (MIS) audit

The [`audit/`](audit/) project applies a common observation-sensitivity framework across the ten studies.

For each paper, the current audit selects **one explicitly documented estimand**, validates the baseline estimation sample and coefficient, searches for small deletion sets that move the coefficient upward or downward, and then re-estimates the selected deletion sets using the paper-specific estimator.

The default deletion path considers sizes up to approximately **5% of the estimation sample**, subject to paper-specific feasibility restrictions.

The MIS exercise should be interpreted narrowly:

- it is an **observation-sensitivity audit of a selected coefficient**;
- it is **not** an audit of every result in the paper;
- it is **not** a test of the experimental design or randomization itself; and
- for models that are not directly compatible with the shared linear MIS engine, the paper-specific audit documents the exact linear specification or transformation being audited.

Important examples include the use of an OLS companion specification for the IV/GMM paper, a reported linear-probability robustness model instead of a headline ordered-logit model, exact transformations for fixed-weight WLS, and frozen nuisance specifications when the published workflow uses model selection.

See **[`audit/README.md`](audit/README.md)** for the full protocol, paper-by-paper audit targets, implementation details, interpretation, and run instructions.

---

## Documentation hierarchy

When working with this repository, use the documentation in the following order:

1. **This README** — project overview and navigation.
2. **Paper-folder README** — required data, packages, execution instructions, and paper-specific scope.
3. **Paper replication summary, if present** — detailed reproduction results and known discrepancies.
4. **`audit/README.md`** — cross-paper MIS methodology and audit-specific scope.
5. **Script headers and code comments** — exact estimands, validation checks, implementation decisions, and technical details.

This separation is deliberate: the root README describes the project, while paper-specific and audit-specific assumptions remain documented next to the code that implements them.
