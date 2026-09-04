# R Replication: The Power of Information

This folder provides an R implementation of the replication workflow for:

**Nina Bruvik Westberg, Sofie Waage Skjeflo, and Steffen Kallbekken, “The Power of Information: A Survey Experiment on Public Support for Electricity Price Compensation Schemes.”**

- Published article: https://www.sciencedirect.com/science/article/pii/S0301421525002447
- Original replication package: https://osf.io/5dfvg/overview

The authors' original replication is organized as a single Quarto analysis file, `Replication.qmd`. This repository reorganizes that workflow into numbered R scripts while preserving the authors' data preparation, model specifications, hypothesis tests, tables, and figures.

## Running the replication

From the paper replication root, run:

```r
source("code/00_run_all.R")
```

The master script runs the numbered analysis scripts in order and writes generated outputs under `results/` with an `R-` prefix.

## Repository structure

```text
11_TPoI/
├── code/
│   ├── 00_run_all.R
│   ├── 01_setup.R
│   ├── 02_data_prep_quality.R
│   ├── 03_descriptives.R
│   ├── 04_primary_hypotheses.R
│   ├── 05_prior_beliefs.R
│   ├── 06_heterogeneity_beliefs.R
│   ├── 07_financial_impact.R
│   └── 08_session_info.R
│
├── data/
│   └── source/
│       └── cleaned/
│           └── surveydata.rds
│
└── results/
    ├── tables/
    ├── figures/
    └── R-session-info.txt
```

## Source data

The official replication workflow begins directly from the author-provided anonymized analysis dataset:

```text
data/surveydata.rds
```

Accordingly, the clean R repository uses:

```text
data/source/cleaned/surveydata.rds
```

Classification:

```text
surveydata.rds
→ data/source/cleaned/surveydata.rds
→ Author-prepared analysis dataset used directly by Replication.qmd
→ REQUIRED CLEANED INPUT
```

No raw-data reconstruction is required because the authors' official analysis itself starts from this analysis-ready `.rds` file.

## Script map

### `00_run_all.R`

Master runner. Clears the workspace, loads setup, and executes the numbered analysis scripts in sequence.

### `01_setup.R`

Defines project-relative paths, checks the required source file and R packages, creates output directories, sets the locale when available, and defines standardized `R-` output paths.

### `02_data_prep_quality.R`

Reproduces the initial data preparation and sample checks:

- treatment indicators for the four randomized groups;
- two-question attention check;
- final analysis sample;
- Table A2 sample-composition summaries;
- Table A3 summary statistics and balance checks.

### `03_descriptives.R`

Produces Figure 1, showing support for the two compensation schemes in the control group.

### `04_primary_hypotheses.R`

Produces the main experimental results and robustness analyses:

- Table 2 — ordered-logit treatment effects;
- Table A4 — Benjamini-Hochberg multiple-testing adjustment;
- Table A5 — linear-probability robustness models with HC1 standard errors;
- Figure 2A — price-subsidy support by treatment group;
- Figure 2B — lump-sum support by treatment group.

### `05_prior_beliefs.R`

Produces:

- Table 3A — prior beliefs about the distribution of compensation;
- Table 3B — prior beliefs about electricity-saving incentives;
- Table 4 — association between prior beliefs and policy support in the control group.

### `06_heterogeneity_beliefs.R`

Produces:

- Table 5 — heterogeneous treatment effects by whether prior beliefs are incorrect;
- Table A6 — heterogeneous treatment effects by type of incorrect belief.

### `07_financial_impact.R`

Produces:

- Table 6 — policy support and household financial characteristics in the control group;
- Table 7 — heterogeneous treatment effects by electricity expenditure.

### `08_session_info.R`

Writes the R software environment to:

```text
results/R-session-info.txt
```

## Generated outputs

The R workflow generates counterparts to the authors' saved replication outputs:

```text
results/figures/R-Figure1.png
results/figures/R-Figure2A.png
results/figures/R-Figure2B.png

results/tables/R-Table2.html
results/tables/R-Table3A.xlsx
results/tables/R-Table3B.xlsx
results/tables/R-Table4.html
results/tables/R-Table5.html
results/tables/R-Table6.html
results/tables/R-Table7.html
results/tables/R-TableA3.docx
results/tables/R-TableA4.xlsx
results/tables/R-TableA5.html
results/tables/R-TableA6.html
```

The workflow additionally writes:

```text
results/tables/R-TableA2.xlsx
```

The original Quarto file prints the Table A2 sample-composition components but does not save them as a standalone file. `R-TableA2.xlsx` is therefore an additional convenience output rather than a claimed counterpart to an original saved artifact.

## Validation status

The final reviewed replication reproduces the original results cleanly for the outputs included in the direct comparison set.

- **Figure 1:** byte-for-byte identical.
- **Figure 2A:** underlying data and final layout match; residual error-bar differences are at most approximately one pixel and are consistent with rendering/anti-aliasing noise.
- **Figure 2B:** matches apart from at most approximately one-pixel rendering noise in the error bars.
- **Table 3A:** identical values at full precision.
- **Table 3B:** identical values.
- **Table A3:** extracted table content matches, including the final `male` label.
- **Table A4:** identical raw and adjusted p-values.
- **Table A5:** byte-for-byte identical in the reviewed comparison.

Tables 2, 4, 5, 6, 7, and A6 are generated by the final R scripts using the same model specifications as the authors' Quarto workflow. They were not included in the reported direct file-comparison set, so the strongest file-level validation claim is reserved for the outputs listed above.

See `REPLICATION_SUMMARY.md` for the final validation record and stopping decision.

## Implementation notes

1. **Figure 2A layout.** The final R version suppresses the response legend to match the authors' saved Figure 2A. This affects layout only, not the plotted values or confidence intervals.

2. **Table A3 gender label.** The final implementation targets the derived `male` variable and preserves the original displayed row label `male`. This resolves the variable/label inconsistency in the source code while matching the saved original table.

3. **Figure 1 response coding.** The authors' Figure 1 block uses response code `99` for “Don't know,” whereas later support analyses omit response code `6`. The R translation follows each source block as written rather than silently reconciling the coding difference.

4. **Norwegian locale.** The source requests `nb_NO.UTF-8`. The setup script attempts this locale but does not fail if it is unavailable on the operating system.

5. **Saved regression tables.** Several `stargazer(..., out = ...)` lines are commented in the source Quarto file. The clean replication enables those exports without changing the underlying statistical specifications.

## Final status

The R translation is treated as complete. The outputs directly compared against the authors' saved files reproduce their numerical content, and the remaining visible figure differences are negligible rendering effects rather than statistical differences.

No further code revision is recommended solely to pursue byte-level equality across software-generated metadata or sub-pixel graphics rendering.
