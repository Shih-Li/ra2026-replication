# Replication Target 01

**Emma Riley (2023), “Resisting Social Pressure in the Household Using Mobile Money: Experimental Evidence on Microenterprise Investment in Uganda.”**

This is a clean R translation of the authors' analysis workflow. It starts from the same six author-supplied analysis-ready datasets used when `global clean = 0` in the original `master.do`. It does **not** reconstruct the optional upstream Stata cleaning pipeline.

## Required source data

Place these files in `data/source/cleaned/`:

- `survey_data.dta`
- `BRAC admin data.dta`
- `BRAC_MM_merged.dta`
- `MM_balances.dta`
- `MM_transactions.dta`
- `MM_useage.dta`

Do not rename them unless you also update the filenames in the R scripts.

## R packages

Install the core packages once:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "fixest", "knitr", "grf"
))
```

## Run

Open `01_RSPitHUMM.Rproj`, then run:

```r
source("code/00_master.R")
```

The switches at the top of `code/01_setup.R` are:

```r
RUN_GRAPHS <- TRUE
RUN_PERMUTATION <- TRUE
RUN_CAUSAL_FOREST <- TRUE
PERM_REPS <- 1000L
```

Set the costly pieces to `FALSE` during development if desired.

## Script map

- `code/00_master.R` — orchestrates the R workflow.
- `code/01_setup.R` — paths, outcome lists, Stata-style fixed-effect/robust-regression helpers, table writers, FDR and permutation helpers.
- `code/02_takeup.R` — translation of `analysis_takeup.do`; Figure 1 and Table A3.
- `code/03_balance.R` — translation of `analysis_balancecheck2.do`; Table A2.
- `code/04_main_analysis.R` — translation of the analysis sections of `master.do`; main Tables 1–6 and most appendix outputs.
- `code/05_causal_forest.R` — direct R/`grf` translation of `causal_forest.do`; Table A21.
- `code/06_kmeans.R` — translation of `k-means.do`; Tables A23–A24.
- `code/functions/weightave.R` — faithful translation of the supplied `_gweightave.ado` for validation/future from-raw reconstruction.

## Important replication notes

1. **Starting data level.** The authors' `master.do` only runs the six cleaning scripts when `global clean == 1`; otherwise it loads `input/survey_data.dta` and begins analysis. This R version therefore treats the six supplied `input/` datasets as source/cleaned inputs.

2. **Robust fixed-effect regressions.** Stata `areg ..., absorb(strata_fixed_base) vce(robust)` is translated with `fixest::feols(... | strata_fixed_base, vcov = "hetero")`, with fixed effects included in the HC1 small-sample parameter count.

3. **FDR q-values.** The Benjamini-Yekutieli adjustment is implemented with `p.adjust(..., method = "BY")` across the same outcome families used in the Stata code.

4. **Permutation Table A14.** The source Stata `permute` commands regress the outcome only on `treatment2 treatment3` while permuting one treatment indicator within randomization strata. They do **not** include the baseline outcome or absorbed strata in the permutation regression. The R code preserves that source choice.

5. **Causal forest Table A21.** The original replication README states that the now-unmaintained `mlrtime` implementation does not reproduce exactly across machines. The R translation uses `grf::causal_forest` directly and reproduces the source's two-fold holdout/cross-fitting logic. Small numerical differences from the published table should therefore be expected.

6. **K-means Table A23/A24.** K-means implementations and cluster numbering can differ across software even under fixed seeds. The code preserves standardization, `k = 4`, seed 123 for initialization, and the source regression specification, but cluster numbers should be validated against the authors' output before attaching the paper's semantic cluster names.

7. **`multe` contamination diagnostic.** `master.do` contains two text-only `multe ... est(OWN) diff` calls. The external `multe` implementation was not among the supplied files, so the R code does not invent a replacement. It writes a note to `output/tables/appendix/contamination_bias_NOT_REPLICATED.txt`. This does not remove any of the named table/figure scripts listed in the replication README.

8. **Table formatting.** The R scripts write both `.csv` and `.tex` numerical tables. The `.tex` files are clean `knitr::kable` tables, not byte-for-byte reproductions of the authors' hand-formatted `esttab/outreg2/texsave` LaTeX wrappers. The estimands, specifications, test statistics, and output content are the replication target.

## Replication status

The R implementation reproduces the authors' Stata results essentially exactly for
the deterministic tables examined, including coefficient estimates, observation
counts, robust standard errors, R-squared values, and the full set of reported
outcomes.

The figures reproduce the same underlying values and substantive patterns, with only
minor presentational differences such as ordering, axis formatting, graphical
geometry, colors, and themes.

Some numerical variation may remain in software-sensitive procedures such as the
causal-forest analysis.

See [REPLICATION_SUMMARY.md](REPLICATION_SUMMARY.md) for a detailed comparison.