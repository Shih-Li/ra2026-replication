# R Replication: Indirect Effects of Access to Finance

This folder provides an R implementation of the replication workflow for:

**Jing Cai and Adam Szeidl, “Indirect Effects of Access to Finance”**

The authors’ replication package identifies the manuscript as AER-2022-0711, final version submitted to the *American Economic Review* in January 2024, with OpenICPSR project 197302.

The original analysis was written for Stata 17. This replication translates the analysis into R while starting from the same author-prepared analysis datasets used by the original workflow.

## Running the replication

From the paper replication root, run:

```r
source("code/00_run_all.R")
```

For example:

```r
source("E:/26S2/RA/00_RA/09_IEoAtF/code/00_run_all.R")
```

The master script runs the numbered analysis scripts in order.

## Repository structure

```text
09_IEoAtF/
├── code/
│   ├── 00_run_all.R
│   ├── 01_setup.R
│   ├── 02_helpers.R
│   ├── 03_baseline_borrowing.R
│   ├── 04_main_effects_robustness.R
│   ├── 05_business_consumer.R
│   ├── 06_market_effects.R
│   ├── 07_local_spillovers_iv.R
│   ├── 08_welfare.R
│   └── 09_figure3.R
│
├── data/
│   └── source/
│       └── cleaned/
│           ├── loanmain.dta
│           ├── market.dta
│           ├── loan_welfare.dta
│           └── Figure3_do.xlsx        # optional; required only for Figure 3
│
└── results/
    ├── tables/
    └── figures/
```

Generated outputs are prefixed with `R-`.

Tables are written in both `.csv` and `.tex` formats. Figures are written as `.pdf`.

## Source data

The R workflow starts from the authors’ analysis-ready datasets, matching the starting point of the original Stata workflow.

Required inputs:

- `data/source/cleaned/loanmain.dta`
- `data/source/cleaned/market.dta`
- `data/source/cleaned/loan_welfare.dta`

Optional input:

- `data/source/cleaned/Figure3_do.xlsx`

`Figure3_do.xlsx` is an author-prepared workbook read directly by the original `Figure3.do`. It is required only to reproduce the final four-panel Figure 3. If the workbook is absent, the rest of the replication still runs and `09_figure3.R` skips Figure 3 with an explanatory message.

No additional raw datasets are required because the official analysis begins from these author-prepared analysis files.

## Code map

### `00_run_all.R`

Master runner. Loads setup and helpers, then executes all analysis scripts in sequence.

### `01_setup.R`

Defines project-relative paths, checks source inputs and required packages, creates output directories, and sets default replication counts for Romano-Wolf and welfare bootstrap procedures.

### `02_helpers.R`

Shared translation utilities, including:

- Stata data preparation,
- fixed-effects regressions,
- clustered inference,
- IV estimation,
- Stata-style variable and dummy handling,
- joint Wald tests,
- Romano-Wolf stepdown adjustment,
- cluster bootstrap utilities,
- standardized table and figure output.

### `03_baseline_borrowing.R`

Produces the baseline and borrowing analyses, including:

- Table 1,
- Table 2,
- Tables A1, A2, and A4,
- market-structure descriptive output,
- Figure 2.

### `04_main_effects_robustness.R`

Produces the main firm-level treatment-effect and robustness analyses, including:

- Table 3,
- Tables A3, A5, A6, and A7,
- the Figure 3 nonlinearity-check table.

The nonlinearity-check table is a diagnostic output and is not a substitute for the authors’ final Figure 3 workbook-based plot.

### `05_business_consumer.R`

Produces business and consumer outcome analyses:

- Table 4,
- Table 5,
- Tables A8 and A9.

### `06_market_effects.R`

Produces Table 6 market-level effects.

### `07_local_spillovers_iv.R`

Produces local/non-local spillover and instrumental-variable analyses:

- Table 7,
- Table 8,
- Table 9,
- Table A10.

### `08_welfare.R`

Produces welfare and return decompositions:

- Table 10,
- Table 11,
- Table A11,
- Table A12,
- welfare bootstrap diagnostics.

The welfare bootstrap follows the authors’ town-cluster resampling logic. Negative-yield bootstrap draws are retained as valid draws according to the original Stata program rather than classified as failed replications.

### `09_figure3.R`

Translates the authors’ separate `Figure3.do` workflow.

If `Figure3_do.xlsx` is available, it reads the `borrow`, `sales`, `profit`, and `labor` sheets and generates:

```text
results/figures/R-Figure3.pdf
```

If the workbook is unavailable, only Figure 3 is skipped.

## Required R packages

The main workflow uses:

- `haven`
- `dplyr`
- `tidyr`
- `purrr`
- `tibble`
- `stringr`
- `fixest`
- `ggplot2`
- `patchwork`
- `knitr`

If `Figure3_do.xlsx` is supplied, `readxl` is also required.

## Replication status

The current R implementation is treated as the completed statistical translation.

The validation exercise finds strong agreement with the original Stata results:

- the main coefficient estimates and standard errors reproduce closely across Tables 1–9;
- Table 6 and the first-stage/IV results in Table 9 reproduce particularly closely;
- the welfare results in Tables 10 and 11 are populated and reproduce the original reported values;
- the substantive patterns in Figure 2 are reproduced;
- the remaining differences are concentrated in reporting conventions, some fixed-effects observation counts, some adjusted multiple-testing p-values, and graphical styling.

Two residual differences are intentionally documented rather than tuned away:

1. some fixed-effects specifications report different observation counts in R and Stata despite essentially unchanged coefficient estimates; and
2. some Romano-Wolf adjusted p-values differ between the R and Stata implementations, with a small number of secondary outcomes crossing conventional significance thresholds.

These differences do not alter the paper’s main substantive conclusions.

The R tables use a standardized tidy output format rather than attempting to reproduce every LaTeX formatting feature of the published Stata tables.

## Figures

### Figure 2

The R figure reproduces the substantive density patterns of the original figure. Styling differs because the replication uses `ggplot2` rather than Stata graphics.

### Figure 3

Exact reproduction requires the authors’ `Figure3_do.xlsx` workbook. Without that file, the R workflow does not claim to reproduce the final four-panel Figure 3.

## Validation

See [`REPLICATION_SUMMARY.md`](REPLICATION_SUMMARY.md) for the detailed validation assessment, remaining differences, and final stopping decision.

## Overall assessment

This replication should be characterized as a **successful R translation of the authors’ Stata analysis with strong numerical agreement and a small number of documented implementation-level differences**.

The remaining differences do not materially change the empirical conclusions of the paper, so the current code is treated as the final replication version rather than being further tuned for exact cell-for-cell Stata equivalence.
