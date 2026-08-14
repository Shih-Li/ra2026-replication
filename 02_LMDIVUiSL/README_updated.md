# Replication Target 02

**Niccolò F. Meriggi, Maarten Voors, Madison Levine, Vasudha Ramakrishna, Desmond Maada Kangbai, Michael Rozelle, Ella Tyler, Sellu Kallon, Junisa Nabieu, Sarah Cundy, and Ahmed Mushfiq Mobarak, “Last-mile delivery increases vaccine uptake in Sierra Leone.”**

This directory provides a clean R translation of the authors' `vaccine_replication.do` workflow. The original Stata master begins directly from three de-identified, author-supplied analysis-ready datasets; it does not reconstruct them from raw survey files. The R replication therefore starts from the same cleaned data level.

## Replication status

**Successful replication.**

Systematic comparison against the authors' supplied outputs shows that, across all shared tables that were compared, the R implementation reproduces the original results exactly for:

- coefficient estimates;
- standard errors;
- sample sizes; and
- control means.

Reported R-squared values differ only because of display precision: the original output is generally rounded to two decimal places, whereas the R output reports three decimal places.

Wild-bootstrap p-values can differ slightly across Stata and R because the two implementations use different random-number generators and bootstrap implementations. These small differences do not change the substantive conclusions.

For the six figures that could be compared independently, every plotted numerical value matched the original output, including treatment effects, significance indicators, bar heights, literature-review effect sizes and costs, and sample counts. The remaining three supplementary figures could not be independently compared because the original and replication SVGs had identical filenames and one set overwrote the other in the comparison directory:

- `comm_vax_count_dist`
- `comm_vax_rate_dist`
- `vac_team`

These should be re-compared later using distinct filenames such as `_original` and `_rep` if a complete visual audit is desired.

See `REPLICATION_SUMMARY.md` for the detailed validation record.

## Required source data

Place exactly these files in `data/source/cleaned/`:

- `individual_level.dta`
- `community_data.dta`
- `lit_review.dta`

The original codebooks, original graphs/tables, and `list.txt` are not required for normal execution. Temporary datasets created by the Stata master are regenerated in memory by the R scripts.

## R packages

Install the core packages once:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "purrr", "tibble", "stringr",
  "ggplot2", "fixest", "dqrng", "knitr", "writexl", "nnet"
))
```

For the Stata `boottest` translations, install `fwildclusterboot`. If it is not available from your default CRAN mirror, use the maintainer's R-universe:

```r
install.packages(
  "fwildclusterboot",
  repos = c("https://s3alfisc.r-universe.dev", "https://cloud.r-project.org")
)
```

## Run

Open `02_LMDIVUiSL.Rproj`, then run:

```r
source("code/00_master.R")
```

Execution switches are at the top of `code/01_setup.R`:

```r
RUN_IN_TEXT <- TRUE
RUN_FIGURES <- TRUE
RUN_TABLES <- TRUE
RUN_SUPPLEMENT <- TRUE
RUN_WILD_BOOTSTRAP <- TRUE
WILD_REPS <- 999L
WILD_SEED <- 420L
```

Set `RUN_WILD_BOOTSTRAP <- FALSE` for faster development runs. Point estimates and conventional standard errors will still be produced, while bootstrap p-values will be left blank.

## Script map

- `code/00_master.R` — orchestrates the complete R workflow.
- `code/01_setup.R` — paths, source-data checks, fixed-effect/clustered regression helpers, wild-bootstrap wrapper, sharpened FDR q-values, balance-test helpers, and table/figure writers.
- `code/02_intext_calculations.R` — translates the source do-file's prose/in-text calculations and writes diagnostics to `output/diagnostics/`.
- `code/03_figures.R` — translates main-text Figures 2–7.
- `code/04_tables.R` — translates Tables 1–8 and 10.
- `code/05_supplementary.R` — translates CONSORT counts, SI Figures A2–A4, and the 16-part systematic literature-review Table A1.

## Output map

Main figures are written to `output/figures/main/` using the original stems:

- `vacrate_pooled`
- `vaccount_pooled`
- `knowledge_attitudes`
- `respondent_characteristics`
- `literature_effects`
- `literature_cost`

Supplementary figures are written to `output/figures/appendix/`:

- `comm_vax_rate_dist`
- `comm_vax_count_dist`
- `vac_team`

Main and extended-data numerical tables are written to `output/tables/main/` as `.csv` and `.tex`; the balance/sample-comparison tables also receive `.xlsx` files. The systematic literature-review appendix is written to `output/tables/appendix/`.

## Important replication notes

1. **Starting data level.** The source `vaccine_replication.do` directly loads `individual_level.dta`, `community_data.dta`, and `lit_review.dta`. This R version therefore treats all three as required cleaned inputs and does not invent an upstream raw-data pipeline.

2. **Community-covariate merge.** The Stata source uses `merge ..., update replace` for five community-level covariates. `merge_community_covariates()` explicitly gives nonmissing `community_data.dta` values precedence to preserve that behavior.

3. **Fixed effects and clustered inference.** Stata `areg`/`reghdfe` specifications are translated with `fixest::feols()`, including the source fixed effects and clustering level.

4. **Wild bootstrap.** Source `boottest ..., seed(420) reps(999)` calls are translated using `fwildclusterboot`. Clustered specifications use the supported fixed-effect bootstrap path. The unclustered absorbed-fixed-effect specification in Table 3 column (3) is represented equivalently with explicit fixed-effect dummies for the heteroskedastic bootstrap. Both `set.seed()` and `dqrng::dqset.seed()` are set before each bootstrap call.

5. **Bootstrap reproducibility across software.** The Stata and R implementations do not use identical RNG/bootstrap engines, so bootstrap p-values need not match draw-for-draw even when the same nominal seed and number of replications are used. Validation found only small p-value differences, with no substantive change in inference.

6. **Sharpened FDR q-values.** Tables 6 and 7 use the two-stage, 0.001-grid sharpened q-value algorithm coded explicitly in the Stata master. The R helper `sharpened_qvalues()` translates that algorithm directly rather than replacing it with `p.adjust()`.

7. **Figure 3 temporary files.** The Stata source saves and merges three temporary community-level datasets. The R code recreates all three definitions in memory and does not write generated intermediates into `data/`.

8. **Implicit Stata state removed.** Figure 7 implicitly continues from the Figure 6 literature-review dataset, and Table 7 implicitly continues from Table 6's restricted individual sample. The R scripts explicitly reconstruct those inputs so each section is reproducible independently.

9. **Formatting differences.** The translation targets the estimands, samples, standard errors, bootstrap p-values, q-values, statistics, and plotted values rather than byte-for-byte formatting. The R output therefore uses simpler `knitr` LaTeX, standard R numeric formatting, and native `ggplot2` rendering. Differences such as `0.05` versus `.05`, `16.7` versus `16.700001`, or `***` versus Stata macro wrappers are cosmetic only.

10. **Output-inventory note.** In one comparison workspace, files such as `effect_size_table_1–5.tex`, `att_willing.tex`, and `census_bal.tex` appeared only on the replication side. However, the original package inventory supplied for this project also lists these outputs, so they should not be interpreted as new substantive analyses added by the R replication.

## Validation conclusion

The replication is considered successful. All shared tabular estimands and conventional inference statistics match, and all six independently comparable figures match in their underlying plotted data. The only remaining validation task is an optional side-by-side check of the three supplementary SVG figures whose filenames collided during comparison.
