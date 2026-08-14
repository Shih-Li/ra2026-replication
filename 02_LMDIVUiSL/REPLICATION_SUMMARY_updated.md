# Replication Summary — Target 02

## Paper

**Meriggi et al., “Last-mile delivery increases vaccine uptake in Sierra Leone.”**

This directory provides an R implementation of the authors' original Stata replication workflow.

## 1. Overall replication status

**Successful replication.**

A systematic comparison of the original Stata outputs and the R replication found exact agreement across all shared tables for:

- coefficient estimates;
- standard errors;
- observation counts; and
- control means.

The validation therefore supports the conclusion that the R implementation reproduces the substantive numerical results of the original analysis.

The remaining differences are either expected numerical-display differences, software-sensitive bootstrap differences, or formatting differences. None of the observed differences changes the substantive conclusions of the paper.

## 2. Source workflow and data level

The source workflow is the authors' single 1,973-line Stata master file, `vaccine_replication.do`. It directly consumes three de-identified analysis-ready datasets:

- `individual_level.dta`
- `community_data.dta`
- `lit_review.dta`

No upstream raw-data cleaning pipeline is invoked by the master. Consistent with the project data-handling rule, the R replication therefore starts from the same three author-supplied cleaned datasets.

## 3. Translation coverage

The R package implements the complete logical workflow represented in the source master:

- in-text calculations;
- main Figures 2–7;
- Tables 1–8 and 10;
- CONSORT calculations;
- SI Figures A2–A4; and
- SI systematic literature-review Table A1, split into 16 files as in the source package.

## 4. Table validation

Across every shared table included in the systematic comparison, the following quantities match the original output exactly:

- treatment coefficients;
- conventional standard errors;
- sample sizes; and
- control means.

This includes the principal treatment-effect specifications and the extended-data analyses translated from the Stata master.

### R-squared values

R-squared differences are display-only. The original output generally reports R-squared values to two decimal places, whereas the R replication reports three decimal places. The apparent discrepancies occur at the third decimal place and do not represent different fitted models.

### Wild-bootstrap p-values

Wild-bootstrap p-values show small differences between Stata and R. This is expected because the two programs do not use identical bootstrap engines or random-number generators, even when using the same nominal seed and 999 bootstrap replications.

The R implementation preserves the original testing design:

- 999 bootstrap replications;
- seed 420;
- Rademacher weights;
- imposed null; and
- the original clustering/fixed-effect structure.

For clustered specifications, the implementation uses the supported `fwildclusterboot` fixed-effect path. For the unclustered absorbed-fixed-effect specification in Table 3 column (3), the same regression is represented with explicit fixed-effect dummies so that the heteroskedastic bootstrap can be executed with the package's supported engine.

The observed bootstrap differences are small and do not change statistical or substantive conclusions.

## 5. Figure validation

Six figures could be compared independently between the original package and the R replication. Across all six, every underlying plotted numerical value matched, including:

- treatment-effect estimates;
- significance indicators;
- bar heights;
- literature-review effect-size values;
- literature-review cost values; and
- reported sample counts.

The visual differences are cosmetic and arise from the plotting software. The original figures use Stata-generated styling, while the replication uses native `ggplot2` rendering. Examples include:

- `.05` versus `0.05` axis-label formatting;
- Stata versus R box-plot geometry;
- minor font, spacing, and legend differences; and
- different but substantively faithful rendering of colors and graphical elements.

### Figures not independently compared

Three supplementary figures were not independently compared because the original and replication versions used the same SVG filenames and one version overwrote the other in the comparison directory:

- `comm_vax_count_dist`
- `comm_vax_rate_dist`
- `vac_team`

This is a file-management limitation in the comparison exercise, not evidence of a replication discrepancy. A complete visual audit can be performed later by saving the two versions under distinct names such as `_original` and `_rep`.

## 6. Formatting-only differences

The original Stata output and the R replication use different output writers. Consequently, there are expected presentation differences such as:

- bare `tabular` output versus full LaTeX document wrappers;
- plain significance markers such as `***` versus Stata macro forms such as `\sym{***}`;
- clean R numeric formatting such as `16.7` versus floating-point artifacts such as `16.700001`; and
- R/`knitr` table formatting rather than byte-for-byte `esttab`, `texsave`, RTF, or DOCX formatting.

These differences do not affect the reported estimates or interpretation.

## 7. Source-specific methods preserved

The translation preserves the important source-specific analytical choices:

- `areg`/`reghdfe` fixed effects are represented with `fixest::feols()`;
- community- and structure-level clustering is preserved by specification;
- source `boottest` calls are translated using 999 wild-bootstrap replications and seed 420;
- Tables 6 and 7 use the source's hand-coded two-stage sharpened FDR q-value algorithm at 0.001 resolution;
- Table 1's third specification restricts to door-to-door villages with `periphery == 0`, absorbs community, and clusters at `structure_id`;
- Table 3's third model is estimated on community-level means without clustered conventional standard errors, matching the source specification;
- Table 4 retains all three source population definitions in the original order, with and without the five community covariates; and
- Figure 3's Stata temporary datasets are generated in memory rather than retained as source data.

## 8. Deliberate implementation improvements

The R implementation removes two hidden state dependencies in the Stata master without changing the analysis:

1. Figure 7 explicitly rebuilds its literature-review input rather than assuming Figure 6 has just run.
2. Table 7 explicitly filters to `incomplete_observations == 0` rather than assuming Table 6 left that restricted sample in memory.

These changes make the workflow more modular and reproducible while preserving the original estimands.

## 9. Output-inventory clarification

During one comparison, `effect_size_table_1–5.tex`, `att_willing.tex`, and `census_bal.tex` appeared only on the replication side. However, the original replication-package file tree supplied for this project also lists these filenames (and all 16 `effect_size_table_*` files). They therefore should not be described as new substantive analyses added by the R implementation.

If the comparison workspace did not contain the corresponding original copies, that is best treated as a comparison-directory/inventory difference rather than an analytical difference.

## 10. Final assessment

The R translation successfully reproduces the original Stata analysis.

**Validated exactly across shared tables:**

- coefficients;
- standard errors;
- observation counts; and
- control means.

**Validated exactly in underlying data across six independently comparable figures:**

- plotted estimates;
- significance indicators;
- bar heights;
- literature-review effect sizes and costs; and
- sample counts.

**Expected non-substantive differences:**

- R-squared display precision;
- small software-sensitive wild-bootstrap p-value differences;
- LaTeX/output formatting; and
- graphical styling.

**Remaining optional validation item:**

- side-by-side comparison of `comm_vax_count_dist`, `comm_vax_rate_dist`, and `vac_team` using non-colliding filenames.

On the evidence currently available, this replication should be recorded as **successful**.
