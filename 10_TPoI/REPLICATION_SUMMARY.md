# Replication Summary

## Overview

This document summarizes the final validation of the R replication of Nina Bruvik Westberg, Sofie Waage Skjeflo, and Steffen Kallbekken, **“The Power of Information: A Survey Experiment on Public Support for Electricity Price Compensation Schemes.”**

The authors' official workflow is contained in a single Quarto file and begins directly from the anonymized author-prepared dataset `surveydata.rds`.

The clean R replication starts from the same analysis-ready dataset:

```text
data/source/cleaned/surveydata.rds
```

The Quarto workflow has been reorganized into numbered R scripts without changing the substantive model specifications.

The current code is treated as the final replication version.

## Overall validation result

The R implementation successfully reproduces the authors' replication workflow for the outputs included in the final direct comparison.

The reviewed matched set shows:

- identical numerical table contents;
- identical treatment-effect and robustness results where compared;
- identical plotted proportions and confidence intervals;
- no substantive discrepancy in any matched result;
- only negligible file-format, metadata, transparency, or anti-aliasing differences where files are not byte-identical.

The replication is therefore considered successful.

## Directly validated outputs

### Figure 1

`R-Figure1.png` is identical to the authors' `O-Figure1.png` in the final review.

No graphical or substantive discrepancy remains.

### Figure 2A

An earlier R version displayed the response legend, while the authors' saved Figure 2A suppresses it. The legend compressed the plotting area and shifted the bars horizontally even though the underlying data were identical.

The final code suppresses the legend to match the original layout.

In the final comparison:

- bar centers align with the original;
- stacked-segment boundaries agree;
- confidence-interval positions agree;
- the remaining whole-image difference is approximately 0.067%;
- residual error-bar differences are at most about one pixel and are consistent with anti-aliasing/rendering noise.

Figure 2A is therefore treated as successfully reproduced.

### Figure 2B

The plotted proportions and confidence intervals agree with the original.

The remaining image difference is limited to approximately one-pixel rendering variation around error bars. The original file's transparency/alpha channel also explains byte-level differences that disappear when the images are compared on a common white background.

Figure 2B is treated as successfully reproduced.

### Table 3A

The category labels, scheme labels, and reported shares reproduce exactly at the value level and full available precision.

Differences in spreadsheet file hashes are attributable to workbook metadata/timestamps rather than cell content.

### Table 3B

The reported values reproduce exactly.

Any byte-level workbook differences are metadata-related rather than analytical.

### Table A3

The final table content matches the original.

An earlier R version displayed the derived gender row as `Male`; the original saved table displays `male`. The final code restores `male`, and the extracted Word-table content now agrees with the original throughout.

### Table A4

The unadjusted p-values and Benjamini-Hochberg adjusted p-values match the original values.

Spreadsheet hash differences, where present, reflect file metadata rather than cell values.

### Table A5

The final reviewed output is byte-for-byte identical to the original saved HTML table.

## Regression-table coverage

The final R code generates the main and exploratory regression tables that are saved by the clean replication:

```text
R-Table2.html
R-Table4.html
R-Table5.html
R-Table6.html
R-Table7.html
R-TableA6.html
```

These outputs are produced by `04_primary_hypotheses.R` through `07_financial_impact.R` using the same `MASS::polr` specifications and `stargazer` reporting choices as the corresponding blocks in the authors' Quarto file.

The final comparison report did not include these six R HTML files in its matched file set. Accordingly, this is a **comparison-coverage limitation**, not a missing-code or missing-output-path limitation.

The validation claim is strongest for the outputs directly compared above. Exact file-level equivalence is not separately claimed for Tables 2, 4, 5, 6, 7, and A6 without a direct original-versus-R file comparison.

## Additional output: Table A2

The R workflow writes:

```text
R-TableA2.xlsx
```

This file combines the gender, age-group, and regional sample-composition summaries that the authors' Quarto workflow prints during the Table A2 section.

Because the original replication package does not contain a standalone saved Table A2 file, `R-TableA2.xlsx` is an additional convenience output. It is not used to claim an original-file match.

## Data preparation

The final code preserves the authors' workflow:

1. load `surveydata.rds`;
2. construct control and treatment indicators from `gruppe`;
3. construct the attention-check indicator from `Qattention` and `Qattention2`;
4. restrict the main analysis sample to respondents passing both checks;
5. retain the full sample for the balance analysis;
6. construct analysis variables at the point they are introduced in the original Quarto workflow.

No raw survey reconstruction is introduced because the official replication itself starts from the prepared `.rds` dataset.

## Primary hypotheses and robustness

The R translation preserves the authors' primary ordered-logit specifications for support for:

- the price-subsidy scheme; and
- the lump-sum scheme.

The treatment indicators `T1`, `T2`, and `T3` are estimated relative to the no-information control group.

The workflow also preserves:

- the six-test Benjamini-Hochberg adjustment reported in Table A4;
- the binary-support transformation;
- the linear-probability robustness models;
- HC1 heteroskedasticity-robust standard errors for Table A5.

The directly compared Table A4 and Table A5 outputs reproduce the original results.

## Exploratory analyses

The translation retains the original exploratory analyses of:

- prior beliefs about distributional effects;
- prior beliefs about electricity-saving incentives;
- correlations between prior beliefs and support in the control group;
- heterogeneous treatment effects by incorrect prior beliefs;
- heterogeneous treatment effects by type of incorrect belief;
- correlations between support and household financial characteristics;
- heterogeneous treatment effects by electricity expenditure.

No alternative specifications were introduced to improve visual or numerical matching.

## Source-code quirks retained or documented

### Figure 1 response coding

The Figure 1 section treats response code `99` as “Don't know,” whereas the later support variables treat response code `6` as the value to omit.

The R replication follows each original source block as written rather than imposing a new harmonized coding rule.

### Table A3 label handling

The source workflow creates a derived variable `male` but contains an inconsistent label mapping involving the original gender variable name.

The clean code applies the label directly to `male` and uses the displayed label `male`, matching the authors' saved output while preserving the intended balance variable.

### Commented output commands

The authors' Quarto file comments out several table and figure export statements even though the replication package contains corresponding saved outputs.

The clean R repository enables the relevant exports and redirects them to `results/tables/` and `results/figures/` with an `R-` prefix. The statistical code itself is unchanged.

### Locale handling

The original workflow requests the Norwegian UTF-8 locale `nb_NO.UTF-8`.

Because locale names vary across operating systems, the clean setup attempts that locale without making it a fatal requirement. This does not change the statistical analysis.

## File-format differences

Exact byte equality is not expected for all `.xlsx`, `.docx`, and `.png` outputs because generated files can differ in:

- timestamps and ZIP-container metadata;
- workbook/document metadata;
- PNG alpha-channel representation;
- anti-aliasing and sub-pixel rendering.

For this replication, numerical cell content and plotted values are treated as the relevant validation targets unless byte-level equality is independently observed.

The final comparison confirms that the remaining non-byte-identical matched outputs differ only for these non-substantive reasons.

## Effect on the paper's conclusions

No discrepancy identified in the final matched-output review changes a statistical result or substantive interpretation.

The final R workflow reproduces the information-treatment analyses, prior-belief analyses, robustness checks, and heterogeneous-effect specifications of the authors' replication code.

For the directly validated outputs, the reported numerical results agree with the originals.

## Stopping decision

Further code revision is not recommended.

The stopping rule is satisfied because:

1. the R workflow starts from the same author-prepared analysis dataset as the official replication;
2. the authors' data preparation and statistical specifications have been translated directly;
3. every output in the direct matched comparison reproduces its original numerical content;
4. Figure 2A's earlier layout discrepancy has been corrected;
5. Table A3's earlier label discrepancy has been corrected;
6. the remaining graphical differences are only approximately one-pixel rendering effects;
7. the additional Table A2 file is clearly identified as a convenience output;
8. the six regression HTML tables are present in the final code's output paths, even though they were outside the reported direct comparison set;
9. no remaining documented difference affects the substantive conclusions.

The recommended final workflow is therefore to:

1. freeze the current R analysis code;
2. retain the original-versus-replication comparison record;
3. retain the validation-coverage note for Tables 2, 4, 5, 6, 7, and A6;
4. preserve `R-TableA2.xlsx` as an explicitly additional output;
5. treat Paper 11 as a completed R replication.

## Final assessment

Paper 11 should be characterized as a **successful R replication with exact numerical reproduction across the directly compared table outputs, matching plotted data and layout, and only negligible software-rendering differences in the figures**.

The final code is suitable to freeze as the completed replication version.
