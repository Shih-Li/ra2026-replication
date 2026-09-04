# Replication Summary

## Project

**PAL Mexico cash-versus-in-kind replication**  
Cunha–De Giorgi–Jayachandran project  
R translation of the original Stata workflow

## Overall assessment

The replication is **substantially successful, with documented residual discrepancies**.

The R workflow correctly reconstructs the main household/person samples, survey-wave structure, treatment groups, locality structure, and central consumption variables from raw inputs. The strongest evidence is the close match of the no-controls aggregate consumption estimates after correcting non-food expenditure.

The remaining differences are concentrated rather than systemic. They concern:

1. one class-attendance output variable;
2. the controlled aggregate-consumption specification;
3. a limited set of disaggregated outcomes and output-scope differences.

The project is therefore concluded without additional code revision, with these differences documented below.

---

## 1. Data construction and sample validation

The final R workflow starts from the same raw-data level as the original Stata workflow.

Raw inputs are stored under:

```text
data/source/raw/
```

R-generated cleaned/intermediate datasets are stored under:

```text
data/intermediate/
```

Final analysis-ready datasets are stored under:

```text
data/processed/
```

The workflow does not rely on author-generated intermediate Stata datasets as normal inputs.

The build includes stage-level checkpoints in:

```text
output/build_checkpoints.csv
```

These checks were used to verify baseline/follow-up sample retention, household and person identifiers, locality counts, duplicate-key behavior, and household-module merge behavior.

The debugging process identified and corrected several Stata-to-R translation issues, including identifier precision, duplicate-key merge behavior, Stata string-missing semantics, and non-food expenditure construction.

---

## 2. Results that now reproduce the original

### 2.1 `baseline_means`

Status: **matches the original**

After correcting `pc_exp_nfood`, the baseline means reproduce the original values.

In particular, the non-food expenditure means now match:

| Group | R replication |
|---|---:|
| Control | 180.01 |
| In-kind | 168.35 |
| Cash | 173.83 |

The corresponding balance-test p-values also match the original. Other baseline variables had already been matching to rounding.

**Conclusion:** baseline household construction and treatment-group balance are successfully reproduced.

### 2.2 `cons_agg_no_ctrls`

Status: **matches the original**

This is the strongest replication check.

After correcting non-food expenditure:

- `exp_food` matches;
- `exp_inkind` matches;
- non-in-kind expenditure matches;
- `exp_nfood` matches;
- `exp_total` matches.

The food and in-kind interaction effects had already been reproducing almost exactly before the non-food correction.

**Conclusion:** the core sample, treatment coding, follow-up indicator, interaction variables, and principal consumption outcomes are correctly constructed.

### 2.3 Core food and in-kind effects in controlled tables

Even where the fully controlled tables differ, the food and in-kind components remain close to the original.

This consistency supports the conclusion that the primary PAL food/in-kind construction is functioning correctly.

---

## 3. Known remaining discrepancies

### 3.1 `classes`

Status: **partially reproduced**

The following columns reproduce the original:

- `class`
- `class_noORG`
- `n_class`

However, `n_class_noORG` is still an exact duplicate of `n_class` in the R output.

Current R means:

```text
4.16 / 5.03 / 4.37
```

Original means:

```text
3.73 / 4.63 / 3.98
```

The corresponding p-values in the R output are therefore also duplicated from the `n_class` results.

There is also a residual difference in the `class_noORG` comparison between the in-kind-no-classes and cash-plus-classes groups.

**Interpretation:** this is a localized output/construction bug and does not affect the broader household consumption results.

### 3.2 `cons_agg` — controlled aggregate consumption

Status: **partially reproduced**

The controlled specification does not fully reproduce the original `exp_total` and `exp_nfood` estimates.

The food, in-kind, and non-in-kind components remain close, but the total and non-food estimates differ materially.

Example for the follow-up (`fu`) coefficient on total expenditure:

```text
R replication: approximately 103.2
Original:      approximately 196.1
```

The associated standard error is also substantially larger in the R implementation.

The same pattern appears in non-food expenditure.

Because `cons_agg_no_ctrls` now reproduces correctly, this discrepancy is most consistent with a remaining difference in the controlled specification rather than a general outcome-construction or treatment-coding problem.

Potential sources include treatment of the control set, month-of-interview indicators, fixed-effect/indicator implementation, effective regression sample, or clustered variance-covariance implementation.

No further revision was undertaken for the present replication.

### 3.3 `cons_agg_robust`

Status: **partially reproduced**

The robust controlled specification inherits the same pattern as `cons_agg`.

Food and in-kind results remain close, while total and non-food coefficients diverge from the original.

This is treated as the same unresolved controlled-specification issue rather than a separate data-construction failure.

### 3.4 `cons_disagg_new`

Status: **partially reproduced**

The R table contains 26 goods compared with 30 in the original.

The four missing categories are:

- chicken;
- beef/pork;
- seafood;
- canned fish.

For the overlapping food staples, the treatment estimates are generally close.

Some non-food categories remain different, although the non-food expenditure correction moved several estimates toward the original values.

**Interpretation:** the remaining differences are partly table-scope differences and partly linked to the unresolved controlled specification.

---

## 4. Output-scope differences

### Original outputs not reproduced

The current R output set does not include direct counterparts to:

- `tab_prediction`
- `tab_prediction_bygroup`

These are the PAL take-up prediction regressions.

### Additional R outputs

The R implementation includes outputs not present as direct counterparts in the original set, including:

- `baseline_indiv`
- `baseline_women`
- `em_nb_summary`
- `pal_food_summary`
- `pal_receipt`
- `health_age0_1`
- `health_age0_6`
- `health_women`
- `health_women_height_12_21`
- `health_hetero_by_bmi`
- `nutrition`
- `nutrition_below_rda`
- `nutrition_mothers`
- `build_checkpoints.csv`

These are retained as supplemental outputs.

---

## 5. Figure differences

### `fig_em_nb`

The original and R implementations do not use exactly the same presentation.

Differences include:

- the R version contains fewer goods because canned fish is absent;
- the R version overlays cash and in-kind CDFs;
- the original uses a different pooled presentation.

This should be treated as an output-definition difference rather than a direct numerical replication test.

### `fig_em_nb_value`

The concept is reproduced, but presentation differs in axis range, legend/annotation, and mean-line treatment.

### Health density figures

The R workflow also produces health/anthropometric density figures that do not have direct counterparts in the original output set.

---

## 6. Important correction made during replication

The largest resolved discrepancy concerned non-food expenditure.

The original Stata aggregate is:

```text
n10*
n201exp-n207exp
n301exp-n313exp
```

It does not sum every available `n???exp` variable.

The original Section 7 naming also maps the final five expenditure variables to:

```text
n311exp
n312exp
n313exp
n314exp
n315exp
```

After correcting the R naming and aggregate definition:

- baseline `pc_exp_nfood` matched;
- baseline balance p-values matched;
- no-controls `exp_nfood` matched;
- no-controls `exp_total` matched.

This correction confirms that the earlier total-expenditure discrepancy was driven by the non-food construction rather than the food or treatment variables.

---

## 7. Final replication judgment

### Successfully reproduced

The R replication successfully reproduces:

- the main household/person sample structure;
- baseline and follow-up waves;
- treatment-group coding;
- locality structure;
- baseline household means;
- food expenditure;
- in-kind expenditure;
- non-in-kind expenditure;
- non-food expenditure after correction;
- total expenditure after correction;
- the principal no-controls treatment-effect regressions.

### Partially reproduced

The following remain partially reproduced:

- the classes table because of `n_class_noORG`;
- controlled aggregate consumption;
- robust controlled aggregate consumption;
- disaggregated consumption because four categories are absent and some controlled estimates differ.

### Not reproduced one-for-one

Some original prediction tables and figure definitions are outside the final R output scope.

---

## 8. Conclusion

The replication should be characterized as **substantively successful but not numerically identical in every specification**.

The close match of the no-controls aggregate consumption results is particularly important because it validates the core raw-data construction, household panel, treatment coding, food/in-kind construction, corrected non-food construction, and treatment-by-follow-up interactions.

The remaining numerical gaps are concentrated in controlled specifications and a small number of output-specific variables rather than across the replication as a whole.

For the present project, these residual differences are documented as limitations rather than grounds for another revision cycle.
