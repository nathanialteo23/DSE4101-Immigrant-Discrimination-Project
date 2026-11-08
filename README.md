# DSE4101 Immigrant Discrimination Project

Group project for DSE4101 (Digital Economy, Group 1), National University of Singapore. We use causal forests to study whether the callback penalty for foreign-sounding names on resumes varies with resume characteristics, using the resume-audit field experiment of Oreopoulos (2011).

## Research question

Foreign-sounding names receive fewer callbacks on average. We ask whether that penalty differs across resumes, and in particular across three pre-specified resume characteristics: language skills, extracurricular activities and multinational firm experience. We compare pre-specified interaction regressions with causal forests (grf), and validate the forests with `test_calibration` and RATE/TOC before interpreting any heterogeneity.

## Data

The data are the replication file for:

> Oreopoulos, P. (2011). Why do skilled immigrants struggle in the labor market? A field experiment with thirteen thousand resumes. *American Economic Journal: Economic Policy, 3*(4), 148–171.

The raw Stata file is in `data/` and is not modified. The analysis sample keeps Type 0 and Type 1 names with Canadian, Chinese, Indian or Pakistani name ethnicity: 5,657 resumes from 3,269 firms. The treatment `D` equals 1 for a foreign-sounding name, and the outcome is `callback`.

## Repository structure

| Path | Type | Purpose |
| --- | --- | --- |
| `data/oreopoulos-resume-study-replication-data-file.dta` | Raw data | Original Stata file, unchanged |
| `cleaned_data.Rmd` | Script (step 1) | Cleans the data and writes `cleaned data.Rda` |
| `cleaned data.Rda` | Data | Cleaned analysis sample (data frame `df_int1`) |
| `Causal Forest Balance Table.Rmd` | Script (step 2) | Covariate balance tables and tests |
| `Causal-Forest-Balance-Table.pdf` | Output | Rendered output of step 2 |
| `regression_working/interaction_regression_updated.Rmd` | Script (step 3) | ATE and interaction regressions |
| `regression_working/interaction_regression_updated.html` | Output | Rendered output of step 3 |
| `Causal_Forest.Rmd` | Script (step 4) | Causal forests, results and validation |
| `Causal_Forest.pdf`, `Causal_Forest.html` | Output | Rendered outputs of step 4 |
| `figures/figure1_rate.png` | Output | RATE/TOC figure, written by step 4 |
| `comparing_subgroup_treatment_effect_plot.R` | Script (step 5) | Regression-versus-forest subgroup figure (built from estimates copied from step 4) |
| `archive/` | Folder | Superseded exploratory scripts, not needed for reproduction |

## How to reproduce

1. Clone the repository and keep its folder structure. The scripts use relative paths, so do not move or rename files (including the spaces in `cleaned data.Rda` and `Causal Forest Balance Table.Rmd`).
2. Open R with the repository root as the working directory.
3. Knit the scripts in this order:
   1. `cleaned_data.Rmd` — reads the raw `.dta` file and saves `cleaned data.Rda`. The other scripts depend on this file.
   2. `Causal Forest Balance Table.Rmd` — covariate balance (PDF).
   3. `regression_working/interaction_regression_updated.Rmd` — regressions (HTML).
   4. `Causal_Forest.Rmd` — causal forests and validation (PDF/HTML). This fits many forests and can take a few minutes.
4. Packages that are not installed are installed automatically by the setup chunks.

### What `Causal_Forest.Rmd` contains

| Section | Content |
| --- | --- |
| 1 | Setup, covariates, moderators, wave dummies |
| 2 | Diagnostics: missingness by wave, sample checks, name penalty by wave, propensity overlap |
| 3 | The two forests: cf1w (three moderators plus wave) and cf2 (all seven covariates plus wave) |
| 4 | Results: ATE, predicted effects, subgroup effects, regression-versus-forest moderator table, best linear projection |
| 5 | Validation: `test_calibration`, RATE/TOC with a firm-level split, combined verdict, tuning robustness |

### Seeds

Forests are fitted with `set.seed(4101)` immediately before each fit. The RATE/TOC firm splits use seeds 1 to 5, with seed 1 as the headline split fixed in advance. The tuning robustness check uses `seed = 123` for its calibration forests.

## Software environment

R 4.4.2 on Windows 11, with `grf` 2.6.1, `estimatr` 1.0.6, `dplyr` 1.1.4, `tidyr` 1.3.1, `ggplot2` 3.5.1, `patchwork` 1.3.0 and `knitr` 1.49. `cleaned_data.Rmd` also uses `haven` and `modelsummary`. The full `sessionInfo()` output is in `sessionInfo.txt`. Other package versions may give slightly different forest results.

## Main findings

- Foreign-sounding names receive about 4.4 percentage points fewer callbacks on average, and this is robust to including controls.
- The pre-specified interaction regressions give no statistically significant interaction at the 5% level.
- Neither forest passes validation: `test_calibration` and RATE/TOC both find no statistically reliable evidence that the penalty varies across resumes, so forest subgroup patterns are not interpreted as heterogeneity. This may reflect limited power rather than a constant effect.

See the project report for details.
