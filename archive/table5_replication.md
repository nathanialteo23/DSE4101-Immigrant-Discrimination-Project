Oreopoulos (2011) Replication — Table 5
================
DSE4101 Immigrant Discrimination Project
2026-09-07

- [Setup](#setup)
- [Table 5: Effects of offsetting resume
  characteristics](#table-5-effects-of-offsetting-resume-characteristics)
  - [Panel A: pooled specifications](#panel-a-pooled-specifications)
    - [Combined table: columns (1)–(4)](#combined-table-columns-14)
  - [Panel B: per-type breakdown of each characteristic’s
    effect](#panel-b-per-type-breakdown-of-each-characteristics-effect)
    - [Combined table: Type 0–4](#combined-table-type-04)

This document is a faithful R translation of the relevant section of the
author’s original Stata do-file
(`oreopoulos-resume-study-replication-do-file.do`), covering **Table 5**
only. See `table4_replication.Rmd` for Table 4.

**Note on standard errors.** The original Stata code uses `robust`
(heteroskedasticity-robust, Eicker-Huber-White) SEs throughout, and
`absorb(firmid)` (firm fixed effects, *not* clustering) as an
alternative specification. It does **not** use cluster-robust SEs
anywhere. This script replicates that exactly. If your own extension
analysis (interaction models, causal forest) clusters by `firmid`
instead, that is a deliberate, defensible deviation — just document it
explicitly, since it differs from what the original paper did.

# Setup

``` r
# --- Package bootstrap: install anything missing, then load ------------------
required_packages <- c("haven", "estimatr", "dplyr", "modelsummary")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(haven)
library(estimatr)     # lm_robust() for robust SEs and fixed effects
library(dplyr)
library(modelsummary)  # side-by-side regression tables

# --- Load data -------------------------------------------------------------
# When knitting, knitr sets the working directory to this file's location, so a
# path relative to the project root works for both knit and interactive runs.
data_path <- file.path("data", "oreopoulos-resume-study-replication-data-file.dta")
if (!file.exists(data_path)) {
  stop("Data file not found. Expected it at: ", normalizePath(data_path, mustWork = FALSE))
}
df <- read_dta(data_path)

# Recode type and fall_data as factors (mirrors Stata's `i.type i.fall_data`)
df <- df %>%
  mutate(
    type      = factor(type),
    fall_data = factor(fall_data)
  )
```

# Table 5: Effects of offsetting resume characteristics

Original Stata:

``` stata
drop British, Chn-Cdn, Greek                       // keep Canada + Indian/Pakistani/Chinese
replace missing same_exp/reference/accreditation/legal/extracurricular = 0
(1) reg  callback i.type i.fall_data, robust
(2) areg callback i.type, robust absorb(firmid)     // firm FIXED EFFECTS
(3) reg  callback i.type [controls] i.fall_data, robust
(4) areg callback i.type [controls], robust absorb(firmid)
```

then separate regressions of `callback ~ [controls] i.fall_data`, by
`type`.

``` r
df5 <- df %>%
  filter(!name_ethnicity %in% c("British", "Chn-Cdn", "Greek")) %>%
  mutate(
    same_exp               = ifelse(is.na(same_exp), 0, same_exp),
    reference              = ifelse(is.na(reference), 0, reference),
    accreditation          = ifelse(is.na(accreditation), 0, accreditation),
    legal                  = ifelse(is.na(legal), 0, legal),
    extracurricular_skills = ifelse(is.na(extracurricular_skills), 0, extracurricular_skills)
  )

controls_formula <- ~ female + ba_quality + extracurricular_skills + language_skills +
                      ma + same_exp + exp_highquality + reference + accreditation + legal
```

## Panel A: pooled specifications

``` r
# (1) Baseline, no controls, no firm FE
t5_1 <- lm_robust(callback ~ type + fall_data, data = df5, se_type = "HC1")

# (2) Firm fixed effects, no other controls
#     lm_robust's `fixed_effects` argument replicates Stata's `areg ... absorb()`
t5_2 <- lm_robust(callback ~ type, fixed_effects = ~ firmid, data = df5, se_type = "HC1")

# (3) With full controls, no firm FE
t5_3 <- lm_robust(
  update.formula(controls_formula, callback ~ type + fall_data + .),
  data = df5, se_type = "HC1"
)

# (4) With full controls AND firm fixed effects
t5_4 <- lm_robust(
  update.formula(controls_formula, callback ~ type + .),
  fixed_effects = ~ firmid, data = df5, se_type = "HC1"
)
```

### Combined table: columns (1)–(4)

`fall_data` dummy coefficients are omitted; all `type` and control
coefficients are shown so the offsetting-characteristics effects are
visible directly.

``` r
modelsummary(
  list(
    "(1) No controls"        = t5_1,
    "(2) Firm FE"             = t5_2,
    "(3) Controls"            = t5_3,
    "(4) Controls + Firm FE"  = t5_4
  ),
  stars    = TRUE,
  coef_omit = "fall_data",
  gof_omit  = "IC|Log|Adj|F|RMSE",
  title     = "Table 5, Panel A: Effects of offsetting resume characteristics"
)
```

<table style="width:97%;">
<caption>Table 5, Panel A: Effects of offsetting resume
characteristics</caption>
<colgroup>
<col style="width: 25%" />
<col style="width: 18%" />
<col style="width: 14%" />
<col style="width: 15%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr>
<th></th>
<th><ol type="1">
<li>No controls</li>
</ol></th>
<th><ol start="2" type="1">
<li>Firm FE</li>
</ol></th>
<th><ol start="3" type="1">
<li>Controls</li>
</ol></th>
<th><ol start="4" type="1">
<li>Controls + Firm FE</li>
</ol></th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.157***</td>
<td></td>
<td>0.142***</td>
<td></td>
</tr>
<tr>
<td></td>
<td>(0.008)</td>
<td></td>
<td>(0.011)</td>
<td></td>
</tr>
<tr>
<td>type1</td>
<td>-0.044***</td>
<td>-0.046***</td>
<td>-0.044***</td>
<td>-0.046***</td>
</tr>
<tr>
<td></td>
<td>(0.009)</td>
<td>(0.007)</td>
<td>(0.009)</td>
<td>(0.007)</td>
</tr>
<tr>
<td>type2</td>
<td>-0.048***</td>
<td>-0.051***</td>
<td>-0.049***</td>
<td>-0.052***</td>
</tr>
<tr>
<td></td>
<td>(0.010)</td>
<td>(0.008)</td>
<td>(0.010)</td>
<td>(0.008)</td>
</tr>
<tr>
<td>type3</td>
<td>-0.073***</td>
<td>-0.064***</td>
<td>-0.074***</td>
<td>-0.066***</td>
</tr>
<tr>
<td></td>
<td>(0.009)</td>
<td>(0.008)</td>
<td>(0.010)</td>
<td>(0.008)</td>
</tr>
<tr>
<td>type4</td>
<td>-0.094***</td>
<td>-0.100***</td>
<td>-0.096***</td>
<td>-0.103***</td>
</tr>
<tr>
<td></td>
<td>(0.009)</td>
<td>(0.009)</td>
<td>(0.009)</td>
<td>(0.009)</td>
</tr>
<tr>
<td>female</td>
<td></td>
<td></td>
<td>0.019**</td>
<td>0.016**</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.006)</td>
<td>(0.006)</td>
</tr>
<tr>
<td>ba_quality</td>
<td></td>
<td></td>
<td>-0.005</td>
<td>-0.006</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.006)</td>
<td>(0.006)</td>
</tr>
<tr>
<td>extracurricular_skills</td>
<td></td>
<td></td>
<td>0.006</td>
<td>0.005</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.006)</td>
<td>(0.005)</td>
</tr>
<tr>
<td>language_skills</td>
<td></td>
<td></td>
<td>0.020**</td>
<td>0.008</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.007)</td>
<td>(0.007)</td>
</tr>
<tr>
<td>ma</td>
<td></td>
<td></td>
<td>-0.002</td>
<td>-0.006</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.008)</td>
<td>(0.008)</td>
</tr>
<tr>
<td>same_exp</td>
<td></td>
<td></td>
<td>-0.005</td>
<td>0.004</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.009)</td>
<td>(0.011)</td>
</tr>
<tr>
<td>exp_highquality</td>
<td></td>
<td></td>
<td>0.009</td>
<td>0.006</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.008)</td>
<td>(0.007)</td>
</tr>
<tr>
<td>reference</td>
<td></td>
<td></td>
<td>0.003</td>
<td>0.001</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.016)</td>
<td>(0.015)</td>
</tr>
<tr>
<td>accreditation</td>
<td></td>
<td></td>
<td>0.001</td>
<td>-0.001</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.014)</td>
<td>(0.014)</td>
</tr>
<tr>
<td>legal</td>
<td></td>
<td></td>
<td>0.004</td>
<td>0.007</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.014)</td>
<td>(0.014)</td>
</tr>
<tr>
<td>Num.Obs.</td>
<td>10184</td>
<td>10184</td>
<td>10184</td>
<td>10184</td>
</tr>
<tr>
<td>R2</td>
<td>0.011</td>
<td>0.607</td>
<td>0.013</td>
<td>0.608</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="5"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

## Panel B: per-type breakdown of each characteristic’s effect

Type 0 structurally never has `reference`, `accreditation`, or `legal`
variation (those characteristics are undefined for that resume type), so
its model omits those three terms; the combined table below will show
blank cells for Type 0 on those rows rather than a coefficient.

``` r
run_table5_by_type <- function(data, type_val) {
  sub <- data %>% filter(type == type_val)
  if (type_val == 0) {
    lm_robust(callback ~ female + ba_quality + extracurricular_skills +
                language_skills + ma + same_exp + exp_highquality + fall_data,
              data = sub, se_type = "HC1")
  } else {
    lm_robust(callback ~ female + ba_quality + extracurricular_skills +
                language_skills + ma + same_exp + exp_highquality +
                reference + accreditation + legal + fall_data,
              data = sub, se_type = "HC1")
  }
}
```

``` r
t5_type0 <- run_table5_by_type(df5, 0)
t5_type1 <- run_table5_by_type(df5, 1)
t5_type2 <- run_table5_by_type(df5, 2)
t5_type3 <- run_table5_by_type(df5, 3)
t5_type4 <- run_table5_by_type(df5, 4)
```

### Combined table: Type 0–4

``` r
modelsummary(
  list(
    "Type 0" = t5_type0,
    "Type 1" = t5_type1,
    "Type 2" = t5_type2,
    "Type 3" = t5_type3,
    "Type 4" = t5_type4
  ),
  stars    = TRUE,
  coef_omit = "fall_data",
  gof_omit  = "IC|Log|Adj|F|RMSE",
  title     = "Table 5, Panel B: Per-type breakdown of characteristic effects"
)
```

<table style="width:95%;">
<caption>Table 5, Panel B: Per-type breakdown of characteristic
effects</caption>
<colgroup>
<col style="width: 30%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 12%" />
</colgroup>
<thead>
<tr>
<th></th>
<th>Type 0</th>
<th>Type 1</th>
<th>Type 2</th>
<th>Type 3</th>
<th>Type 4</th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.153***</td>
<td>0.079***</td>
<td>0.075***</td>
<td>0.105***</td>
<td>0.031*</td>
</tr>
<tr>
<td></td>
<td>(0.019)</td>
<td>(0.017)</td>
<td>(0.022)</td>
<td>(0.022)</td>
<td>(0.014)</td>
</tr>
<tr>
<td>female</td>
<td>0.049***</td>
<td>0.012</td>
<td>0.007</td>
<td>0.001</td>
<td>0.007</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.012)</td>
<td>(0.015)</td>
<td>(0.014)</td>
<td>(0.012)</td>
</tr>
<tr>
<td>ba_quality</td>
<td>-0.022</td>
<td>0.015</td>
<td>0.008</td>
<td>-0.018</td>
<td>-0.002</td>
</tr>
<tr>
<td></td>
<td>(0.014)</td>
<td>(0.013)</td>
<td>(0.016)</td>
<td>(0.015)</td>
<td>(0.014)</td>
</tr>
<tr>
<td>extracurricular_skills</td>
<td>-0.007</td>
<td>0.023+</td>
<td>0.003</td>
<td>0.006</td>
<td>0.001</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.012)</td>
<td>(0.015)</td>
<td>(0.014)</td>
<td>(0.012)</td>
</tr>
<tr>
<td>language_skills</td>
<td>0.020</td>
<td>0.007</td>
<td>0.058**</td>
<td>-0.010</td>
<td>0.036*</td>
</tr>
<tr>
<td></td>
<td>(0.015)</td>
<td>(0.014)</td>
<td>(0.019)</td>
<td>(0.015)</td>
<td>(0.016)</td>
</tr>
<tr>
<td>ma</td>
<td>-0.003</td>
<td>0.006</td>
<td>-0.034*</td>
<td>0.019</td>
<td></td>
</tr>
<tr>
<td></td>
<td>(0.016)</td>
<td>(0.015)</td>
<td>(0.017)</td>
<td>(0.019)</td>
<td></td>
</tr>
<tr>
<td>same_exp</td>
<td>-0.020</td>
<td>0.010</td>
<td>0.027</td>
<td>-0.056**</td>
<td>0.028</td>
</tr>
<tr>
<td></td>
<td>(0.018)</td>
<td>(0.020)</td>
<td>(0.025)</td>
<td>(0.021)</td>
<td>(0.018)</td>
</tr>
<tr>
<td>exp_highquality</td>
<td>0.006</td>
<td>0.002</td>
<td>0.015</td>
<td>0.034+</td>
<td>-0.004</td>
</tr>
<tr>
<td></td>
<td>(0.016)</td>
<td>(0.015)</td>
<td>(0.020)</td>
<td>(0.019)</td>
<td>(0.016)</td>
</tr>
<tr>
<td>accreditation</td>
<td></td>
<td></td>
<td>0.007</td>
<td>0.004</td>
<td>-0.014</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.024)</td>
<td>(0.025)</td>
<td>(0.025)</td>
</tr>
<tr>
<td>legal</td>
<td></td>
<td></td>
<td>-0.019</td>
<td>0.005</td>
<td>0.025</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.024)</td>
<td>(0.025)</td>
<td>(0.027)</td>
</tr>
<tr>
<td>reference</td>
<td></td>
<td></td>
<td></td>
<td>-0.002</td>
<td>-0.006</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td></td>
<td>(0.023)</td>
<td>(0.025)</td>
</tr>
<tr>
<td>Num.Obs.</td>
<td>3026</td>
<td>2631</td>
<td>1618</td>
<td>1501</td>
<td>1408</td>
</tr>
<tr>
<td>R2</td>
<td>0.008</td>
<td>0.004</td>
<td>0.013</td>
<td>0.010</td>
<td>0.011</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="6"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>
