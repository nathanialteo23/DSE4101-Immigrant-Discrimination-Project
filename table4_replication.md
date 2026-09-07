Oreopoulos (2011) Replication — Table 4
================
DSE4101 Immigrant Discrimination Project
2026-09-06

- [Setup](#setup)
- [Table 4: Callback rates by resume type and ethnic
  origin](#table-4-callback-rates-by-resume-type-and-ethnic-origin)
  - [Combined table: callback rates by ethnic origin
    vs. Canada](#combined-table-callback-rates-by-ethnic-origin-vs-canada)

This document is a faithful R translation of the relevant section of the
author’s original Stata do-file
(`oreopoulos-resume-study-replication-do-file.do`), covering **Table 4**
only. See `table5_replication.Rmd` for Table 5.

**Note on standard errors.** The original Stata code uses `robust`
(heteroskedasticity-robust, Eicker-Huber-White) SEs throughout. It does
**not** use cluster-robust SEs anywhere. This script replicates that
exactly. If your own extension analysis (interaction models, causal
forest) clusters by `firmid` instead, that is a deliberate, defensible
deviation — just document it explicitly, since it differs from what the
original paper did.

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

# Table 4: Callback rates by resume type and ethnic origin

Original Stata:

``` stata
drop if accreditation==1 | reference==1 | legal==1
reg callback i.type i.fall_data if name_ethnicity=="X" | name_ethnicity=="Canada", robust
```

run separately for each ethnicity, plus a pooled
Indian/Pakistani/Chinese column.

**On the sample filter.** In Stata, `drop if accreditation==1` treats
missing values as *not* equal to 1 (so they are kept). In R,
`filter(x != 1)` treats `NA != 1` as `NA`, which `filter()` silently
drops — this would incorrectly delete every row from the earlier
sampling periods where these variables are missing (they were only
introduced in the later wave). We replace `NA` with `0` first so the
filter behaves the way the original Stata code actually does.

``` r
df4 <- df %>%
  mutate(
    accreditation = ifelse(is.na(accreditation), 0, accreditation),
    reference     = ifelse(is.na(reference), 0, reference),
    legal         = ifelse(is.na(legal), 0, legal)
  ) %>%
  filter(accreditation != 1, reference != 1, legal != 1)

run_table4 <- function(data, ethnicities) {
  sub <- data %>% filter(name_ethnicity %in% c(ethnicities, "Canada"))
  lm_robust(callback ~ type + fall_data, data = sub, se_type = "HC1")
}
```

``` r
t4_indian    <- run_table4(df4, "Indian")
t4_pakistani <- run_table4(df4, "Pakistani")
t4_chinese   <- run_table4(df4, "Chinese")
t4_chncdn    <- run_table4(df4, "Chn-Cdn")
t4_british   <- run_table4(df4, "British")
t4_greek     <- run_table4(df4, "Greek")
t4_pooled    <- run_table4(df4, c("Indian", "Pakistani", "Chinese"))
```

## Combined table: callback rates by ethnic origin vs. Canada

Each column is a separate regression of `callback` on resume `type` and
`fall_data`, restricted to the named ethnicity plus the Canadian
baseline. `fall_data` dummy coefficients are omitted from display since
they are a sampling-wave control rather than a quantity of substantive
interest here.

``` r
modelsummary(
  list(
    "Indian"    = t4_indian,
    "Pakistani" = t4_pakistani,
    "Chinese"   = t4_chinese,
    "Chn-Cdn"   = t4_chncdn,
    "British"   = t4_british,
    "Greek"     = t4_greek,
    "Pooled"    = t4_pooled
  ),
  stars    = TRUE,
  coef_omit = "fall_data",
  gof_omit  = "IC|Log|Adj|F|RMSE",
  title     = "Table 4: Callback rates by resume type and ethnic origin"
)
```

<table style="width:94%;">
<caption>Table 4: Callback rates by resume type and ethnic
origin</caption>
<colgroup>
<col style="width: 13%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 11%" />
<col style="width: 10%" />
<col style="width: 10%" />
<col style="width: 11%" />
</colgroup>
<thead>
<tr>
<th></th>
<th>Indian</th>
<th>Pakistani</th>
<th>Chinese</th>
<th>Chn-Cdn</th>
<th>British</th>
<th>Greek</th>
<th>Pooled</th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.160***</td>
<td>0.158***</td>
<td>0.154***</td>
<td>0.158***</td>
<td>0.158***</td>
<td>0.158***</td>
<td>0.157***</td>
</tr>
<tr>
<td></td>
<td>(0.009)</td>
<td>(0.012)</td>
<td>(0.010)</td>
<td>(0.012)</td>
<td>(0.012)</td>
<td>(0.012)</td>
<td>(0.008)</td>
</tr>
<tr>
<td>type1</td>
<td>-0.045***</td>
<td>-0.048**</td>
<td>-0.041**</td>
<td>-0.033*</td>
<td></td>
<td>-0.040*</td>
<td>-0.044***</td>
</tr>
<tr>
<td></td>
<td>(0.012)</td>
<td>(0.016)</td>
<td>(0.013)</td>
<td>(0.014)</td>
<td></td>
<td>(0.019)</td>
<td>(0.009)</td>
</tr>
<tr>
<td>type2</td>
<td>-0.045**</td>
<td>-0.018</td>
<td>-0.057***</td>
<td>-0.077***</td>
<td>-0.029</td>
<td></td>
<td>-0.047***</td>
</tr>
<tr>
<td></td>
<td>(0.015)</td>
<td>(0.027)</td>
<td>(0.015)</td>
<td>(0.016)</td>
<td>(0.019)</td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>type3</td>
<td>-0.085***</td>
<td>-0.080***</td>
<td>-0.053**</td>
<td>-0.060**</td>
<td>-0.001</td>
<td></td>
<td>-0.072***</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.020)</td>
<td>(0.016)</td>
<td>(0.020)</td>
<td>(0.023)</td>
<td></td>
<td>(0.010)</td>
</tr>
<tr>
<td>type4</td>
<td>-0.098***</td>
<td>-0.106***</td>
<td>-0.095***</td>
<td>-0.083***</td>
<td>-0.017</td>
<td></td>
<td>-0.098***</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.015)</td>
<td>(0.014)</td>
<td>(0.017)</td>
<td>(0.021)</td>
<td></td>
<td>(0.009)</td>
</tr>
<tr>
<td>Num.Obs.</td>
<td>5907</td>
<td>3983</td>
<td>5392</td>
<td>4309</td>
<td>3940</td>
<td>3392</td>
<td>9230</td>
</tr>
<tr>
<td>R2</td>
<td>0.011</td>
<td>0.012</td>
<td>0.007</td>
<td>0.010</td>
<td>0.003</td>
<td>0.003</td>
<td>0.011</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="8"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>
