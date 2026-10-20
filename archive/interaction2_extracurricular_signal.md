Pre-Specified Interaction Model 1 — Name x Extracurricular Signal
================
DSE4101 Immigrant Discrimination Project
2026-09-10

- [Setup](#setup)
- [Sample construction](#sample-construction)
- [Model:
  `callback ~ W * extracurricular_skills + controls`](#model-callback--w--extracurricular_skills--controls)
  - [Combined table](#combined-table)

**Base treatment (fixed across all interaction models in this series):**
`W = 1(type == 1)` vs. `type == 0`, restricted to
`name_ethnicity %in% c("Canada", "Chinese", "Indian", "Pakistani")`.
Type 0 to Type 1 changes *only* the applicant’s name — education and
work-experience origin are held at “Canadian” for both — so `W` isolates
the pure name effect, uncontaminated by the foreign-credential confounds
present in Types 2–4. This is narrower than the Type 0–4 comparison used
in the Table 4/5 baseline replication, and matches TC’s instruction to
restrict the causal forest step to this cleaner contrast.

**This model’s moderator:** `extracurricular_skills`, i.e. whether the
resume lists additional extracurricular activities. This directly
operationalizes the statistical discrimination test in Oreopoulos (2011)
and Phelps (1972): the name penalty should shrink when a resume supplies
a stronger, unambiguous productivity signal.

**On standard errors.** The original Stata code uses `robust` SEs only,
never clustering. Our own group proposal specifies clustering by
`firmid` instead, since ~4 resumes are sent to each employer in most
waves. Both are reported below side by side so the deviation is explicit
and auditable rather than silently substituted.

# Setup

``` r
required_packages <- c("haven", "estimatr", "dplyr", "modelsummary")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(haven)
library(estimatr)
library(dplyr)
library(modelsummary)

data_path <- file.path("data", "oreopoulos-resume-study-replication-data-file.dta")
if (!file.exists(data_path)) {
  stop("Data file not found. Expected it at: ", normalizePath(data_path, mustWork = FALSE))
}
df <- read_dta(data_path)
```

# Sample construction

``` r
df_int1 <- df %>%
  filter(
    type %in% c(0, 1),
    name_ethnicity %in% c("Canada", "Chinese", "Indian", "Pakistani")
  ) %>%
  mutate(
    W              = as.numeric(type == 1),   # 1 = foreign name (Type 1), 0 = Type 0 baseline
    fall_data      = factor(fall_data),
    female                 = ifelse(is.na(female), 0, female),
    ba_quality              = ifelse(is.na(ba_quality), 0, ba_quality),
    language_skills         = ifelse(is.na(language_skills), 0, language_skills),
    ma                      = ifelse(is.na(ma), 0, ma),
    same_exp                = ifelse(is.na(same_exp), 0, same_exp),
    exp_highquality         = ifelse(is.na(exp_highquality), 0, exp_highquality),
    extracurricular_skills  = ifelse(is.na(extracurricular_skills), 0, extracurricular_skills)
  )

cat("Analysis sample size:", nrow(df_int1), "\n")
```

    ## Analysis sample size: 5657

``` r
cat("Treatment (W) distribution:\n")
```

    ## Treatment (W) distribution:

``` r
print(table(df_int1$W))
```

    ## 
    ##    0    1 
    ## 3026 2631

# Model: `callback ~ W * extracurricular_skills + controls`

``` r
# (1) Robust SEs only -- matching Oreopoulos's original approach
m_robust <- lm_robust(
  callback ~ W + extracurricular_skills + W:extracurricular_skills + female + ba_quality +
           language_skills + ma + same_exp + exp_highquality + fall_data,
  data = df_int1, se_type = "HC1"
)

# (2) Clustered by firmid
m_clustered <- lm_robust(
  callback ~ W + extracurricular_skills + W:extracurricular_skills + female + ba_quality +
             language_skills + ma + same_exp + exp_highquality + fall_data,
  data = df_int1, clusters = firmid, se_type = "CR2"
)
```

## Combined table

``` r
modelsummary(
  list(
    "Robust SE (Oreopoulos-style)" = m_robust,
    "Clustered by firm (proposal)"  = m_clustered
  ),
  stars    = TRUE,
  coef_omit = "fall_data",
  gof_omit  = "IC|Log|Adj|F|RMSE",
  title     = "Interaction Model 1: Name Penalty x Extracurricular Signal (Type 0 vs. Type 1)"
)
```

<table style="width:99%;">
<caption>Interaction Model 1: Name Penalty x Extracurricular Signal
(Type 0 vs. Type 1)</caption>
<colgroup>
<col style="width: 31%" />
<col style="width: 33%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th></th>
<th>Robust SE (Oreopoulos-style)</th>
<th>Clustered by firm (proposal)</th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.149***</td>
<td>0.149***</td>
</tr>
<tr>
<td></td>
<td>(0.015)</td>
<td>(0.016)</td>
</tr>
<tr>
<td>W</td>
<td>-0.063***</td>
<td>-0.063***</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.013)</td>
</tr>
<tr>
<td>extracurricular_skills</td>
<td>-0.008</td>
<td>-0.008</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.013)</td>
</tr>
<tr>
<td>female</td>
<td>0.031***</td>
<td>0.031***</td>
</tr>
<tr>
<td></td>
<td>(0.009)</td>
<td>(0.009)</td>
</tr>
<tr>
<td>ba_quality</td>
<td>-0.005</td>
<td>-0.005</td>
</tr>
<tr>
<td></td>
<td>(0.010)</td>
<td>(0.010)</td>
</tr>
<tr>
<td>language_skills</td>
<td>0.013</td>
<td>0.013</td>
</tr>
<tr>
<td></td>
<td>(0.010)</td>
<td>(0.010)</td>
</tr>
<tr>
<td>ma</td>
<td>0.002</td>
<td>0.002</td>
</tr>
<tr>
<td></td>
<td>(0.011)</td>
<td>(0.011)</td>
</tr>
<tr>
<td>same_exp</td>
<td>-0.008</td>
<td>-0.008</td>
</tr>
<tr>
<td></td>
<td>(0.013)</td>
<td>(0.014)</td>
</tr>
<tr>
<td>exp_highquality</td>
<td>0.004</td>
<td>0.004</td>
</tr>
<tr>
<td></td>
<td>(0.011)</td>
<td>(0.011)</td>
</tr>
<tr>
<td>W × extracurricular_skills</td>
<td>0.031+</td>
<td>0.031+</td>
</tr>
<tr>
<td></td>
<td>(0.017)</td>
<td>(0.018)</td>
</tr>
<tr>
<td>Num.Obs.</td>
<td>5657</td>
<td>5657</td>
</tr>
<tr>
<td>R2</td>
<td>0.009</td>
<td>0.009</td>
</tr>
<tr>
<td>Std.Errors</td>
<td></td>
<td>by: firmid</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="3"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

The coefficient of interest is `W:extracurricular_skills`. A positive
and significant estimate means that listing extracurricular activities
reduces the name penalty associated with having a foreign name.
Specifically, the positive interaction coefficient of 0.031 indicates
that the difference in callback rates between foreign- and baseline-name
applicants becomes 0.031 larger when extracurricular skills are listed.
This is consistent with statistical discrimination, as employers appear
to respond to an observable signal of applicant quality that reduces
their reliance on the name as a signal of expected productivity.
