Cleaned Data
================
DSE4101 Immigrant Discrimination Project
2026-11-13

- [Setup](#setup)
- [Variables excluded and reasons:](#variables-excluded-and-reasons)
- [1. Variables with no variation \[reference, accreditation,
  legal\]](#1-variables-with-no-variation-reference-accreditation-legal)
- [After filtering for Type 0 and Type 1 and excluding Greek and
  Chinese-Canadian](#after-filtering-for-type-0-and-type-1-and-excluding-greek-and-chinese-canadian)
- [names, these variables contain only 0s or missing
  values.](#names-these-variables-contain-only-0s-or-missing-values)
- [As they exhibit no variation in the filtered sample, they provide no
  useful](#as-they-exhibit-no-variation-in-the-filtered-sample-they-provide-no-useful)
- [information for our main
  analysis.](#information-for-our-main-analysis)
- [2. Ethnicity and name variables \[name_ethnicity, name, chinese,
  indian,
  british,](#2-ethnicity-and-name-variables-name_ethnicity-name-chinese-indian-british)
- [pakistani, chn_cdn\]:](#pakistani-chn_cdn)
- [We are interested in the effect of having a foreign name on callback,
  rather than differences across specific
  ethnicities.](#we-are-interested-in-the-effect-of-having-a-foreign-name-on-callback-rather-than-differences-across-specific-ethnicities)
- [These variables identify specific names or ethnic groups and are not
  needed for our main
  analysis.](#these-variables-identify-specific-names-or-ethnic-groups-and-are-not-needed-for-our-main-analysis)
- [3. Firm and occupation variables \[firmid,
  city,](#3-firm-and-occupation-variables-firmid-city)
- [additional_credential,occupation_type, skillspeaking,
  skillsocialper,](#additional_credentialoccupation_type-skillspeaking-skillsocialper)
- [skillwriting\]:](#skillwriting)
- [Our research question is about heterogeneity across resume
  characteristics, but these variables describe the firm or job posting,
  not the
  resume.](#our-research-question-is-about-heterogeneity-across-resume-characteristics-but-these-variables-describe-the-firm-or-job-posting-not-the-resume)
- [4. Unknown variables \[listedaccreditation,
  certificate\]:](#4-unknown-variables-listedaccreditation-certificate)
- [No definition found in the paper or README file, so we could not
  confirm what they measure or whether they are pre-treatment. Excluded
  to avoid including variables with unclear
  meaning.](#no-definition-found-in-the-paper-or-readme-file-so-we-could-not-confirm-what-they-measure-or-whether-they-are-pre-treatment-excluded-to-avoid-including-variables-with-unclear-meaning)
- [5. Other callback-related outcomes \[interview,
  second_callback\]:](#5-other-callback-related-outcomes-interview-second_callback)
- [These variables are excluded because our outcome of interest is
  callback. Including these post-treatment variables as predictors could
  introduce outcome leakage because they describe aspects of the
  callback outcome rather than characteristics known independently of
  it.](#these-variables-are-excluded-because-our-outcome-of-interest-is-callback-including-these-post-treatment-variables-as-predictors-could-introduce-outcome-leakage-because-they-describe-aspects-of-the-callback-outcome-rather-than-characteristics-known-independently-of-it)
- [Check for missing values across waves
  (fall_data)](#check-for-missing-values-across-waves-fall_data)
- [Save dataset](#save-dataset)

# Setup

``` r
required_packages <- c("haven", "estimatr", "dplyr", "tidyr", "modelsummary")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(haven)
library(estimatr)
library(dplyr)
library(modelsummary)
library(tidyr)

data_path <- file.path("data", "oreopoulos-resume-study-replication-data-file.dta")
if (!file.exists(data_path)) {
  stop("Data file not found. Expected it at: ", normalizePath(data_path, mustWork = FALSE))
}
df <- read_dta(data_path)
```

``` r
vars <- c("type", "female", "ba_quality", "extracurricular_skills", "language_skills",
          "ma", "same_exp", "exp_highquality", "fall_data", "reference",
          "accreditation", "legal")
```

``` r
length(vars)
```

    ## [1] 12

``` r
df_int1 <- df %>%
  filter(
    type %in% c(0, 1),
    name_ethnicity %in% c("Canada", "Chinese", "Indian", "Pakistani")
  )
```

``` r
# Counts of 0s, 1s and Missing Values by Variable

counts <- df_int1 %>%
  select(all_of(vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n_0 = sum(value == 0, na.rm = TRUE),
    n_1 = sum(value == 1, na.rm = TRUE),
    n_missing = sum(is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(match(variable, vars))

View(counts)
```

``` r
# Exclude reference, accreditation, and legal as they don't provide any useful information (contain only 0 or NA after filtering)

vars <- setdiff(vars, c("reference", "accreditation", "legal"))
```

# Variables excluded and reasons:

# 1. Variables with no variation \[reference, accreditation, legal\]

# After filtering for Type 0 and Type 1 and excluding Greek and Chinese-Canadian

# names, these variables contain only 0s or missing values.

# As they exhibit no variation in the filtered sample, they provide no useful

# information for our main analysis.

# 2. Ethnicity and name variables \[name_ethnicity, name, chinese, indian, british,

# pakistani, chn_cdn\]:

# We are interested in the effect of having a foreign name on callback, rather than differences across specific ethnicities.

# These variables identify specific names or ethnic groups and are not needed for our main analysis.

# 3. Firm and occupation variables \[firmid, city,

# additional_credential,occupation_type, skillspeaking, skillsocialper,

# skillwriting\]:

# Our research question is about heterogeneity across resume characteristics, but these variables describe the firm or job posting, not the resume.

# 4. Unknown variables \[listedaccreditation, certificate\]:

# No definition found in the paper or README file, so we could not confirm what they measure or whether they are pre-treatment. Excluded to avoid including variables with unclear meaning.

# 5. Other callback-related outcomes \[interview, second_callback\]:

# These variables are excluded because our outcome of interest is callback. Including these post-treatment variables as predictors could introduce outcome leakage because they describe aspects of the callback outcome rather than characteristics known independently of it.

``` r
df_int1 <- df_int1 %>%
  mutate(
    D = as.integer(type == 1), # 1 = foreign name (Type 1), 0 = Type 0 baseline
    fall_data = factor(fall_data) # Treats wave as a category rather than a continuous numerical variable
  ) 

cat("Analysis sample size:", nrow(df_int1), "\n")
```

    ## Analysis sample size: 5657

``` r
cat("Treatment (D) distribution:\n")
```

    ## Treatment (D) distribution:

``` r
print(table(df_int1$D))
```

    ## 
    ##    0    1 
    ## 3026 2631

# Check for missing values across waves (fall_data)

``` r
vars_no_fall <- setdiff(vars, "fall_data")

# N with a non-empty value, by variable and wave
counts_long <- df_int1 %>%
  mutate(across(all_of(vars_no_fall), as.character)) %>%
  pivot_longer(all_of(vars_no_fall), names_to = "variable", values_to = "value") %>%
  group_by(fall_data, variable) %>%
  summarise(n = sum(!is.na(value) & trimws(value) != ""), .groups = "drop")

# Total rows per wave, shown as the first row of the table
total_rows <- df_int1 %>%
  count(fall_data, name = "n") %>%
  mutate(variable = "Total rows")

balance_table <- bind_rows(total_rows, counts_long) %>%
  mutate(variable = factor(variable, levels = c("Total rows", vars_no_fall))) %>%
  arrange(variable) %>%
  pivot_wider(
    names_from   = fall_data,
    values_from  = n,
    names_prefix = "Wave ",
    names_sort   = TRUE
  ) %>%
  mutate(Total = rowSums(across(starts_with("Wave "))))

print(balance_table, n = Inf)
```

    ## # A tibble: 9 × 5
    ##   variable               `Wave 0` `Wave 1` `Wave 2` Total
    ##   <fct>                     <int>    <int>    <int> <dbl>
    ## 1 Total rows                 1902     1495     2260  5657
    ## 2 type                       1902     1495     2260  5657
    ## 3 female                     1902     1495     2260  5657
    ## 4 ba_quality                 1902     1495     2260  5657
    ## 5 extracurricular_skills     1902     1495     2260  5657
    ## 6 language_skills            1902     1495     2260  5657
    ## 7 ma                         1902     1495     2260  5657
    ## 8 same_exp                   1902     1495        0  3397
    ## 9 exp_highquality            1902     1495     2260  5657

# Save dataset

``` r
# Save the corrected dataset
save(df_int1, file = "cleaned data.Rda")

cat("Dataset successfully updated!\n")
```

    ## Dataset successfully updated!

``` r
cat("Observations:", nrow(df_int1), "\n")
```

    ## Observations: 5657

``` r
cat("Variables:", ncol(df_int1), "\n")
```

    ## Variables: 32
