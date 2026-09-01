# =============================================================================
# Oreopoulos (2011) Replication -- Tables 4, 5, 6
# Faithful R translation of the author's original Stata do-file
# (oreopoulos-resume-study-replication-do-file.do)
# =============================================================================
#
# NOTE ON STANDARD ERRORS:
# The original Stata code uses `robust` (heteroskedasticity-robust, Eicker-
# Huber-White) SEs throughout, and `absorb(firmid)` (firm fixed effects, NOT
# clustering) as an alternative specification in Table 5. It does NOT use
# cluster-robust SEs anywhere. This script replicates that exactly for
# faithfulness to the original. If your own extension analysis (interaction
# models, causal forest) uses clustering by firmid instead, that is a
# deliberate, defensible deviation -- just document it explicitly as such,
# since it differs from what the original paper did.

# --- Package bootstrap: install anything missing, then load --------------------
required_packages <- c("haven", "estimatr", "dplyr")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(haven)
library(estimatr)   # lm_robust() for robust SEs and fixed effects
library(dplyr)

# --- Locate the data file -----------------------------------------------------
# Resolve relative to this script's directory so it runs regardless of the
# current working directory (source(), Rscript, or an IDE "Run" button).
this_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) getwd()
)
data_path <- file.path(this_dir, "data",
                       "oreopoulos-resume-study-replication-data-file.dta")
if (!file.exists(data_path)) {
  # Fall back to a path relative to the current working directory.
  data_path <- file.path("data", "oreopoulos-resume-study-replication-data-file.dta")
}
if (!file.exists(data_path)) {
  stop("Data file not found. Expected it at: ",
       normalizePath(file.path(this_dir, "data"), mustWork = FALSE),
       "/oreopoulos-resume-study-replication-data-file.dta")
}

df <- read_dta(data_path)

# Recode type and fall_data as factors (mirrors Stata's `i.type i.fall_data`)
df <- df %>%
  mutate(
    type      = factor(type),
    fall_data = factor(fall_data)
  )

# =============================================================================
# TABLE 4: Callback rates by resume type and ethnic origin
# =============================================================================
# Original Stata:
#   drop if accreditation==1 | reference==1 | legal==1
#   reg callback i.type i.fall_data if name_ethnicity=="X" | name_ethnicity=="Canada", robust
# ... run separately for each ethnicity, plus a pooled Indian/Pakistani/Chinese column

# IMPORTANT: in Stata, `drop if accreditation==1` treats missing values as
# NOT equal to 1 (so they are kept). In R, `filter(x != 1)` treats `NA != 1`
# as NA, which filter() silently drops -- this would incorrectly delete every
# row from the earlier sampling periods where these variables are missing
# (they were only introduced in the later wave). Replace NA with 0 first so
# the filter behaves the way the original Stata code actually does.
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

t4_indian    <- run_table4(df4, "Indian")
t4_pakistani <- run_table4(df4, "Pakistani")
t4_chinese   <- run_table4(df4, "Chinese")
t4_chncdn    <- run_table4(df4, "Chn-Cdn")
t4_british   <- run_table4(df4, "British")
t4_greek     <- run_table4(df4, "Greek")
t4_pooled    <- run_table4(df4, c("Indian", "Pakistani", "Chinese"))

cat("\n===== TABLE 4: Indian vs. Canada =====\n")
print(summary(t4_indian))
cat("\n===== TABLE 4: Pakistani vs. Canada =====\n")
print(summary(t4_pakistani))
cat("\n===== TABLE 4: Chinese vs. Canada =====\n")
print(summary(t4_chinese))
cat("\n===== TABLE 4: Chn-Cdn vs. Canada =====\n")
print(summary(t4_chncdn))
cat("\n===== TABLE 4: British vs. Canada =====\n")
print(summary(t4_british))
cat("\n===== TABLE 4: Greek vs. Canada =====\n")
print(summary(t4_greek))
cat("\n===== TABLE 4: Pooled Indian/Pakistani/Chinese vs. Canada =====\n")
print(summary(t4_pooled))

# =============================================================================
# TABLE 5: Effects of offsetting resume characteristics
# =============================================================================
# Original Stata:
#   drop British, Chn-Cdn, Greek (keep Canada + Indian/Pakistani/Chinese only)
#   replace missing same_exp/reference/accreditation/legal/extracurricular = 0
#   (1) reg callback i.type i.fall_data, robust
#   (2) areg callback i.type, robust absorb(firmid)     -- firm FIXED EFFECTS
#   (3) reg callback i.type [controls] i.fall_data, robust
#   (4) areg callback i.type [controls], robust absorb(firmid)
#   then separate regressions of callback ~ [controls] i.fall_data, by type

df5 <- df %>%
  filter(!name_ethnicity %in% c("British", "Chn-Cdn", "Greek")) %>%
  mutate(
    same_exp               = ifelse(is.na(same_exp), 0, same_exp),
    reference               = ifelse(is.na(reference), 0, reference),
    accreditation           = ifelse(is.na(accreditation), 0, accreditation),
    legal                   = ifelse(is.na(legal), 0, legal),
    extracurricular_skills  = ifelse(is.na(extracurricular_skills), 0, extracurricular_skills)
  )

controls_formula <- ~ female + ba_quality + extracurricular_skills + language_skills +
                       ma + same_exp + exp_highquality + reference + accreditation + legal

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

cat("\n===== TABLE 5, Col (1): baseline, no controls =====\n")
print(summary(t5_1))
cat("\n===== TABLE 5, Col (2): firm fixed effects =====\n")
print(summary(t5_2))
cat("\n===== TABLE 5, Col (3): with controls =====\n")
print(summary(t5_3))
cat("\n===== TABLE 5, Col (4): controls + firm fixed effects =====\n")
print(summary(t5_4))

# --- Table 5 Panel B: per-type breakdown of each characteristic's effect ---
run_table5_by_type <- function(data, type_val) {
  sub <- data %>% filter(type == type_val)
  if (type_val == 0) {
    # Type 0 never has reference/accreditation/legal (structurally undefined)
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

for (t in 0:4) {
  cat(sprintf("\n===== TABLE 5 Panel B: Type %d =====\n", t))
  print(summary(run_table5_by_type(df5, t)))
}

# =============================================================================
# TABLE 6: Callback differences by job language/social skill requirement decile
# =============================================================================
# Original Stata:
#   rescale each O*NET score by /100, sum into one combined score
#   split into deciles (percentile cutoffs)
#   run reg callback i.type i.fall_data within each decile bin

df6 <- df5 %>%
  mutate(
    skill_combined = (skillspeaking / 100) + (skillsocialper / 100) + (skillwriting / 100)
  )

deciles <- quantile(df6$skill_combined, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
decile_bounds <- unique(c(-Inf, deciles, Inf))
# NOTE: the underlying O*NET scores take only a limited number of distinct
# values (tied to ~20 occupation categories), so several percentile cutoffs
# coincide. This collapses some deciles together -- expected behavior with a
# discrete underlying variable, not an error.
n_groups <- length(decile_bounds) - 1
df6$decile <- cut(df6$skill_combined, breaks = decile_bounds, labels = 1:n_groups, include.lowest = TRUE)
