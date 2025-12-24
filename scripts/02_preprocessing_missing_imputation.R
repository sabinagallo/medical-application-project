# =============================================================================
# Medical Application Project - Preprocessing and missing imputation
# 02_preprocessing_missing_imputation.R
# ==============================================================================

source("scripts/01_setup_load.R")

# =============================================================================
# Missingness audit
# ==============================================================================

# Missing rate per variable (baseline)
miss_base <- baseline %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), 
               names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

cat("\nTop 10 baseline missing rates:\n")
print(head(miss_base, 10))

# Missing rate per variable (long)
miss_long <- long %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable",
               values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

cat("\nTop 10 longitudinal missing rates:\n")
print(head(miss_long, 10))

# Missingness by (CYCLE; MONTH) for key outcomes
key_vars <- c("MMDs", "HIT6", "HADSA", "HADSD", "INT", "MIDAS")
key_vars <- intersect(key_vars, names(long))

miss_by_time <- long %>%
  group_by(CYCLE, MONTH) %>%
  summarise(across(all_of(key_vars), ~mean(is.na(.)), .names = "miss_{.col}"),
            .groups = "drop")

cat("\nMissingness by cycle/month (first rows):\n")
print(head(miss_by_time, 10))
