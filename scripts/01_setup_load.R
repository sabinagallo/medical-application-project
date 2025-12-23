# =============================================================================
# Medical Application Project - Setup & Load
# 01_setup_load.R
# ============================================================================

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

baseline <- readr::read_delim("data/MigraineBaselineVars.csv", 
                        delim = ";", show_col_types = FALSE)
long <- readr::read_delim("data/MigraineLongitudianlVars.csv", 
                 delim = ";", show_col_types = FALSE)

cat("OK load. \n")
cat("baseline dim:", dim(baseline), "\n")
cat("long dim:", dim(long), "\n")

# =============================================================================
#  Basic checks (IDs + duplicates)
# =============================================================================

# Number of unique patients
cat("\n --- ID counts ---\n")
cat("n_distinct baseline SUBJECT_ID:", 
    n_distinct(baseline$SUBJECT_ID), "\n")
cat("n_distinct long SUBJECT_ID", 
    n_distinct(long$SUBJECT_ID), 
    "\n")

# Patients present in one dataset but not the other
ids_base <- unique(baseline$SUBJECT_ID)
ids_long <- unique(long$SUBJECT_ID)

cat("\n--- ID overlap ---\n")
cat("In long not in baseline:", 
    length(setdiff(ids_long, ids_base)), "\n")
cat("In baseline not in long:", length(setdiff(ids_base, ids_long)), "\n")

# Duplicate in baseline by SUBJECT_ID
dup_base <- baseline %>%
  count(SUBJECT_ID, name = "n") %>%
  filter(n > 1)

cat("\n--- Duplicates ---\n")
cat("Baseline duplicates on SUBJECT_ID:", nrow(dup_base), "\n")

# Duplicate in longitudinal by (SUBJECT_ID, CYCLE, MONTH)
dup_long <- long %>%
  count(SUBJECT_ID, CYCLE, MONTH, name = "n") %>%
  filter(n> 1)

cat("Long duplicates on (SUBJECT_ID, CYCLE, MONTH):", nrow(dup_long),
    "\n")

# =============================================================================
#  Variable types (factors vs numeric)
# =============================================================================

# ---- Baseline dataset ----
baseline <- baseline %>%
  mutate(
    # SEX coding: 1=Female, 2=Male
    SEX = factor(SEX, levels = c(1, 2), 
                 labels = c("Female", "Male")),
    
    # DIAGNOSIS coding: 1=Chronic, 2=Medication Overuse, £=HF Episodic
    DIAGNOSIS = factor(DIAGNOSIS, levels = c(1, 2, 3),
                       labels = c("Chronic", "Medication OOveruse",
                                  "HF Episodic"))
  )

# Convert selected binary baseline variables to Yes/no only if they exist
bin_base_vars <- intersect(
  c("Sleep_Disorders", "Psycopathological", "Hypertension",
    "Familiarity", "Aura", "Bbloc", "Caant", "Tricyclic", "Antiepil",
    "SSRISNRI", "Antiipnt", "Botulin", "DETOXPRE"),
  names(baseline)
)

baseline <- baseline %>%
  mutate(across(all_of(bin_base_vars),
                ~ factor(., levels = c(1, 2),
                         labels = c("Yes", "No"))))


