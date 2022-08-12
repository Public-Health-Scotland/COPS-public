#***********************************************************************************************
# Prepare data for early pregnancy outcome analysis:
# bring in baby data and select variables for pregnancy analysis
# This file:
#***********************************************************************************************

#### Housekeeping ####

library(janitor)
library(dplyr)
library(tidyr)
library(forcats)
library(renv)
library(here)
library(odbc)
library(magrittr)
library(lubridate)
library(readr)
library(hablar)
library(labelled)
library(purrr)
library(tictoc)

setwd("x")
baby.data <- readRDS("x")
warnings()

folder_temp_data <- "x"
folder_results <- "x"

# Check available variables
names(baby.data)

# check for issues with the data files
warnings()

#### Basic cleaning####

# remove labels from data
look_for(baby.data) %>% View()

baby.data <- remove_labels(baby.data, user_na_to_na = TRUE)

# Number of foetuses
nrow(baby.data)
# Number of pregnancies
length(unique(baby.data$pregnancy_id))

# Create variable which categorises when events occur (i.e. pandemic or pre-pandemic)
baby.data$overall_outcome_year <- format(as.Date(baby.data$x_pregnancy_end_date, format = "%Y-%M-%D"), "%Y")
unique(baby.data$overall_outcome_year)

baby.data$overall_outcome_year <- fct_explicit_na(baby.data$overall_outcome_year, na_level = "Ongoing")
unique(baby.data$overall_outcome_year)

baby.data$overall_conception_year <- format(as.Date(baby.data$x_est_conception_date, format = "%Y-%M-%D"), "%Y")
unique(baby.data$overall_conception_year)

table(baby.data$overall_outcome_year)
table(baby.data$overall_outcome_year, baby.data$outcome, exclude = NULL)

# drop 2014 outcomes
baby.data <- subset(baby.data, overall_outcome_year != 2014)

# Drop all women missing age, as these are women who were outside of age criteria for COPS
# and should not be included
summary(baby.data$x_mother_age_at_conception)
unique(baby.data$x_mother_age_at_conception)

sum(is.na(baby.data$x_mother_age_at_conception))

agecheck <- baby.data[is.na(baby.data$x_mother_age_at_conception), ]
length(unique(agecheck$mother_eave_linkno)) 
length(unique(agecheck$pregnancy_id)) 

summary(agecheck$x_mother_dob)
agecheck$x_age_from_dob <- (agecheck$x_est_conception_date - agecheck$x_mother_dob) / 365
table(agecheck$x_age_from_dob)

baby.data <- baby.data[!is.na(baby.data$x_mother_age_at_conception), ]

# Number of foetuses (having removed 2014 and those where maternal age missing)
nrow(baby.data)
# Number of pregnancies
length(unique(baby.data$pregnancy_id))

# Create variable counting number of fetus for a given pregnancy
baby.data$count <- 1
baby.data <- baby.data %>%
  group_by(pregnancy_id) %>%
  mutate(total_foetus = n()) %>%
  mutate(order_foetus = cumsum(count == 1), NA) %>% 
  ungroup()

addmargins(table(baby.data$total_foetus))

# Create factor variable with pregnancy outcomes
addmargins(table(baby.data$outcome, exclude = NULL))

baby.data$overall_outcome.f <- factor(baby.data$outcome, levels = c("Termination",
                                                                    "Molar pregnancy", 
                                                                    "Miscarriage", 
                                                                    "Ectopic pregnancy",
                                                                    "Stillbirth", 
                                                                    "Live birth", 
                                                                    "Ongoing", 
                                                                    "Unknown"))
is.factor(baby.data$overall_outcome.f)
addmargins(table(baby.data$overall_outcome.f, exclude = NULL))

#### Number of events over time ####

# Create a table of pregnancy outcomes by year
outcomes_by_year_baby <- baby.data %>%
  tabyl(overall_outcome.f, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

write.csv(outcomes_by_year_baby, paste(folder_results, "baby_events_by_year.csv", sep = ""))

#### Check number of selective reductions####
table(baby.data$aas_selective_reduction)

selectivereduction <- baby.data %>%
  arrange(mother_eave_linkno) %>%
  group_by(mother_eave_linkno) %>%
  mutate(anyreducation = min_(aas_selective_reduction)) %>%
  ungroup()

selectivereduction <- subset(selectivereduction, anyreducation == 1)

# we're going to use this table to look for selective reductions
# where there were only terminations recorded
table(selectivereduction$mother_eave_linkno, selectivereduction$outcome)

# let's double check it here with some automation from the table to find just the eave numbers
table(selectivereduction$mother_eave_linkno, selectivereduction$outcome) %>%
 as.data.frame() %>%
  select(mother_eave_linkno = Var1, outcome = Var2, freq = Freq) %>%
  pivot_wider(names_from = outcome, values_from = freq) %>%
  janitor::clean_names() %>%
  filter(live_birth == 0 & miscarriage == 0 & termination != 0) %>%
  select(mother_eave_linkno) 

#### Prepare pregnancy file####

baby.data2 <- subset(baby.data, aas_selective_reduction == 2 | is.na(aas_selective_reduction))

names(baby.data2)

# this has been split into two parts to try to sort out speeding it up
pregnancies <- 
  baby.data2 %>%
  #summarise() won't alter NA date values, so use 1970-01-01 as a stand-in and change it back later
  mutate(x_pregnancy_end_date = replace_na(x_pregnancy_end_date, as.Date("1970-01-01"))) %>%
  arrange(pregnancy_id) %>%
  group_by(pregnancy_id) %>%
  mutate(overall_outcome = case_when(
    "Termination" %in% outcome ~ "Termination",
    "Stillbirth" %in% outcome ~ "Stillbirth",
    "Ectopic pregnancy" %in% outcome ~ "Ectopic pregnancy",
    "Molar pregnancy" %in% outcome ~ "Molar pregnancy",
    "Miscarriage" %in% outcome ~ "Miscarriage",
    "Live birth" %in% outcome ~ "Live birth",
    "Ongoing" %in% outcome ~ "Ongoing",
    "Unknown" %in% outcome ~ "Unknown"
  )) %>%
  ungroup()

pregnancies2 <- 
pregnancies %>%
  group_by(pregnancy_id) %>%
  summarise(
  mother_eave_linkno = first_(mother_eave_linkno),
  gestation_at_outcome = max_(x_gestation_at_outcome),
  pregnancy_end_date = as.Date(max_(x_pregnancy_end_date)),
  est_conception_date = as.Date(min_(x_est_conception_date)),
  overall_outcome = first_(overall_outcome),
  first_wave = max_(x_first_wave),
  full_cohort = max_(x_full_cohort),
  mother_dob = first_(x_mother_dob),
  mother_age_at_conception = first_(x_mother_age_at_conception),
  mother_age_at_outcome = first_(x_mother_age_at_outcome),
  hbres = first_(x_hbres),
  simd = first_(x_simd),
  bmi = first_(x_bmi),
  booking_bmi = first_(smr02_bmi),
  booking_smoking_status = first_(x_booking_smoking_status),
  gp_smoking_status = first_(x_gp_smoking_status),
  overall_smoking_status = first_(x_overall_smoking_status),
  ethnicity_code = first_(x_ethnicity_code),
  ethnicity_description = first_(x_ethnicity_desc),
  urban_rural_description = first_(x_urban_rural_8_description),
  births_this_pregnancy = max_(x_births_this_pregnancy),
  diabetes = max_(x_diabetes),
  shielding = max_(shielding_shield),
  shielding_group1 = max_(shielding_group1),
  shielding_group2 = max_(shielding_group2),
  shielding_group3 = max_(shielding_group3),
  shielding_group4 = max_(shielding_group4),
  shielding_group5 = max_(shielding_group5),
  shielding_group6 = max_(shielding_group6),
  shielding_group7 = max_(shielding_group7),
  shielding_group_any = max_(shielding_group_any),
  q_covid = max_(q_covid),
  q_bmi = first_(q_bmi),
  q_bmi_40_plus = max_(q_bmi_40_plus),
  q_diabetes_type = max_(q_diabetes_type),
  q_diag_af = max_(q_diag_af),
  q_diag_asthma = max_(q_diag_asthma),
  q_diag_blood_cancer = max_(q_diag_blood_cancer),
  q_diag_ccf = max_(q_diag_ccf),
  q_diag_cerebralpalsy = max_(q_diag_cerebralpalsy),
  q_diag_chd = max_(q_diag_chd),
  q_diag_cirrhosis = max_(q_diag_cirrhosis),
  q_diag_ckd3 = max_(q_diag_ckd3),
  q_diag_ckd4 = max_(q_diag_ckd4),
  q_diag_ckd5 = max_(q_diag_ckd5),
  q_diag_congen_hd = max_(q_diag_congen_hd),
  q_diag_copd = max_(q_diag_copd),
  q_diag_dementia = max_(q_diag_dementia),
  q_diag_diabetes_1 = max_(q_diag_diabetes_1),
  q_diag_diabetes_2 = max_(q_diag_diabetes_2),
  q_diag_epilepsy = max_(q_diag_epilepsy),
  q_diag_fracture = max_(q_diag_fracture),
  q_diag_neuro = max_(q_diag_neuro),
  q_diag_parkinsons = max_(q_diag_parkinsons),
  q_diag_pulm_hyper = max(q_diag_pulm_hyper),
  q_diag_pulm_rare = max_(q_diag_pulm_rare),
  q_diag_pvd = max_(q_diag_pvd),
  q_diag_ra_sle = max_(q_diag_ra_sle),
  q_diag_resp_cancer = max_(q_diag_resp_cancer),
  q_diag_sev_ment_ill = max_(q_diag_sev_ment_ill),
  q_diag_sickle_cell = max_(q_diag_sickle_cell),
  q_diag_stroke = max_(q_diag_stroke),
  q_diag_vte = max_(q_diag_vte),
  q_diag_renal_failure = max_(q_diag_renal_failure),
  q_ethnicity = first_(q_ethnicity),
  q_ethnicity_mapped9 = first_(q_ethnicity_mapped9),
  q_home_cat = first_(q_home_cat), # Should we use first_() or max_() here?
  q_learn_cat = first_(q_learn_cat), # Should we use first_() or max_() here?
  q_preexisting_diabetes = max_(q_preexisting_diabetes),
  cv_clinical_vulnerability_category = first_(cv_clinical_vulnerability_category),
  dose_1_vacc_occurence_date = first_(dose_1_vacc_occurence_date),
  dose_1_vacc_product_name = first_(dose_1_vacc_product_name),
  dose_1_vacc_location_health_board_name = first_(dose_1_vacc_location_health_board_name),
  dose_2_vacc_occurence_date = first_(dose_2_vacc_occurence_date),
  dose_2_vacc_product_name = first_(dose_2_vacc_product_name),
  dose_2_vacc_location_health_board_name = first_(dose_2_vacc_location_health_board_name),
  dose_3_vacc_occurence_date = first_(dose_3_vacc_occurence_date),
  dose_3_vacc_product_name = first_(dose_3_vacc_product_name),
  dose_3_vacc_location_health_board_name = first_(dose_3_vacc_location_health_board_name),
  dose_4_vacc_occurence_date = first_(dose_4_vacc_occurence_date),
  dose_4_vacc_product_name = first_(dose_4_vacc_product_name),
  dose_4_vacc_location_health_board_name = first_(dose_4_vacc_location_health_board_name),
  mother_earliest_positive_test = first_(tests_mother_earliest_positive_test),
  mother_tested_positive_during_pregnancy = max_(tests_mother_positive_test_during_pregnancy),
  mother_total_positive_during_pregnancy = first_(tests_mother_total_positives_during_this_pregnancy),
  mother_earliest_positive_test_during_pregnancy = first_(tests_mother_earliest_positive_test_during_pregnancy),
  mother_has_pos_test_at_any_point = first_(tests_mother_has_pos_test_at_any_point),
  mother_positive_test_during_pregnancy = first_(tests_mother_positive_test_during_pregnancy),
  mother_value_positive_test_during_pregnancy_1 = first_(tests_mother_value_positive_test_during_pregnancy_1),
  mother_value_positive_test_during_pregnancy_2 = first_(tests_mother_value_positive_test_during_pregnancy_2),
  total_foetus = max_(total_foetus),
  gp_data_status = "GP data included",
  chi_validity = first_(chi_validity)
)

pregnancies2$pregnancy_end_date[pregnancies2$pregnancy_end_date == as.Date("1970-01-01")] <- NA

#### Write pregnancy-level file ####
pregnancies2 %>% write_rds(paste0(folder_temp_data, "pregnancy_level_record_hierarchy2.rds"), compress = "gz")
