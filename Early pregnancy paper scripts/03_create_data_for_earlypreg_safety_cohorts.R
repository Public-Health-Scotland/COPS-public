#***********************************************************************************************
# Prepare data for early pregnancy outcome analysis
# This file:
#  1. Prepares the pregnancy data for then making cohorts
#***********************************************************************************************

#### Housekeeping####

library(expss)
library(tidyverse)

setwd("x")

folder_temp_data <- "x"
folder_results <- "x"

preg.data.ready <- readRDS(paste0(folder_temp_data, "pregnancy_level_record_ready.rds"))

# Work with reduced dataset
names(preg.data.ready)
table(preg.data.ready$final_symptomatic_covid_infection_1)

z <- preg.data.ready %>%
  select(
    # identifiers

    mother_eave_linkno,
    pregnancy_id,

    # pregnancy information
    gestation_at_outcome,
    pregnancy_end_date,
    est_conception_date,
    overall_outcome,
    overall_outcome.f,
    mother_age_at_conception,
    gestation_at_outcome_using_conception_date,

    # vaccine info
    dose_1_vacc_occurence_date,
    dose_2_vacc_occurence_date,
    dose_3_vacc_occurence_date,
    dose_4_vacc_occurence_date,
    dose,
    anydose,
    dose_1_vacc_product_name,
    dose_2_vacc_product_name,
    dose_3_vacc_product_name,
    dose_4_vacc_product_name,

    # covid info
    mother_tested_positive_during_pregnancy,
    mother_earliest_positive_test_during_pregnancy,

    # index dates (index = date of positive covid test)
    index_date_covid_infection_1,
    index_date_covid_infection_2,
    index_date_covid_infection_3,
    index_date_covid_infection_4,
    final_symptomatic_covid_infection_1,
    final_symptomatic_covid_infection_2,
    final_symptomatic_covid_infection_3,
    final_symptomatic_covid_infection_4,

    # variables for analysis
    bmi_cat,
    simd,
    ethnicity_cat,
    diabetes_cat,
    overall_smoking_status,
    cv_clinical_vulnerability_category,
    UR8_categories,
    UR6_categories,
    UR2_categories
  )


#### Prepare relavant variables for whole cohort ####

# Post vaccination risk period to be examined -----------------------------

# Create a variable to capture pregnancy outcome status at the endpoint of interest,
# defined by the outcome of interest

# For miscarriage and ectopic pregnancy our post vaccination period to be examined is up to 19+6 weeks,

# This code looks at pregnancy outcomes at different weeks of gestation and identifies
# whether pregnancies were:
# - known to still be ongoing at the time period post vaccination to be examined
# - whether they were ongoing and had not yet reached the time period to examined

# Note that conception date is 2+0 weeks gestation, not 0 weeks gestation,
# so below the end date of interest for miscarriage is set to 17+6 weeks, not 19+6 weeks

# first, let's check if we have any missing pregnancy_end_dates with est_conception_date present
# we do, so we'll need to account for that in our code below
z %>% filter(is.na(pregnancy_end_date))
z %>% filter(is.na(pregnancy_end_date), is.na(est_conception_date))

z <-
  z %>%
  #  if the pregnancy_end_date is missing, use est_conception_date to estimate end_date
  # also set flag so we can filter out on this later
  mutate(pregnancy_end_date_estimated_from_est_conception_date = case_when(
    is.na(pregnancy_end_date) ~ "yes",
    !is.na(pregnancy_end_date) ~ "no"
  )) %>%
  mutate(pregnancy_end_date = case_when(
    !is.na(pregnancy_end_date) ~ pregnancy_end_date,
    # using 40 weeks pregnancy here, or 38 from est_conception_date
    is.na(pregnancy_end_date) ~ est_conception_date + (38 * 7)
  )) %>%
  # set each risk study end date to the post vaccination risk period to be examined in the protocol
  mutate(
    miscarriage_study_end = est_conception_date + ((17 * 7) + 6),
    ectopic_study_end = est_conception_date + ((17 * 7) + 6)
  ) %>%
  # - if the pregnancy has ended before the study end date, we want to use the
  #   known end date
  # - if the pregnany has not ended before the study end date, ie the pregnancy is
  #   ongoing, we use the study_end date as our variable here so we can see that
  #   the pregnancy is ongoing

  # e.g. if the pregnancy ended at four weeks, use that date as our study_pregnancy_end_date,
  # but if the actual pregnancy_end_date was e.g. 37weeks, we'll use our study_end date of 19+6 weeks
  # we do this to create a time period (est_conception_date : study_pregnancy_end_date) where we can
  # see both the outcome AND what vaccine/infection events happened in this time period

  mutate(miscarriage_study_pregnancy_end_date = case_when(
    # if actual pregnancy end date is over the 19+6 week study period, use the study end data
    pregnancy_end_date > miscarriage_study_end ~ miscarriage_study_end,
    # if actual pregnancy end date is less than or equal to the 19+6 week study period, use actual preg end date
    pregnancy_end_date <= miscarriage_study_end ~ pregnancy_end_date,
    # if pregnancy_end_date is missing, use miscarriage_study_end date (est_conception_date + 17+6)
    is.na(pregnancy_end_date) ~ miscarriage_study_end
  )) %>%
  mutate(ectopic_study_pregnancy_end_date = case_when(
    pregnancy_end_date > ectopic_study_end ~ ectopic_study_end,
    pregnancy_end_date <= ectopic_study_end ~ pregnancy_end_date,
    is.na(pregnancy_end_date) ~ ectopic_study_end
  )) 

# double check we're not missing anything here - have we got any pregnancy_end_dates LESS THAN (<) study_end_date?
# we should have lots of pregnancy_end_date less than the study_end - which is 19+6 
z %>% filter(pregnancy_end_date < miscarriage_study_end)
z %>% filter(pregnancy_end_date < ectopic_study_end)

# but we should have no remaining results where pregnancy_end_date is less than study_pregnancy_end_date,
# as this was calculated using the logic above
z %>% filter(pregnancy_end_date < miscarriage_study_pregnancy_end_date)
z %>% filter(pregnancy_end_date < ectopic_study_pregnancy_end_date)

# if our actual pregnancy end date is greater than our follow up time point for miscarriage,
# we're just going to say it was ongoing after week 19 etc as that's all we're interested in for this study
# for the others, we'll use the overall_outcome as the risk period is post pregnancy_end_date
z <- z %>%
  mutate(miscarriage_study_outcome = case_when(
    pregnancy_end_date > miscarriage_study_pregnancy_end_date ~ "Ongoing wk 19",
    TRUE ~ overall_outcome
  )) %>%
  mutate(ectopic_study_outcome = case_when(
    pregnancy_end_date > ectopic_study_pregnancy_end_date ~ "Ongoing wk 19",
    TRUE ~ overall_outcome
  ))

# check to see if 'Ongoing wk 19' is before 20 week or after
table(z$gestation_at_outcome, exclude = NULL)
table(z$gestation_at_outcome, z$miscarriage_study_outcome, exclude = NULL)
table(z$gestation_at_outcome, z$ectopic_study_outcome, exclude = NULL)

# Vaccination information -------------------------------------------------

# As well as timing of outcome period to follow up, we have different lengths
# of time post vaccination to include. Let's include these variables now so we
# can filter them out later.
z <-
  z %>%
  mutate(
    minus_6_to_zero_timing_of_vaccination_to_include_up_to = est_conception_date,
    zero_to_19_timing_of_vaccination_to_include_up_to = est_conception_date + (17 * 7) + 6,
    zero_to_02_timing_of_vaccination_to_include_up_to = est_conception_date + (0 * 7) + 6,
    miscarriage_study_timing_of_vaccination_to_include_up_to = est_conception_date + (17 * 7) + 6,
    ectopic_study_timing_of_vaccination_to_include_up_to = est_conception_date + (0 * 7) + 6
  )

# We can then add in the vaccination period we're looking at
z <- z %>%
  mutate(
    minus_6_to_zero_vaccination_timing_period_start = as.Date(est_conception_date) - (6 * 7),
    minus_6_to_zero_vaccination_timing_period_end = minus_6_to_zero_timing_of_vaccination_to_include_up_to,
    zero_to_19_vaccination_timing_period_start = as.Date(est_conception_date),
    zero_to_19_vaccination_timing_period_end = zero_to_19_timing_of_vaccination_to_include_up_to,
    zero_to_02_vaccination_timing_period_start = as.Date(est_conception_date),
    zero_to_02_vaccination_timing_period_end = zero_to_02_timing_of_vaccination_to_include_up_to,
    miscarriage_vaccination_timing_period_start = as.Date(est_conception_date) - (6 * 7),
    miscarriage_vaccination_timing_period_end = miscarriage_study_timing_of_vaccination_to_include_up_to,
    ectopic_vaccination_timing_period_start = as.Date(est_conception_date) - (6 * 7),
    ectopic_vaccination_timing_period_end = ectopic_study_timing_of_vaccination_to_include_up_to
  )

# Now lets add in if vaccination occured during the vaccination timing period.
# For it to occur in the vaccination period, it needs to be between the vaccination timing period and
# before the study pregnancy end date.

vaccine_dose_mutate_function <- function(df, dose, condition) {
  col_name <- rlang::sym(paste(dose, "_", condition, "_vaccination_timing_period", sep = ""))
  dose_column <- rlang::sym(paste(dose, "_vacc_occurence_date", sep = ""))
  vaccination_period_end <- rlang::sym(paste(condition, "_vaccination_timing_period_end", sep = ""))
  vaccination_period_start <- rlang::sym(paste(condition, "_vaccination_timing_period_start", sep = ""))

  df %>%
    mutate(!!col_name := case_when(
      (!!dose_column) < (!!vaccination_period_end) &
        (!!dose_column) > (!!vaccination_period_start) &
        (!!dose_column) < pregnancy_end_date ~ 1,
      TRUE ~ 0
    ))
}

number_of_doses <- c("dose_1", "dose_2", "dose_3", "dose_4")
conditions <- c("miscarriage", "ectopic", "minus_6_to_zero", "zero_to_19", "zero_to_02")

for (dose in number_of_doses) {
  for (condition in conditions) {
    z <- z %>% vaccine_dose_mutate_function(dose = dose, condition = condition)
  }
}

# now use this to create a dose history column
z <- z %>%
  unite("dose_history_miscarriage_vaccination_timing_period",
    dose_1_miscarriage_vaccination_timing_period,
    dose_2_miscarriage_vaccination_timing_period,
    dose_3_miscarriage_vaccination_timing_period,
    dose_4_miscarriage_vaccination_timing_period,
    remove = FALSE, sep = ""
  ) %>%
  unite("dose_history_ectopic_vaccination_timing_period",
    dose_1_ectopic_vaccination_timing_period,
    dose_2_ectopic_vaccination_timing_period,
    dose_3_ectopic_vaccination_timing_period,
    dose_4_ectopic_vaccination_timing_period,
    remove = FALSE, sep = ""
  ) %>%
  unite("dose_history_minus_6_to_zero_vaccination_timing_period",
    dose_1_minus_6_to_zero_vaccination_timing_period,
    dose_2_minus_6_to_zero_vaccination_timing_period,
    dose_3_minus_6_to_zero_vaccination_timing_period,
    dose_4_minus_6_to_zero_vaccination_timing_period,
    remove = FALSE, sep = ""
  ) %>%
  unite("dose_history_zero_to_19_vaccination_timing_period",
    dose_1_zero_to_19_vaccination_timing_period,
    dose_2_zero_to_19_vaccination_timing_period,
    dose_3_zero_to_19_vaccination_timing_period,
    dose_4_zero_to_19_vaccination_timing_period,
    remove = FALSE, sep = ""
  )   %>%
  unite("dose_history_zero_to_02_vaccination_timing_period",
    dose_1_zero_to_02_vaccination_timing_period,
    dose_2_zero_to_02_vaccination_timing_period,
    dose_3_zero_to_02_vaccination_timing_period,
    dose_4_zero_to_02_vaccination_timing_period,
    remove = FALSE, sep = ""
  ) 

unique(z$dose_history_zero_to_19_vaccination_timing_period)

# and then interpret the united columns

dose_history_function <- function(df, condition) {
  col_name <- rlang::sym(paste(condition, "_vaccination_dose_information", sep = ""))
  dose_history_col <- rlang::sym(paste("dose_history_", condition, "_vaccination_timing_period", sep = ""))

  df %>%
    mutate((!!col_name) := case_when(
      (!!dose_history_col) == "0000" ~ "no dose",
      (!!dose_history_col) == "1000" ~ "only dose 1",
      (!!dose_history_col) == "0100" ~ "only dose 2",
      (!!dose_history_col) == "0010" ~ "only dose 3",
      (!!dose_history_col) == "0001" ~ "only dose 4",
      (!!dose_history_col) == "0110" ~ "dose 2 & dose 3",
      (!!dose_history_col) == "1001" ~ "dose 1 & dose 4",
      (!!dose_history_col) == "0011" ~ "dose 3 & dose 4",
      (!!dose_history_col) == "1100" ~ "dose 1 & dose 2",
      (!!dose_history_col) == "0101" ~ "dose 2 & dose 4",
      (!!dose_history_col) == "1010" ~ "dose 1 & dose 3",
      (!!dose_history_col) == "1110" ~ "dose 1 & dose 2 & dose 3",
      (!!dose_history_col) == "0111" ~ "dose 2 & dose 3 & dose 4",
      (!!dose_history_col) == "1011" ~ "dose 1 & dose 3 & dose 4",
      (!!dose_history_col) == "1101" ~ "dose 1 & dose 2 & dose 4",
      (!!dose_history_col) == "1111" ~ "dose 1 & dose 2 & dose 3 & dose 4",
      str_detect((!!dose_history_col), "NA") ~ NA_character_
    ))
}

z <- z %>%
  dose_history_function("miscarriage") %>%
  dose_history_function("ectopic") %>%
  dose_history_function("minus_6_to_zero") %>%
  dose_history_function("zero_to_19") %>%
  dose_history_function("zero_to_02") 

# create a column for if they recieved any dose vs no dose during timing period
z <-
  z %>%
  mutate(
    miscarriage_any_vaccine_dose = if_else(miscarriage_vaccination_dose_information == "no dose", "no", "yes"),
    ectopic_any_vaccine_dose = if_else(ectopic_vaccination_dose_information == "no dose", "no", "yes"),
    minus_6_to_zero_any_vaccine_dose = if_else(minus_6_to_zero_vaccination_dose_information == "no dose", "no", "yes"),
    zero_to_19_any_vaccine_dose = if_else(zero_to_19_vaccination_dose_information == "no dose", "no", "yes"),
    zero_to_02_any_vaccine_dose = if_else(zero_to_02_vaccination_dose_information == "no dose", "no", "yes")
  )

# Reference date - date of their first dose during the study vaccination period
z <-
  z %>%
  mutate(miscarriage_reference_date = case_when(
    str_extract(miscarriage_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(miscarriage_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(miscarriage_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(miscarriage_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date
  )) %>%
  mutate(ectopic_reference_date = case_when(
    str_extract(ectopic_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(ectopic_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(ectopic_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(ectopic_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date
  )) %>%
  mutate(minus_6_to_zero_reference_date = case_when(
    str_extract(minus_6_to_zero_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(minus_6_to_zero_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(minus_6_to_zero_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(minus_6_to_zero_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date
  )) %>%
  mutate(zero_to_19_reference_date = case_when(
    str_extract(zero_to_19_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(zero_to_19_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(zero_to_19_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(zero_to_19_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date
  )) %>%
  mutate(zero_to_02_reference_date = case_when(
    str_extract(zero_to_02_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(zero_to_02_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(zero_to_02_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(zero_to_02_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date
  )) %>%
  mutate(
    miscarriage_gestation_at_reference_date = floor(as.numeric(miscarriage_reference_date - est_conception_date) / 7) + 2,
    ectopic_gestation_at_reference_date = floor(as.numeric(ectopic_reference_date - est_conception_date) / 7) + 2,
    minus_6_to_zero_gestation_at_reference_date = floor(as.numeric(minus_6_to_zero_reference_date - est_conception_date) / 7) + 2,
    zero_to_19_gestation_at_reference_date = floor(as.numeric(zero_to_19_reference_date - est_conception_date) / 7) + 2,
    zero_to_02_gestation_at_reference_date = floor(as.numeric(zero_to_02_reference_date - est_conception_date) / 7) + 2
   )

# add vaccination period information:
# vaccine period starts on 8th December 2020
z <- z %>%
  mutate(miscarriage_vaccine_period_during_preg = case_when(
    miscarriage_study_pregnancy_end_date >= "2020-12-08" ~ "Pregnancy ended during vaccination period",
    miscarriage_study_pregnancy_end_date < "2020-12-08" ~ "Pregnancy ended prevaccination"
  )) %>%
  mutate(ectopic_vaccine_period_during_preg = case_when(
    ectopic_study_pregnancy_end_date >= "2020-12-08" ~ "Pregnancy ended during vaccination period",
    ectopic_study_pregnancy_end_date < "2020-12-08" ~ "Pregnancy ended prevaccination"
  )) %>%
  # add info about gestation length at start of vaccination period, and if gestation
  # was at 20 weeks at the start of the vaccination period
  mutate(
    days_gestation_at_start_of_vaccination_period = difftime(as.Date("2020-12-08"), est_conception_date, units = "days"),
    days_gestation_at_start_of_testing_period = difftime(as.Date("2020-05-18"), est_conception_date, units = "days")
  )

# number of doses and vaccine information
z <-
  z %>%
  mutate(miscarriage_vaccination_number_of_doses = str_count(miscarriage_vaccination_dose_information, "[0-9]")) %>%
  mutate(
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 mRNA Vaccine ")),
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 Vaccine "))
  ) %>%
  mutate(gestation_at_reference_date_categories = case_when(
    miscarriage_gestation_at_reference_date < 2 ~ "Pre-conception",
    miscarriage_gestation_at_reference_date >= 2 & miscarriage_gestation_at_reference_date <= 5 ~ "2-5 weeks",
    miscarriage_gestation_at_reference_date >= 6 & miscarriage_gestation_at_reference_date <= 10 ~ "6-10 weeks",
    miscarriage_gestation_at_reference_date >= 11 & miscarriage_gestation_at_reference_date <= 15 ~ "11-15 weeks",
    miscarriage_gestation_at_reference_date >= 16 & miscarriage_gestation_at_reference_date <= 20 ~ "16-20 weeks"
  )) %>%
  mutate(type_of_vaccination_at_reference_date = case_when(
    miscarriage_reference_date == dose_1_vacc_occurence_date ~ dose_1_vacc_product_name,
    miscarriage_reference_date == dose_2_vacc_occurence_date ~ dose_2_vacc_product_name,
    miscarriage_reference_date == dose_3_vacc_occurence_date ~ dose_3_vacc_product_name,
    miscarriage_reference_date == dose_4_vacc_occurence_date ~ dose_4_vacc_product_name
  )) %>%
  mutate(miscarriage_type_of_vaccination_during_period = case_when(
    miscarriage_vaccination_dose_information == "no dose" ~ "None",
    miscarriage_vaccination_dose_information == "only dose 1" ~ dose_1_vacc_product_name,
    miscarriage_vaccination_dose_information == "only dose 2" ~ dose_2_vacc_product_name,
    miscarriage_vaccination_dose_information == "only dose 3" ~ dose_3_vacc_product_name,
    miscarriage_vaccination_dose_information == "only dose 4" ~ dose_4_vacc_product_name,
    miscarriage_vaccination_dose_information == "dose 1 & dose 2" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    miscarriage_vaccination_dose_information == "dose 1 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    miscarriage_vaccination_dose_information == "dose 3 & dose 4" ~
      if_else(dose_3_vacc_product_name == dose_4_vacc_product_name, dose_3_vacc_product_name, "Mixed doses"),
    miscarriage_vaccination_dose_information == "dose 2 & dose 3" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name, dose_2_vacc_product_name, "Mixed doses"),
    miscarriage_vaccination_dose_information == "dose 1 & dose 2 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name,
        dose_1_vacc_product_name, "Mixed doses"
      ),
    miscarriage_vaccination_dose_information == "dose 1 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name & dose_1_vacc_product_name == dose_4_vacc_product_name,
        dose_1_vacc_product_name, "Mixed doses"
      ),
    miscarriage_vaccination_dose_information == "dose 2 & dose 3 & dose 4" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name & dose_2_vacc_product_name == dose_4_vacc_product_name,
        dose_2_vacc_product_name, "Mixed doses"
      ),
    miscarriage_vaccination_dose_information == "dose 1 & dose 2 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name &
        dose_1_vacc_product_name == dose_4_vacc_product_name,
      dose_2_vacc_product_name, "Mixed doses"
      ),
    TRUE ~ "Unknown"
  )) %>%
  mutate(ectopic_type_of_vaccination_during_period = miscarriage_type_of_vaccination_during_period)

# vaccination in pre-conception and risk period ------------------------

# For sensitivity analyses we want a variable to identify if women had a vaccine during the
# preconception AND during the risk period

# we already have a variable for if they had a dose during the risk period

z <-
  z %>%
  mutate(miscarriage_dose_preconception_and_during_risk_period = case_when(
    miscarriage_any_vaccine_dose == "yes" &
      (dose_1_vacc_occurence_date < est_conception_date | dose_2_vacc_occurence_date < est_conception_date |
        dose_3_vacc_occurence_date < est_conception_date | dose_4_vacc_occurence_date < est_conception_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(ectopic_dose_preconception_and_during_risk_period = case_when(
    ectopic_any_vaccine_dose == "yes" &
      (dose_1_vacc_occurence_date < est_conception_date | dose_2_vacc_occurence_date < est_conception_date |
        dose_3_vacc_occurence_date < est_conception_date | dose_4_vacc_occurence_date < est_conception_date) ~ "yes",
    TRUE ~ "no"
  ))


# vaccination checks ------------------------------------------------------

# need to make sure that vaccination happened in risk period but BEFORE pregnancy outcome
# e.g. if vaccination happened after pregnancy end date, even if still in risk period,
# doesn't count

# checking here that for anybody that we've classified as having a dose during the miscarriage
# risk period that this happens before the pregnancy end date and after the vaccine timing period starts
z %>%
  filter(miscarriage_any_vaccine_dose == "yes") %>%
  filter(!(dose_1_vacc_occurence_date < pregnancy_end_date & dose_1_vacc_occurence_date > miscarriage_vaccination_timing_period_start |
    dose_2_vacc_occurence_date < pregnancy_end_date & dose_2_vacc_occurence_date > miscarriage_vaccination_timing_period_start |
    dose_3_vacc_occurence_date < pregnancy_end_date & dose_3_vacc_occurence_date > miscarriage_vaccination_timing_period_start |
    dose_4_vacc_occurence_date < pregnancy_end_date & dose_4_vacc_occurence_date > miscarriage_vaccination_timing_period_start))

z %>%
  filter(ectopic_any_vaccine_dose == "yes") %>%
  filter(!(dose_1_vacc_occurence_date < pregnancy_end_date & dose_1_vacc_occurence_date > ectopic_vaccination_timing_period_start |
    dose_2_vacc_occurence_date < pregnancy_end_date & dose_2_vacc_occurence_date > ectopic_vaccination_timing_period_start |
    dose_3_vacc_occurence_date < pregnancy_end_date & dose_3_vacc_occurence_date > ectopic_vaccination_timing_period_start |
    dose_4_vacc_occurence_date < pregnancy_end_date & dose_4_vacc_occurence_date > ectopic_vaccination_timing_period_start))


# infection information ---------------------------------------------------

z <- z %>%
  mutate(ever_infected = case_when(
    !is.na(index_date_covid_infection_1) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(miscarriage_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(miscarriage_infected_during_risk_period_sense = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(ectopic_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(ectopic_infected_during_risk_period_sense = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  ))

# check there's no infections after pregnancy end date

z %>%
  filter(miscarriage_infected_during_risk_period == "yes") %>%
  filter(!(index_date_covid_infection_1 < pregnancy_end_date & index_date_covid_infection_1 > miscarriage_vaccination_timing_period_start |
    index_date_covid_infection_2 < pregnancy_end_date & index_date_covid_infection_2 > miscarriage_vaccination_timing_period_start |
    index_date_covid_infection_3 < pregnancy_end_date & index_date_covid_infection_3 > miscarriage_vaccination_timing_period_start |
    index_date_covid_infection_4 < pregnancy_end_date & index_date_covid_infection_4 > miscarriage_vaccination_timing_period_start))

z %>%
  filter(ectopic_infected_during_risk_period == "yes") %>%
  filter(!(index_date_covid_infection_1 < pregnancy_end_date & index_date_covid_infection_1 > ectopic_vaccination_timing_period_start |
    index_date_covid_infection_2 < pregnancy_end_date & index_date_covid_infection_2 > ectopic_vaccination_timing_period_start |
    index_date_covid_infection_3 < pregnancy_end_date & index_date_covid_infection_3 > ectopic_vaccination_timing_period_start |
    index_date_covid_infection_4 < pregnancy_end_date & index_date_covid_infection_4 > ectopic_vaccination_timing_period_start))

# gestation at covid infection during risk period
# instead of gestation at vaccination, for some of our cohorts we want the
# gestation at time of infection during the risk period only
z <-
  z %>%
  mutate(miscarriage_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(miscarriage_covid_index_date_sense = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(ectopic_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(ectopic_covid_index_date_sense = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(
    miscarriage_gestation_at_index_date = floor(as.numeric(miscarriage_covid_index_date - est_conception_date) / 7) + 2,
    ectopic_gestation_at_index_date = floor(as.numeric(ectopic_covid_index_date - est_conception_date) / 7) + 2
  )

# other variables ---------------------------------------------------------

# Create a variable to capture season of conception
z$conception_quarter <- as.numeric(substr(z$est_conception_date, 6, 7))
z$conception_quarter <- cut(z$conception_quarter, breaks = c(0, 3, 6, 9, 12), label = 1:4)

# Add variable for if pregnancy ended pre-pandemic
z <- z %>%
  mutate(
    overall_pregnancy_ending_pandemic = if_else(pregnancy_end_date >= as.Date("2020-03-01"),
      "Pandemic", "Pre-pandemic"
    ),
    miscarriage_study_pregnancy_ending_pandemic = if_else(miscarriage_study_pregnancy_end_date >= as.Date("2020-03-01"),
      "Pandemic", "Pre-pandemic"
    ),
    ectopic_study_pregnancy_ending_pandemic = if_else(ectopic_study_pregnancy_end_date >= as.Date("2020-03-01"),
      "Pandemic", "Pre-pandemic"
    )
  )

# drop unknown outcomes ---------------------------------------------------

# Drop pregnancies with unknown outcome
nrow(z %>% filter(overall_outcome == "Unknown")) / nrow(z) * 100
z <- subset(z, !(overall_outcome == "Unknown"))

# write out data ----------------------------------------------------------
z %>%
  write_rds(paste0(folder_temp_data, "pregnancy_level_record_for_making_cohort.rds"), compress = "gz")
