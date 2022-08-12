#*******************************************************************************************************************
# Prepare data for early pregnancy outcome analysis
#
# This file:
#  1. Generates cohorts to look at association between infection and ectopic pregnancy
#     We prepare four matched cohorts within this file:
#     a) infected pregnancies (no vaccine) matched with historical pregnancies (cohort 1 - primary)
#     b) infected pregnancies (no vaccine) matched with contemporary pregnancies (cohort 3 - secondary)
#     c) infected pregnancies (vaccine) matched with contemporary pregnancies (with vaccine) (cohort 4 - secondary)
#     d) Primary analysis but removing pregnancies which only had infection preconception (cohort 5 - sensitivity)
#
# For this analysis, we're matching pregnancies on first infection during risk period.
#
#*******************************************************************************************************************

#### Housekeeping####

library(tidyverse)
library(data.table)
library(tictoc)
library(here)
library(glue)


setwd("x")

folder_temp_data <- "x"
folder_results <- "x"

preg_data <- readRDS(paste0(folder_temp_data, "pregnancy_level_record_for_making_cohort.rds"))
z0 <- preg_data

# seed for sampling of cohort
set.seed(42)

# functions for creating cohorts
source(here("x/03_functions_for_cohort_creation.R"))

# Exclusion criteria ------------------------------------------------------

# For this analysis, we're matching on first infection during risk period.

# Main analysis is infected vs uninfected

# Important:
# Study period was previously 8th December 2020 for the vacc period, now starts at
# May 18th 2020 which is when widespread testing started.

# use index dates again for infection, and primary exposure is any confirmed infection from -6 to 2+6

# matching criteria are exactly the same as for vaccination - age, first date of index during risk period (as long as it happened in the period)

# 1 - End of dataset
#
# We want to make sure we have enough time for any events during gestation to
# be recorded through NHS systems. Because of this we have chosen to drop any
# gestation records where they have not reached 19w6d by the 31st January 2022.

# 2 - Pre-pandemic patients are removed
#
# Our exposed group are only from the pandemic period (unexposed are pre-pandemic). We have
# a variable - ectopic_study_pregnancy_ending_pandemic - that checks if the
# pregnancy at the follow up date for ectopic (19+6w) NOT overall pregnancy_end
# is pre-pandemic or during the pandemic.
#   e.g. if the pregnancy is at 20w after 2020-03-01 it is counted as a pandemic pregnancy
#
# 3 - Equal chance to be infected
#
# For our cohort we need everyone to have the opportunity to be identified as infected
# We therefore drop records where pregnancy had ended or reached 19+6w before the widespread
# period started. 

# Exclusion 1 - end of dataset --------------------------------------------

# drop all records haven't reached end of exposure risk period (19w6d) by 31st January 2022.
z1 <- z0 %>%
  mutate(days_gestation_at_31st_january_2022 = difftime("2022-01-31", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_31st_january_2022 >= (17 * 7) + 6)

# we lose this many because they were concieved after 31st January 2022 minus 17+6w
nrow(z0) - nrow(z1)

# z is our total pregnancies with known outcome and ongoing pregnancies:
# Jan 2015-31st January 2022
# N = 526,608

# Exclusion 2 - pre-pandemic ----------------------------------------------

# drop all records that meet 19+6w before the pandemic start period on 2020-03-01

z2.pandemic <- z1 %>% filter(ectopic_study_pregnancy_ending_pandemic == "Pandemic")
z2.prepandemic <- z1 %>% filter(ectopic_study_pregnancy_ending_pandemic == "Pre-pandemic")


# Pre-pandemic cohort -------------------------------------------------------
z2.prepandemic

# If pregnancy has reached 19w6d then they are still included.
z3.prepandemic <- z2.prepandemic %>%
  mutate(days_gestation_at_pandemic_start = difftime("2020-02-29", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_pandemic_start >= (17 * 7) + 6)

nrow(z2.prepandemic) - nrow(z3.prepandemic)

# Pandemic cohort ---------------------------------------------------------

z3.pandemic <- z2.pandemic %>%
  mutate(beyond_20_weeks_at_testing_period = case_when(
    days_gestation_at_start_of_testing_period <= ((17 * 7) + 6) ~ "less than or equal to 19+6w",
    days_gestation_at_start_of_testing_period > ((17 * 7) + 6) ~ "more than 19+6w"
  )) %>%
  filter(beyond_20_weeks_at_testing_period == "less than or equal to 19+6w")

z3.pandemic <- z3.pandemic %>%
  mutate(complete_pregs = difftime("2020-05-18", pregnancy_end_date, units = "days")) %>%
  filter(complete_pregs < 0)

summary(z3.pandemic$pregnancy_end_date)

# infected vs non-infected ------------------------------------------------

# should be our main cohorts here
z4.pandemic.noinfection <- z3.pandemic %>%
  filter(ectopic_infected_during_risk_period == "no")

z4.pandemic.infection <- z3.pandemic %>%
  filter(ectopic_infected_during_risk_period == "yes")

# vacc / no vacc + infected / no infection --------------------------------

z5.pandemic.infection.vaccinated <-
  z4.pandemic.infection %>%
  filter(ectopic_any_vaccine_dose == "yes")

z5.pandemic.infection.notvaccinated <-
  z4.pandemic.infection %>%
  filter(ectopic_any_vaccine_dose == "no")

z5.pandemic.noinfection.vaccinated <-
  z4.pandemic.noinfection %>%
  filter(ectopic_any_vaccine_dose == "yes")

z5.pandemic.noinfection.notvaccinated <-
  z4.pandemic.noinfection %>%
  filter(ectopic_any_vaccine_dose == "no")


# datafile names for cohort making ----------------------------------------

# list of all our dataframes that will be used for making the cohorts:

z3.prepandemic
z4.pandemic.infection

z5.pandemic.infection.notvaccinated
z5.pandemic.infection.vaccinated

z5.pandemic.noinfection.notvaccinated
z5.pandemic.noinfection.vaccinated

# Create matched cohort: primary with historical controls -------------------------------------------------

# Match on:
# - maternal age (+/- 1 year);
# - gestation week using vaccination date as reference date
#       (if they have more than one dose - use the date of the 1st dose as reference date)
# - season of conception (quarter)
# and obtain 3 uninfected women matched to each infected woman

z5.pandemic.infection.notvaccinated
z3.prepandemic # controls

z1_inf <-
  z5.pandemic.infection.notvaccinated %>%
  infected_mutate_function(variable = "ectopic")

z1_uninf <-
  z3.prepandemic %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  uninfected_mutate_function()

z1_inf_dt <- data.table(z1_inf) %>% setorder(conception_quarter_inf)
z1_uninf_dt <- data.table(z1_uninf) %>% setorder(conception_quarter_uninf)

z1_cohort <- cohort_matching_function_infection_with_conception_quarter(z1_inf_dt, z1_uninf_dt)

z1_cohort_ordered <- cohort_ordering_function_infection(dt = z1_cohort)

dtspl <- split(z1_cohort_ordered, z1_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

gc()

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_uninf %in% prev$pregnancy_id_uninf, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z1_cohort_to_match <- out

cohort_one <- z1_cohort_to_match %>%
  select(pregnancy_id_inf, pregnancy_id_uninf, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "inf_or_uninf",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(preg_data, by = "pregnancy_id") %>%
  unique()

cohort_one %>% write_rds(paste0(folder_temp_data, "matched_ectopic_infection_cohort_one.rds"), compress = "gz")

gc()

# Create matched cohort: secondary with contemporary controls ----------------------------------------
# excluding all women with covid-19 vaccination in exposed period
# pregnancy ended during pandemic

z5.pandemic.infection.notvaccinated
z5.pandemic.noinfection.notvaccinated

z3_uninf <- z5.pandemic.noinfection.notvaccinated %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  mutate(mother_age_at_conception = case_when(
    mother_age_at_conception < 20 ~ 16,
    mother_age_at_conception > 40 ~ 45,
    TRUE ~ mother_age_at_conception
  )) %>%
  uninfected_mutate_function()

z3_inf <- z5.pandemic.infection.notvaccinated %>%
  mutate(mother_age_at_conception = case_when(
    mother_age_at_conception < 20 ~ 16,
    mother_age_at_conception > 40 ~ 45,
    TRUE ~ mother_age_at_conception
  )) %>%
  infected_mutate_function(variable = "ectopic")

z3_inf_dt <- data.table(z3_inf) %>% setorder(conception_quarter_inf)
z3_uninf_dt <- data.table(z3_uninf) %>% setorder(conception_quarter_uninf)

z3_cohort <- cohort_matching_function_infection_no_conception_quarter(dt_first = z3_inf_dt, dt_to_join = z3_uninf_dt)

z3_cohort_ordered <- cohort_ordering_function_infection(dt = z3_cohort)

gc()

dtspl <- split(z3_cohort_ordered, z3_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

gc()

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_uninf %in% prev$pregnancy_id_uninf, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z3_cohort_to_match <- out

cohort_three <- z3_cohort_to_match %>%
  select(pregnancy_id_inf, pregnancy_id_uninf, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "inf_or_uninf",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(preg_data, by = "pregnancy_id") %>%
  unique()

cohort_three %>% write_rds(paste0(folder_temp_data, "matched_ectopic_infection_cohort_three.rds"), compress = "gz")

rm(list = c("z3_cohort", "z3_cohort_ordered", "dtspl", "out"))

gc()

