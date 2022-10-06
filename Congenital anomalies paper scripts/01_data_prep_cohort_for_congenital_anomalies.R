#***********************************************************************************************
# Prepare data for congenital anomaly analysis
# This file:
#  1. Conducts some basic cleaning
#  1. Looks at data availability & quality
#  2. Prepares the covariates for analysis
#  3. Merges in the congenital anomaly data
#  4. Merges in the SARS-CoV-2 test data
#***********************************************************************************************

#### Housekeeping ####

# Restarting the session as a way to hopefully stop the warnings() issue
# for unknown column
# rstudioapi::restartSession(command = "print('source file 02')")

# Restart your session manually please before running below

library(janitor)
library(expss)
library(forcats)
library(tidyverse)
library(fuzzyjoin)
library(lubridate)
library(haven)
library(labelled)
library(data.table)

setwd("x")

# read in data required for this analysis
baby.data <- readRDS("x/cops_baby_level_record_anonymised.rds")
congen.data <- readRDS("x/anomalies.rds")
tests.data_all <- readRDS("x/tests_for_cohort.rds")

folder_temp_data <- "x"
folder_results <- "x"

# Check for issues with the data files
warnings()

# Check variables available in each dataset
names(baby.data)
colnames(baby.data)[colnames(baby.data) == "bmi"] <- "bmi_2"
colnames(baby.data) <- gsub("x_", "", colnames(baby.data))
names(congen.data)

#****************************************************

#### Basic cleaning####

# remove labels from data
look_for(baby.data) %>% View()
baby.data <- remove_labels(baby.data, user_na_to_na = TRUE)

# Number of foetuses
nrow(baby.data)
# Number of pregnancies
length(unique(baby.data$pregnancy_id))

# Create variable which categorises when events occur (i.e. pandemic or pre-pandemic)
baby.data$overall_outcome_year <- format(as.Date(baby.data$pregnancy_end_date, format = "%Y-%M-%D"), "%Y")
unique(baby.data$overall_outcome_year)

baby.data$overall_outcome_year <- fct_explicit_na(baby.data$overall_outcome_year, na_level = "Ongoing")
unique(baby.data$overall_outcome_year)

baby.data$overall_conception_year <- format(as.Date(baby.data$est_conception_date, format = "%Y-%M-%D"), "%Y")
unique(baby.data$overall_conception_year)

table(baby.data$overall_outcome_year)
table(baby.data$overall_outcome_year, baby.data$outcome, exclude = NULL)

# Drop 2014 outcomes
baby.data <- subset(baby.data, overall_outcome_year != 2014)

# Drop all women missing age, as these are women who were outside of age criteria for COPS
# and should not be included
summary(baby.data$mother_age_at_conception)
unique(baby.data$mother_age_at_conception)

sum(is.na(baby.data$mother_age_at_conception))

agecheck <- baby.data[is.na(baby.data$mother_age_at_conception), ]
length(unique(agecheck$mother_eave_linkno)) # 10
length(unique(agecheck$pregnancy_id)) # 11

summary(agecheck$mother_dob) 
agecheck$age_from_dob <- (agecheck$est_conception_date - agecheck$mother_dob) / 365
table(agecheck$age_from_dob)

baby.data <- baby.data[!is.na(baby.data$mother_age_at_conception), ]

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
triplets <- baby.data %>%
  filter(total_foetus==3)

# Create factor variable with pregnancy outcomes
addmargins(table(baby.data$outcome, exclude = NULL))

baby.data$overall_outcome.f <- factor(baby.data$outcome, levels = c(
  "Termination",
  "Molar pregnancy",
  "Miscarriage",
  "Ectopic pregnancy",
  "Stillbirth",
  "Live birth",
  "Ongoing",
  "Unknown"
))
is.factor(baby.data$overall_outcome.f)
addmargins(table(baby.data$overall_outcome.f, exclude = NULL))

baby.data_fortable <- baby.data %>%
  filter(overall_outcome.f!="Ongoing") %>%
  filter(overall_outcome.f!="Unknown")
addmargins(table(baby.data_fortable$overall_outcome.f, exclude = NULL))

#****************************************************

#### Data availability & quality####

# Total number of women & pregnancies
dim(baby.data) 
length(unique(baby.data$pregnancy_id)) 
length(unique(baby.data$mother_eave_linkno)) 

# Dates of conception, birth outcomes & vaccination
summary(baby.data$est_conception_date)
summary(baby.data$pregnancy_end_date)
summary(baby.data$dose_1_vacc_occurence_date)

# Create variable which categorises when events occur
baby.data$overall_outcome_year <- format(as.Date(baby.data$pregnancy_end_date, format = "%Y-%M-%D"), "%Y")
baby.data$overall_outcome_year <- fct_explicit_na(baby.data$overall_outcome_year, na_level = "Ongoing")
baby.data$overall_conception_year <- format(as.Date(baby.data$est_conception_date, format = "%Y-%M-%D"), "%Y")
table(baby.data$overall_outcome_year)

table(baby.data$full_cohort, baby.data$overall_outcome_year)

# Look at pregnancy outcomes by year
outcomes_by_year_baby <- baby.data %>%
  tabyl(overall_outcome.f, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_year_baby, paste(folder_results, "baby_events_by_year.csv", sep = ""))

outcomes_by_year_baby_nounknowns <- baby.data_fortable %>%
  tabyl(overall_outcome.f, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_year_baby_nounknowns, paste(folder_results, "baby_events_by_year_nounknowns.csv", sep = ""))

multips_by_year_baby_nounknowns <- baby.data_fortable %>%
  tabyl(total_foetus, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(multips_by_year_baby_nounknowns, paste(folder_results, "multips_by_year_nounknowns.csv", sep = ""))

SMR2_diabetes_by_year_baby_nounknowns <- baby.data_fortable %>%
  tabyl(smr02_diabetes, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(SMR2_diabetes_by_year_baby_nounknowns, paste(folder_results, "SMR2_diabetes_year_nounknowns.csv", sep = ""))

#***********************************************************

#### Prepares the covariates####

## AGE##

summary(baby.data$mother_age_at_conception)

# Check distribution over time
age <- baby.data %>%
  group_by(overall_outcome_year, mother_age_at_conception) %>%
  summarise(count.sum = sum(count)) %>%
  ungroup()

ggplot(age, aes(x = mother_age_at_conception, y = count.sum, group = overall_outcome_year, color = overall_outcome_year)) +
  geom_line()

## BMI##

summary(baby.data$bmi)
summary(baby.data$q_bmi)
summary(baby.data$booking_bmi)

# code variable
baby.data$bmi_cat <- cut(baby.data$bmi,
  breaks = c(0, 18.4, 24.9, 29.5, 100),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese/severely obese")
)
baby.data$bmi_cat <- factor(baby.data$bmi_cat, levels = c(levels(baby.data$bmi_cat), NA), labels = c(levels(baby.data$bmi_cat), "Unknown/Missing"), exclude = NULL)
addmargins(table(baby.data$bmi_cat, exclude = NULL))

# check distribution over time
bmi_distribution <- baby.data %>%
  group_by(overall_outcome_year, bmi_cat) %>%
  summarise(count.sum = sum(count)) %>%
  ungroup()

bmi_distribution <- bmi_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100) %>%
  ungroup()

ggplot(bmi_distribution, aes(fill = bmi_cat, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "bmi_missing_time.png"))

# check distribution by pregnancy outcomes
bmi_distribution <- baby.data %>%
  group_by(overall_outcome.f, bmi_cat) %>%
  summarise(count.sum = sum(count))

bmi_distribution <- bmi_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(bmi_distribution, aes(fill = bmi_cat, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "bmi_missing_outcome.png"))

## DEPRIVATION SCORE##

addmargins(table(baby.data$simd, exclude = NULL))
baby.data$simd[is.na(baby.data$simd)] <- "Unknown/Missing"
baby.data$simd[baby.data$simd == 9] <- "Unknown/Missing"
addmargins(table(baby.data$simd, exclude = NULL))

# check distribution over time
simd_distribution <- baby.data %>%
  group_by(overall_outcome_year, simd) %>%
  summarise(count.sum = sum(count))

simd_distribution <- simd_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(simd_distribution, aes(fill = simd, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "simd_missing_time.png"))

# check distribution by pregnancy outcomes
simd_distribution <- baby.data %>%
  group_by(overall_outcome.f, simd) %>%
  summarise(count.sum = sum(count))

simd_distribution <- simd_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(simd_distribution, aes(fill = simd, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "simd_missing_outcome.png"))

## URBAN/RURAL STATUS##

addmargins(table(baby.data$urban_rural_8_description, exclude = NULL))
baby.data$urban_rural_8_description[is.na(baby.data$urban_rural_8_description)] <- "Unknown/Missing"
addmargins(table(baby.data$urban_rural_8_description, exclude = NULL))

# check distribution over time
urban_rural_description_distribution <- baby.data %>%
  group_by(overall_outcome_year, urban_rural_8_description) %>%
  summarise(count.sum = sum(count))

urban_rural_description_distribution <- urban_rural_description_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(urban_rural_description_distribution, aes(fill = urban_rural_8_description, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "urban_rural_description_missing_time.png"))

# check distribution by pregnancy outcomes
urban_rural_description_distribution <- baby.data %>%
  group_by(overall_outcome.f, urban_rural_8_description) %>%
  summarise(count.sum = sum(count))

urban_rural_description_distribution <- urban_rural_description_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(urban_rural_description_distribution, aes(fill = urban_rural_8_description, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "urban_rural_description_missing_outcome.png"))

# add additional categories for analysis
# instead of 8 categories, also have 6 levels and 2 levels we can use if needed
baby.data$urban_rural_8_description %>% unique()

baby.data <-
  baby.data %>%
  mutate(
    UR8_categories = case_when(
      urban_rural_8_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_8_description == "large urban areas" ~ "1",
      urban_rural_8_description == "other urban areas" ~ "2",
      urban_rural_8_description == "accessible small towns" ~ "3",
      urban_rural_8_description == "remote small towns" ~ "4",
      urban_rural_8_description == "very remote small towns" ~ "5",
      urban_rural_8_description == "accessible rural areas" ~ "6",
      urban_rural_8_description == "remote rural areas" ~ "7",
      urban_rural_8_description == "very remote rural areas" ~ "8"
    ),
    UR6_categories = case_when(
      urban_rural_8_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_8_description == "large urban areas" ~ "1",
      urban_rural_8_description == "other urban areas" ~ "2",
      urban_rural_8_description == "accessible small towns" ~ "3",
      urban_rural_8_description == "remote small towns" ~ "4",
      urban_rural_8_description == "very remote small towns" ~ "4",
      urban_rural_8_description == "accessible rural areas" ~ "5",
      urban_rural_8_description == "remote rural areas" ~ "6",
      urban_rural_8_description == "very remote rural areas" ~ "6"
    ),
    UR2_categories = case_when(
      urban_rural_8_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_8_description == "large urban areas" ~ "1",
      urban_rural_8_description == "other urban areas" ~ "1",
      urban_rural_8_description == "accessible small towns" ~ "1",
      urban_rural_8_description == "remote small towns" ~ "1",
      urban_rural_8_description == "very remote small towns" ~ "1",
      urban_rural_8_description == "accessible rural areas" ~ "2",
      urban_rural_8_description == "remote rural areas" ~ "2",
      urban_rural_8_description == "very remote rural areas" ~ "2"
    )
  )

## ETHNICITY##

table(baby.data$ethnicity_description, baby.data$ethnicity_code, exclude = NULL)
addmargins(table(baby.data$ethnicity_code, exclude = NULL))

baby.data <- baby.data %>%
  mutate(ethnicity_cat = case_when(
    str_starts(ethnicity_code, "1") ~ "1 White",
    str_detect(toupper(ethnicity_code), "WHITE") ~ "1 White",
    substr(toupper(ethnicity_code), 1, 2) %in% c("3F", "3G", "3H") ~ "2 South Asian",
    str_detect(toupper(ethnicity_code), "INDIAN") | str_detect(toupper(ethnicity_code), "PAKISTANI") ~ "2 South Asian",
    substr(toupper(ethnicity_code), 1, 2) %in% c("5D", "5C", "5Y", "4D", "4Y", "5X") ~ "3 Black/Caribbean/African",
    str_detect(toupper(ethnicity_code), "BLACK") ~ "3 Black/Caribbean/African",
    str_detect(toupper(ethnicity_code), "AFRICAN") ~ "3 Black/Caribbean/African",
    substr(toupper(ethnicity_code), 1, 2) %in% c("3J", "3X", "2A", "6A", "3Z", "6Z", "OT") ~ "4 Other or mixed ethnicity",
    str_detect(toupper(ethnicity_code), "MIXED") | str_detect(toupper(ethnicity_code), "OTHER") ~ "4 Other or mixed ethnicity",
    is.na(ethnicity_code) ~ "5 Unknown/missing",
    substr(toupper(ethnicity_code), 1, 2) %in% c("99", "98") ~ "5 Unknown/missing",
    toupper(ethnicity_code) == "UNKNOWN" | str_detect(toupper(ethnicity_code), "REFUSED") ~ "5 Unknown/missing"
  ))
addmargins(table(baby.data$ethnicity_cat, exclude = NULL))

baby.data$ethnicity_cat[is.na(baby.data$ethnicity_cat)] <- "5 Unknown/missing"
baby.data$ethnicity_cat <- factor(baby.data$ethnicity_cat, levels = c("1 White", "2 South Asian", "3 Black/Caribbean/African", "4 Other or mixed ethnicity", "5 Unknown/missing"))

(table(baby.data$ethnicity_cat, baby.data$ethnicity_code))
addmargins(table(baby.data$ethnicity_cat, exclude = NULL))
prop.table(table(baby.data$ethnicity_cat, exclude = NULL))

# check distribution over time
ethnicity_cat_distribution <- baby.data %>%
  group_by(overall_outcome_year, ethnicity_cat) %>%
  summarise(count.sum = sum(count))

ethnicity_cat_distribution <- ethnicity_cat_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(ethnicity_cat_distribution, aes(fill = ethnicity_cat, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "ethnicity_cat_missing_time.png"))

# check distribution by pregnancy outcomes
ethnicity_cat_distribution <- baby.data %>%
  group_by(overall_outcome.f, ethnicity_cat) %>%
  summarise(count.sum = sum(count))

ethnicity_cat_distribution <- ethnicity_cat_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(ethnicity_cat_distribution, aes(fill = ethnicity_cat, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "ethnicity_cat_missing_outcome.png"))

## DIABETES##

table(baby.data$q_diabetes_type)
addmargins(table(baby.data$diabetes, exclude = NULL))

baby.data$diabetes_cat <- dplyr::recode(baby.data$diabetes,
  "assumed_no_diabetes" = "No - assumed & confirmed",
  "confirmed_no_diabetes" = "No - assumed & confirmed",
  "diabetes_onset_unknown" = "Gestational Diabetes/onset unknown",
  "gestational_diabetes" = "Gestational Diabetes/onset unknown",
  "pre-existing_diabetes" = "Pre-existing diabetes",
  "unknown" = "Unknown"
)

table(baby.data$diabetes_cat)
baby.data$diabetes_cat <- factor(baby.data$diabetes_cat, levels = c("No - assumed & confirmed", "Gestational Diabetes/onset unknown", "Pre-existing diabetes", "Unknown"))
addmargins(table(baby.data$diabetes_cat, exclude = NULL))

# check distribution over time
diabetes.f_distribution <- baby.data %>%
  group_by(overall_outcome_year, diabetes_cat) %>%
  summarise(count.sum = sum(count))

diabetes.f_distribution <- diabetes.f_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(diabetes.f_distribution, aes(fill = diabetes_cat, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "diabetes.f_missing_time.png"))

# check distribution by pregnancy outcomes
diabetes.f_distribution <- baby.data %>%
  group_by(overall_outcome.f, diabetes_cat) %>%
  summarise(count.sum = sum(count))

diabetes.f_distribution <- diabetes.f_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(diabetes.f_distribution, aes(fill = diabetes_cat, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "diabetes.f_missing_outcome.png"))

diabetes_by_year_preg <- baby.data %>%
  tabyl(diabetes_cat, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(diabetes_by_year_preg, paste(folder_results, "diabetes_by_year.csv", sep = ""))

## SMOKING##

addmargins(table(baby.data$overall_smoking_status, exclude = NULL))
baby.data$overall_smoking_status[is.na(baby.data$overall_smoking_status)] <- "Unknown/Missing"
addmargins(table(baby.data$overall_smoking_status, exclude = NULL))

# check distribution over time

# overall smoking
overall_smoking_status_distribution <- baby.data %>%
  group_by(overall_outcome_year, overall_smoking_status) %>%
  summarise(count.sum = sum(count))

overall_smoking_status_distribution <- overall_smoking_status_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(overall_smoking_status_distribution, aes(fill = overall_smoking_status, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "overall_smoking_status_missing_time.png"))

# check distribution by pregnancy outcomes

# overall smoking
overall_smoking_status_distribution <- baby.data %>%
  group_by(overall_outcome.f, overall_smoking_status) %>%
  summarise(count.sum = sum(count))

overall_smoking_status_distribution <- overall_smoking_status_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(overall_smoking_status_distribution, aes(fill = overall_smoking_status, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "overall_smoking_status_missing_outcome.png"))

# table
overall_smoking_status_by_year_preg <- baby.data %>%
  tabyl(overall_smoking_status, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(overall_smoking_status_by_year_preg, paste(folder_results, "overall_smoking_status_by_year.csv", sep = ""))

## CLINICAL VULNERABILITY##

addmargins(table(baby.data$cv_clinical_vulnerability_category))
table(baby.data$cv_clinical_vulnerability_category, baby.data$overall_outcome_year, exclude = NULL)

# check distribution over time
cv_clinical_vulnerability_category_distribution <- baby.data %>%
  group_by(overall_outcome_year, cv_clinical_vulnerability_category) %>%
  summarise(count.sum = sum(count))

cv_clinical_vulnerability_category_distribution <- cv_clinical_vulnerability_category_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(cv_clinical_vulnerability_category_distribution, aes(fill = cv_clinical_vulnerability_category, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of foetus with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "cv_clinical_vulnerability_category_missing_time.png"))

# check distribution by pregnancy outcomes
cv_clinical_vulnerability_category_distribution <- baby.data %>%
  group_by(overall_outcome.f, cv_clinical_vulnerability_category) %>%
  summarise(count.sum = sum(count))

cv_clinical_vulnerability_category_distribution <- cv_clinical_vulnerability_category_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(cv_clinical_vulnerability_category_distribution, aes(fill = cv_clinical_vulnerability_category, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "cv_clinical_vulnerability_category_missing_outcome.png"))

cv_clinical_vulnerability_category_by_year_preg <- baby.data %>%
  tabyl(cv_clinical_vulnerability_category, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(cv_clinical_vulnerability_category_by_year_preg, paste(folder_results, "cv_clinical_vulnerability_category_by_year.csv", sep = ""))

#**************************************************************

#### ADD IN COVID TEST DATA####

names(tests.data_all)

# drop variables that won't be used in further analysis
tests.data <-
  tests.data_all %>%
  select(
    mother_eave_linkno = EAVE_LINKNO,
    covid_infection,
    index_date,
    final_symptomatic,
    final_sgene_classification,
    index_test_type
  )

head(tests.data)

# we have up to 4 index_dates (ie, 4 positive tests)
tests.data %>%
  drop_na(mother_eave_linkno) %>%
  group_by(mother_eave_linkno) %>%
  count() %>%
  group_by(n) %>%
  count()

tests.data.pivoted <-
  tests.data %>%
  drop_na(mother_eave_linkno) %>%
  pivot_wider(
    names_from = covid_infection,
    values_from = c(index_date, final_symptomatic, final_sgene_classification, index_test_type),
    names_prefix = "covid_infection_",
    values_fill = NA
  )

baby.data <-
  baby.data %>%
  left_join(tests.data.pivoted, by = "mother_eave_linkno")

# Check Covid-19 vaccination status in more detail
ls(baby.data)
# Create a variable to capture if there is vaccination in pregnancy and what doses
baby.data$dose1 <-
  ifelse(!is.na(baby.data$dose_1_vacc_occurence_date) &
    as.numeric(baby.data$dose_1_vacc_occurence_date - baby.data$est_conception_date) >= -6 * 7 &
    baby.data$dose_1_vacc_occurence_date < baby.data$pregnancy_end_date, 1, 0)

baby.data$dose2 <-
  ifelse(!is.na(baby.data$dose_2_vacc_occurence_date) &
    as.numeric(baby.data$dose_2_vacc_occurence_date - baby.data$est_conception_date) >= -6 * 7 &
    baby.data$dose_2_vacc_occurence_date < baby.data$pregnancy_end_date, 1, 0)

baby.data$dose3 <-
  ifelse(!is.na(baby.data$dose_3_vacc_occurence_date) &
    as.numeric(baby.data$dose_3_vacc_occurence_date - baby.data$est_conception_date) >= -6 * 7 &
    baby.data$dose_3_vacc_occurence_date < baby.data$pregnancy_end_date, 1, 0)

baby.data$dose4 <-
  ifelse(!is.na(baby.data$dose_4_vacc_occurence_date) &
    as.numeric(baby.data$dose_4_vacc_occurence_date - baby.data$est_conception_date) >= -6 * 7 &
    baby.data$dose_4_vacc_occurence_date < baby.data$pregnancy_end_date, 1, 0)

baby.data$dose5 <-
  ifelse(!is.na(baby.data$dose_5_vacc_occurence_date) &
           as.numeric(baby.data$dose_5_vacc_occurence_date - baby.data$est_conception_date) >= -6 * 7 &
           baby.data$dose_5_vacc_occurence_date < baby.data$pregnancy_end_date, 1, 0)

baby.data %>%
  count(dose1, dose2, dose3, dose4, dose5)

# first let's check all the unique dose_history combinations in our data
baby.data %>%
  # NAs here are ok - going off the code below we're ignoring them?
  mutate(dose_history = paste0(dose1, dose2, dose3, dose4, dose5)) %>%
  summarise(unique = unique(dose_history)) %>%
  filter(!str_detect(unique, "NA"))

table(baby.data$dose_history)

# double check that all of these are in the options below!
baby.data <-
  baby.data %>%
  # NAs here are ok
  mutate(dose_history = paste0(dose1, dose2, dose3, dose4, dose5)) %>%
  mutate(dose = case_when(
    dose_history == "00000" ~ "no dose", # 00000
    dose_history == "10000" ~ "only dose 1", # 10000
    dose_history == "01000" ~ "only dose 2", # 01000
    dose_history == "00100" ~ "only dose 3", # 00100
    dose_history == "00010" ~ "only dose 4", # 00010
    dose_history == "00001" ~ "only dose 5", # 00001
    dose_history == "11000" ~ "dose 1 & dose 2", # 11000
    dose_history == "11100" ~ "dose 1 & dose 2 & dose 3", # 11100
    dose_history == "11110" ~ "dose 1 & dose 2 & dose 3 & dose 4", # 11110
    dose_history == "11111" ~ "dose 1 & dose 2 & dose 3 & dose 4 & 5", # 11111

    dose_history == "01100" ~ "dose 2 & dose 3",
    dose_history == "01110" ~ "dose 2 & dose 3 & dose 4",
    dose_history == "10100" ~ "dose 1 & dose 3", 

    dose_history == "00110" ~ "dose 3 & dose 4", # 00110
    dose_history == "00111" ~ "dose 3 & dose 4 & dose 5", # 00111

    dose_history == "10110" ~ "dose 1 & dose 3 & dose 4", # 1011,

    str_detect(dose_history, "NA") ~ NA_character_
  ))

addmargins(table(baby.data$full_cohort, baby.data$dose, exclude = NULL))

baby.data <-
  baby.data %>%
  mutate(anydose = if_else(
    dose1 == 1 | dose2 == 1 | dose3 == 1 | dose4 == 1 | dose3==1,
    "anydose",
    "no dose"
  ))


#***********************************************************

#### Merge in congenital anomaly data####

baby.data %>% names()
congen.data %>% names()

baby.congen.joined.data <- 
baby.data %>%
  left_join(congen.data, by = "anomaly_id")

# check to see if there's any left over

unmatched <- congen.data %>% anti_join(baby.data, by = "anomaly_id") 

#92 congenital anomalies with no match in the baby data
baby.data %>% filter(anomaly_id %in% unmatched$anomaly_id)

#***********************************************************

#### Write baby-level ready file ####

baby.congen.joined.data %>% write_rds(paste0(folder_temp_data, "baby_level_record_ready.rds"), compress = "gz")
