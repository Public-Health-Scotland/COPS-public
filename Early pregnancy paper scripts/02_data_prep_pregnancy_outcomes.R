#***********************************************************************************************
# Prepare data for early pregnancy outcome analysis
# This file:
#  1. Looks at data availability & quality
#  2. Prepares the covariates for analysis
#  3. Looks at the unknown group
#***********************************************************************************************

#### Housekeeping ####
library(ggplot2)
library(dplyr)
library(janitor)
library(expss)
library(forcats)
library(readr)
library(tidyr)
library(tidyverse)

setwd("x")
preg.data <- readRDS("x")

folder_temp_data <- "x"
folder_results <- "x"

# check for issues with the data files
warnings()

#****************************************************

#### Check total numbers & outcomes in dataset ####

# Total number of women & pregnancies
dim(preg.data) 
length(unique(preg.data$pregnancy_id)) 
length(unique(preg.data$mother_eave_linkno)) 

# Dates of conception, birth outcomes & vaccination
summary(preg.data$est_conception_date)
summary(preg.data$pregnancy_end_date)
summary(preg.data$dose_1_vacc_occurence_date)

# Create variable which categorises when events occur
preg.data$overall_outcome_year <- format(as.Date(preg.data$pregnancy_end_date, format = "%Y-%M-%D"), "%Y")
preg.data$overall_outcome_year <- fct_explicit_na(preg.data$overall_outcome_year, na_level = "Ongoing")
preg.data$overall_conception_year <- format(as.Date(preg.data$est_conception_date, format = "%Y-%M-%D"), "%Y")
table(preg.data$overall_outcome_year)

table(preg.data$full_cohort, preg.data$overall_outcome_year)

# Birth outcomes
addmargins(table(preg.data$overall_outcome))

# Create factor variable with pregnancy outcomes
preg.data$overall_outcome.f <- factor(preg.data$overall_outcome, levels = c("Termination", "Molar pregnancy", "Miscarriage", "Ectopic pregnancy", "Stillbirth", "Live birth", "Ongoing", "Unknown"))
is.factor(preg.data$overall_outcome.f)
addmargins(table(preg.data$overall_outcome.f))

# calculate gestation_outcome_using_conception_date
preg.data$gestation_at_outcome_using_conception_date <- floor(as.numeric(preg.data$pregnancy_end_date - preg.data$est_conception_date) / 7) + 2
summary(preg.data$gestation_at_outcome - preg.data$gestation_at_outcome_using_conception_date)
subset(preg.data, gestation_at_outcome != gestation_at_outcome_using_conception_date) 

# Look at outcomes by gestational week

# table
table(preg.data$overall_outcome.f, preg.data$gestation_at_outcome, exclude = NULL)
z.t <- table(preg.data$overall_outcome.f, preg.data$gestation_at_outcome, exclude = NULL)
write.csv(z.t, paste0(folder_results, "outcomes_by_gestation.csv"))

# graph
preg.data$count <- 1
graph1_dta <- preg.data %>%
  group_by(gestation_at_outcome, overall_outcome.f) %>%
  summarise(count.sum = sum(count))
graph1_dta <- subset(graph1_dta, !(overall_outcome.f == "Ongoing"))
ggplot(graph1_dta, aes(gestation_at_outcome, count.sum, class = overall_outcome.f)) +
  geom_line() +
  facet_wrap(~overall_outcome.f, scales = "free_y", ncol = 1) +
  labs(x = "Gestation (week)", y = "Number of Pregnancies") +
  scale_x_continuous(breaks = seq(0, 44, 1)) +
  theme(panel.grid.minor = element_blank())
ggsave(paste0(folder_results, "outcomes_by_gestation.png"))

# Look at outcomes over time

outcomes_by_year_preg <- preg.data %>%
  tabyl(overall_outcome.f, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_year_preg, paste(folder_results, "Pregnancy_events_by_year new.csv", sep = ""))
# by estimated conception date
table(preg.data$overall_outcome, preg.data$overall_conception_year)

#**************************************************************

#### Check Covid-19 vaccination & infection status in more detail ####

# Create a variable to capture if there is vaccination in pregnancy and what doses

preg.data$dose1 <-
  ifelse(!is.na(preg.data$dose_1_vacc_occurence_date) &
    as.numeric(preg.data$dose_1_vacc_occurence_date - preg.data$est_conception_date) >= -6 * 7 &
    preg.data$dose_1_vacc_occurence_date < preg.data$pregnancy_end_date, 1, 0)

preg.data$dose2 <-
  ifelse(!is.na(preg.data$dose_2_vacc_occurence_date) &
    as.numeric(preg.data$dose_2_vacc_occurence_date - preg.data$est_conception_date) >= -6 * 7 &
    preg.data$dose_2_vacc_occurence_date < preg.data$pregnancy_end_date, 1, 0)

preg.data$dose3 <-
  ifelse(!is.na(preg.data$dose_3_vacc_occurence_date) &
    as.numeric(preg.data$dose_3_vacc_occurence_date - preg.data$est_conception_date) >= -6 * 7 &
    preg.data$dose_3_vacc_occurence_date < preg.data$pregnancy_end_date, 1, 0)

preg.data$dose4 <-
  ifelse(!is.na(preg.data$dose_4_vacc_occurence_date) &
    as.numeric(preg.data$dose_4_vacc_occurence_date - preg.data$est_conception_date) >= -6 * 7 &
    preg.data$dose_4_vacc_occurence_date < preg.data$pregnancy_end_date, 1, 0)

preg.data %>%
  count(dose1, dose2, dose3, dose4)

# first let's check all the unique dose_history combinations in our data
preg.data %>%
  # NAs here are ok - going off the code below we're ignoring them?
  mutate(dose_history = paste0(dose1, dose2, dose3, dose4)) %>%
  summarise(unique = unique(dose_history)) %>%
  filter(!str_detect(unique, "NA"))

# double check that all of these are in the options below!
preg.data <-
  preg.data %>%
  # NAs here are ok - going off the code below we're ignoring them?
  mutate(dose_history = paste0(dose1, dose2, dose3, dose4)) %>%
  mutate(dose = case_when(
    dose_history == "0000" ~ "no dose", # 0000
    dose_history == "1000" ~ "only dose 1", # 1000
    dose_history == "0100" ~ "only dose 2", # 0100
    dose_history == "0010" ~ "only dose 3",
    dose_history == "0001" ~ "only dose 4",
    dose_history == "1100" ~ "dose 1 & dose 2", # 1100
    dose_history == "1110" ~ "dose 1 & dose 2 & dose 3",
    dose_history == "1111" ~ "dose 1 & dose 2 & dose 3 & dose 4", # 1111
    dose_history == "0110" ~ "dose 2 & dose 3",
    dose_history == "0111" ~ "dose 2 & dose 3 & dose 4", # 0111
    dose_history == "0011" ~ "dose 3 & dose 4", # 0011
    dose_history == "1011" ~ "dose 1 & dose 3 & dose 4", # 1011,
    str_detect(dose_history, "NA") ~ NA_character_
  ))

addmargins(table(preg.data$full_cohort, preg.data$dose, exclude = NULL))

preg.data <-
  preg.data %>%
  mutate(anydose = if_else(
    dose1 == 1 | dose2 == 1 | dose3 == 1 | dose4 == 1,
    "anydose",
    "no dose"
  ))

addmargins(table(preg.data$full_cohort, preg.data$anydose, exclude = NULL))

# Check Covid-19 testing data
table(preg.data$mother_tested_positive_during_pregnancy, exclude = NULL)

# assume NAs did not have Covid-19
preg.data$mother_tested_positive_during_pregnancy[is.na(preg.data$mother_tested_positive_during_pregnancy)] <- 0
table(preg.data$mother_total_positive_during_pregnancy)
summary(preg.data$mother_earliest_positive_test_during_pregnancy)

#### Check matching & confounder variables & code these ####

## AGE##

summary(preg.data$mother_age_at_conception)

# Check distribution over time
age <- preg.data %>%
  group_by(overall_outcome_year, mother_age_at_conception) %>%
  summarise(count.sum = sum(count)) %>%
  ungroup()

ggplot(age, aes(x = mother_age_at_conception, y = count.sum, group = overall_outcome_year, color = overall_outcome_year)) +
  geom_line()

## BMI##
summary(preg.data$bmi)
summary(preg.data$q_bmi)
summary(preg.data$booking_bmi)

# code variable
preg.data$bmi_cat <- cut(preg.data$bmi,
  breaks = c(0, 18.4, 24.9, 29.5, 100),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese/severely obese")
)
addmargins(table(preg.data$bmi_cat, exclude = NULL))

preg.data$bmi_gp_cat <- cut(preg.data$q_bmi,
  breaks = c(0, 18.4, 24.9, 29.5, 100),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese/severely obese")
)
addmargins(table(preg.data$bmi_gp_cat, exclude = NULL))

preg.data$bmi_anc_cat <- cut(preg.data$booking_bmi,
  breaks = c(0, 18.4, 24.9, 29.5, 100),
  labels = c("Underweight", "Healthy weight", "Overweight", "Obese/severely obese")
)
addmargins(table(preg.data$bmi_anc_cat, exclude = NULL))

# check booking versus gp data
addmargins(table(preg.data$bmi_anc_cat, preg.data$bmi_gp_cat))
bmi_check_table <- preg.data %>%
  tabyl(bmi_anc_cat, bmi_gp_cat) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(bmi_check_table, paste(folder_results, "bmi_anc_gp_check.csv", sep = ""))

# check distribution over time
bmi_distribution <- preg.data %>%
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
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "bmi_missing_time.png"))

# check distribution by pregnancy outcomes
bmi_distribution <- preg.data %>%
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

addmargins(table(preg.data$simd, exclude = NULL))

# check distribution over time
simd_distribution <- preg.data %>%
  group_by(overall_outcome_year, simd) %>%
  summarise(count.sum = sum(count))

simd_distribution <- simd_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(simd_distribution, aes(fill = simd, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "simd_missing_time.png"))

# check distribution by pregnancy outcomes
simd_distribution <- preg.data %>%
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

preg.data$urban_rural_description[is.na(preg.data$urban_rural_description)] <- "Unknown/Missing"
addmargins(table(preg.data$urban_rural_description, exclude = NULL))
prop.table(table(preg.data$urban_rural_description, exclude = NULL))

# check distribution over time
urban_rural_description_distribution <- preg.data %>%
  group_by(overall_outcome_year, urban_rural_description) %>%
  summarise(count.sum = sum(count))

urban_rural_description_distribution <- urban_rural_description_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(urban_rural_description_distribution, aes(fill = urban_rural_description, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "urban_rural_description_missing_time.png"))

# check distribution by pregnancy outcomes
urban_rural_description_distribution <- preg.data %>%
  group_by(overall_outcome.f, urban_rural_description) %>%
  summarise(count.sum = sum(count))

urban_rural_description_distribution <- urban_rural_description_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(urban_rural_description_distribution, aes(fill = urban_rural_description, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "urban_rural_description_missing_outcome.png"))

# categorise for analysis
preg.data$urban_rural_cat <- dplyr::recode(preg.data$urban_rural_description,
  "accessible rural areas" = "Accessible small/rural areas",
  "accessible small towns" = "Accessible small/rural areas",
  "large urban areas" = "Urban areas",
  "other urban areas" = "Urban areas",
  "remote rural areas" = "Remote/very remote areas",
  "remote small towns" = "Remote/very remote areas",
  "very remote rural areas" = "Remote/very remote areas",
  "very remote small towns" = "Remote/very remote areas",
  "Unknown/Missing" = "Unknown/Missing"
)

# add additional categories for analysis
# instead of 8 categories, also have 6 levels and 2 levels we can use if needed
preg.data$urban_rural_description %>% unique()

preg.data <-
  preg.data %>%
  mutate(
    UR8_categories = case_when(
      urban_rural_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_description == "large urban areas" ~ "1",
      urban_rural_description == "other urban areas" ~ "2",
      urban_rural_description == "accessible small towns" ~ "3",
      urban_rural_description == "remote small towns" ~ "4",
      urban_rural_description == "very remote small towns" ~ "5",
      urban_rural_description == "accessible rural areas" ~ "6",
      urban_rural_description == "remote rural areas" ~ "7",
      urban_rural_description == "very remote rural areas" ~ "8"
    ),
    UR6_categories = case_when(
      urban_rural_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_description == "large urban areas" ~ "1",
      urban_rural_description == "other urban areas" ~ "2",
      urban_rural_description == "accessible small towns" ~ "3",
      urban_rural_description == "remote small towns" ~ "4",
      urban_rural_description == "very remote small towns" ~ "4",
      urban_rural_description == "accessible rural areas" ~ "5",
      urban_rural_description == "remote rural areas" ~ "6",
      urban_rural_description == "very remote rural areas" ~ "6"
    ),
    UR2_categories = case_when(
      urban_rural_description == "Unknown/Missing" ~ "Unknown/Missing",
      urban_rural_description == "large urban areas" ~ "1",
      urban_rural_description == "other urban areas" ~ "1",
      urban_rural_description == "accessible small towns" ~ "1",
      urban_rural_description == "remote small towns" ~ "1",
      urban_rural_description == "very remote small towns" ~ "1",
      urban_rural_description == "accessible rural areas" ~ "2",
      urban_rural_description == "remote rural areas" ~ "2",
      urban_rural_description == "very remote rural areas" ~ "2"
    )
  )

table(preg.data$urban_rural_cat, preg.data$urban_rural_description, exclude = NULL)
preg.data$urban_rural_cat <- factor(preg.data$urban_rural_cat, levels = c("Urban areas", "Accessible small/rural areas", "Remote/very remote areas", "Unknown/Missing"))
table(preg.data$urban_rural_cat)
table(preg.data$UR8_categories)
table(preg.data$UR6_categories)
table(preg.data$UR2_categories)

## ETHNICITY##

table(preg.data$ethnicity_description, preg.data$ethnicity_code, exclude = NULL)
addmargins(table(preg.data$ethnicity_code, exclude = NULL))

preg.data <- preg.data %>%
  mutate(ethnicity_cat =case_when(str_starts(ethnicity_code, "1") ~ "1 White",
                                                                str_detect( toupper(ethnicity_code), "WHITE") ~ "1 White",
                                                                substr(toupper(ethnicity_code),1,2) %in% c("3F", "3G", "3H") ~ "2 South Asian",
                                                                str_detect( toupper(ethnicity_code), "INDIAN") | str_detect( toupper(ethnicity_code), "PAKISTANI") ~ "2 South Asian",
                                                                substr(toupper(ethnicity_code),1,2) %in% c("5D", "5C", "5Y", "4D", "4Y", "5X") ~ "3 Black/Caribbean/African" ,
                                                                str_detect( toupper(ethnicity_code), "BLACK") ~ "3 Black/Caribbean/African" ,
                                                                str_detect( toupper(ethnicity_code), "AFRICAN") ~ "3 Black/Caribbean/African" ,
                                                                substr(toupper(ethnicity_code),1,2) %in% c("3J", "3X", "2A", "6A", "3Z", "6Z", "OT") ~ "4 Other or mixed ethnicity",
                                                                str_detect( toupper(ethnicity_code), "MIXED") |str_detect( toupper(ethnicity_code), "OTHER") ~ "4 Other or mixed ethnicity",
                                                                is.na(ethnicity_code) ~ "5 Unknown/missing", 
                                                                substr(toupper(ethnicity_code),1,2) %in% c("99", "98") ~ "5 Unknown/missing", 
                                                                toupper(ethnicity_code)=="UNKNOWN" | str_detect( toupper(ethnicity_code), "REFUSED") ~ "5 Unknown/missing"))
addmargins(table(preg.data$ethnicity_cat, exclude = NULL))

preg.data$ethnicity_cat[is.na(preg.data$ethnicity_cat)] <- "5 Unknown/missing"
preg.data$ethnicity_cat <- factor(preg.data$ethnicity_cat, levels = c("1 White", "2 South Asian", "3 Black/Caribbean/African", "4 Other or mixed ethnicity", "5 Unknown/missing"))

(table(preg.data$ethnicity_cat, preg.data$ethnicity_code))
addmargins(table(preg.data$ethnicity_cat, exclude = NULL))
prop.table(table(preg.data$ethnicity_cat, exclude = NULL))

# check distribution over time
ethnicity_cat_distribution <- preg.data %>%
  group_by(overall_outcome_year, ethnicity_cat) %>%
  summarise(count.sum = sum(count))

ethnicity_cat_distribution <- ethnicity_cat_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(ethnicity_cat_distribution, aes(fill = ethnicity_cat, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "ethnicity_cat_missing_time.png"))

# check distribution by pregnancy outcomes
ethnicity_cat_distribution <- preg.data %>%
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

table(preg.data$q_diabetes_type)
table(preg.data$q_diabetes_type)
table(preg.data$q_preexisting_diabetes)
table(preg.data$q_diag_diabetes_1)
table(preg.data$q_diag_diabetes_2)

table(preg.data$diabetes, exclude = NULL)

preg.data$diabetes_cat <- dplyr::recode(preg.data$diabetes,
  "assumed_no_diabetes" = "No - assumed & confirmed",
  "confirmed_no_diabetes" = "No - assumed & confirmed",
  "diabetes_onset_unknown" = "Gestational Diabetes/onset unknown",
  "gestational_diabetes" = "Gestational Diabetes/onset unknown",
  "pre-existing_diabetes" = "Pre-existing diabetes",
  "unknown" = "Unknown"
)

table(preg.data$diabetes_cat)
preg.data$diabetes_cat <- factor(preg.data$diabetes_cat, levels = c("No - assumed & confirmed", "Gestational Diabetes/onset unknown", "Pre-existing diabetes", "Unknown"))
table(preg.data$diabetes_cat, exclude = NULL)

# check distribution over time
diabetes.f_distribution <- preg.data %>%
  group_by(overall_outcome_year, diabetes_cat) %>%
  summarise(count.sum = sum(count))

diabetes.f_distribution <- diabetes.f_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(diabetes.f_distribution, aes(fill = diabetes_cat, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "diabetes.f_missing_time.png"))

# check distribution by pregnancy outcomes
diabetes.f_distribution <- preg.data %>%
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

diabetes_by_year_preg <- preg.data %>%
  tabyl(diabetes_cat, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(diabetes_by_year_preg, paste(folder_results, "diabetes_by_year.csv", sep = ""))

## SMOKING##

table(preg.data$booking_smoking_status, exclude = NULL)
table(preg.data$gp_smoking_status, exclude = NULL)
table(preg.data$overall_smoking_status, exclude = NULL)
addmargins(table(preg.data$gp_smoking_status, preg.data$booking_smoking_status, exclude = NULL))
smoke_comp <- addmargins(table(preg.data$gp_smoking_status, preg.data$booking_smoking_status))
write.csv(smoke_comp, paste0(folder_results, "smoking_comparison.csv"))

# check distribution over time

# overall smoking
overall_smoking_status_distribution <- preg.data %>%
  group_by(overall_outcome_year, overall_smoking_status) %>%
  summarise(count.sum = sum(count))

overall_smoking_status_distribution <- overall_smoking_status_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(overall_smoking_status_distribution, aes(fill = overall_smoking_status, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "overall_smoking_status_missing_time.png"))

# smoking at booking status
booking_smoking_status_distribution <- preg.data %>%
  group_by(overall_outcome_year, booking_smoking_status) %>%
  summarise(count.sum = sum(count))

booking_smoking_status_distribution <- booking_smoking_status_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(booking_smoking_status_distribution, aes(fill = booking_smoking_status, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "booking_smoking_status_missing_time.png"))

# check distribution by pregnancy outcomes

# overall smoking
overall_smoking_status_distribution <- preg.data %>%
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

# smoking at booking status
booking_smoking_status_distribution <- preg.data %>%
  group_by(overall_outcome.f, booking_smoking_status) %>%
  summarise(count.sum = sum(count))

booking_smoking_status_distribution <- booking_smoking_status_distribution %>%
  group_by(overall_outcome.f) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(booking_smoking_status_distribution, aes(fill = booking_smoking_status, y = prop_outcome, x = overall_outcome.f)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies") +
  xlab("Pregnancy outcome")
ggsave(paste0(folder_results, "booking_smoking_status_missing_outcome.png"))

# table
overall_smoking_status_by_year_preg <- preg.data %>%
  tabyl(overall_smoking_status, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(overall_smoking_status_by_year_preg, paste(folder_results, "overall_smoking_status_by_year.csv", sep = ""))

## CLINICAL VULNERABILITY##

addmargins(table(preg.data$cv_clinical_vulnerability_category))
table(preg.data$cv_clinical_vulnerability_category, preg.data$overall_outcome_year, exclude = NULL)

# check distribution over time
cv_clinical_vulnerability_category_distribution <- preg.data %>%
  group_by(overall_outcome_year, cv_clinical_vulnerability_category) %>%
  summarise(count.sum = sum(count))

cv_clinical_vulnerability_category_distribution <- cv_clinical_vulnerability_category_distribution %>%
  group_by(overall_outcome_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

ggplot(cv_clinical_vulnerability_category_distribution, aes(fill = cv_clinical_vulnerability_category, y = prop_outcome, x = overall_outcome_year)) +
  geom_bar(position = "fill", stat = "identity") +
  ylab("% of pregnancies with an outcome") +
  xlab("Year of pregnancy outcome")
ggsave(paste0(folder_results, "cv_clinical_vulnerability_category_missing_time.png"))

# check distribution by pregnancy outcomes
cv_clinical_vulnerability_category_distribution <- preg.data %>%
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

cv_clinical_vulnerability_category_by_year_preg <- preg.data %>%
  tabyl(cv_clinical_vulnerability_category, overall_outcome_year) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(cv_clinical_vulnerability_category_by_year_preg, paste(folder_results, "cv_clinical_vulnerability_category_by_year.csv", sep = ""))

#**************************************************************

##### Explore pregnancies with unknown outcomes in more detail####
unknown.outcome.data <- subset(preg.data, (overall_outcome.f == "Unknown" | overall_outcome == "Stillbirth" | overall_outcome == "Live birth"))
table(unknown.outcome.data$overall_outcome.f)

unknown.outcome.data$unknown_outcome <- ifelse(unknown.outcome.data$overall_outcome.f == "Unknown", 1, 0)
table(unknown.outcome.data$unknown_outcome)

unknown.outcome.data <- apply_labels(unknown.outcome.data,
  unknown_outcome = c("Known" = 0, "Unknown" = 1),
  full_cohort = c("Pre-pandemic" = 0, "Pandemic" = 1)
)

unknown_by_cohort <- unknown.outcome.data %>%
  tabyl(full_cohort, unknown_outcome) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

unknown.outcome.data <- subset(unknown.outcome.data, full_cohort == 1)

unknown_by_vaccination <- unknown.outcome.data %>%
  tabyl(anydose, unknown_outcome) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
tab1 <- tabyl(unknown.outcome.data, anydose, unknown_outcome)
chisq.test(tab1)

unknown_by_infection <- unknown.outcome.data %>%
  tabyl(mother_tested_positive_during_pregnancy, unknown_outcome) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
tab2 <- tabyl(unknown.outcome.data, mother_tested_positive_during_pregnancy, unknown_outcome)
chisq.test(tab2)

unknown_by_ethnicity <- unknown.outcome.data %>%
  tabyl(ethnicity_cat, unknown_outcome) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(unknown_by_ethnicity, paste(folder_results, "Unknown_outcomes_by_ethnicity.csv", sep = ""))
tab2 <- tabyl(unknown.outcome.data, ethnicity_cat, unknown_outcome)
chisq.test(tab2)

unknown_by_area <- unknown.outcome.data %>%
  tabyl(urban_rural_description, unknown_outcome) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(unknown_by_area, paste(folder_results, "Unknown_outcomes_by_area.csv", sep = ""))
tab2 <- tabyl(unknown.outcome.data, urban_rural_description, unknown_outcome)
chisq.test(tab2)

# check by age
aggregate(unknown.outcome.data$mother_age_at_conception, list(unknown.outcome.data$unknown_outcome), FUN = mean)
aggregate(unknown.outcome.data$mother_age_at_conception, list(unknown.outcome.data$unknown_outcome), FUN = sd)

#**************************************************************

#### ADD IN COVID TEST DATA####
tests.data_all <- readRDS("x")

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

# we have up to 4 index_dates 
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

preg.data.tests <-
  preg.data %>%
  left_join(tests.data.pivoted, by = "mother_eave_linkno")

#### Write pregnancy-level ready file ####
preg.data.tests %>% write_rds(paste0(folder_temp_data, "pregnancy_level_record_ready.rds"), compress = "gz")

