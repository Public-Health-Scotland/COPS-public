#***********************************************************************************************
# Early pregnancy outcome analysis
# This file conducts the primary analysis for miscarriage (with contemporary unvaccinated group - no SARS-CoV-2 infection):
#  1. Prepares data including grouping outcome variable for analysis
#  2. Looks at distribution of covariates in vaccinated and unvaccinated group
#  3. Looks at distribution of pregnancy outcomes at 19+6 weeks gestation in vaccinated and unvaccinated group
#  4. Conducts crude analysis of association between vaccination status and miscarriage (only accounting for matching factors)
#  5. Conducts adjusted analysis of association between vaccination status and miscarriage (accounting for all covariates of interest)
#  6. Conducts subgroup analysis of vaccine type
#***********************************************************************************************

#### HOUSEKEEPING####

library(survival)
library(nnet)
library(hablar)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(ggplot2)
library(tidyverse)
library(gtsummary)

setwd("x")

folder_temp_data <- "x"
folder_results <- "x"

v.misc.data <- readRDS(paste0(folder_temp_data, "matched_miscarriage_cohort_three.rds"))

check <- v.misc.data %>% filter(is.na(pregnancy_id))

set.seed(42)

#### Create varaibles required for analysis####

# create a variable to capture vaccinated versus unvaccinated
table(v.misc.data$vacc_or_unvacc)

v.misc.data <-
  v.misc.data %>%
  mutate(
    vacc_descrip = case_when(
      vacc_or_unvacc == "unvacc" ~ "Unvaccinated contemporary cohort (N=18,780)",
      vacc_or_unvacc == "vacc" ~ "Vaccinated cohort (N=18,780)"
    )
  )
v.misc.data$vacc_cat <- factor(v.misc.data$vacc_descrip, levels = c("Unvaccinated contemporary cohort (N=18,780)", "Vaccinated cohort (N=18,780)"))
v.misc.data$vacc_cat_graph <- factor(v.misc.data$vacc_descrip, levels = c("Vaccinated cohort (N=18,780)", "Unvaccinated contemporary cohort (N=18,780)"))

table(v.misc.data$vacc_descrip)

# create a variable to capture gestational age at matching
v.misc.data$miscarriage_gestation_at_reference_date <- floor(v.misc.data$miscarriage_gestation_at_reference_date)
v.misc.data <- v.misc.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(miscarriage_gestation_at_reference_date)) %>%
  ungroup()
addmargins(table(v.misc.data$gest_at_match))

# create outcome variables
# for multinomial analysis
# note that livebirths are categorised as miscarriages given early gestation precluding survival
# molar pregnancies also grouped with miscarriages
table(v.misc.data$miscarriage_study_outcome)

v.misc.data$outcomes_cat <- dplyr::recode(v.misc.data$miscarriage_study_outcome,
  "Termination" = "Termination",
  "Molar pregnancy" = "Miscarriage",
  "Miscarriage" = "Miscarriage",
  "Ectopic pregnancy" = "Ectopic pregnancy",
  "Live birth" = "Miscarriage",
  "Ongoing wk 19" = "Ongoing pregnancy"
)
v.misc.data$outcomes_cat <- factor(v.misc.data$outcomes_cat, levels = c("Ongoing pregnancy", "Miscarriage", "Termination", "Ectopic pregnancy"))
v.misc.data$outcomes_cat_forgraph <- factor(v.misc.data$outcomes_cat, levels = c("Ectopic pregnancy", "Termination", "Miscarriage", "Ongoing pregnancy"))

addmargins(table(v.misc.data$outcomes_cat, v.misc.data$miscarriage_study_outcome))

# Tidy up covariates as needed
addmargins(table(v.misc.data$ethnicity_cat))
addmargins(table(v.misc.data$cv_clinical_vulnerability_category))
addmargins(table(v.misc.data$UR2_categories, exclude = NULL))
addmargins(table(v.misc.data$bmi_cat))
addmargins(table(v.misc.data$diabetes_cat))
addmargins(table(v.misc.data$overall_smoking_status))
addmargins(table(v.misc.data$simd))

v.misc.data$simd[is.na(v.misc.data$simd)] <- "Unknown/Missing"
v.misc.data$simd[v.misc.data$simd == 9] <- "Unknown/Missing"
v.misc.data$overall_smoking_status[is.na(v.misc.data$overall_smoking_status)] <- "Unknown/Missing"
v.misc.data$bmi_cat <- factor(v.misc.data$bmi_cat, levels = c(levels(v.misc.data$bmi_cat), NA), labels = c(levels(v.misc.data$bmi_cat), 88), exclude = NULL)

#### Descriptive for each group: key characteristics####

# age median and range
vaccination_age_mean <- v.misc.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(
    age_median = median(mother_age_at_conception),
    age_min = min(mother_age_at_conception),
    age_max = max(mother_age_at_conception)
  )
vaccination_age_mean

# Look at outcomes over time
# by pregnancy outcome year
vaccination_by_ethnicity <- v.misc.data %>%
  tabyl(ethnicity_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "secondary1_vaccination_by_ethnicity.csv", sep = ""))

vaccination_by_urban_rural_cat <- v.misc.data %>%
  tabyl(UR6_categories, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "secondary1_vaccination_by_urban_rural_6cat.csv", sep = ""))

vaccination_by_simd <- v.misc.data %>%
  tabyl(simd, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "secondary1_vaccination_by_simd.csv", sep = ""))

vaccination_by_bmi_cat <- v.misc.data %>%
  tabyl(bmi_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_bmi_cat, paste(folder_results, "secondary1_vaccination_by_bmi_cat.csv", sep = ""))

vaccination_by_overall_smoking_status <- v.misc.data %>%
  tabyl(overall_smoking_status, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_overall_smoking_status, paste(folder_results, "secondary1_vaccination_by_overall_smoking_status.csv", sep = ""))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.data %>%
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "secondary1_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ""))

vaccination_by_diabetes_cat <- v.misc.data %>%
  tabyl(diabetes_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_diabetes_cat, paste(folder_results, "secondary1_vaccination_by_diabetes_cat.csv", sep = ""))

#### Supplemental analysis 1: miscarriage in vaccinated versus unvaccinated (contemporary)####

# Decription of vaccinated and unvaccinated

# Check dates of vaccination
summary(v.misc.data$dose_1_vacc_occurence_date)
summary(v.misc.data$dose_2_vacc_occurence_date)
summary(v.misc.data$dose_3_vacc_occurence_date)

# Look at outcomes in vaccinated and unvaccinated
outcomes_by_vacc_status <- v.misc.data %>%
  tabyl(outcomes_cat, vacc_cat_graph) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_seconday1_cohort.csv", sep = ""))

# Graph of outcomes by vaccination status
v.misc.data$count <- 1
outcome_distribution <- v.misc.data %>%
  group_by(vacc_cat_graph, outcomes_cat_forgraph) %>%
  summarise(count.sum = sum(count))

outcome_distribution <- outcome_distribution %>%
  group_by(vacc_cat_graph) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

outcome_distribution <- outcome_distribution %>%
  filter(vacc_cat_graph == "Unvaccinated contemporary cohort (N=18,780)")

outcome_distribution %>% write_rds(paste0(folder_temp_data, "outcome_distribution_secondary1.rds"), compress = "gz")

# Crude analysis

table(v.misc.data$mother_age_at_conception)

v.misc.data$mother_age_at_conception[v.misc.data$mother_age_at_conception < 20] <- 16
v.misc.data$mother_age_at_conception[v.misc.data$mother_age_at_conception > 40] <- 45
addmargins(table(v.misc.data$mother_age_at_conception))

# multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
model3 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data = v.misc.data)
summary(model3)
exp(coef(model3))
exp(confint(model3))
nrow(fitted(model3))

model3 %>% tbl_regression(exponentiate = T, estimate_fun = purrr::partial(style_ratio, digits = 3))

z <- summary(model3)$coefficients / summary(model3)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Adjusted analysis

# multinomial regression adjusting for matching factors + ALL covariates
model4 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter
  + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data = v.misc.data)

model4 %>%
  tbl_regression(exponentiate = T)

mis_model3 <- model3
mis_model4 <- model4

summary(model4)
exp(coef(model4))
exp(confint(model4))
nrow(fitted(model4))

z2 <- summary(model4)$coefficients / summary(model4)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

# check removing those with missing SIMD or area
model_check <- v.misc.data %>%
  filter(simd != "Unknown/Missing" & UR6_categories != "Unknown/Missing")
table(model_check$simd)
table(model_check$UR6_categories)
modelx <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter
  + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data = model_check)
summary(modelx)
exp(coef(modelx))
exp(confint(modelx))

#### Subgroup analysis: vaccine type####------------------

# create a variable to capture vaccination type on all matched sets
table(v.misc.data$miscarriage_type_of_vaccination_during_period)

v.misc.data <- v.misc.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(vaccination_subgroup = first_(miscarriage_type_of_vaccination_during_period)) %>%
  ungroup()

addmargins(table(v.misc.data$vaccination_subgroup))

# check <- v.misc.data[, c("index", "miscarriage_type_of_vaccination_during_period", "vaccination_subgroup" )]

# add a table with full breakdowns of numbers per variable in model
(v.misc.data <-
  v.misc.data %>%
  mutate(cohort_type = case_when(
    miscarriage_any_vaccine_dose == "yes" ~ "vaccinated_group",
    miscarriage_any_vaccine_dose == "no" ~ "unvaccinated_contemporary_controls"
  )))

(contemporary_top_table <-
  v.misc.data %>%
  group_by(vaccination_subgroup, cohort_type) %>%
  summarise(n = n()))

(contemporary_simd <- 
v.misc.data %>%
  group_by(vaccination_subgroup, cohort_type, simd) %>%
  summarise(n = n()) %>%
    mutate(variable_level = simd,
           variable = "simd") %>%
    select(-simd))

(contemporary_ethnicity <-
  v.misc.data %>%
  group_by(vaccination_subgroup, cohort_type, ethnicity_cat) %>%
  summarise(n = n()) %>%
    mutate(variable_level = ethnicity_cat,
           variable = "ethnicity") %>%
    select(-ethnicity_cat))

(contemporary_urban_rural <-
  v.misc.data %>%
  group_by(vaccination_subgroup, cohort_type, UR8_categories) %>%
  summarise(n = n()) %>%
    mutate(variable_level = UR8_categories,
           variable = "UR8") %>%
    select(-UR8_categories))

(contemporary_vuln <-
    v.misc.data %>%
    group_by(vaccination_subgroup, cohort_type, cv_clinical_vulnerability_category) %>%
    summarise(n = n()) %>%
    mutate(variable_level = cv_clinical_vulnerability_category,
           variable = "clinical vulns") %>%
      select(-cv_clinical_vulnerability_category))

contemp_basic_table <- 
rbind(contemporary_top_table,
      contemporary_simd,
      contemporary_urban_rural,
      contemporary_vuln
      ) 

contemp_basic_table %>%
  filter(!str_detect(vaccination_subgroup, "Mixed")) %>%
  pivot_wider(names_from = c(vaccination_subgroup, cohort_type), values_from = n) %>%
  view()

write_csv(contemp_basic_table, paste(folder_results, "contemporary_cohort_analysis_basic_table.csv", sep = ""))

#--
# Pfizer
#--

v.misc.pfizer.data <- filter(v.misc.data, vaccination_subgroup == "Pfizer")

# Look at outcomes in vaccinated and unvaccinated
table(v.misc.pfizer.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.pfizer.data %>%
  tabyl(outcomes_cat, vacc_cat_graph) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

outcomes_by_vacc_status
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_pfizer.csv", sep = ""))

# Look at covarites by exposure status

# age median and range
vaccination_age_mean <- v.misc.pfizer.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(
    age_median = median(mother_age_at_conception),
    age_min = min(mother_age_at_conception),
    age_max = max(mother_age_at_conception)
  )
vaccination_age_mean

vaccination_by_ethnicity <- v.misc.pfizer.data %>%
  tabyl(ethnicity_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_pfizer_vaccination_by_ethnicity.csv", sep = ""))

vaccination_by_urban_rural_cat <- v.misc.pfizer.data %>%
  tabyl(UR6_categories, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_pfizer_vaccination_by_urban_rural_6cat.csv", sep = ""))

vaccination_by_simd <- v.misc.pfizer.data %>%
  tabyl(simd, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_pfizer_vaccination_by_simd.csv", sep = ""))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.pfizer.data %>%
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_pfizer_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ""))

# multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data = v.misc.pfizer.data)
summary(models1)
exp(coef(models1))
tidy(models1, exponentiate = TRUE)
exp(confint(models1))

zs1 <- summary(models1)$coefficients / summary(models1)$standard.errors
ps1 <- (1 - pnorm(abs(zs1), 0, 1)) * 2
ps1

# multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter
  + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data = v.misc.pfizer.data)
summary(models1a)
exp(coef(models1a))
exp(confint(models1a))

zs1a <- summary(models1a)$coefficients / summary(models1a)$standard.errors
ps1a <- (1 - pnorm(abs(zs1a), 0, 1)) * 2
ps1a

mis_pfizer_mod1 <- models1
mis_pfizer_mod2 <- models1a
mis_pfizer_p1 <- ps1
mis_pfizer_p2 <- ps1a

mis_pfizer_mod1 %>% tbl_regression(exponentiate = T)
mis_pfizer_mod2 %>% tbl_regression(exponentiate = T)

models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

#--
# AstraZeneca
#--
v.misc.AstraZeneca.data <- v.misc.data %>%
  filter(vaccination_subgroup == "AstraZeneca")

# Look at outcomes in vaccinated and unvaccinated
table(v.misc.AstraZeneca.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.AstraZeneca.data %>%
  tabyl(outcomes_cat, vacc_cat_graph) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_AstraZeneca.csv", sep = ""))

# Look at covarites by exposure status

# age median and range
vaccination_age_mean <- v.misc.AstraZeneca.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(
    age_median = median(mother_age_at_conception),
    age_min = min(mother_age_at_conception),
    age_max = max(mother_age_at_conception)
  )
vaccination_age_mean

vaccination_by_ethnicity <- v.misc.AstraZeneca.data %>%
  tabyl(ethnicity_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_ethnicity.csv", sep = ""))

vaccination_by_urban_rural_cat <- v.misc.AstraZeneca.data %>%
  tabyl(UR6_categories, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_urban_rural_6cat.csv", sep = ""))

vaccination_by_simd <- v.misc.AstraZeneca.data %>%
  tabyl(simd, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_simd.csv", sep = ""))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.AstraZeneca.data %>%
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ""))

# multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data = v.misc.AstraZeneca.data)
summary(models1)
exp(coef(models1))
exp(confint(models1))

zs1 <- summary(models1)$coefficients / summary(models1)$standard.errors
ps1 <- (1 - pnorm(abs(zs1), 0, 1)) * 2
ps1

# multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter
  + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data = v.misc.AstraZeneca.data)
summary(models1a)
exp(coef(models1a))
exp(confint(models1a))

models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

zs1a <- summary(models1a)$coefficients / summary(models1a)$standard.errors
ps1a <- (1 - pnorm(abs(zs1a), 0, 1)) * 2
ps1a

mis_az_mod1 <- models1
mis_az_mod2 <- models1a
mis_az_p1 <- ps1
mis_az_p2 <- ps1a

mis_az_mod1 %>% tbl_regression(exponentiate = T)
mis_az_mod2 %>% tbl_regression(exponentiate = T)

#--
# Moderna
#--

v.misc.moderna.data <- v.misc.data %>%
  filter(vaccination_subgroup == "Moderna")

# Look at outcomes in vaccinated and unvaccinated
table(v.misc.moderna.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.moderna.data %>%
  tabyl(outcomes_cat, vacc_cat_graph) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_moderna.csv", sep = ""))

# Look at covarites by exposure status

# age median and range
vaccination_age_mean2 <- v.misc.moderna.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(
    age_median = median(mother_age_at_conception),
    age_min = min(mother_age_at_conception),
    age_max = max(mother_age_at_conception)
  )
vaccination_age_mean2

vaccination_by_ethnicity <- v.misc.moderna.data %>%
  tabyl(ethnicity_cat, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_moderna_vaccination_by_ethnicity.csv", sep = ""))

vaccination_by_urban_rural_cat <- v.misc.moderna.data %>%
  tabyl(UR6_categories, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_moderna_vaccination_by_urban_rural_6cat.csv", sep = ""))

vaccination_by_simd <- v.misc.moderna.data %>%
  tabyl(simd, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_moderna_vaccination_by_simd.csv", sep = ""))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.moderna.data %>%
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>%
  adorn_totals(where = "col") %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_moderna_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ""))

# multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data = v.misc.moderna.data)
summary(models1)
exp(coef(models1))
exp(confint(models1))

zs1 <- summary(models1)$coefficients / summary(models1)$standard.errors
ps1 <- (1 - pnorm(abs(zs1), 0, 1)) * 2
ps1

# multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter
  + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data = v.misc.moderna.data)
summary(models1a)
exp(coef(models1a))
exp(confint(models1a))
models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

zs1a <- summary(models1a)$coefficients / summary(models1a)$standard.errors
ps1a <- (1 - pnorm(abs(zs1a), 0, 1)) * 2
ps1a

mis_moderna_mod1 <- models1
mis_moderna_mod2 <- models1a
mis_moderna_p1 <- ps1
mis_moderna_p2 <- ps1a

mis_moderna_mod1 %>% tbl_regression(exponentiate = T)
mis_moderna_mod2 %>% tbl_regression(exponentiate = T)


# Model tidying for table -------------------------------------------------

tidy(mis_moderna_mod1) %>%
  filter(y.level == "Miscarriage") %>%
  view()

miscarriage_table <- rbind(
  tidy(mis_model3, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "total_unadjusted", .before = "y.level"),
  tidy(mis_model4, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "total_adjusted", .before = "y.level"),
  tidy(mis_pfizer_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "pfizer_unadjusted", .before = "y.level"),
  tidy(mis_pfizer_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "pfizer_adjusted", .before = "y.level"),
  tidy(mis_az_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "az_unadjusted", .before = "y.level"),
  tidy(mis_az_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "az_adjusted", .before = "y.level"),
  tidy(mis_moderna_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "moderna_unadjusted", .before = "y.level"),
  tidy(mis_moderna_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Miscarriage") %>% filter(str_detect(term, "vacc_cat")) %>%
    select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
    mutate(type = "moderna_adjusted", .before = "y.level")
)

exp(coef(mis_moderna_mod2))
miscarriage_table %>% gt()
