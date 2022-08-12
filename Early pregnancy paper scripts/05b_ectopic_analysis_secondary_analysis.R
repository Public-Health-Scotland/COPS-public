#***********************************************************************************************
#Early pregnancy outcome analysis
#This file conducts the first supplementary analysis for ectopic pregnancies (with contemporary unvaccinated group - no SARS-CoV-2 infection):
#  1. Prepares data including grouping outcome variable for analysis
#  2. Looks at distribution of covariates in vaccinated and unvaccinated group
#  3. Looks at distribution of pregnancy outcomes at 19+6 weeks gestation in vaccinated and unvaccinated group
#  4. Conducts crude analysis of association between vaccination status and ectopic (only accounting for matching factors)
#  5. Conducts adjusted analysis of association between vaccination status and ectopic (accounting for all covariates of interest)
#  6. Conducts subgroup analysis of vaccine type
#***********************************************************************************************

####HOUSEKEEPING####

library(survival)
library(nnet)
library(hablar)
library(tidyverse)
library(gtsummary)
library(gt)

setwd("x")

folder_temp_data            <- "x"
folder_results              <- "x"

v.ep.data <- readRDS(paste0(folder_temp_data, "matched_ectopic_cohort_three.rds"))

####CREATE VARIABLES REQUIRED FOR ANALYSIS####

#create a variable to capture vaccinated versus unvaccinated
table(v.ep.data$vacc_or_unvacc)

v.ep.data <-
  v.ep.data %>%
  mutate(
    vacc_descrip = case_when(
      vacc_or_unvacc == "unvacc" ~ "Unvaccinated contemporary cohort (N=31,710)",
      vacc_or_unvacc == "vacc" ~ "Vaccinated cohort (N=10,570)"
    ))
v.ep.data$vacc_cat <- factor(v.ep.data$vacc_descrip, levels=c("Unvaccinated contemporary cohort (N=31,710)", "Vaccinated cohort (N=10,570)"))
v.ep.data$vacc_cat_graph <- factor(v.ep.data$vacc_descrip, levels=c("Vaccinated cohort (N=10,570)", "Unvaccinated contemporary cohort (N=31,710)"))

table(v.ep.data$vacc_descrip)

#create a variable to capture gestational age at matching
v.ep.data$ectopic_gestation_at_reference_date <- floor(v.ep.data$ectopic_gestation_at_reference_date)
v.ep.data <- v.ep.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(ectopic_gestation_at_reference_date)) %>%
  ungroup()
addmargins(table(v.ep.data$gest_at_match))

#create outcome variables
#for multinomial analysis 
#note that livebirths are categorised as miscarriages given early gestation precluding survival
#molar pregnancies also grouped with miscarriages
table(v.ep.data$ectopic_study_outcome)

v.ep.data$outcomes_cat <- dplyr::recode(v.ep.data$ectopic_study_outcome, "Termination"="Termination", 
                                          "Molar pregnancy"="Miscarriage", 
                                          "Miscarriage"="Miscarriage", 
                                          "Ectopic pregnancy"="Ectopic pregnancy", 
                                          "Live birth"="Miscarriage", 
                                          "Ongoing wk 19"="Ongoing pregnancy")
v.ep.data$outcomes_cat <- factor(v.ep.data$outcomes_cat, levels=c("Ongoing pregnancy", "Miscarriage", "Termination", "Ectopic pregnancy"))
v.ep.data$outcomes_cat_forgraph <- factor(v.ep.data$outcomes_cat, levels=c("Ectopic pregnancy", "Termination", "Miscarriage", "Ongoing pregnancy"))

addmargins(table(v.ep.data$outcomes_cat, v.ep.data$ectopic_study_outcome)) 

#Tidy up covariates as needed
addmargins(table(v.ep.data$ethnicity_cat)) 
addmargins(table(v.ep.data$cv_clinical_vulnerability_category)) 
addmargins(table(v.ep.data$UR2_categories, exclude=NULL))
addmargins(table(v.ep.data$bmi_cat))
addmargins(table(v.ep.data$diabetes_cat))
addmargins(table(v.ep.data$overall_smoking_status))
addmargins(table(v.ep.data$simd))

v.ep.data$simd[is.na(v.ep.data$simd)] <- "Unknown/Missing"
v.ep.data$simd[v.ep.data$simd==9] <- "Unknown/Missing"
v.ep.data$overall_smoking_status[is.na(v.ep.data$overall_smoking_status)] <- "Unknown/Missing"
v.ep.data$bmi_cat <- factor(v.ep.data$bmi_cat, levels=c(levels(v.ep.data$bmi_cat), NA), labels = c(levels(v.ep.data$bmi_cat), 88), exclude=NULL)

####Descriptive for each group: key characteristics####

#age median and range
vaccination_age_mean <- v.ep.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

#Look at outcomes over time
#by pregnancy outcome year
vaccination_by_ethnicity <- v.ep.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "secondary1_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.ep.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "secondary1_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.ep.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "secondary1_vaccination_by_simd.csv", sep = ''))

vaccination_by_bmi_cat <- v.ep.data %>%  
  tabyl(bmi_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_bmi_cat, paste(folder_results, "secondary1_vaccination_by_bmi_cat.csv", sep = ''))

vaccination_by_overall_smoking_status <- v.ep.data %>%  
  tabyl(overall_smoking_status, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_overall_smoking_status, paste(folder_results, "secondary1_vaccination_by_overall_smoking_status.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.ep.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "secondary1_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

vaccination_by_diabetes_cat <- v.ep.data %>%  
  tabyl(diabetes_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_diabetes_cat, paste(folder_results, "secondary1_vaccination_by_diabetes_cat.csv", sep = ''))

####Supplementary analysis 1: ectopic in vaccinated versus unvaccinated (contemporary)####

#Decription of vaccinated and unvaccinated

#Check dates of vaccination
summary(v.ep.data$dose_1_vacc_occurence_date)
summary(v.ep.data$dose_2_vacc_occurence_date)
summary(v.ep.data$dose_3_vacc_occurence_date)

#Look at outcomes in vaccinated and unvaccinated
outcomes_by_vacc_status <- v.ep.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_seconday1_cohort.csv", sep = ''))

#Graph of outcomes by vaccination status
v.ep.data$count <-1
outcome_distribution <- v.ep.data %>%
  group_by(vacc_cat_graph, outcomes_cat_forgraph) %>%
  summarise(count.sum = sum(count))

outcome_distribution <- outcome_distribution %>%
  group_by(vacc_cat_graph) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

outcome_distribution <- outcome_distribution %>%
  filter(vacc_cat_graph=="Unvaccinated contemporary cohort (N=31,710)")

outcome_distribution %>% write_rds(paste0(folder_temp_data, "outcome_distribution_secondary1.rds"), compress = "gz")

#Crude analysis

table(v.ep.data$mother_age_at_conception)

v.ep.data$mother_age_at_conception[v.ep.data$mother_age_at_conception<20] <- 16
v.ep.data$mother_age_at_conception[v.ep.data$mother_age_at_conception>40] <- 45
addmargins(table(v.ep.data$mother_age_at_conception)) 

#multinomial regression adjusting for matching factors - baseline all non ectopic outcomes
model3 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data=v.ep.data)
summary(model3)
exp(coef(model3)) 
exp(confint(model3))
nrow(fitted(model3))

z <- summary(model3)$coefficients/summary(model3)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p

#Adjusted analysis

#multinomial regression adjusting for matching factors + ALL covariates
model4 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.ep.data)
summary(model4)
exp(coef(model4)) 
exp(confint(model4))
nrow(fitted(model4))

z2 <- summary(model4)$coefficients/summary(model4)$standard.errors
p2 <- (1-pnorm(abs(z2), 0, 1))*2
p2
ect_model3 <- model3
ect_model4 <- model4

ect_model3 %>% tbl_regression(exponentiate = T)
ect_model4 %>% tbl_regression(exponentiate = T)

#check removing those with missing SIMD or area
model_check2 <- v.ep.data %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing")
table(model_check2$simd)
table(model_check2$UR6_categories)
modelxx <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                    + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=model_check2)
summary(modelxx)
exp(coef(modelxx)) 
exp(confint(modelxx))

####Subgroup analysis: vaccine type####------------------

#create a variable to capture vaccination type on all matched sets
table(v.ep.data$ectopic_type_of_vaccination_during_period)
v.ep.data <- v.ep.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(vaccination_subgroup = first_(ectopic_type_of_vaccination_during_period)) %>%
  ungroup()
addmargins(table(v.ep.data$vaccination_subgroup))

#--
#Pfizer
#--

v.ep.pfizer.data <- filter(v.ep.data, vaccination_subgroup == "Pfizer")

#Look at outcomes in vaccinated and unvaccinated
table(v.ep.pfizer.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.ep.pfizer.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_pfizer.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.ep.pfizer.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.ep.pfizer.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_pfizer_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.ep.pfizer.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_pfizer_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.ep.pfizer.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_pfizer_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.ep.pfizer.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_pfizer_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non ectopic outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data=v.ep.pfizer.data)
summary(models1)
exp(coef(models1)) 
exp(confint(models1))

zs1 <- summary(models1)$coefficients/summary(models1)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                     + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.ep.pfizer.data)

models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

summary(models1a)
exp(coef(models1a)) 
exp(confint(models1a))

zs1a <- summary(models1a)$coefficients/summary(models1a)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

ect_pfizer_mod1 <- models1
ect_pfizer_mod2 <- models1a
ect_pfizer_p1 <- ps1
ect_pfizer_p2 <- ps1a

ect_pfizer_mod1 %>% summary()
ect_pfizer_mod1 %>% tbl_regression(exponentiate = T)
ect_pfizer_mod2 %>% tbl_regression(exponentiate = T)

#--
#AstraZeneca
#--
v.ep.AstraZeneca.data <- v.ep.data %>%
  filter(vaccination_subgroup == "AstraZeneca")

#Look at outcomes in vaccinated and unvaccinated
table(v.ep.AstraZeneca.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.ep.AstraZeneca.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_AstraZeneca.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.ep.AstraZeneca.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.ep.AstraZeneca.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.ep.AstraZeneca.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.ep.AstraZeneca.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.ep.AstraZeneca.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_AstraZeneca_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non ectopic outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data=v.ep.AstraZeneca.data)
summary(models1)
exp(coef(models1)) 
exp(confint(models1))

zs1 <- summary(models1)$coefficients/summary(models1)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                     + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.ep.AstraZeneca.data)
models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

summary(models1a)
exp(coef(models1a)) 
exp(confint(models1a))

zs1a <- summary(models1a)$coefficients/summary(models1a)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

ect_az_mod1 <- models1
ect_az_mod2 <- models1a
ect_az_p1 <- ps1
ect_az_p2 <- ps1a

ect_az_mod1 %>% tbl_regression(exponentiate = T)
ect_az_mod2 %>% tbl_regression(exponentiate = T)

#--
#Moderna
#--

v.ep.moderna.data <- v.ep.data %>%
  filter(vaccination_subgroup == "Moderna")

#Look at outcomes in vaccinated and unvaccinated
table(v.ep.moderna.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.ep.moderna.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Secondary_pregnancy_events_by_vacc_cohort_moderna.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.ep.moderna.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.ep.moderna.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "Secondary_moderna_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.ep.moderna.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "Secondary_moderna_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.ep.moderna.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "Secondary_moderna_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.ep.moderna.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "Secondary_moderna_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non ectopic outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception, data=v.ep.moderna.data)
summary(models1)
exp(coef(models1)) 
exp(confint(models1))

zs1 <- summary(models1)$coefficients/summary(models1)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                     + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.ep.moderna.data)
models1a %>% tbl_regression(include = "vacc_cat", exponentiate = TRUE) %>% add_n(location = "level")

summary(models1a)
exp(coef(models1a)) 
  exp(confint(models1a))

zs1a <- summary(models1a)$coefficients/summary(models1a)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

ect_moderna_mod1 <- models1
ect_moderna_mod2 <- models1a
ect_moderna_p1 <- ps1
ect_moderna_p2 <- ps1a

ect_moderna_mod1 %>% tbl_regression(exponentiate = T)
ect_moderna_mod2 %>% tbl_regression(exponentiate = T)

# tidy subgroups ----------------------------------------------------------

tidy(ect_moderna_mod1) %>% filter(y.level == "Ectopic pregnancy")

ectopic_table <- 
rbind(
tidy(ect_model3, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>% 
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "total_unadjusted", .before = "y.level"),
tidy(ect_model4, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "total_adjusted", .before = "y.level"),

tidy(ect_pfizer_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "pfizer_unadjusted", .before = "y.level"),
tidy(ect_pfizer_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "pfizer_adjusted", .before = "y.level"),

tidy(ect_az_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "az_unadjusted", .before = "y.level"),
tidy(ect_az_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "az_adjusted", .before = "y.level"),

tidy(ect_moderna_mod1, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "moderna_unadjusted", .before = "y.level"),
tidy(ect_moderna_mod2, conf.int = TRUE, exponentiate = TRUE) %>% filter(y.level == "Ectopic pregnancy") %>% filter(str_detect(term, "vacc_cat")) %>%
  select(y.level, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(type = "moderna_adjusted", .before = "y.level")
)

ectopic_table %>% gt()

exp(coef(ect_pfizer_mod2))

