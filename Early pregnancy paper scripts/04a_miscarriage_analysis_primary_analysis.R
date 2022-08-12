#**************************************************************************************************************************************
#Early pregnancy outcome analysis
#This file conducts the primary analysis for miscarriage (with historical unvaccinated group):
#  1. Prepares data including grouping outcome variable for analysis
#  2. Looks at distribution of covariates in vaccinated and unvaccinated group
#  3. Looks at distribution of pregnancy outcomes at 19+6 weeks gestation in vaccinated and unvaccinated group
#  4. Conducts crude analysis of association between vaccination status and miscarriage (only accounting for matching factors)
#  5. Conducts adjusted analysis of association between vaccination status and miscarriage (accounting for all covariates of interest)
#  6. Conducts subgroup analysis of vaccine type  
#**************************************************************************************************************************************

####HOUSEKEEPING####

library(survival)
library(nnet)
library(hablar)
library(dplyr)
library(expss)
library(readr)
library(janitor) 
library(ggplot2)

setwd("x")

folder_temp_data            <- "x"
folder_results              <- "x"

v.misc.data <- readRDS(paste0(folder_temp_data, "matched_miscarriage_cohort_one.rds"))

ls(v.misc.data)

####Create variables required for analysis####--------------

#create a variable to capture vaccinated versus unvaccinated
table(v.misc.data$vacc_or_unvacc)

v.misc.data <-
v.misc.data %>%
  mutate(
    vacc_descrip = case_when(
      vacc_or_unvacc == "unvacc" ~ "Unvaccinated historical cohort (N=56,340)",
      vacc_or_unvacc == "vacc" ~ "Vaccinated cohort (N=18,780)"
      ))
v.misc.data$vacc_cat <- factor(v.misc.data$vacc_descrip, levels=c("Unvaccinated historical cohort (N=56,340)", "Vaccinated cohort (N=18,780)"))
v.misc.data$vacc_cat_graph <- factor(v.misc.data$vacc_descrip, levels=c("Vaccinated cohort (N=18,780)", "Unvaccinated historical cohort (N=56,340)"))

table(v.misc.data$vacc_descrip)

#create a variable to capture gestational age at matching
table(v.misc.data$miscarriage_gestation_at_reference_date)
v.misc.data$miscarriage_gestation_at_reference_date <- floor(v.misc.data$miscarriage_gestation_at_reference_date)
v.misc.data <- v.misc.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(miscarriage_gestation_at_reference_date)) %>%
  ungroup()
addmargins(table(v.misc.data$gest_at_match))

#create outcome variables
#for multinomial analysis 
#note that livebirths are categorised as miscarriages given early gestation precluding survival
#molar pregnancies also grouped with miscarriages
table(v.misc.data$miscarriage_study_outcome)

v.misc.data$outcomes_cat <- dplyr::recode(v.misc.data$miscarriage_study_outcome, "Termination"="Termination", 
                                                                             "Molar pregnancy"="Miscarriage", 
                                                                             "Miscarriage"="Miscarriage", 
                                                                             "Ectopic pregnancy"="Ectopic pregnancy", 
                                                                             "Live birth"="Miscarriage", 
                                                                             "Ongoing wk 19"="Ongoing pregnancy")
v.misc.data$outcomes_cat <- factor(v.misc.data$outcomes_cat, levels=c("Ongoing pregnancy", "Miscarriage", "Termination", "Ectopic pregnancy"))
v.misc.data$outcomes_cat_forgraph <- factor(v.misc.data$outcomes_cat, levels=c("Ectopic pregnancy", "Termination", "Miscarriage", "Ongoing pregnancy"))

addmargins(table(v.misc.data$outcomes_cat, v.misc.data$miscarriage_study_outcome)) 

miscarriage <- v.misc.data %>%
  filter(miscarriage_study_outcome=="Miscarriage")
table(miscarriage$gestation_at_outcome)

#Tidy up covariates as needed
addmargins(table(v.misc.data$ethnicity_cat)) 
addmargins(table(v.misc.data$cv_clinical_vulnerability_category)) 
addmargins(table(v.misc.data$UR2_categories, exclude=NULL))
addmargins(table(v.misc.data$bmi_cat))
addmargins(table(v.misc.data$diabetes_cat))
addmargins(table(v.misc.data$overall_smoking_status))
addmargins(table(v.misc.data$simd))

v.misc.data$simd[is.na(v.misc.data$simd)] <- "Unknown/Missing"
v.misc.data$simd[v.misc.data$simd==9] <- "Unknown/Missing"
v.misc.data$overall_smoking_status[is.na(v.misc.data$overall_smoking_status)] <- "Unknown/Missing"
v.misc.data$bmi_cat <- factor(v.misc.data$bmi_cat, levels=c(levels(v.misc.data$bmi_cat), NA), labels = c(levels(v.misc.data$bmi_cat), 88), exclude=NULL)

# Tables for checking numbers in cohort -----------------------------------

v.misc.data %>%
  group_by(vacc_or_unvacc) %>%
  count()

v.misc.data %>%
  group_by(vacc_or_unvacc, miscarriage_study_outcome) %>%
  count()

v.misc.data %>%
  group_by(vacc_or_unvacc, miscarriage_type_of_vaccination_during_period) %>%
  count()
  
v.misc.data %>%
  group_by(vacc_or_unvacc, gestation_at_outcome) %>%
  count() %>% View()

####Descriptive for each group: key characteristics####

#age median and range
vaccination_age_mean <- v.misc.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
          age_min = min(mother_age_at_conception),
          age_max = max(mother_age_at_conception))
vaccination_age_mean

#Look at outcomes over time
#by pregnancy outcome year
vaccination_by_ethnicity <- v.misc.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "primary_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.misc.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "primary_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.misc.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "primary_vaccination_by_simd.csv", sep = ''))

vaccination_by_bmi_cat <- v.misc.data %>%  
  tabyl(bmi_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_bmi_cat, paste(folder_results, "primary_vaccination_by_bmi_cat.csv", sep = ''))

vaccination_by_overall_smoking_status <- v.misc.data %>%  
  tabyl(overall_smoking_status, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_overall_smoking_status, paste(folder_results, "primary_vaccination_by_overall_smoking_status.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "primary_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

vaccination_by_diabetes_cat <- v.misc.data %>%  
  tabyl(diabetes_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_diabetes_cat, paste(folder_results, "primary_vaccination_by_diabetes_cat.csv", sep = ''))

#look at some descriptives for vaccinated cohort
v.misc.data.vaccinated <- v.misc.data %>%
  filter(vacc_or_unvacc=="vacc") %>%
  mutate(gest_group = case_when(
    (gest_at_match<2) ~ "1. pre-conception",
    (gest_at_match>=2 & gest_at_match<6) ~ "2. 2-5 weeks",
    (gest_at_match>=6 & gest_at_match<11) ~ "3. 6-10 weeks",
    (gest_at_match>=11 & gest_at_match<16) ~ "4. 11-15 weeks",
    (gest_at_match>=16 & gest_at_match<20) ~ "5. 16-20 weeks"
  ))
addmargins(table(v.misc.data.vaccinated$gest_group))  

ls(v.misc.data.vaccinated)

addmargins(table(v.misc.data.vaccinated$miscarriage_vaccination_dose_information))
v.misc.data.vaccinated <- v.misc.data.vaccinated %>%
  mutate(vaccine_no = case_when(
    (miscarriage_vaccination_dose_information=="only dose 1") ~ "1",
    (miscarriage_vaccination_dose_information=="only dose 2") ~ "1",
    (miscarriage_vaccination_dose_information=="only dose 3") ~ "1",
    (miscarriage_vaccination_dose_information=="dose 1 & dose 2") ~ "2",
    (miscarriage_vaccination_dose_information=="dose 1 & dose 3") ~ "2",
    (miscarriage_vaccination_dose_information=="dose 2 & dose 3") ~ "2",
    (miscarriage_vaccination_dose_information=="dose 3 & dose 4") ~ "2",
    (miscarriage_vaccination_dose_information=="dose 1 & dose 2 & dose 3") ~ "2"
  ))
addmargins(table(v.misc.data.vaccinated$vaccine_no))

addmargins(table(v.misc.data.vaccinated$miscarriage_type_of_vaccination_during_period))

#graph of type of vaccine over time

v.misc.data.vaccinated$count <-1
outcome_distribution <- v.misc.data.vaccinated %>%
  group_by(miscarriage_type_of_vaccination_during_period, miscarriage_gestation_at_reference_date) %>%
  summarise(count.sum = sum(count))

vaccine_distribution <- outcome_distribution %>%
  group_by(miscarriage_type_of_vaccination_during_period) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

vaccine_distribution <- vaccine_distribution %>%
  filter(miscarriage_type_of_vaccination_during_period!="Mixed doses")

ggplot(data=vaccine_distribution, aes(x=miscarriage_gestation_at_reference_date, y=prop_outcome, group=miscarriage_type_of_vaccination_during_period)) +
  geom_line(aes(linetype=miscarriage_type_of_vaccination_during_period)) +
  labs(
    x = "Gestation at first vaccination in pregnancy risk perio",
    y = "% of pregnant woman recieving vaccination at given gestation, stratified by vaccine type") +
  guides(fill = guide_legend(title = "Vaccine")) 

table(v.misc.data.vaccinated$dose_3_vacc_product_name)

#create a graph of vaccine exposure over time

#Check dates of vaccination
summary(v.misc.data$dose_1_vacc_occurence_date)
summary(v.misc.data$dose_2_vacc_occurence_date)
summary(v.misc.data$dose_3_vacc_occurence_date)

dose_1_valid <- c("dose 1 & dose 2","dose 1 & dose 2 & dose 3", "dose 1 & dose 3", "only dose 1" )
dose_2_valid <- c( "dose 2 & dose 3", "only dose 2" )
dose_3_valid <- c("dose 3 & dose 4","only dose 3" )
dose_4_valid <- c(  "only dose 4" )

dose_1_all_exposures <- c("dose 1 & dose 2","dose 1 & dose 2 & dose 3", "dose 1 & dose 3", "only dose 1" )
dose_2_all_exposures <- c( "dose 2 & dose 3", "only dose 2", "dose 1 & dose 2","dose 1 & dose 2 & dose 3")
dose_3_all_exposures <- c("dose 3 & dose 4","only dose 3", "dose 1 & dose 3", "dose 1 & dose 2 & dose 3")
dose_4_all_exposures <- c( "dose 3 & dose 4",  "only dose 4" )

v.misc.data.vaccinated <- v.misc.data.vaccinated %>%
mutate(dose_1_vacc_product = case_when(
  dose_1_vacc_product_name == "Pfizer" ~ "BNT162b2",
  dose_1_vacc_product_name == "Moderna" ~ "mRNA-1273",
  dose_1_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
)) %>%
mutate(dose_2_vacc_product = case_when(
  dose_2_vacc_product_name == "Pfizer" ~ "BNT162b2",
  dose_2_vacc_product_name == "Moderna" ~ "mRNA-1273",
  dose_2_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  )) %>%
mutate(dose_3_vacc_product = case_when(
  dose_3_vacc_product_name == "Pfizer" ~ "BNT162b2",
  dose_3_vacc_product_name == "Moderna" ~ "mRNA-1273",
  dose_3_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  )) %>%
mutate(dose_4_vacc_product = case_when(
  dose_4_vacc_product_name == "Pfizer" ~ "BNT162b2",
  dose_4_vacc_product_name == "Moderna" ~ "mRNA-1273",
  dose_4_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  ))

vacc_time_data <- v.misc.data.vaccinated %>% 
  filter(is.na(dose_1_vacc_occurence_date) == FALSE,
         is.na(dose_1_vacc_product) == FALSE) %>% 
  filter(miscarriage_vaccination_dose_information %in% dose_1_valid) %>%
  group_by(dose_1_vacc_product, dose_1_vacc_occurence_date) %>% 
  summarise(n = n()) %>% mutate(dose = 1) %>% 
  rename(vax_name = dose_1_vacc_product, vax_date = dose_1_vacc_occurence_date) %>%  
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_2_vacc_occurence_date) == FALSE,
                     is.na(dose_2_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_2_valid) %>%
              group_by(dose_2_vacc_product, dose_2_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 2) %>% 
              rename(vax_name = dose_2_vacc_product, vax_date = dose_2_vacc_occurence_date)) %>% 
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_3_vacc_occurence_date) == FALSE,
                     is.na(dose_3_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_3_valid) %>%
              group_by(dose_3_vacc_product, dose_3_vacc_occurence_date) %>% 
              summarise(n = n()) %>%  mutate(dose = 3) %>%  
              rename(vax_name = dose_3_vacc_product, vax_date = dose_3_vacc_occurence_date)) %>% 
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_4_vacc_occurence_date) == FALSE,
                     is.na(dose_4_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_4_valid) %>%
              group_by(dose_4_vacc_product, dose_4_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 4) %>%  
              rename(vax_name = dose_4_vacc_product, vax_date = dose_4_vacc_occurence_date)) %>% 
  mutate(dose = factor(dose))# %>% 

vacc_time_data$vax_name <- factor(vacc_time_data$vax_name, levels=c("BNT162b2", "mRNA-1273", "ChAdOx1-S/nCoV-19"))

vacc_time_data %>%
  ggplot(aes(x = vax_date, y = n, fill = dose, colour = dose)) +
  geom_line() +
  facet_wrap(~vax_name,  ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "", y = "")

vacc_time_all_doses_miscarriage <- v.misc.data.vaccinated %>%
  mutate(study_outcome = case_when(gestation_at_outcome >=20 ~ "Ongoing",  
                                   overall_outcome %in% c("Live birth" ,"Molar pregnancy") & gestation_at_outcome < 20 ~ "Miscarriage", 
                                   TRUE ~ overall_outcome),
         miscarriage_vaccination_dose_information = as.factor(miscarriage_vaccination_dose_information)) %>% 
  filter(is.na(dose_1_vacc_occurence_date) == FALSE,
         is.na(dose_1_vacc_product) == FALSE) %>% 
  filter(miscarriage_vaccination_dose_information %in% dose_1_all_exposures) %>%
  group_by(dose_1_vacc_product, dose_1_vacc_occurence_date) %>% 
  summarise(n = n()) %>% mutate(dose = 1) %>% 
  rename(vax_name = dose_1_vacc_product, vax_date = dose_1_vacc_occurence_date) %>%  
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_2_vacc_occurence_date) == FALSE,
                     is.na(dose_2_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_2_all_exposures) %>%
              group_by(dose_2_vacc_product, dose_2_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 2) %>% 
              rename(vax_name = dose_2_vacc_product, vax_date = dose_2_vacc_occurence_date)) %>% 
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_3_vacc_occurence_date) == FALSE,
                     is.na(dose_3_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_3_all_exposures) %>%
              group_by(dose_3_vacc_product, dose_3_vacc_occurence_date) %>% 
              summarise(n = n()) %>%  mutate(dose = 3) %>%  
              rename(vax_name = dose_3_vacc_product, vax_date = dose_3_vacc_occurence_date)) %>% 
  bind_rows(v.misc.data.vaccinated %>% 
              filter(is.na(dose_4_vacc_occurence_date) == FALSE,
                     is.na(dose_4_vacc_product) == FALSE) %>% 
              filter(miscarriage_vaccination_dose_information %in% dose_4_all_exposures) %>%
              group_by(dose_4_vacc_product, dose_4_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 4) %>%  
              rename(vax_name = dose_4_vacc_product, vax_date = dose_4_vacc_occurence_date)) %>% 
  mutate(dose = factor(dose))# %>% 

vacc_time_all_doses_miscarriage$vax_name <- factor(vacc_time_all_doses_miscarriage$vax_name, levels=c("BNT162b2", "mRNA-1273", "ChAdOx1-S/nCoV-19"))

vacc_time_all_doses_miscarriage %>%
  ggplot(aes(x = vax_date, y = n, fill = dose, colour = dose)) +
  geom_line() +
  facet_wrap(~vax_name,  ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "", y = "")
  
####Primary analysis: miscarriage in vaccinated versus unvaccinated (pre-pandemic)####

#Look at outcomes in vaccinated and unvaccinated
outcomes_by_vacc_status <- v.misc.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_cohort.csv", sep = ''))

outcomes_by_vacc_status <- v.misc.data %>%  
  tabyl(outcomes_cat, vacc_detail) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_cohort_detail.csv", sep = ''))

#Graph of outcomes by vaccination status
v.misc.data$count <-1
outcome_distribution <- v.misc.data %>%
  group_by(vacc_cat_graph, outcomes_cat_forgraph) %>%
  summarise(count.sum = sum(count))

outcome_distribution_primary <- outcome_distribution %>%
  group_by(vacc_cat_graph) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

outcome_distribution_primary %>% write_rds(paste0(folder_temp_data, "outcome_distribution_primary.rds"), compress = "gz")

#Crude analysis

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
model3 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter, data=v.misc.data)
summary(model3)
exp(coef(model3)) 
exp(confint(model3))

z <- summary(model3)$coefficients/summary(model3)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p

#Adjusted analysis

#multinomial regression adjusting for matching factors + ALL covariates except ethnicity
model4 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.misc.data)
summary(model4)
exp(coef(model4)) 
exp(confint(model4))

z2 <- summary(model4)$coefficients/summary(model4)$standard.errors
p2 <- (1-pnorm(abs(z2), 0, 1))*2
p2

#check removing those with missing SIMD or area
model_check <- v.misc.data %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing")
table(model_check$simd)
table(model_check$UR6_categories)
modelx <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + UR6_categories + simd + cv_clinical_vulnerability_category, data=model_check)
summary(modelx)
exp(coef(modelx)) 
exp(confint(modelx))

####Subgroup analysis: vaccine type####------------------

#create a variable to capture vaccination type on all matched sets
table(v.misc.data$miscarriage_type_of_vaccination_during_period)
v.misc.data <- v.misc.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(vaccination_subgroup = first_(miscarriage_type_of_vaccination_during_period)) %>%
  ungroup()
addmargins(table(v.misc.data$vaccination_subgroup))

check <- v.misc.data[, c("index", "miscarriage_type_of_vaccination_during_period", "vaccination_subgroup" )]

------
#Pfizer
------

v.misc.pfizer.data <- v.misc.data %>%
  filter(vaccination_subgroup == "Pfizer")

#Look at outcomes in vaccinated and unvaccinated
table(v.misc.pfizer.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.pfizer.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_cohort_pfizer.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.misc.pfizer.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.misc.pfizer.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "pfizer_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.misc.pfizer.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "pfizer_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.misc.pfizer.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "pfizer_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.pfizer.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "pfizer_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter, data=v.misc.pfizer.data)
summary(models1)
exp(coef(models1)) 
exp(confint(models1))

zs1 <- summary(models1)$coefficients/summary(models1)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                    + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.misc.pfizer.data)
summary(models1a)
exp(coef(models1a)) 
exp(confint(models1a))

zs1a <- summary(models1a)$coefficients/summary(models1a)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

------
#AstraZeneca
------
v.misc.AstraZeneca.data <- v.misc.data %>%
  filter(vaccination_subgroup == "AstraZeneca")

#Look at outcomes in vaccinated and unvaccinated
table(v.misc.AstraZeneca.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.AstraZeneca.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_cohort_AstraZeneca.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.misc.AstraZeneca.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.misc.AstraZeneca.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "AstraZeneca_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.misc.AstraZeneca.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "AstraZeneca_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.misc.AstraZeneca.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "AstraZeneca_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.AstraZeneca.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "AstraZeneca_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
model1az <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter, data=v.misc.AstraZeneca.data)
summary(model1az)
exp(coef(model1az)) 
exp(confint(model1az))

zs1 <- summary(model1az)$coefficients/summary(model1az)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
model2az <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                     + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.misc.AstraZeneca.data)
summary(model2az)
exp(coef(model2az)) 
exp(confint(model2az))

zs1a <- summary(model2az)$coefficients/summary(model2az)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

------
#Moderna
------
v.misc.moderna.data <- v.misc.data %>%
  filter(vaccination_subgroup == "Moderna")

#Look at outcomes in vaccinated and unvaccinated
table(v.misc.moderna.data$vacc_cat_graph)

outcomes_by_vacc_status <- v.misc.moderna.data %>%  
  tabyl(outcomes_cat, vacc_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_vacc_status, paste(folder_results, "Pregnancy_events_by_vacc_cohort_moderna.csv", sep = ''))

#Look at covarites by exposure status

#age median and range
vaccination_age_mean <- v.misc.moderna.data %>%
  group_by(vacc_or_unvacc) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
vaccination_age_mean

vaccination_by_ethnicity <- v.misc.moderna.data %>%  
  tabyl(ethnicity_cat, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "moderna_vaccination_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.misc.moderna.data %>%  
  tabyl(UR6_categories, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "moderna_vaccination_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.misc.moderna.data %>%  
  tabyl(simd, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "moderna_vaccination_by_simd.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.misc.moderna.data %>%  
  tabyl(cv_clinical_vulnerability_category, vacc_or_unvacc) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "moderna_vaccination_by_cv_clinical_vulnerability_category.csv", sep = ''))

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
models1 <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter, data=v.misc.moderna.data)
summary(models1)
exp(coef(models1)) 
exp(confint(models1))

zs1 <- summary(models1)$coefficients/summary(models1)$standard.errors
ps1 <- (1-pnorm(abs(zs1), 0, 1))*2
ps1

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
models1a <- multinom(outcomes_cat ~ vacc_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                     + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.misc.moderna.data)
summary(models1a)
exp(coef(models1a)) 
exp(confint(models1a))

zs1a <- summary(models1a)$coefficients/summary(models1a)$standard.errors
ps1a <- (1-pnorm(abs(zs1a), 0, 1))*2
ps1a

