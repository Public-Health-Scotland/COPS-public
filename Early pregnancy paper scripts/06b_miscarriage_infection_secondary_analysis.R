#***********************************************************************************************
#Early pregnancy outcome analysis
#This file conducts the first secondary analysis for infection and miscarriage (with contemporary uninfected group - no vaccine):
#  1. Prepares data including grouping outcome variable for analysis
#  2. Looks at distribution of covariates in infected and uninfected group
#  3. Looks at distribution of pregnancy outcomes at 19+6 weeks gestation in infected and uninfected group
#  4. Conducts crude analysis of association between infection and miscarriage (only accounting for matching factors)
#  5. Conducts adjusted analysis of association between infection and miscarriage (accounting for all covariates of interest)
#***********************************************************************************************

####HOUSEKEEPING####

library(survival)
library(nnet)
library(hablar)

setwd("x")

folder_temp_data            <- "x"
folder_results              <- "x"

v.misc.inf.data <- readRDS(paste0(folder_temp_data, "matched_miscarriage_infection_cohort_three.rds"))

ls(v.misc.inf.data)

####Create variables for analysis####

#create a variable to capture infected versus uninfected
table(v.misc.inf.data$inf_or_uninf)

v.misc.inf.data <-
  v.misc.inf.data %>%
  mutate(
    inf_descrip = case_when(
      inf_or_uninf == "uninf" ~ "Uninfected contemporary cohort (N=9,198)",
      inf_or_uninf == "inf" ~ "Infected cohort (N=3,066)"
    ))
v.misc.inf.data$inf_cat <- factor(v.misc.inf.data$inf_descrip, levels=c("Uninfected contemporary cohort (N=9,198)", "Infected cohort (N=3,066)"))
v.misc.inf.data$inf_cat_graph <- factor(v.misc.inf.data$inf_descrip, levels=c("Infected cohort (N=3,066)", "Uninfected contemporary cohort (N=9,198)"))

table(v.misc.inf.data$inf_descrip)

#create a variable to capture gestational age at matching
v.misc.inf.data$miscarriage_gestation_at_index_date <- floor(v.misc.inf.data$miscarriage_gestation_at_index_date)
v.misc.inf.data <- v.misc.inf.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(miscarriage_gestation_at_index_date)) %>%
  ungroup()
addmargins(table(v.misc.inf.data$gest_at_match))

#create outcome variables
#for multinomial analysis 
#note that livebirths are categorised as miscarriages given early gestation precluding survival
#molar pregnancies also grouped with miscarriages
table(v.misc.inf.data$miscarriage_study_outcome)

v.misc.inf.data$outcomes_cat <- dplyr::recode(v.misc.inf.data$miscarriage_study_outcome, "Termination"="Termination", 
                                          "Molar pregnancy"="Miscarriage", 
                                          "Miscarriage"="Miscarriage", 
                                          "Ectopic pregnancy"="Ectopic pregnancy", 
                                          "Live birth"="Miscarriage", 
                                          "Ongoing wk 19"="Ongoing pregnancy")
v.misc.inf.data$outcomes_cat <- factor(v.misc.inf.data$outcomes_cat, levels=c("Ongoing pregnancy", "Miscarriage", "Termination", "Ectopic pregnancy"))
v.misc.inf.data$outcomes_cat_forgraph <- factor(v.misc.inf.data$outcomes_cat, levels=c("Ectopic pregnancy", "Termination", "Miscarriage", "Ongoing pregnancy"))

addmargins(table(v.misc.inf.data$outcomes_cat, v.misc.inf.data$miscarriage_study_outcome)) 

#Tidy up covariates as needed
addmargins(table(v.misc.inf.data$ethnicity_cat)) 
addmargins(table(v.misc.inf.data$cv_clinical_vulnerability_category)) 
addmargins(table(v.misc.inf.data$UR2_categories, exclude=NULL))
addmargins(table(v.misc.inf.data$bmi_cat))
addmargins(table(v.misc.inf.data$diabetes_cat))
addmargins(table(v.misc.inf.data$overall_smoking_status))
addmargins(table(v.misc.inf.data$simd))

v.misc.inf.data$simd[is.na(v.misc.inf.data$simd)] <- "Unknown/Missing"
v.misc.inf.data$simd[v.misc.inf.data$simd==9] <- "Unknown/Missing"
v.misc.inf.data$overall_smoking_status[is.na(v.misc.inf.data$overall_smoking_status)] <- "Unknown/Missing"
v.misc.inf.data$bmi_cat <- factor(v.misc.inf.data$bmi_cat, levels=c(levels(v.misc.inf.data$bmi_cat), NA), labels = c(levels(v.misc.inf.data$bmi_cat), 88), exclude=NULL)

####Descriptive for each group: key characteristics####

#age median and range
infection_age_mean <- v.misc.inf.data %>%
  group_by(inf_or_uninf) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
infection_age_mean

#Look at outcomes over time
#by pregnancy outcome year
infection_by_ethnicity <- v.misc.inf.data %>%  
  tabyl(ethnicity_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_ethnicity, paste(folder_results, "secondary1_infection_by_ethnicity.csv", sep = ''))

infection_by_urban_rural_cat <- v.misc.inf.data %>%  
  tabyl(UR6_categories, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_urban_rural_cat, paste(folder_results, "secondary1_infection_by_urban_rural_6cat.csv", sep = ''))

infection_by_simd <- v.misc.inf.data %>%  
  tabyl(simd, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_simd, paste(folder_results, "secondary1_infection_by_simd.csv", sep = ''))

infection_by_bmi_cat <- v.misc.inf.data %>%  
  tabyl(bmi_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_bmi_cat, paste(folder_results, "secondary1_infection_by_bmi_cat.csv", sep = ''))

infection_by_overall_smoking_status <- v.misc.inf.data %>%  
  tabyl(overall_smoking_status, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_overall_smoking_status, paste(folder_results, "secondary1_infection_by_overall_smoking_status.csv", sep = ''))

infection_by_cv_clinical_vulnerability_category <- v.misc.inf.data %>%  
  tabyl(cv_clinical_vulnerability_category, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_cv_clinical_vulnerability_category, paste(folder_results, "secondary1_infection_by_cv_clinical_vulnerability_category.csv", sep = ''))

infection_by_diabetes_cat <- v.misc.inf.data %>%  
  tabyl(diabetes_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(infection_by_diabetes_cat, paste(folder_results, "secondary1_infection_by_diabetes_cat.csv", sep = ''))

####Supplementary analysis 1: miscarriage in infected versus uninfected (contemporary)####

#Decription of infected and uninfected

#Look at outcomes in infected and uninfected
outcomes_by_inf_status <- v.misc.inf.data %>%  
  tabyl(outcomes_cat, inf_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_inf_status, paste(folder_results, "Pregnancy_events_by_inf_seconday1_cohort.csv", sep = ''))

#Graph of outcomes by infection status
v.misc.inf.data$count <-1
outcome_distribution <- v.misc.inf.data %>%
  group_by(inf_cat_graph, outcomes_cat_forgraph) %>%
  summarise(count.sum = sum(count))

outcome_distribution <- outcome_distribution %>%
  group_by(inf_cat_graph) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

outcome_distribution <- outcome_distribution %>%
  filter(inf_cat_graph=="Uninfected contemporary cohort (N=4,128)")

outcome_distribution %>% write_rds(paste0(folder_temp_data, "outcome_distribution_infection_miscarriagesecondary1.rds"), compress = "gz")

#Crude analysis

table(v.misc.inf.data$mother_age_at_conception)

v.misc.inf.data$mother_age_at_conception[v.misc.inf.data$mother_age_at_conception<20] <- 16
v.misc.inf.data$mother_age_at_conception[v.misc.inf.data$mother_age_at_conception>40] <- 45
addmargins(table(v.misc.inf.data$mother_age_at_conception)) 

table(v.misc.inf.data$outcomes_cat)
table(v.misc.inf.data$inf_cat)

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
model3 <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception, data=v.misc.inf.data)
summary(model3)
exp(coef(model3)) 
exp(confint(model3))
nrow(fitted(model3))

z <- summary(model3)$coefficients/summary(model3)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p

#Adjusted analysis

#multinomial regression adjusting for matching factors + ALL covariates
model4 <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.misc.inf.data)
summary(model4)
exp(coef(model4)) 
exp(confint(model4))
nrow(fitted(model4))

z2 <- summary(model4)$coefficients/summary(model4)$standard.errors
p2 <- (1-pnorm(abs(z2), 0, 1))*2
p2

#check removing those with missing SIMD or area
model_check <- v.misc.inf.data %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing")
table(model_check$simd)
table(model_check$UR6_categories)
modelx <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + ethnicity_cat + UR6_categories + simd + cv_clinical_vulnerability_category, data=model_check)
summary(modelx)
exp(coef(modelx)) 
exp(confint(modelx))




