#**************************************************************************************************************************************
#Early pregnancy outcome analysis
#This file conducts the primary and sensitivity analysis for infection and ectopic pregnancy (with historical uninfected group):
#  1. Prepares data including grouping outcome variable for analysis
#  2. Looks at distribution of covariates in infected and uninfected group
#  3. Looks at distribution of pregnancy outcomes at 19+6 weeks gestation in infected and uninfected group
#  4. Conducts crude analysis of association between infection and ectopic pregnancy (only accounting for matching factors)
#  5. Conducts adjusted analysis of association between infection and ectopic pregnancy (accounting for all covariates of interest)
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

v.ep.inf.data <- readRDS(paste0(folder_temp_data, "matched_ectopic_infection_cohort_one.rds"))

ls(v.ep.inf.data)

########Create variables for analysis########

#create a variable to capture vaccinated versus unvaccinated
table(v.ep.inf.data$inf_or_uninf)

v.ep.inf.data <-
v.ep.inf.data %>%
  mutate(
    inf_descrip = case_when(
      inf_or_uninf == "uninf" ~ "Uninfected historical cohort (N=2,799)",
      inf_or_uninf == "inf" ~ "Infected cohort (N=933)"
      ))
v.ep.inf.data$inf_cat <- factor(v.ep.inf.data$inf_descrip, levels=c("Uninfected historical cohort (N=2,799)", "Infected cohort (N=933)"))
v.ep.inf.data$inf_cat_graph <- factor(v.ep.inf.data$inf_descrip, levels=c("Infected cohort (N=933)", "Uninfected historical cohort (N=2,799)"))

table(v.ep.inf.data$inf_descrip)

#create a variable to capture gestational age at matching
table(v.ep.inf.data$ectopic_gestation_at_index_date)
v.ep.inf.data$ectopic_gestation_at_index_date <- floor(v.ep.inf.data$ectopic_gestation_at_index_date)
v.ep.inf.data <- v.ep.inf.data %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(ectopic_gestation_at_index_date)) %>%
  ungroup()
addmargins(table(v.ep.inf.data$gest_at_match))

#create outcome variables
#for multinomial analysis 
#note that livebirths are categorised as miscarriages given early gestation precluding survival
#molar pregnancies also grouped with miscarriages
table(v.ep.inf.data$miscarriage_study_outcome)

v.ep.inf.data$outcomes_cat <- dplyr::recode(v.ep.inf.data$ectopic_study_outcome, "Termination"="Termination", 
                                                                             "Molar pregnancy"="Miscarriage", 
                                                                             "Miscarriage"="Miscarriage", 
                                                                             "Ectopic pregnancy"="Ectopic pregnancy", 
                                                                             "Live birth"="Miscarriage", 
                                                                             "Ongoing wk 19"="Ongoing pregnancy")
v.ep.inf.data$outcomes_cat <- factor(v.ep.inf.data$outcomes_cat, levels=c("Ongoing pregnancy", "Miscarriage", "Termination", "Ectopic pregnancy"))
v.ep.inf.data$outcomes_cat_forgraph <- factor(v.ep.inf.data$outcomes_cat, levels=c("Ectopic pregnancy", "Termination", "Miscarriage", "Ongoing pregnancy"))

addmargins(table(v.ep.inf.data$outcomes_cat, v.ep.inf.data$ectopic_study_outcome)) 

#Tidy up covariates as needed
addmargins(table(v.ep.inf.data$ethnicity_cat)) 
addmargins(table(v.ep.inf.data$cv_clinical_vulnerability_category)) 
addmargins(table(v.ep.inf.data$UR2_categories, exclude=NULL))
addmargins(table(v.ep.inf.data$bmi_cat))
addmargins(table(v.ep.inf.data$diabetes_cat))
addmargins(table(v.ep.inf.data$overall_smoking_status))
addmargins(table(v.ep.inf.data$simd))

v.ep.inf.data$simd[is.na(v.ep.inf.data$simd)] <- "Unknown/Missing"
v.ep.inf.data$simd[v.ep.inf.data$simd==9] <- "Unknown/Missing"
v.ep.inf.data$overall_smoking_status[is.na(v.ep.inf.data$overall_smoking_status)] <- "Unknown/Missing"
v.ep.inf.data$bmi_cat <- factor(v.ep.inf.data$bmi_cat, levels=c(levels(v.ep.inf.data$bmi_cat), NA), labels = c(levels(v.ep.inf.data$bmi_cat), 88), exclude=NULL)

####Descriptive for each group: key characteristics####

#age median and range
infection_age_mean <- v.ep.inf.data %>%
  group_by(inf_or_uninf) %>%
  summarise(age_median=median(mother_age_at_conception),
            age_min = min(mother_age_at_conception),
            age_max = max(mother_age_at_conception))
infection_age_mean

#Look at outcomes over time
#by pregnancy outcome year
vaccination_by_ethnicity <- v.ep.inf.data %>%  
  tabyl(ethnicity_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_ethnicity, paste(folder_results, "primary_infected_by_ethnicity.csv", sep = ''))

vaccination_by_urban_rural_cat <- v.ep.inf.data %>%  
  tabyl(UR6_categories, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_urban_rural_cat, paste(folder_results, "primary_infected_by_urban_rural_6cat.csv", sep = ''))

vaccination_by_simd <- v.ep.inf.data %>%  
  tabyl(simd, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_simd, paste(folder_results, "primary_infected_by_simd.csv", sep = ''))

vaccination_by_bmi_cat <- v.ep.inf.data %>%  
  tabyl(bmi_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_bmi_cat, paste(folder_results, "primary_infected_by_bmi_cat.csv", sep = ''))

vaccination_by_overall_smoking_status <- v.ep.inf.data %>%  
  tabyl(overall_smoking_status, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_overall_smoking_status, paste(folder_results, "primary_infected_by_overall_smoking_status.csv", sep = ''))

vaccination_by_cv_clinical_vulnerability_category <- v.ep.inf.data %>%  
  tabyl(cv_clinical_vulnerability_category, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_cv_clinical_vulnerability_category, paste(folder_results, "primary_infected_by_cv_clinical_vulnerability_category.csv", sep = ''))

vaccination_by_diabetes_cat <- v.ep.inf.data %>%  
  tabyl(diabetes_cat, inf_or_uninf) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(vaccination_by_diabetes_cat, paste(folder_results, "primary_infected_by_diabetes_cat.csv", sep = ''))

#look at some descriptives for infected cohort

#timing of fist infection
v.ep.inf.data.infected <- v.ep.inf.data %>%
  filter(inf_or_uninf=="inf") %>%
  mutate(gest_group = case_when(
    (gest_at_match<2) ~ "1. pre-conception",
    (gest_at_match>=2 & gest_at_match<6) ~ "2. 2-5 weeks",
    (gest_at_match>=6 & gest_at_match<11) ~ "3. 6-10 weeks",
    (gest_at_match>=11 & gest_at_match<16) ~ "4. 11-15 weeks",
    (gest_at_match>=16 & gest_at_match<20) ~ "5. 16-20 weeks"
  ))
addmargins(table(v.ep.inf.data.infected$gest_group))  

#number of infections
v.ep.inf.data.infected$infection1 <- ifelse(!is.na(v.ep.inf.data.infected$index_date_covid_infection_1) &
                                                as.numeric(v.ep.inf.data.infected$index_date_covid_infection_1-v.ep.inf.data.infected$est_conception_date)>=-6*7 &
                                                v.ep.inf.data.infected$index_date_covid_infection_1<=v.ep.inf.data.infected$ectopic_vaccination_timing_period_end &
                                                v.ep.inf.data.infected$index_date_covid_infection_1<=v.ep.inf.data.infected$pregnancy_end_date, 1,0)

v.ep.inf.data.infected$infection2 <- ifelse(!is.na(v.ep.inf.data.infected$index_date_covid_infection_2) &
                                                as.numeric(v.ep.inf.data.infected$index_date_covid_infection_2-v.ep.inf.data.infected$est_conception_date)>=-6*7 &
                                                v.ep.inf.data.infected$index_date_covid_infection_2<=v.ep.inf.data.infected$ectopic_vaccination_timing_period_end &
                                                v.ep.inf.data.infected$index_date_covid_infection_2<=v.ep.inf.data.infected$pregnancy_end_date, 1,0)

v.ep.inf.data.infected$infection3 <- ifelse(!is.na(v.ep.inf.data.infected$index_date_covid_infection_3) &
                                                as.numeric(v.ep.inf.data.infected$index_date_covid_infection_3-v.ep.inf.data.infected$est_conception_date)>=-6*7 &
                                                v.ep.inf.data.infected$index_date_covid_infection_3<=v.ep.inf.data.infected$ectopic_vaccination_timing_period_end &
                                                v.ep.inf.data.infected$index_date_covid_infection_3<=v.ep.inf.data.infected$pregnancy_end_date, 1,0)

v.ep.inf.data.infected$infection4 <- ifelse(!is.na(v.ep.inf.data.infected$index_date_covid_infection_4) &
                                                as.numeric(v.ep.inf.data.infected$index_date_covid_infection_4-v.ep.inf.data.infected$est_conception_date)>=-6*7 &
                                                v.ep.inf.data.infected$index_date_covid_infection_4<=v.ep.inf.data.infected$ectopic_vaccination_timing_period_end &
                                                v.ep.inf.data.infected$index_date_covid_infection_4<=v.ep.inf.data.infected$pregnancy_end_date, 1,0)

table(v.ep.inf.data.infected$infection1)
table(v.ep.inf.data.infected$infection2)
table(v.ep.inf.data.infected$infection3)
table(v.ep.inf.data.infected$infection4)

v.ep.inf.data.infected$infection <- ifelse(v.ep.inf.data.infected$infection1==1 | v.ep.inf.data.infected$infection2==1 | v.ep.inf.data.infected$infection3==1 | v.ep.inf.data.infected$infection4==1, 1, 0)
addmargins(table(v.ep.inf.data.infected$infection, exclude=NULL))

v.ep.inf.data.infected <- v.ep.inf.data.infected %>%
  mutate(infection_no = case_when(
    (infection1==1 & infection2==1 & infection3==1) ~ "3",
    (infection1==1 & infection2==1 & infection3==0) ~ "2",
    (infection1==1 & infection2==0 & infection3==1) ~ "2",
    (infection1==0 & infection2==1 & infection3==1) ~ "2",
    (infection1==1 & infection2==0 & infection3==0) ~ "1",
    (infection1==0 & infection2==1 & infection3==0) ~ "1",
    (infection1==0 & infection2==0 & infection3==1) ~ "1"
  ))
table(v.ep.inf.data.infected$infection_no)

#symptomatic or non sypmtomatic
v.ep.inf.data.infected <- v.ep.inf.data.infected %>%
  mutate(symptomatic_infection = case_when(
    (infection1==1 & final_symptomatic_covid_infection_1=="true") ~ "Symptomatic",
    (infection2==1 & final_symptomatic_covid_infection_2=="true") ~ "Symptomatic",
    (infection3==1 & final_symptomatic_covid_infection_3=="true") ~ "Symptomatic"
  ))
table(v.ep.inf.data.infected$symptomatic_infection)

####Primary analysis: ectopic pregnacy in infected versus uninfected (pre-pandemic)####

#Decription of infected and uninfected

#Look at outcomes in infected and uninfected
outcomes_by_inf_status <- v.ep.inf.data %>%  
  tabyl(outcomes_cat, inf_cat_graph) %>% 
  adorn_totals(where="col") %>%
  adorn_totals(where="row") %>% 
  adorn_percentages(denominator="col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")
write.csv(outcomes_by_inf_status, paste(folder_results, "Pregnancy_events_by_inf_cohort.csv", sep = ''))

#Graph of outcomes by infection status
v.ep.inf.data$count <-1
outcome_distribution <- v.ep.inf.data %>%
  group_by(inf_cat_graph, outcomes_cat_forgraph) %>%
  summarise(count.sum = sum(count))

outcome_distribution_primary <- outcome_distribution %>%
  group_by(inf_cat_graph) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100)

outcome_distribution_primary %>% write_rds(paste0(folder_temp_data, "outcome_distribution_primary_infection_ectopic.rds"), compress = "gz")

#Crude analysis

#multinomial regression adjusting for matching factors - baseline all non miscarriage outcomes
model3 <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception + conception_quarter, data=v.ep.inf.data)
summary(model3)
exp(coef(model3)) 
exp(confint(model3))
nrow(fitted(model3))

z <- summary(model3)$coefficients/summary(model3)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
p

#multinomial regression adjusting for matching factors + other covariates (excluding ethnicity)
model5 <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + UR6_categories + simd + cv_clinical_vulnerability_category, data=v.ep.inf.data)
summary(model5)
exp(coef(model5)) 
exp(confint(model5))

z3 <- summary(model5)$coefficients/summary(model5)$standard.errors
p3 <- (1-pnorm(abs(z3), 0, 1))*2
p3

#sensitvity removing missing simd & urban/rural
model_check <- v.ep.inf.data %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing")
table(model_check$simd)
table(model_check$UR6_categories)
modelx <- multinom(outcomes_cat ~ inf_cat + gest_at_match + mother_age_at_conception + conception_quarter 
                   + UR6_categories + simd + cv_clinical_vulnerability_category, data=model_check)
summary(modelx)
exp(coef(modelx)) 
exp(confint(modelx))


