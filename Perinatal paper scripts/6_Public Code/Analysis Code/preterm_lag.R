library(tidyverse)
library(lubridate)
library(kableExtra)
library(dplyr)
library(casebase)
library(survival)
library(ggplot2)
library(survminer)


folder_scripts <- ""


source(paste0(folder_scripts, "/00-setup.R"))

# calculate lag between infection and end of pregnancy 
cohort3_lag_infection_and_birth <- cohort3_infect_contemp_controls_singletons %>%
  filter(inf_or_uninf == "inf") %>%
  mutate(lag = difftime(pregnancy_end_date, ptb_covid_index_date, unit="days"), 
         lag_group = case_when(between(lag, 0, 6) ~ "0-6 days", 
                               between(lag, 7, 27) ~ "7-27 days", 
                               lag>=28 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 37 ~ "Term")) %>%
  group_by(lag_group, smr02_onset_of_delivery_process, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  group_by(smr02_onset_of_delivery_process) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1)) %>%
  arrange(smr02_onset_of_delivery_process)


cohort3_lag_infection_and_birth_total <- cohort3_infect_contemp_controls_singletons %>%
  filter(inf_or_uninf == "inf") %>%
  mutate(lag = difftime(pregnancy_end_date, ptb_covid_index_date, unit="days"), 
         lag_group = case_when(between(lag, 0, 6) ~ "0-6 days", 
                               between(lag, 7, 27) ~ "7-27 days", 
                               lag>=28 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 37 ~ "Term")) %>%
  group_by(lag_group, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1))



cohort3_lag_control_and_birth <- cohort3_infect_contemp_controls_singletons %>%
  group_by(index) %>%
  mutate(gestation_at_matching = max(ptb_gestation_at_index_date, na.rm=T)) %>%
  filter(inf_or_uninf == "uninf") %>%
  mutate(lag = gestation_at_outcome - gestation_at_matching, 
         lag_group = case_when(lag < 1 ~ "0-6 days", 
                               between(lag, 1, 4) ~ "7-27 days", 
                               lag>=4 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 37 ~ "Term")) %>%
  group_by(lag_group, smr02_onset_of_delivery_process, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  group_by(smr02_onset_of_delivery_process) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1)) %>%
  arrange(smr02_onset_of_delivery_process)


cohort3_lag_control_and_birth_total <- cohort3_infect_contemp_controls_singletons %>%
  group_by(index) %>%
  mutate(gestation_at_matching = max(ptb_gestation_at_index_date, na.rm=T)) %>%
  filter(inf_or_uninf == "uninf") %>%
  mutate(lag = gestation_at_outcome - gestation_at_matching, 
         lag_group = case_when(lag < 1 ~ "0-6 days", 
                               between(lag, 1, 4) ~ "7-27 days", 
                               lag>=4 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 37 ~ "Term")) %>%
  group_by(lag_group, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1))

cohort4_lag_infection_and_birth <- cohort4_infect_contemp_controls_singletons %>%
  filter(inf_or_uninf == "inf") %>%
  mutate(lag = difftime(pregnancy_end_date, ptb_covid_index_date, unit="days"), 
         lag_group = case_when(between(lag, 0, 6) ~ "0-6 days", 
                               between(lag, 7, 27) ~ "7-27 days", 
                               lag>=28 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 32 ~ "Term")) %>%
  group_by(lag_group, smr02_onset_of_delivery_process, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  group_by(smr02_onset_of_delivery_process) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1)) %>%
  arrange(smr02_onset_of_delivery_process)


cohort4_lag_infection_and_birth_total <- cohort4_infect_contemp_controls_singletons %>%
  filter(inf_or_uninf == "inf") %>%
  mutate(lag = difftime(pregnancy_end_date, ptb_covid_index_date, unit="days"), 
         lag_group = case_when(between(lag, 0, 6) ~ "0-6 days", 
                               between(lag, 7, 27) ~ "7-27 days", 
                               lag>=28 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 32 ~ "Term")) %>%
  group_by(lag_group, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1))


cohort4_lag_control_and_birth <- cohort4_infect_contemp_controls_singletons %>%
  group_by(index) %>%
  mutate(gestation_at_matching = max(vptb_gestation_at_index_date, na.rm=T)) %>%
  filter(inf_or_uninf == "uninf") %>%
  mutate(lag = gestation_at_outcome - gestation_at_matching, 
         lag_group = case_when(lag < 1 ~ "0-6 days", 
                               between(lag, 1, 4) ~ "7-27 days", 
                               lag>=4 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 32 ~ "Term")) %>%
  group_by(lag_group, smr02_onset_of_delivery_process, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  group_by(smr02_onset_of_delivery_process) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1)) %>%
  arrange(smr02_onset_of_delivery_process)


cohort4_lag_control_and_birth_total <- cohort4_infect_contemp_controls_singletons %>%
  group_by(index) %>%
  mutate(gestation_at_matching = max(vptb_gestation_at_index_date, na.rm=T)) %>%
  filter(inf_or_uninf == "uninf") %>%
  mutate(lag = gestation_at_outcome - gestation_at_matching, 
         lag_group = case_when(lag < 1 ~ "0-6 days", 
                               between(lag, 1, 4) ~ "7-27 days", 
                               lag>=4 ~ ">28 days"), 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ "Preterm", 
                                    outcome == "Live birth" & gestation_at_outcome >= 32 ~ "Term")) %>%
  group_by(lag_group, cohort_outcome) %>%
  tally() %>%
  filter(cohort_outcome == "Preterm") %>%
  ungroup() %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round((n/total)*100, 1))

## create cumulative incidence plot


cohort3_cumulative_incidence <- cohort3_infect_contemp_controls_singletons %>%
  group_by(index) %>%
  mutate(gestation_at_matching = max(ptb_gestation_at_index_date, na.rm=T)) %>%
  ungroup() %>%
  mutate(lag = gestation_at_outcome - gestation_at_matching, 
         cohort_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ 1, 
                                    outcome == "Live birth" & gestation_at_outcome >= 37 ~ 0), 
         status = case_when(inf_or_uninf == "inf" ~ 1, 
                            inf_or_uninf == "uninf" ~ 0)) 
## Add survival object
# # cohort3_cumulative_incidence$SurvObj <- with(cohort3_cumulative_incidence, Surv(lag))
# 
# mod_cohort3_glm <- fitSmoothHazard(outcome ~ inf_or_uninf*lag,
#                                    data = cohort3_cumulative_incidence,
#                                    time = "lag", ratio = 10)
# summary(mod_cohort3_glm)  
# 
# smooth_risk_preterm <- absoluteRisk(object = mod_cohort3_glm, 
#                                      newdata = cohort3_cumulative_incidence[c(1,10000),])
# 
# class(smooth_risk_preterm)
# plot(smooth_risk_preterm)



fit <- survfit(Surv(lag, cohort_outcome) ~ inf_or_uninf, data = cohort3_cumulative_incidence)

ggsurvplot(fit, fun = "event")


