# create forest plots for sub-group analyses 

# import pacakges
library(janitor)
library(dplyr)
library(tidyr)
library(forcats)
library(renv)
library(here)
library(magrittr)
library(lubridate)
library(readr)
# library(hablar)
library(labelled)
library(purrr)
library(broom)
library(ggplot2)
library(survival)
library(phsstyles)

# file paths
folder_working_data <- "/data/HPS/COPS_non_confi/COPS_VaccineSafety_Perinatal/2_Working_Data/"
folder_results <- "/data/HPS/COPS_non_confi/COPS_VaccineSafety_Perinatal/4_Results/"
folder_models <- "/data/HPS/COPS_non_confi/COPS_VaccineSafety_Perinatal/4_Results/models/"

# read in models 

## infection pre-20 weeks gestation
pre_stillbirth <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_stillbirth_unadjusted.rds"))
pre_perinatal_death <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_perinatal_mortality_adjusted.rds"))
pre_nnd <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_nnd_unadjusted.rds"))
pre_sga <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_small_gestational_age_adjusted.rds"))
pre_very_sga <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_very_small_gestational_age_adjusted.rds"))
pre_low_apgar <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_low_apgar_adjusted.rds"))
pre_very_low_apgar <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_very_low_apgar_unadjusted.rds"))
pre_preterm <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_preterm_adjusted.rds"))
pre_preterm_spontaneous <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_spontaneous_preterm_adjusted.rds"))
pre_preterm_provider_initiated <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_provider_initiated_preterm_adjusted.rds"))
pre_very_preterm <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_very_preterm_adjusted.rds"))
pre_very_preterm_spontaneous <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_spontaneous_very_preterm_adjusted.rds"))
pre_very_preterm_provider_initiated <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_provider_initiated_very_preterm_unadjusted.rds"))

pre_hypertension <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_hypertension_adjusted.rds"))
pre_vte <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_vte_unadjusted.rds"))
pre_bleeding <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_bleeding_adjusted.rds"))
pre_icu <- readRDS(paste0(folder_models, "subgroup_analysis_pre20weeks_icu_adjusted.rds"))


# infection post 20 weeks gestation 
post_stillbirth <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_stillbirth_adjusted.rds"))
post_perinatal_death <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_perinatal_mortality_adjusted.rds"))
post_nnd <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_nnd_unadjusted.rds"))
post_sga <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_small_gestational_age_adjusted.rds"))
post_very_sga <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_very_small_gestational_age_adjusted.rds"))
post_low_apgar <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_low_apgar_adjusted.rds"))
post_very_low_apgar <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_very_low_apgar_unadjusted.rds"))
post_preterm <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_preterm_adjusted.rds"))
post_preterm_spontaneous <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_spontaneous_preterm_adjusted.rds"))
post_preterm_provider_initiated <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_provider_initiated_preterm_adjusted.rds"))
post_very_preterm <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_very_preterm_adjusted.rds"))
post_very_preterm_spontaneous <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_spontaneous_very_preterm_adjusted.rds"))
post_very_preterm_provider_initiated <- readRDS(paste0(folder_models, "subgroup_analysis_20weeks_provider_initiated_very_preterm_unadjusted.rds"))

post_hypertension <- readRDS(paste0(folder_models, "subgroup_analysis_post20weeks_hypertension_adjusted.rds"))
post_vte <- readRDS(paste0(folder_models, "subgroup_analysis_post20weeks_vte_adjusted.rds"))
post_bleeding <- readRDS(paste0(folder_models, "subgroup_analysis_post20weeks_bleeding_adjusted.rds"))
post_icu <- readRDS(paste0(folder_models, "subgroup_analysis_post20weeks_icu_adjusted.rds"))


pre_models <- bind_rows(
  bind_cols(as.data.frame(exp(coef(pre_stillbirth))), 
            as.data.frame(exp(confint.default(pre_stillbirth)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_stillbirth))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Stillbirth", 
           group = "Baby Outcomes"),
  
  bind_cols(as.data.frame(exp(coef(pre_perinatal_death))), 
            as.data.frame(exp(confint.default(pre_perinatal_death)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_perinatal_death))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Perinatal Mortality", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_nnd))), 
            as.data.frame(exp(confint.default(pre_nnd)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_nnd))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Neonatal Death", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_sga))), 
            as.data.frame(exp(confint.default(pre_sga)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_sga))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Small for \nGestational Age", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_very_sga))), 
            as.data.frame(exp(confint.default(pre_very_sga)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_very_sga))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Small for \nGestational Age", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_low_apgar))), 
            as.data.frame(exp(confint.default(pre_low_apgar)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_low_apgar))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Low Apgar \nScore (<7)", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_very_low_apgar))), 
            as.data.frame(exp(confint.default(pre_very_low_apgar)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_very_low_apgar))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Low Apgar \nScore (<4)", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_preterm))), 
            as.data.frame(exp(confint.default(pre_preterm)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_preterm))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_preterm_spontaneous))), 
            as.data.frame(exp(confint.default(pre_preterm_spontaneous)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_preterm_spontaneous))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Spontaneous \nPreterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_preterm_provider_initiated))), 
            as.data.frame(exp(confint.default(pre_preterm_provider_initiated)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_preterm_provider_initiated))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Provider Initiated \nPreterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_very_preterm))), 
            as.data.frame(exp(confint.default(pre_very_preterm)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_very_preterm))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_very_preterm_spontaneous))), 
            as.data.frame(exp(confint.default(pre_very_preterm_spontaneous)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_very_preterm_spontaneous))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Spontaneous \nVery Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_very_preterm_provider_initiated))), 
            as.data.frame(exp(confint.default(pre_very_preterm_provider_initiated)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_very_preterm_provider_initiated))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Provider Initiated \nVery Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_icu))), 
            as.data.frame(exp(confint.default(pre_icu)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_icu))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "ICU and/or death", 
           group = "Maternal Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_vte))), 
            as.data.frame(exp(confint.default(pre_vte)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_vte))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Venous Thromboembolism", 
           group = "Maternal Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_hypertension))), 
            as.data.frame(exp(confint.default(pre_hypertension)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_hypertension))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Hypertensive disorders \nof pregnancy", 
           group = "Maternal Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(pre_bleeding))), 
            as.data.frame(exp(confint.default(pre_bleeding)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(pre_bleeding))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Pregnancy-related bleeding", 
           group = "Maternal Outcomes")
  
  
) %>%
  select(group, outcome, estimate, lower, upper) %>%
  mutate(analysis = "<= 19+6 weeks") %>%
  mutate(index = seq(from = 17.2, to = 1.2, by = -1))

post_models <- bind_rows(
  bind_cols(as.data.frame(exp(coef(post_stillbirth))), 
            as.data.frame(exp(confint.default(post_stillbirth)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_stillbirth))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Stillbirth", 
           group = "Baby Outcomes"),
  
  bind_cols(as.data.frame(exp(coef(post_perinatal_death))), 
            as.data.frame(exp(confint.default(post_perinatal_death)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_perinatal_death))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Perinatal Mortality", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_nnd))), 
            as.data.frame(exp(confint.default(post_nnd)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_nnd))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Neonatal Death", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_sga))), 
            as.data.frame(exp(confint.default(post_sga)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_sga))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Small for \nGestational Age", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_very_sga))), 
            as.data.frame(exp(confint.default(post_very_sga)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_very_sga))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Small for \nGestational Age", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_low_apgar))), 
            as.data.frame(exp(confint.default(post_low_apgar)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_low_apgar))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Low Apgar \nScore (<7)", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_very_low_apgar))), 
            as.data.frame(exp(confint.default(post_very_low_apgar)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_very_low_apgar))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Low Apgar \nScore (<4)", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_preterm))), 
            as.data.frame(exp(confint.default(post_preterm)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_preterm))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_preterm_spontaneous))), 
            as.data.frame(exp(confint.default(post_preterm_spontaneous)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_preterm_spontaneous))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Spontaneous \nPreterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_preterm_provider_initiated))), 
            as.data.frame(exp(confint.default(post_preterm_provider_initiated)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_preterm_provider_initiated))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Provider Initiated \nPreterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_very_preterm))), 
            as.data.frame(exp(confint.default(post_very_preterm)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_very_preterm))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Very Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_very_preterm_spontaneous))), 
            as.data.frame(exp(confint.default(post_very_preterm_spontaneous)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_very_preterm_spontaneous))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Spontaneous \nVery Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_very_preterm_provider_initiated))), 
            as.data.frame(exp(confint.default(post_very_preterm_provider_initiated)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_very_preterm_provider_initiated))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Provider Initiated \nVery Preterm Birth", 
           group = "Baby Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_icu))), 
            as.data.frame(exp(confint.default(post_icu)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_icu))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "ICU and/or death", 
           group = "Maternal Outcomes"),
  
  bind_cols(as.data.frame(exp(coef(post_vte))), 
            as.data.frame(exp(confint.default(post_vte)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_vte))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Venous Thromboembolism", 
           group = "Maternal Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_hypertension))), 
            as.data.frame(exp(confint.default(post_hypertension)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_hypertension))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Hypertensive disorders \nof pregnancy", 
           group = "Maternal Outcomes"), 
  
  bind_cols(as.data.frame(exp(coef(post_bleeding))), 
            as.data.frame(exp(confint.default(post_bleeding)))) %>%
    filter(row.names(.)=="exposure") %>%
    rename(estimate = "exp(coef(post_bleeding))", 
           lower = "2.5 %", 
           upper = "97.5 %") %>%
    mutate(outcome = "Pregnancy-related bleeding", 
           group = "Maternal Outcomes")
  
) %>%
  select(group, outcome, estimate, lower, upper) %>%
  mutate(analysis = ">= 20 weeks") %>%
  mutate(index = seq(from = 17, to = 1, by = -1))

subgroup_model_output <- bind_rows(pre_models, post_models) %>% mutate(group = case_when(group == "Maternal Outcomes" ~ "Maternal \nOutcomes", 
                                                                                         T ~ group))

y_axis_names <- subgroup_model_output %>% select(index, outcome) %>% group_by(outcome) %>% slice(1) %>% arrange(index)

forest_plot <- ggplot(subgroup_model_output, aes(x = estimate,
                                                 y = index, 
                                                 xmin = lower, 
                                                 xmax = upper, 
                                                 colour = analysis)) + 
  geom_point() + 
  geom_errorbarh(height=.1) +
  # geom_hline(yintercept=4.5, color='black', linetype='solid', alpha=1) +
  # geom_text(aes(x=0.044, y = 4.25, label = "Maternal Outcomes"), show.legend = FALSE, color="black") +
  # geom_text(aes(x=0.035, y = 17.6, label = "Baby Outcomes"), show.legend = FALSE, color="black") +
  xlim(min(subgroup_model_output$lower), 15.5) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_y_continuous(name = "Outcome", breaks=1:nrow(y_axis_names), labels=y_axis_names$outcome) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(x='Adjusted Odds Ratio', y = 'Outcome', color = "") + 
  geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  scale_color_manual(values = phs_colours(c("phs-liberty-80", "phs-purple"))) +
  scale_x_continuous(trans='log10')

  

forest_plot


ggsave(paste0(folder_results, "subgroup_analysis_forest_plot.png"), forest_plot)
ggsave(paste0(folder_results, "subgroup_analysis_forest_plot.pdf"), forest_plot)
ggsave(paste0(folder_results, "subgroup_analysis_forest_plot.jpeg"), forest_plot)













