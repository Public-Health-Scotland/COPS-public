# run script 0b. setup.r in the main COPS code before this script

end_date <- as.Date("2022-01-31")


# import baby data 
baby_level <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>%
  filter(outcome == "Live birth") %>%
  filter(x_full_cohort == T) %>% 
  mutate(baby_chi_validity = chi_check(baby_upi))

# create data frame for babies(only looking at babies who have a valid CHI regardless of whether their mother UPI is valid or not)
babies <- baby_level %>%
  group_by(baby_upi) %>%
  slice(1) %>%
  filter(baby_chi_validity == "Valid CHI") %>% # Only include babies with a valid CHI 
  select(-chi_validity) %>%
  mutate(maternal_age_group = cops_age_group(x_mother_age_at_conception)) %>%
  mutate(ethnicity_description = cops_reporting_ethnicity(x_ethnicity_code)) %>%
  mutate(ethnicity_description = ifelse(is.na(ethnicity_description), "5 Unknown/missing", ethnicity_description)) %>%
  mutate(simd = as.character(x_simd)) %>%
  mutate(simd = case_when(is.na(x_simd) ~ "9 Unknown",
                          x_simd == 9 ~ "9 Unknown",
                          T ~ simd), 
         gestation_at_birth = case_when(between(x_gestation_at_outcome, 22, 31) ~ "1 Very preterm (22-31w)", 
                                        between(x_gestation_at_outcome, 32, 36) ~ "2 Preterm (32-36w)", 
                                        between(x_gestation_at_outcome, 37, 41) ~ "3 Term (37-41w)", 
                                        between(x_gestation_at_outcome, 42, 44) ~ "4 Post-term (42-44w)", 
                                        T ~ "5 Unknown")) %>%
  ungroup()

covid_babies <- babies %>% 
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  filter(days_old_had_covid_test >=0 & days_old_had_covid_test <=27,
         tests_baby_earliest_positive_test <= end_date)

babies <- babies %>% 
  filter(x_pregnancy_end_date <= end_date)

# create a data frame for babies whose mother have a valid chi 
group3_babies <- baby_level %>%
  group_by(baby_upi) %>%
  slice(1) %>%
  filter(chi_validity == "Valid CHI", # Only include mothers with a valid CHI 
         baby_chi_validity == "Valid CHI") %>% # Only include babies with a valid CHI 
  select(-chi_validity) %>%
  mutate(maternal_age_group = cops_age_group(x_mother_age_at_conception)) %>%
  mutate(ethnicity_description = cops_reporting_ethnicity(x_ethnicity_code)) %>%
  mutate(ethnicity_description = ifelse(is.na(ethnicity_description), "5 Unknown/missing", ethnicity_description)) %>%
  mutate(simd = as.character(x_simd)) %>%
  mutate(simd = case_when(is.na(x_simd) ~ "9 Unknown",
                          x_simd == 9 ~ "9 Unknown",
                          T ~ simd), 
         gestation_at_birth = case_when(between(x_gestation_at_outcome, 22, 31) ~ "1 Very preterm (22-31w)", 
                                        between(x_gestation_at_outcome, 32, 36) ~ "2 Preterm (32-36w)", 
                                        between(x_gestation_at_outcome, 37, 41) ~ "3 Term (37-41w)", 
                                        between(x_gestation_at_outcome, 42, 44) ~ "4 Post-term (42-44w)", 
                                        T ~ "5 Unknown")) %>%
  ungroup() %>%
  filter(x_pregnancy_end_date <= end_date)

group3_covid_babies <- group3_babies %>% 
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  filter(days_old_had_covid_test >=0 & days_old_had_covid_test <=27,
         tests_baby_earliest_positive_test <= end_date)

#### read in testing data ####

neonate_tests <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/neonate_tests.rds")) %>% 
  filter(age_in_days >= 0)
post_neonate_tests <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/post_neonate_tests.rds"))
one_to_four_yo_tests <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/one_to_four_yo_tests.rds"))
five_to_11_yo_tests <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/five_to_11_yo_tests.rds"))
twelve_to_17_yo_tests <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/twelve_to_17_yo_tests.rds"))

#### run rates code ####
source(file.path(paste0(path_to_scripts, "Ad-Hoc-Analysis/Neonatal_infections_paper/overall rates.R")))


#### age at infection
age_data <- covid_babies %>% 
  tabyl(days_old_had_covid_test) %>% 
  mutate(cumulative = cumsum(n)) %>% 
  select(-percent) 

age_table <- age_data %>% 
  adorn_totals() %>% 
  mutate(cumulative = if_else(days_old_had_covid_test == "Total", max(n), cumulative))
  
#### Admission info ####

### Neonatal
# read in and check we have temporally associated admissions only
associated_sbr_admissions <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/associated_sbr_babies.rds")) %>% 
  filter(time_associated_neonatal == 1) %>% 
  ungroup() %>% 
  mutate(time_associated_neonatal_contains_code = recode(time_associated_neonatal_contains_code, `1` = "1 - Yes", `0` = "2 - No"))
  

# NICU as highest level of care?
nicu_table_skeleton <- expand_grid(nicu = c("1 - Yes", "2 - No"), time_associated_neonatal_contains_code = c("1 - Yes", "2 - No"))
nicu_table <- associated_sbr_admissions %>% 
  mutate(nicu = recode(nicu, `1` = "1 - Yes", `0` = "2 - No")) %>% 
  group_by(nicu, time_associated_neonatal_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nicu_table_skeleton) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nicu, names_from = time_associated_neonatal_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nicu, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# Nosocomial infection?
nosocomial_table_skeleton_sbr <- expand_grid(nosocomial = c("1 - Probable", "2 - Possible", "3 - No"), time_associated_neonatal_contains_code = c("1 - Yes", "2 - No"))
nosocomial_sbr_table <- associated_sbr_admissions %>% 
  group_by(nosocomial, time_associated_neonatal_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nosocomial_table_skeleton_sbr) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nosocomial, names_from = time_associated_neonatal_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nosocomial, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# LOS
los_skeleton_sbr <- nicu_table_skeleton %>% 
  select(time_associated_neonatal_contains_code) %>%  distinct()

sbr_los_data <- associated_sbr_admissions %>% 
  group_by(time_associated_neonatal_contains_code) %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) %>% 
  right_join(los_skeleton_sbr) %>% 
  mutate(mean_los = as.numeric(mean_los),
         median_los = as.numeric(median_los)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  arrange(time_associated_neonatal_contains_code)

sbr_los_data_totals <- associated_sbr_admissions %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) 
sbr_los_code <- paste0(round(sbr_los_data$mean_los[1], digits = 1), ", (", round(sbr_los_data$median_los[1], digits = 1),
                         ", ", round(sbr_los_data$iqr_los[1], digits = 1), ")")
sbr_los_no_code <- paste0(round(sbr_los_data$mean_los[2], digits = 1), ", (", round(sbr_los_data$median_los[2], digits = 1),
                            ", ", round(sbr_los_data$iqr_los[2], digits = 1), ")")
sbr_los_total <- paste0(round(sbr_los_data_totals$mean_los[1], digits = 1), ", (", round(sbr_los_data_totals$median_los[1], digits = 1),
                          ", ", round(sbr_los_data_totals$iqr_los[1], digits = 1), ")")


### SMR01
# read in and check we have temporally associated admissions only
associated_smr01_admissions <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/covid_smr01_admissions.rds")) %>% 
  filter(time_associated_paeds == 1) %>% 
  ungroup() %>% 
  mutate(time_associated_paeds_contains_code = recode(time_associated_paeds_contains_code, `1` = "1 - Yes", `0` = "2 - No"))
  

# PICU as highest level of care?
picu_table_skeleton <- expand_grid(picu = c("1 - Yes", "2 - No"), time_associated_paeds_contains_code = c("1 - Yes", "2 - No"))
picu_table <- associated_smr01_admissions %>% 
  mutate(picu = recode(picu, `1` = "1 - Yes", `0` = "2 - No")) %>% 
  group_by(picu, time_associated_paeds_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(picu_table_skeleton) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = picu, names_from = time_associated_paeds_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(picu, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# Nosocomial infection?
nosocomial_table_skeleton_smr01 <- expand_grid(nosocomial = c("1 - Probable", "2 - Possible", "3 - No"), time_associated_paeds_contains_code = c("1 - Yes", "2 - No"))
nosocomial_smr01_table <- associated_smr01_admissions %>% 
  group_by(nosocomial, time_associated_paeds_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nosocomial_table_skeleton_smr01) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nosocomial, names_from = time_associated_paeds_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nosocomial, code_1_yes, code_2_no) %>% 
  arrange(nosocomial) %>% 
  adorn_totals(where = "both")

# LOS
los_skeleton_smr01 <- picu_table_skeleton %>% 
  select(time_associated_paeds_contains_code) %>%  distinct()
smr01_los_data <- associated_smr01_admissions %>% 
  group_by(time_associated_paeds_contains_code) %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) %>% 
  right_join(los_skeleton_smr01) %>% 
  mutate(mean_los = as.numeric(mean_los),
         median_los = as.numeric(median_los)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  arrange(time_associated_paeds_contains_code)

smr01_los_data_totals <- associated_smr01_admissions %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) 
smr01_los_code <- paste0(round(smr01_los_data$mean_los[1], digits = 1), ", (", round(smr01_los_data$median_los[1], digits = 1),
                  ", ", round(smr01_los_data$iqr_los[1], digits = 1), ")")
smr01_los_no_code <- paste0(round(smr01_los_data$mean_los[2], digits = 1), ", (", round(smr01_los_data$median_los[2], digits = 1),
                         ", ", round(smr01_los_data$iqr_los[2], digits = 1), ")")
smr01_los_total <- paste0(round(smr01_los_data_totals$mean_los[1], digits = 1), ", (", round(smr01_los_data_totals$median_los[1], digits = 1),
                         ", ", round(smr01_los_data_totals$iqr_los[1], digits = 1), ")")

#### Admission info: term babies only ####

term_covid_babies <- covid_babies %>% 
  filter(x_gestation_at_outcome >= 37)

### Neonatal
# read in and check we have temporally associated admissions only
associated_sbr_admissions_term <- associated_sbr_admissions %>% 
  filter(x_gestation_at_outcome >= 37)

# NICU as highest level of care?
nicu_table_term <- associated_sbr_admissions_term %>% 
  mutate(nicu = recode(nicu, `1` = "1 - Yes", `0` = "2 - No")) %>% 
  group_by(nicu, time_associated_neonatal_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nicu_table_skeleton) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nicu, names_from = time_associated_neonatal_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nicu, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# Nosocomial infection?
nosocomial_sbr_table_term <- associated_sbr_admissions_term %>% 
  group_by(nosocomial, time_associated_neonatal_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nosocomial_table_skeleton_sbr) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nosocomial, names_from = time_associated_neonatal_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nosocomial, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# LOS
sbr_los_data_term <- associated_sbr_admissions_term %>% 
  group_by(time_associated_neonatal_contains_code) %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) %>% 
  right_join(los_skeleton_sbr) %>% 
  mutate(mean_los = as.numeric(mean_los),
         median_los = as.numeric(median_los)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  arrange(time_associated_neonatal_contains_code)

sbr_los_data_totals_term <- associated_sbr_admissions_term %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) 
sbr_los_code_term <- paste0(round(sbr_los_data_term$mean_los[1], digits = 1), ", (", round(sbr_los_data_term$median_los[1], digits = 1),
                       ", ", round(sbr_los_data_term$iqr_los[1], digits = 1), ")")
sbr_los_no_code_term <- paste0(round(sbr_los_data_term$mean_los[2], digits = 1), ", (", round(sbr_los_data_term$median_los[2], digits = 1),
                          ", ", round(sbr_los_data_term$iqr_los[2], digits = 1), ")")
sbr_los_total_term <- paste0(round(sbr_los_data_totals_term$mean_los[1], digits = 1), ", (", round(sbr_los_data_totals_term$median_los[1], digits = 1),
                        ", ", round(sbr_los_data_totals_term$iqr_los[1], digits = 1), ")")


### SMR01
# read in and check we have temporally associated admissions only
associated_smr01_admissions_term <- associated_smr01_admissions %>% 
  filter(x_gestation_at_outcome >= 37) 

# PICU as highest level of care?
picu_table_term <- associated_smr01_admissions_term %>% 
  mutate(picu = recode(picu, `1` = "1 - Yes", `0` = "2 - No")) %>% 
  group_by(picu, time_associated_paeds_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(picu_table_skeleton) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = picu, names_from = time_associated_paeds_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(picu, code_1_yes, code_2_no) %>% 
  adorn_totals(where = "both")

# Nosocomial infection?
nosocomial_smr01_table_term <- associated_smr01_admissions_term %>% 
  group_by(nosocomial, time_associated_paeds_contains_code) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  right_join(nosocomial_table_skeleton_smr01) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  pivot_wider(id_cols = nosocomial, names_from = time_associated_paeds_contains_code, values_from = num,
              names_prefix = "code_") %>% 
  clean_names() %>% 
  select(nosocomial, code_1_yes, code_2_no) %>% 
  arrange(nosocomial) %>% 
  adorn_totals(where = "both")

# LOS
smr01_los_data_term <- associated_smr01_admissions_term %>% 
  group_by(time_associated_paeds_contains_code) %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) %>% 
  right_join(los_skeleton_smr01) %>% 
  mutate(mean_los = as.numeric(mean_los),
         median_los = as.numeric(median_los)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  arrange(time_associated_paeds_contains_code)

smr01_los_data_totals_term <- associated_smr01_admissions_term %>% 
  summarise(mean_los = mean(los),
            median_los = median(los),
            iqr_los = IQR(los)) 
smr01_los_code_term <- paste0(round(smr01_los_data_term$mean_los[1], digits = 1), ", (", round(smr01_los_data_term$median_los[1], digits = 1),
                         ", ", round(smr01_los_data_term$iqr_los[1], digits = 1), ")")
smr01_los_no_code_term <- paste0(round(smr01_los_data_term$mean_los[2], digits = 1), ", (", round(smr01_los_data_term$median_los[2], digits = 1),
                            ", ", round(smr01_los_data_term$iqr_los[2], digits = 1), ")")
smr01_los_total_term <- paste0(round(smr01_los_data_totals_term$mean_los[1], digits = 1), ", (", round(smr01_los_data_totals_term$median_los[1], digits = 1),
                          ", ", round(smr01_los_data_totals_term$iqr_los[1], digits = 1), ")")

overall_term_admissions <- associated_sbr_admissions_term %>% 
  full_join(associated_smr01_admissions_term, by = c("baby_upi" = "baby_upi")) %>% 
  filter(time_associated_neonatal == 1 | time_associated_paeds == 1)

#### Admission rate over time ####
## any temporally-associated admission
months_total_admissions <- associated_sbr_admissions %>%
  full_join(associated_smr01_admissions, by = c("baby_upi" = "baby_upi")) %>% 
  group_by(baby_upi) %>% 
  summarise(time_associated = max_(c(time_associated_neonatal,
                                   time_associated_paeds)),
            time_associated_contains_code = max_(c(time_associated_neonatal_contains_code,
                                                 time_associated_paeds_contains_code)),
            month = min_(c(month_infection.x, month_infection.y))) %>% 
  ungroup() %>% 
  group_by(month) %>%
  summarise(total_admissions = n())

neonates_admissions <- months_total_births %>%
  left_join(., months_total_admissions, by=c("month")) %>%
  mutate(total_admissions = replace_na(total_admissions, 0)) %>%
  arrange(month) %>% 
  adorn_totals()

neonates_admission_rates <- neonates_admissions %>%
  mutate(admission_rate = (total_admissions/total_live_births)*100000)

## covid-coded admissions
months_total_admissions_coded <- associated_sbr_admissions %>%
  full_join(associated_smr01_admissions, by = c("baby_upi" = "baby_upi")) %>% 
  group_by(baby_upi) %>% 
  summarise(time_associated = max_(c(time_associated_neonatal,
                                     time_associated_paeds)),
            time_associated_contains_code = max_(c(time_associated_neonatal_contains_code,
                                                   time_associated_paeds_contains_code)),
            month = min_(c(month_infection.x, month_infection.y))) %>% 
  ungroup() %>% 
  filter(time_associated_contains_code == "1 - Yes") %>% 
  group_by(month) %>%
  summarise(total_admissions_coded = n())

neonates_admissions_coded <- months_total_births %>%
  left_join(., months_total_admissions_coded, by=c("month")) %>%
  mutate(total_admissions_coded = replace_na(total_admissions_coded, 0)) %>%
  arrange(month) %>% 
  adorn_totals()

neonates_admission_rates_coded <- neonates_admissions_coded %>%
  mutate(coded_admission_rate = (total_admissions_coded/total_live_births)*100000)

# combine to build table
admissions_table <- neonates_covid_rates %>% 
  select(-c(lower_ci, upper_ci)) %>% 
  left_join(neonates_admission_rates) %>% 
  left_join(neonates_admission_rates_coded) %>% 
  select(month, total_live_births, total_positive_covid_neonates, total_admissions, total_admissions_coded, rate, admission_rate, coded_admission_rate)
  

#### Read out into template ####

template <- openxlsx::loadWorkbook(file.path(paste(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/Confirmed COVID in neonates output tables template.xlsx", sep = "/")))

writeData(template, "Cohort Numbers", nrow(neonate_tests), startCol = 2, startRow = 3, colNames = FALSE)
writeData(template, "Cohort Numbers", nrow(covid_babies), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "Cohort Numbers", nrow(group3_covid_babies), startCol = 2, startRow = 5, colNames = FALSE)
writeData(template, "Cohort Numbers", total_births_nrs, startCol = 2, startRow = 14, colNames = FALSE)
writeData(template, "Cohort Numbers", total_births_nhslb, startCol = 2, startRow = 15, colNames = FALSE)
writeData(template, "Cohort Numbers", total_births_COPS_with_baby_chi, startCol = 2, startRow = 16, colNames = FALSE)
writeData(template, "Cohort Numbers", total_births_COPS_with_mother_and_baby_chi, startCol = 2, startRow = 17, colNames = FALSE)


writeData(template, "Overall Rate", select(neonates_covid_rates, -c(month)), startCol = 2, startRow = 3, colNames = FALSE)
# plot rate over time
neonates_covid_rates_for_plot <- neonates_covid_rates %>% 
  select(month, rate) %>% 
  filter(month != "Total")
theme_set(theme_classic())
fig_1 <- ggplot(neonates_covid_rates_for_plot, aes(month, rate, group = 1)) +
  geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title = element_text(size = 10)) +
  xlab("Month of onset") + ylab("Rate of confirmed SARS-CoV-2\ninfection per 100,000 live births")
fig_1

insertPlot(template, "Overall Rate", width = 6, startCol = 8, startRow = 1,
           height = 3.5, fileType = "png", units = "in")
ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/fig_1_rate_over_time.jpeg"), dpi = 320)
# plot incidence by age
post_neonates_total_covid_chart <- post_neonates_total_covid %>% 
  select(month, rate) %>% 
  mutate(age = "2 - post-neonates")
one_to_four_yo_covid_chart <- one_to_four_yo_covid %>% 
  select(month, rate) %>% 
  mutate(age = "3 - 1-4 year olds")
five_to_eleven_yo_covid_chart <- five_to_eleven_yo_covid %>% 
  select(month, rate) %>% 
  mutate(age = "4 - 5-11 year olds")
twelve_to_seventeen_yo_covid_chart <- twelve_to_seventeen_yo_covid %>% 
  select(month, rate) %>% 
  mutate(age = "5 - 12-17 year olds")

rate_by_child_age_data <- neonates_covid_rates %>% 
  select(month, rate) %>% 
  mutate(age = "1 - neonates") %>% 
  bind_rows(post_neonates_total_covid_chart, one_to_four_yo_covid_chart, five_to_eleven_yo_covid_chart, twelve_to_seventeen_yo_covid_chart) %>% 
  filter(month != "Total")

fig_2 <- ggplot(rate_by_child_age_data, aes(month, rate, colour = age, group = age)) +
  geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position="bottom", axis.title = element_text(size = 9.5)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  +
  labs(x = "Month of onset", y = "Rate of confirmed SARS-CoV-2\ninfection per 100,000", color = "Age group") +
  scale_colour_colorblind(labels = c("Neonates", "Post-neonates", "1-4 year olds", "5-11 year olds", "12-17 year olds"))
fig_2
insertPlot(template, "Overall Rate", width = 7, startCol = 8, startRow = 14,
           height = 3.5, fileType = "png", units = "in")

ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/fig_2_comparison_rates_over_time.jpeg"), dpi = 320, width=5.3, height=3.4)

combinedfigure <- plot_grid(fig_1, fig_2,
                    labels = c("A", "B"), label_x = 0, label_y = 0.1,
                    hjust = -0.5, vjust = -1,
                    ncol = 1, nrow = 2)
combinedfigure
ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/combined_figure.jpeg"), dpi = 320, width=5.3, height=6)

writeData(template, "Overall Rate", select(post_neonates_total_covid, -c(month)), startCol = 2, startRow = 31, colNames = FALSE)
writeData(template, "Overall Rate", select(one_to_four_yo_covid, -c(month)), startCol = 5, startRow = 31, colNames = FALSE)
writeData(template, "Overall Rate", select(five_to_eleven_yo_covid, -c(month)), startCol = 8, startRow = 31, colNames = FALSE)
writeData(template, "Overall Rate", select(twelve_to_seventeen_yo_covid, -c(month)), startCol = 11, startRow = 31, colNames = FALSE)
writeData(template, "Overall Rate", select(pregnancy_rates, -c(month)), startCol = 2, startRow = 63, colNames = FALSE)


writeData(template, "Maternal Demographics", select(neonates_covid_rates_maternal_age, -c(maternal_age_group)), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "Maternal Demographics", select(neonates_covid_rates_simd, -c(simd)), startCol = 2, startRow = 14, colNames = FALSE)
writeData(template, "Maternal Demographics", select(neonates_covid_rates_ethnicity, -c(ethnicity_description)), startCol = 2, startRow = 23, colNames = FALSE)


writeData(template, "Baby Information", select(neonates_covid_rates_baby_sex, -c(x_baby_sex)), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "Baby Information", select(neonates_covid_rates_gestation, -c(gestation_at_birth)), startCol = 2, startRow = 9, colNames = FALSE)

writeData(template, "Date of onset", select(age_table, -c(days_old_had_covid_test)), startCol = 2, startRow = 3, colNames = FALSE)
# plot cumulative incidence 
ggplot(age_data, aes(days_old_had_covid_test, cumulative)) +
  geom_step() +
  xlab("Days old at first positive SARS-CoV-2 test") + ylab("Cumulative number")+ 
  scale_x_continuous(breaks = seq(0, 27, by = 5)) + 
  scale_y_continuous(breaks = seq(0, 140, by = 20))
insertPlot(template, "Date of onset", width = 6, startCol = 5, startRow = 2,
           height = 3, fileType = "png", units = "in")
ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/fig_3_cumulative_infections_age.jpeg"), dpi = 320)

ggplot(age_data, aes(days_old_had_covid_test, n)) +
  geom_line() +
  xlab("Days old at first positive SARS-CoV-2 test") + ylab("Number of infections")+ 
  scale_x_continuous(breaks = seq(0, 27, by = 5)) + 
  scale_y_continuous(breaks = seq(0, 10, by = 2))
insertPlot(template, "Date of onset", width = 6, startCol = 5, startRow = 19,
           height = 3, fileType = "png", units = "in")  
ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/fig_4_non-cumulative_infections_age.jpeg"), dpi = 320)

writeData(template, "Maternal Infection Status", select(neonates_covid_rates_maternal_infection_status, -c(maternal_infection_status_14_days_before_delivery)), startCol = 2, startRow = 4, colNames = FALSE)
writeData(template, "Maternal Infection Status", covid_babies_infected_mothers, startCol = 1, startRow = 12, colNames = FALSE)


covid_admission_rate <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/covid_admission_rate.rds"))
no_covid_admission_rate <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/no_covid_admission_rate.rds"))

writeData(template, "Outcomes", covid_admission_rate, startCol = 2, startRow = 3, colNames = FALSE)
writeData(template, "Outcomes", no_covid_admission_rate, startCol = 2, startRow = 4, colNames = FALSE)

covid_death_rate <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/covid_death_rate.rds"))
no_covid_death_rate <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/no_covid_death_rate.rds"))

writeData(template, "Outcomes", covid_death_rate, startCol = 2, startRow = 8, colNames = FALSE)
writeData(template, "Outcomes", no_covid_death_rate, startCol = 2, startRow = 9, colNames = FALSE)

# Number of babies with covid-associated admission
writeData(template, "Additional info on adm cases", nrow(covid_babies), startCol = 2, startRow = 3, colNames = FALSE)
all_covid_admissions <- read_csv(paste0(folder_temp_data, "neonate_paper_temp/covid_neonate_admissions.csv")) %>% 
  filter(time_associated_neonatal == 1 | time_associated_paeds == 1)
writeData(template, "Additional info on adm cases", nrow(all_covid_admissions), startCol = 2, startRow = 4, colNames = FALSE)

# SBR info, all babies
writeData(template, "Additional info on adm cases", nrow(associated_sbr_admissions), startCol = 2, startRow = 7, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nicu_table, -c(nicu)), startCol = 2, startRow = 12, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nosocomial_sbr_table, -c(nosocomial)), startCol = 2, startRow = 16, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_code, startCol = 2, startRow = 20, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_no_code, startCol = 3, startRow = 20, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_total, startCol = 4, startRow = 20, colNames = FALSE)

#SMR01 info, all babies
writeData(template, "Additional info on adm cases", nrow(associated_smr01_admissions), startCol = 2, startRow = 22, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(picu_table, -c(picu)), startCol = 2, startRow = 27, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nosocomial_smr01_table, -c(nosocomial)), startCol = 2, startRow = 31, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_code, startCol = 2, startRow = 35, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_no_code, startCol = 3, startRow = 35, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_total, startCol = 4, startRow = 35, colNames = FALSE)

# Number of term babies with covid-associated admission
writeData(template, "Additional info on adm cases", nrow(term_covid_babies), startCol = 2, startRow = 43, colNames = FALSE)
writeData(template, "Additional info on adm cases", nrow(overall_term_admissions), startCol = 2, startRow = 44, colNames = FALSE)

# SBR info, term babies
writeData(template, "Additional info on adm cases", nrow(associated_sbr_admissions_term), startCol = 2, startRow = 46, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nicu_table_term, -c(nicu)), startCol = 2, startRow = 51, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nosocomial_sbr_table_term, -c(nosocomial)), startCol = 2, startRow = 55, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_code_term, startCol = 2, startRow = 59, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_no_code_term, startCol = 3, startRow = 59, colNames = FALSE)
writeData(template, "Additional info on adm cases", sbr_los_total_term, startCol = 4, startRow = 59, colNames = FALSE)

#SMR01 info, term babies
writeData(template, "Additional info on adm cases", nrow(associated_smr01_admissions_term), startCol = 2, startRow = 61, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(picu_table_term, -c(picu)), startCol = 2, startRow = 66, colNames = FALSE)
writeData(template, "Additional info on adm cases", select(nosocomial_smr01_table_term, -c(nosocomial)), startCol = 2, startRow = 69, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_code_term, startCol = 2, startRow = 74, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_no_code_term, startCol = 3, startRow = 74, colNames = FALSE)
writeData(template, "Additional info on adm cases", smr01_los_total_term, startCol = 4, startRow = 74, colNames = FALSE)

# admission rate over time
writeData(template, "Admission rate over time", select(admissions_table, -c(month)), startCol = 2, startRow = 3, colNames = FALSE)
admission_rate_chart_data <- admissions_table %>% 
  select(month, rate, admission_rate, coded_admission_rate) %>% 
  rename(a_rate = rate, b_admission_rate = admission_rate, c_coded_admission_rate = coded_admission_rate) %>% 
  pivot_longer(cols = !month) %>% 
  filter(month != "Total")
ggplot(admission_rate_chart_data, aes(month, value, colour = name, group = name)) +
  geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position="bottom") +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))  +
  scale_colour_colorblind(labels = c("Overall rate of confirmed SARS-CoV-2 infection in neonates", "Overall rate of confirmed SARS-CoV-2 infection with temporally associated admission in neonates", 
                                     "Overall rate of confirmed SARS-CoV-2 infection with temporally associated admission with\nCOVID-19 as main diagnosis in neonates")) +
  labs(x = "Month of onset", y = "Rate per 100,000 \nlive births", color = "")
insertPlot(template, "Admission rate over time", width = 6.8, startCol = 9, startRow = 2,
           height = 4, fileType = "png", units = "in")  
ggsave(paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/fig_5_admission_rates_over_time.jpeg"), dpi = 320, width = 6.8,
       height = 4)


#### Save out workbook ####
saveWorkbook(template, (paste0(folder_outputs, "Ad-Hoc-Analysis/Neonatal-infections/Neonatal_infection_paper_output_", Sys.Date(), ".xlsx")), overwrite =TRUE)

