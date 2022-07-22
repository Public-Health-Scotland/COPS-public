
# import excel template (make excel template)

template <- loadWorkbook(paste0(folder_templates, "Infection in neonates_template.xlsx"))

#### data updatedness tab ####

dates <- readRDS(paste0(folder_temp_data, "all_dates.rds"))
overall_testing_chi_completeness_perc <- readRDS(file.path(paste(folder_temp_data, "overall_testing_chi_completeness_perc.rds", sep ="/")))
chi_completeness_lfd_pos_perc <- readRDS(file.path(paste(folder_temp_data, "lfd_pos_chi_completeness_perc.rds", sep ="/")))
chi_completeness_pcr_pos_perc <- readRDS(file.path(paste(folder_temp_data, "pcr_pos_chi_completeness_perc.rds", sep ="/")))
number_pos_neonates_testing <- readRDS(file.path(paste(folder_temp_data, "number_neonatal_infections_testing_only.rds", sep ="/")))

baby_level <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>%
  filter(outcome == "Live birth") %>%
  filter(x_full_cohort == T) %>% 
  filter(x_pregnancy_end_date <= publication_latest_vacc_date)

completed_pregnancies <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>%
  filter(x_full_cohort == T & outcome != "Ongoing") %>% 
  filter(x_pregnancy_end_date <= publication_latest_vacc_date)

# check if baby has valid chi (baby upi) using phs methods package (chicheck function)
baby_level <- baby_level %>%
  mutate(baby_chi_validity = chi_check(baby_upi))

# check for duplicates 
dups_babies_upi = baby_level %>%
  # select(baby_upi, mother_upi) %>%
  group_by(baby_upi) %>%
  filter(n()>1)

# number of live births 
n_babies <- baby_level %>% 
  group_by(baby_upi) %>%
  slice(1) %>%
  nrow()

# number of completed pregnancies
n_pregnancies <- completed_pregnancies %>% 
  group_by(pregnancy_id) %>%
  slice(1) %>%
  nrow()

# number of mothers 
n_mother <- completed_pregnancies %>% 
  group_by(mother_upi) %>%
  slice(1) %>%
  nrow()


temp_baby_valid_chi <- baby_level %>% # how many live births have a valid mother and baby chi
  select(baby_upi, baby_chi_validity, chi_validity) %>%
  group_by(baby_upi) %>%
  mutate(chi_validity_double = if_else(baby_chi_validity =="Valid CHI" & chi_validity == "Valid CHI", "Valid CHI", "Missing CHI")) %>% 
  slice(1) %>%
  tabyl(chi_validity_double) 

temp_preg_valid_chi <- completed_pregnancies %>% # how many completed pregnancies have a mother with a valid chi
  select(pregnancy_id, chi_validity) %>%
  group_by(pregnancy_id) %>%
  slice(1) %>%
  tabyl(chi_validity)

temp_wom_valid_chi <- completed_pregnancies %>% # how many mothers have a valid chi
  select(mother_upi, chi_validity) %>%
  group_by(mother_upi) %>%
  slice(1) %>%
  tabyl(chi_validity)


# number of babies with both mother and baby valid CHI
n_baby_valid_chi <-  temp_baby_valid_chi[2,2]
#Percent babies with both mother and baby valid CHI
perc_baby_valid_chi <-  format(round(temp_baby_valid_chi[2,3] * 100, digits = 1))

# number of pregnancies with valid chi
n_preg_valid_chi <-  temp_preg_valid_chi[2,2]
#Percent pregnancies with a valid CHI
perc_preg_valid_chi <-  format(round(temp_preg_valid_chi[2,3] * 100, digits = 1))


#Number women with valid CHI
n_wom_valid_chi <-  temp_wom_valid_chi[2,2]
#Percent women with valid CHI
perc_wom_valid_chi <-  format(round(temp_wom_valid_chi[2,3] * 100, digits = 1))


##################################
#### create babies data frame ####

babies <- baby_level %>%
  group_by(baby_upi) %>%
  slice(1) %>%
  filter(chi_validity == "Valid CHI", # Only include mothers with a valid CHI 
         baby_chi_validity == "Valid CHI") %>% # Only include babies with a valid CHI 
  select(-chi_validity) %>%
  mutate(x_hbres = case_when(is.na(x_hbres) ~ "z Not Known",
                             x_hbres=="England/Wales/Northern Ireland" ~ "z Not Known",
                             x_hbres=="Outside UK" ~ "z Not Known",
                             x_hbres=="Not Known" ~ "z Not Known",
                             T ~ x_hbres)) %>%
  mutate(maternal_age_group = cops_age_group(x_mother_age_at_conception)) %>%
  mutate(ethnicity_description = cops_reporting_ethnicity(x_ethnicity_code)) %>%
  mutate(simd = as.character(x_simd)) %>%
  mutate(simd = case_when(is.na(x_simd) ~ "9 Unknown",
                          x_simd=="9" ~ "9 Unknown",
                          T ~ simd)) %>%
  ungroup()


##############################
#### create month look up ####

month_lookup <-  babies %>% 
  filter(x_pregnancy_end_date <= publication_latest_vacc_date) %>% 
  arrange(x_pregnancy_end_date) %>% 
  mutate(month_in_words = format(as.Date(x_pregnancy_end_date), "%b %Y")) %>% 
  mutate(month = format(as.Date(x_pregnancy_end_date), "%Y-%m")) %>% 
  select(month_in_words, month) %>% distinct()

###################################
#### excel text for all sheets ####

subtitle <- paste0("Based on COPS pregnancy database updated in mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", and cases of COVID-19 with date of onset up to ", format(publication_latest_vacc_date, "%d %B %Y"))

############################
#### Data Sources Sheet ####
# double check data sources text 

data_sources_text_1 <- paste0("This publication is based on the mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " update of the COPS study database (based on data extracts taken around that time) and provides information on babies born up to the end of ", format(publication_latest_vacc_date, "%B %Y"))
data_sources_text_2 <- paste0("By mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", NHS live birth notifications for babies born up to end ", format(publication_latest_vacc_date, "%B %Y"), 
                              " will be complete, and hence we would expect this version of the COPS database to include almost all live births occurring up to end ", format(publication_latest_vacc_date, "%B %Y"))
data_sources_text_3 <- paste0("This release includes records of tests taken on up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), ".")
data_sources_text_4 <- paste0("The extract of testing data was taken from the NHS Scotland Corporate Data Warehouse on ", format(dates$read_in_date[dates$dataset == "Testing"], "%d %B %Y"), "." )
data_sources_text_6 <- paste0("This release includes records of vaccinations delivered on up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), ".")
data_sources_text_7 <- paste0("The extract of vaccination data was taken from the National Clinical Data Store on ", format(dates$read_in_date[dates$dataset == "Vaccines"], "%d %B %Y"), "." )
data_sources_text_8 <- paste0("Overall, ", chi_completeness_pcr_pos_perc, "% of all positive viral PCR test records for samples taken from the start of the COVID-19 pandemic to ", format(publication_latest_vacc_date, "%d %B %Y"), " have an associated CHI number")
data_sources_text_9 <- paste0("As at mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", the COPS database included a total of ", format(n_babies, big.mark=","), " live births from " 
                               ,format(n_pregnancies, big.mark=","), " completed pregnancies to ", format(n_mother, big.mark=","), " women: ") 
data_sources_text_10 <- paste0(format(n_baby_valid_chi, big.mark=","), " (", perc_baby_valid_chi, "%) of the live births (and ", format(n_preg_valid_chi, big.mark=","), ", ", perc_preg_valid_chi, "% of the pregnancies and ", format(n_wom_valid_chi, big.mark=","), ", ", perc_wom_valid_chi, "% of the women) have an associated CHI number.")
data_sources_text_11 <- paste0("and ", chi_completeness_lfd_pos_perc, "% of all positive viral LFD test records for samples taken from 6 Jan 2022 to ", format(publication_latest_vacc_date, "%d %B %Y"), " have an associated CHI number.")
data_sources_text_12 <- paste0("Looking at national testing data only (i.e. all test records, not just records that have linked to the COPS study database) shows that up to end ", format(publication_latest_vacc_date, "%B %Y"), " there has been a total of ", format(number_pos_neonates_testing, big.mark=",")," confirmed cases of COVID-19 in neonates.")


writeData(template, "Data sources", data_sources_text_1, startCol = 1, startRow = 15, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_2, startCol = 1, startRow = 16, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_3, startCol = 1, startRow = 32, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_4, startCol = 1, startRow = 33, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_6, startCol = 1, startRow = 44, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_7, startCol = 1, startRow = 45, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_8, startCol = 1, startRow = 49, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_11, startCol = 1, startRow = 50, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_9, startCol = 1, startRow = 51, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_10, startCol = 1, startRow = 52, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_12, startCol = 1, startRow = 54, colNames = FALSE)

#####################################
#### N cases rate by month sheet ####

# Overall confirmed cases of COVID-19 in neonates and infection rate #
# rows 7-9 # 
months_total_births <- babies %>% # calculate the total number of live births each month 
  filter(x_pregnancy_end_date <= publication_latest_vacc_date) %>%
  mutate(month = format(as.Date(x_pregnancy_end_date), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_live_births = n())

sum(months_total_births$total_live_births)

months_total_covid <- babies %>% 
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  filter(x_pregnancy_end_date <= publication_latest_vacc_date, 
         days_old_had_covid_test >=0 & days_old_had_covid_test <=27) %>%
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to date positive test was on
  group_by(month) %>%
  summarise(total_positive_covid_neonates = n())

sum(months_total_covid$total_positive_covid_neonates)

neonates_covid_rates <- months_total_covid %>%
  right_join(., months_total_births, by=c("month")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%
  arrange(month)

neonates_covid_rates <- neonates_covid_rates %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%
  mutate("Rate of COVID-19 in neonates (per 100,000 babies)" = total_positive_covid_neonates / total_live_births * 100000) %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_birth <- babies %>%
  filter(x_pregnancy_end_date <= publication_latest_vacc_date) %>%
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  group_by(baby_upi) %>%
  mutate(total_births = n()) %>%
  mutate(baby_tested_positive_within_28_days_life = ifelse(days_old_had_covid_test>=0&days_old_had_covid_test<=27, 1, 0)) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total_covid_babies = sum(baby_tested_positive_within_28_days_life, na.rm=T),
            total_babies = sum(total_births)) %>%
  mutate(percent = total_covid_babies / total_babies * 100000) %>%
  pivot_longer(cols = total_covid_babies:percent, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

neonates_covid_rates <- neonates_covid_rates %>%
  bind_cols(total_birth)

neonates_covid_rates %>% write_rds(paste0(folder_temp_data, "network_folder/neonates_covid_rates.rds"))

# add info to data sources sheet#
data_sources_text_13 <- paste0(format(sum(months_total_covid$total_positive_covid_neonates), big.mark=",")," of these cases are included in the COPS database and reported on here.")
writeData(template, "Data sources", data_sources_text_13, startCol = 1, startRow = 55, colNames = FALSE)

# Additional information on confirmed cases of COVID-19 in neonates #
# Age at diagnosis #
# rows 13 - 14 #

covid_babies <- babies %>% # create a data frame that includes only babies who tested positive in first 27 days 
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  mutate(days_between_mother_tests_positive_baby_born = case_when(is.na(tests_mother_value_positive_test_during_pregnancy_2)==T ~ round(difftime(tests_mother_value_positive_test_during_pregnancy_1, 
                                                                                                                                     x_pregnancy_end_date, units="days")), 
                                                                  TRUE ~ round(difftime(tests_mother_value_positive_test_during_pregnancy_2, x_pregnancy_end_date, units="days")))) %>%
  mutate(mother_vaccine_status = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, x_pregnancy_end_date)) %>%
  filter(x_pregnancy_end_date <= publication_latest_vacc_date, 
         days_old_had_covid_test >=0 & days_old_had_covid_test <=27)

months_baby_age_at_diagnosis <- covid_babies %>%
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to covid test date
  mutate(baby_tested_positive_within_6_days_life = ifelse(days_old_had_covid_test >= 0 & days_old_had_covid_test < 7, 1, 0), 
         baby_tested_positive_within_7_to_27_days_life = ifelse(days_old_had_covid_test >= 7 & days_old_had_covid_test <= 27, 1, 0)) %>%
  group_by(month) %>%
  summarise(zero_to_six_days = sum(baby_tested_positive_within_6_days_life), 
            seven_to_27_days = sum(baby_tested_positive_within_7_to_27_days_life)) %>%
  right_join(month_lookup) %>% # need to include months where there are no covid babies 
  mutate(zero_to_six_days = replace_na(zero_to_six_days, 0),
         seven_to_27_days = replace_na(seven_to_27_days, 0)) %>%
  arrange(month) 

baby_age_at_diagnosis <- months_baby_age_at_diagnosis %>%
  select(-month_in_words) %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% # and could not use pivot_longer above with month_in_words coz it is a character, so rejoining it here
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_ages <- months_baby_age_at_diagnosis %>%
  ungroup() %>%
  summarise(zero_to_six_days = sum(zero_to_six_days), 
            seven_to_27_days = sum(seven_to_27_days)) %>%
  pivot_longer(cols = zero_to_six_days:seven_to_27_days, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

baby_age_at_diagnosis <- baby_age_at_diagnosis %>%
  bind_cols(total_ages)

baby_age_at_diagnosis %>% write_rds(paste0(folder_temp_data, "network_folder/baby_age_at_diagnosis.rds"))




# Maternal vaccination status at delivery # 
# rows 21-23 #

month_maternal_vaccination_status <- covid_babies %>%
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% 
  mutate(mother_unvaccinated = ifelse(mother_vaccine_status=="0 - Unvaccinated", 1, 0), 
         mother_one_dose = ifelse(mother_vaccine_status == "1 - One dose", 1, 0), 
         mother_two_dose = ifelse(mother_vaccine_status == "2 - Two doses", 1, 0), 
         mother_three_dose = ifelse(mother_vaccine_status == "3 - Three doses", 1, 0)) %>% # will need to change to include 3rd doses
  group_by(month) %>%
  summarise(unvaccinated = sum(mother_unvaccinated), 
            one_dose = sum(mother_one_dose), 
            two_dose = sum(mother_two_dose), 
            three_dose = sum(mother_three_dose)) %>%
  right_join(month_lookup) %>% # need to include months where there are no covid babies 
  mutate(unvaccinated = replace_na(unvaccinated, 0),
         one_dose = replace_na(one_dose, 0), 
         two_dose = replace_na(two_dose, 0), 
         three_dose = replace_na(three_dose, 0)) %>%
  arrange(month) %>%
  select(-month_in_words)

month_maternal_vaccination_status %>% write_rds(paste0(folder_temp_data, "network_folder/maternal_vaccination_status_long.rds"))

maternal_vaccination_status <- month_maternal_vaccination_status %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% # and could not use pivot_longer above with month_in_words coz it is a character, so rejoining it here
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

sum(month_maternal_vaccination_status$unvaccinated) # 86
sum(month_maternal_vaccination_status$one_dose) # 4
sum(month_maternal_vaccination_status$two_dose) # 2
sum(month_maternal_vaccination_status$three_dose) 

total_maternal_vaccination_status <- month_maternal_vaccination_status %>%
  ungroup() %>%
  summarise(unvaccinated = sum(unvaccinated), 
            one_dose = sum(one_dose), 
            two_dose = sum(two_dose), 
            three_dose = sum(three_dose)) %>%
  pivot_longer(cols = unvaccinated:three_dose, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

maternal_vaccination_status <- maternal_vaccination_status %>%
  bind_cols(total_maternal_vaccination_status)

maternal_vaccination_status %>% write_rds(paste0(folder_temp_data, "network_folder/maternal_vaccination_status.rds"))

# write data to template # 

writeData(template, "N cases rate by month", subtitle, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "N cases rate by month", select(neonates_covid_rates, -c(indicator)), startCol=2, startRow=7, colNames = F)
writeData(template, "N cases rate by month", select(baby_age_at_diagnosis, -c(indicator)), startCol=2, startRow=13, colNames = F)
writeData(template, "N cases rate by month", select(maternal_vaccination_status, -c(indicator)), startCol=2, startRow=17, colNames = F)



##################################
#### N severe cases NFP sheet ####

# test <- babies %>%
#   filter(x_neonatal_death != "Survived neonatal period", 
#          tests_baby_has_tested_positive == TRUE) 

# Total confirmed cases of COVID-19 # 
# row 8-9 # 

severe_cases_months_total_covid <- covid_babies %>% 
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to date positive test was on
  mutate(subsequent_deaths = ifelse(x_neonatal_death != "Survived neonatal period", 1, 0)) %>%
  group_by(month) %>%
  summarise(total_positive_covid_neonates = n(),
            total_positive_deaths = sum(subsequent_deaths)) %>%
  right_join(month_lookup) %>% # need to include months where there are no covid babies 
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0),
         total_positive_deaths = replace_na(total_positive_deaths, 0)) %>%
  arrange(month) %>%
  select(-month_in_words)

sum(severe_cases_months_total_covid$total_positive_covid_neonates) 
sum(severe_cases_months_total_covid$total_positive_deaths) 

severe_cases_covid <- severe_cases_months_total_covid %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% # and could not use pivot_longer above with month_in_words coz it is a character, so rejoining it here
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_severe_cases_covid <- severe_cases_months_total_covid %>%
  ungroup() %>%
  summarise(total_positive_covid_neonates = sum(total_positive_covid_neonates), 
            total_positive_deaths = sum(total_positive_deaths)) %>%
  pivot_longer(cols = total_positive_covid_neonates:total_positive_deaths, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

severe_cases_covid <- severe_cases_covid %>%
  bind_cols(total_severe_cases_covid)

severe_cases_covid %>% write_rds(paste0(folder_temp_data, "network_folder/severe_cases_covid.rds"))


# Severe Cases - Baby age at diagnosis # 
# row 11-15 # 

severe_cases_baby_age_months <- covid_babies %>%
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to date positive test was on
  mutate(baby_tested_positive_within_6_days_life = ifelse(days_old_had_covid_test>=0&days_old_had_covid_test<=6, 1, 0),
         baby_tested_positive_within_7_to_27_days_life = ifelse(days_old_had_covid_test>=7&days_old_had_covid_test<=27, 1, 0)) %>%
  mutate(subsequent_deaths_diagnosis_within_6_days = ifelse(x_neonatal_death != "Survived neonatal period" & baby_tested_positive_within_6_days_life == 1, 1, 0), 
         subsequent_deaths_diagnosis_after_6_days = ifelse(x_neonatal_death != "Survived neonatal period" & baby_tested_positive_within_7_to_27_days_life == 1, 1, 0)) %>%  
  group_by(month) %>%
  summarise(zero_to_six_days = sum(baby_tested_positive_within_6_days_life),
            zero_to_six_days_deaths = sum(subsequent_deaths_diagnosis_within_6_days),
            seven_to_27_days = sum(baby_tested_positive_within_7_to_27_days_life), 
            seven_to_27_days_deaths = sum(subsequent_deaths_diagnosis_after_6_days)) %>%
  right_join(month_lookup) %>% # need to include months where there are no covid babies 
  mutate(zero_to_six_days = replace_na(zero_to_six_days, 0),         
         zero_to_six_days_deaths = replace_na(zero_to_six_days_deaths, 0),
         seven_to_27_days = replace_na(seven_to_27_days, 0), 
         seven_to_27_days_deaths = replace_na(seven_to_27_days_deaths, 0)) %>%
  arrange(month) %>%
  select(-month_in_words)

sum(severe_cases_baby_age_months$zero_to_six_days) 
sum(severe_cases_baby_age_months$zero_to_six_days_deaths) 
sum(severe_cases_baby_age_months$seven_to_27_days) 
sum(severe_cases_baby_age_months$seven_to_27_days_deaths)

severe_cases_baby_age <- severe_cases_baby_age_months %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% # and could not use pivot_longer above with month_in_words coz it is a character, so rejoining it here
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_severe_cases_baby_age <- severe_cases_baby_age_months %>%
  ungroup() %>%
  summarise(zero_to_six_days = sum(zero_to_six_days), 
            zero_to_six_days_deaths = sum(zero_to_six_days_deaths), 
            seven_to_27_days = sum(seven_to_27_days), 
            seven_to_27_days_deaths = sum(seven_to_27_days_deaths)) %>%
  pivot_longer(cols = zero_to_six_days:seven_to_27_days_deaths, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

severe_cases_baby_age <- severe_cases_baby_age %>%
  bind_cols(total_severe_cases_baby_age)

severe_cases_baby_age %>% write_rds(paste0(folder_temp_data, "network_folder/severe_cases_baby_age.rds"))




# severe outcomes - vaccination status # 
# row 24-29 #

severe_cases_maternal_vaccination_status_month <- covid_babies %>%
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% 
  mutate(mother_unvaccinated = ifelse(mother_vaccine_status=="0 - Unvaccinated", 1, 0), 
         mother_one_dose = ifelse(mother_vaccine_status == "1 - One dose", 1, 0), 
         mother_two_dose = ifelse(mother_vaccine_status == "2 - Two doses", 1, 0), 
         mother_three_dose = ifelse(mother_vaccine_status == "3 - Three doses", 1, 0)) %>% # will need to change to include 3rd doses
  mutate(subsequent_deaths_mother_unvaccinated = ifelse(x_neonatal_death != "Survived neonatal period" & mother_unvaccinated == 1, 1, 0), 
         subsequent_deaths_mother_one_dose = ifelse(x_neonatal_death != "Survived neonatal period" & mother_one_dose == 1, 1, 0), 
         subsequent_deaths_mother_two_dose = ifelse(x_neonatal_death != "Survived neonatal period" & mother_two_dose == 1, 1, 0), 
         subsequent_deaths_mother_three_dose = ifelse(x_neonatal_death != "Survived neonatal period" & mother_three_dose == 1, 1, 0)) %>%  
  group_by(month) %>%
  summarise(unvaccinated = sum(mother_unvaccinated),
            unvaccinated_deaths = sum(subsequent_deaths_mother_unvaccinated),
            one_dose = sum(mother_one_dose), 
            one_dose_deaths = sum(subsequent_deaths_mother_one_dose), 
            two_dose=sum(mother_two_dose), 
            two_dose_deaths = sum(subsequent_deaths_mother_two_dose), 
            three_dose = sum(mother_three_dose), 
            three_dose_deaths = sum(subsequent_deaths_mother_three_dose)) %>%
  right_join(month_lookup) %>% # need to include months where there are no covid babies 
  mutate(unvaccinated = replace_na(unvaccinated, 0),         
         unvaccinated_deaths = replace_na(unvaccinated_deaths, 0),
         one_dose = replace_na(one_dose, 0), 
         one_dose_deaths = replace_na(one_dose_deaths, 0), 
         two_dose = replace_na(two_dose, 0), 
         two_dose_deaths = replace_na(two_dose_deaths, 0), 
         three_dose = replace_na(three_dose, 0), 
         three_dose_deaths = replace_na(three_dose_deaths, 0)) %>%
  arrange(month) %>%
  select(-month_in_words)

sum(severe_cases_maternal_vaccination_status_month$unvaccinated) 
sum(severe_cases_maternal_vaccination_status_month$unvaccinated_deaths)
sum(severe_cases_maternal_vaccination_status_month$one_dose)
sum(severe_cases_maternal_vaccination_status_month$one_dose_deaths)
sum(severe_cases_maternal_vaccination_status_month$two_dose)
sum(severe_cases_maternal_vaccination_status_month$two_dose_deaths)
sum(severe_cases_maternal_vaccination_status_month$three_dose) 
sum(severe_cases_maternal_vaccination_status_month$three_dose_deaths) 

severe_cases_maternal_vaccination_status <- severe_cases_maternal_vaccination_status_month %>%
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>%
  left_join(month_lookup) %>% # and could not use pivot_longer above with month_in_words coz it is a character, so rejoining it here
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_severe_cases_vaccination_status <- severe_cases_maternal_vaccination_status_month %>%
  ungroup() %>%
  summarise(unvaccinated = sum(unvaccinated), 
            unvaccinated_deaths = sum(unvaccinated_deaths), 
            one_dose = sum(one_dose), 
            one_dose_deaths = sum(one_dose_deaths), 
            two_dose = sum(two_dose), 
            two_dose_deaths = sum(two_dose_deaths), 
            three_dose = sum(three_dose),
            three_dose_deaths = sum(three_dose_deaths)) %>%
  pivot_longer(cols = unvaccinated:three_dose_deaths, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

severe_cases_maternal_vaccination_status <- severe_cases_maternal_vaccination_status %>%
  bind_cols(total_severe_cases_vaccination_status)

severe_cases_maternal_vaccination_status %>% write_rds(paste0(folder_temp_data, "network_folder/severe_cases_maternal_vaccination_status.rds"))


# write data to template # 
writeData(template, "N severe cases NFP", subtitle, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "N severe cases NFP", select(severe_cases_covid, -c(indicator)), startCol=2, startRow=8, colNames = F)
writeData(template, "N severe cases NFP", select(severe_cases_baby_age, -c(indicator)), startCol=2, startRow=12, colNames = F)
writeData(template, "N severe cases NFP", select(severe_cases_maternal_vaccination_status, -c(indicator)), startCol=2, startRow=18, colNames = F)



### save template ###
saveWorkbook(template, (paste0(folder_outputs, "network_folder/Neonates_output_", Sys.Date(), ".xlsx")), overwrite = TRUE)








