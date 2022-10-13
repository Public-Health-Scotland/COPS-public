# SETUP ####
#removed for public release. ##


# READ IN DATA ####
cohort <- read_rds(file.path(folder_temp_data, "script6b_pregnancy_level_record.rds")) %>% 
  filter(chi_validity == "Valid CHI") %>% 
  select(-chi_validity)

end_date <- today() 

infections_end <- as.Date("2022-04-30")
## SMR01 ####
df_smr01 <- read_rds(paste0(folder_temp_data, "smr01_flagged_stays.rds"))

smr01_stays <- df_smr01 %>% 
  select(mother_upi, admission_date, discharge_date, data_source) 


## SMR02 ####
df_smr02 <- read_rds(paste0(folder_temp_data, "smr02_flagged_episodes.rds"))

smr02_stays <- df_smr02 %>% 
  select(mother_upi, admission_date, discharge_date, data_source) 


## SMR01 and SMR02 combined ####
combined_smr_stays <- smr01_stays %>% 
  bind_rows(smr02_stays) %>% 
  mutate(adm_id = row_number()) %>% select(adm_id, everything()) # useful dummy id, unique for each admission

#dates
dataset_dates("SMR01_and_SMR02", combined_smr_stays$admission_date)

## DEATHS ####
df_deaths <- read_rds(paste0(folder_temp_data, "deaths_flagged.rds"))

deaths <- df_deaths %>% 
  select(mother_upi, date_of_death, flag_acute_covid19, data_source)


## SICSAG ####
df_icu <- read_rds(paste0(folder_temp_data, "sicsag_episodes.rds"))

icu <- df_icu %>% 
  select(mother_upi, admission_date, discharge_date, covid_ic_uor_hdu, preg_status, data_source)

icu$covid_ic_uor_hdu %>% table() # majority are "unit no longer in use or obsetrics not included"

#dates
dataset_dates("SICSAG", icu$admission_date)


# COVID RELATED SMR01/SMR02 ADMISSIONS ####

covid_associated_admission_days <- 14 # an admission is COVID associated if the +ve test is within the admission, or this number of days before the admission

# one row per covid test, preg start/end dates, with COVID +ve test dates
infections_during_pregnancy <- cohort %>%
  select(mother_upi, pregnancy_id, est_conception_date, pregnancy_end_date, starts_with("mother_positive_test_during_pregnancy_"), ends_with("vacc_occurence_date")) %>%
  pivot_longer(starts_with("mother_positive_test_during_pregnancy_"), names_to = "covid_positive_test_num", values_to = "covid_positive_test_date", names_prefix = "mother_positive_test_during_pregnancy_") %>%
  filter(is.na(covid_positive_test_date) == FALSE) %>%
  arrange(mother_upi, covid_positive_test_num) %>%
  filter(is.na(covid_positive_test_date) | covid_positive_test_date <= infections_end)

# join on the admissions data and flag if admissions are COVID associated....
# note: there are rows for each combination of test and admission -- e.g. if a mother had two +ve tests during a pregnancy and three admissions, they will have 6 rows
# pregnancy_id, test_num, and adm_id fields used to disambiguate and filter during aggregation
infections_during_pregnancy_flagged_admissions <- infections_during_pregnancy %>%
  left_join(combined_smr_stays) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(flag_covid_in_stay = covid_positive_test_date %within% stay_interval) %>% # TRUE if +ve test taken during the stay
  mutate(admission_to_test_days = covid_positive_test_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_covid_temporal_stay = if_else(admission_to_test_days >= 0 & admission_to_test_days <= covid_associated_admission_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 14 days before admission
  mutate(flag_covid_associated = if_else(flag_covid_temporal_stay == TRUE | flag_covid_in_stay == TRUE, TRUE, FALSE)) %>%  # TRUE if either previous flag TRUE
  select(-admission_to_test_days, -stay_interval) %>% 
  mutate(month = format(as.Date(covid_positive_test_date), "%Y-%m"),
         week_ending = floor_date(covid_positive_test_date, unit = "weeks", week_start = 1) + days(6)) %>% 
  mutate(gestation_days = 14 + (est_conception_date %--% covid_positive_test_date / days(1))) %>%
  mutate(trimester = trimester(est_conception_date, covid_positive_test_date))

## MONTHLY INFECTIONS OUTPUT ####
# this should match existing output
infections_during_pregnancy_monthly <- infections_during_pregnancy_flagged_admissions %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_infections = n()) %>% 
  arrange(month)

infections_during_pregnancy_monthly

# monthly infections in pregnancy with associated admission
# subset of the above with an associated SMR01/02 admission
infections_during_pregnancy_flagged_admissions_monthly <- infections_during_pregnancy_flagged_admissions %>% 
  filter(flag_covid_associated == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)

# monthly admissions in pregnancy by trimester ####
flagged_admissions_monthly_by_trimester <- infections_during_pregnancy_flagged_admissions %>% 
  filter(flag_covid_associated == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, trimester) %>% 
  distinct() %>% 
  group_by(month, trimester) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)

# add together into table by trimester
admissions_monthly_by_trimester <- infections_during_pregnancy_flagged_admissions_monthly %>% 
  mutate(trimester = "Total") %>% 
  bind_rows(flagged_admissions_monthly_by_trimester) %>% 
  mutate(admission = "2 - any hospital admission")

# read out for use in infections output script
admissions_monthly_by_trimester %>% write_rds(paste0(folder_temp_data, "infection_output_tables/admissions_by_trimester.rds"))


## admissions by vaccination status ####
admissions_by_vaccination <- infections_during_pregnancy_flagged_admissions %>% 
  filter(flag_covid_associated == TRUE) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, covid_positive_test_date)) %>% 
  select(mother_upi, covid_positive_test_date, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  group_by(month, vaccination_status_at_infection) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)
  
# add together into table by vaccination status 
admissions_monthly_by_vaccination <-  infections_during_pregnancy_flagged_admissions_monthly %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  bind_rows(admissions_by_vaccination)%>% 
  mutate(admission = "2 - any hospital admission")

# read out for use in infections output script
admissions_monthly_by_vaccination %>% write_rds(paste0(folder_temp_data, "infection_output_tables/admissions_by_vaccination.rds"))
  

# COVID RELATED SICSAG ####
covid_associated_icu_days <- 21

infections_during_pregnancy_flagged_icu <- infections_during_pregnancy %>%
  left_join(icu) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(flag_covid_in_stay = covid_positive_test_date %within% stay_interval) %>% # TRUE if +ve test taken during the stay
  mutate(admission_to_test_days = covid_positive_test_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_covid_temporal_stay = if_else(admission_to_test_days >= 0 & admission_to_test_days <= covid_associated_icu_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 21 days before admission
  mutate(flag_covid_associated = if_else(flag_covid_temporal_stay == TRUE | flag_covid_in_stay == TRUE, TRUE, FALSE)) %>%  # TRUE if either previous flag TRUE
  select(-admission_to_test_days, -stay_interval) %>% 
  mutate(month = format(as.Date(covid_positive_test_date), "%Y-%m"),
         week_ending = floor_date(covid_positive_test_date, unit = "weeks", week_start = 1) + days(6)) %>% 
  mutate(gestation_days = 14 + (est_conception_date %--% covid_positive_test_date / days(1))) %>%
  mutate(trimester = trimester(est_conception_date, covid_positive_test_date))

# monthly infections in pregnancy with associated ICU admission
infections_during_pregnancy_flagged_icu_monthly <- infections_during_pregnancy_flagged_icu %>% 
  filter(flag_covid_associated == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)

# monthly ICU admissions in pregnancy by trimester ####
flagged_icu_monthly_by_trimester <- infections_during_pregnancy_flagged_icu %>% 
  filter(flag_covid_associated == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, trimester) %>% 
  distinct() %>% 
  group_by(month, trimester) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)

# add together into table by trimester
icu_monthly_by_trimester <- infections_during_pregnancy_flagged_icu_monthly %>% 
  mutate(trimester = "Total") %>% 
  bind_rows(flagged_icu_monthly_by_trimester) %>% 
  mutate(admission = "3 - ICU")

# read out for use in infections output script
icu_monthly_by_trimester %>% write_rds(paste0(folder_temp_data, "infection_output_tables/icu_admissions_by_trimester.rds"))


## admissions by vaccination status ####
icu_by_vaccination <- infections_during_pregnancy_flagged_icu %>% 
  filter(flag_covid_associated == TRUE) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, covid_positive_test_date)) %>% 
  select(mother_upi, covid_positive_test_date, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  group_by(month, vaccination_status_at_infection) %>% 
  summarise(n_covid_adm = n()) %>% 
  arrange(month)

# add together into table by vaccination status 
icu_monthly_by_vaccination <-  infections_during_pregnancy_flagged_icu_monthly %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  bind_rows(icu_by_vaccination)%>% 
  mutate(admission = "3 - ICU")

# read out for use in infections output script
icu_monthly_by_vaccination %>% write_rds(paste0(folder_temp_data, "infection_output_tables/icu_admissions_by_vaccination.rds"))
  

# COVID RELATED DEATHS ####
maternal_covid_death_days <- 28 # "Maternal death within 28 days is defined as death of the woman from any cause occurring on or up to 27 days following the date of onset of COVID-19"
subsequent_maternal_death_days <- 41 # "Any subsequent maternal death is defined as death of the woman from any cause occurring on or or at any point following the date of onset of COVID-19, up to 41 days following the end date of pregnancy"
one_year_maternal_death_days <- 365

infections_during_pregnancy_flagged_death <- infections_during_pregnancy %>% 
  left_join(deaths) %>% 
  mutate(covid_associated_death_interval = interval(covid_positive_test_date, covid_positive_test_date + days(27))) %>% 
  mutate(follow_up_date = if_else(is.na(pregnancy_end_date), end_date, pregnancy_end_date)) %>% 
  mutate(covid_subsequent_death_interval = interval(start = covid_positive_test_date, 
                                                    end =  follow_up_date + days(subsequent_maternal_death_days))) %>% 
  mutate(covid_oneyear_death_interval = interval(start = covid_positive_test_date, 
                                                    end =  follow_up_date + days(one_year_maternal_death_days))) %>% 
  mutate(flag_covid_associated_death = if_else(date_of_death %within% covid_associated_death_interval, TRUE, FALSE)) %>%
  mutate(flag_covid_subsequent_death = if_else(date_of_death %within% covid_subsequent_death_interval, TRUE, FALSE)) %>% 
  mutate(flag_covid_oneyear_subsequent_death = if_else(date_of_death %within% covid_oneyear_death_interval, TRUE, FALSE)) %>% 
  mutate(month = format(as.Date(covid_positive_test_date), "%Y-%m"),
         week_ending = floor_date(covid_positive_test_date, unit = "weeks", week_start = 1) + days(6)) %>% 
  mutate(gestation_days = 14 + (est_conception_date %--% covid_positive_test_date / days(1))) %>%
  mutate(trimester = trimester(est_conception_date, covid_positive_test_date)) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, covid_positive_test_date))
  
infections_during_pregnancy_flagged_death %>% write_rds(paste0(folder_temp_data, "infection_output_tables/infections_during_pregnancy_flagged_death.rds"))


# monthly infections in pregnancy with COVID associated death
infections_during_pregnancy_flagged_death_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_covid_deaths = n()) %>% 
  arrange(month)

# monthly infections in pregnancy with COVID subsequent death
infections_during_pregnancy_flagged_subsequent_death_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_covid_subsequent_deaths = n()) %>% 
  arrange(month)

# monthly infections in pregnancy with one year COVID subsequent death
infections_during_pregnancy_flagged_oneyear_subsequent_death_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_oneyear_subsequent_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n_covid_oneyear_deaths = n()) %>% 
  arrange(month)

# inspect the cause of death codes for those in the one year subsequent death group
oneyear_subsequent_death_upis <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_oneyear_subsequent_death == TRUE) %>% pull(mother_upi)

df_deaths %>% filter(mother_upi %in% oneyear_subsequent_death_upis)


# maternal deaths by trimester

# monthly infections in pregnancy with COVID associated death within 28 days
mat_covid_associated_deaths_trimester_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester)

mat_covid_associated_deaths_trimester_monthly_row_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

mat_covid_associated_deaths_trimester_monthly_row_col_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

mat_covid_associated_deaths_trimester <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  select(mother_upi, trimester) %>% 
  distinct() %>% 
  count(trimester) %>% 
  mutate(month = "Total") %>% 
  bind_rows(mat_covid_associated_deaths_trimester_monthly) %>% 
  bind_rows(mat_covid_associated_deaths_trimester_monthly_row_total) %>% 
  bind_rows(mat_covid_associated_deaths_trimester_monthly_row_col_total) %>% 
  mutate(indicator = "covid associated death")

# monthly infections in pregnancy with COVID subsequent death
mat_subsequent_deaths_trimester_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester)

mat_subsequent_deaths_trimester_monthly_row_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

mat_subsequent_deaths_trimester_monthly_row_col_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

mat_subsequent_deaths_trimester <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  select(mother_upi, trimester) %>% 
  distinct() %>% 
  count(trimester) %>% 
  mutate(month = "Total") %>% 
  bind_rows(mat_subsequent_deaths_trimester_monthly) %>% 
  bind_rows(mat_subsequent_deaths_trimester_monthly_row_total) %>% 
  bind_rows(mat_subsequent_deaths_trimester_monthly_row_col_total) %>% 
  mutate(indicator = "subsequent maternal death")

# 
mat_subsequent_deaths_trimester %>% 
  bind_rows(mat_covid_associated_deaths_trimester) %>% write_rds(paste0(folder_temp_data, "infection_output_tables/maternal_deaths_by_gestation.rds"))


# maternal deaths by vaccination status

# monthly infections in pregnancy with COVID associated death within 28 days
mat_covid_associated_deaths_vacc_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection)

mat_covid_associated_deaths_vacc_monthly_row_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  select(mother_upi, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection) 

mat_covid_associated_deaths_vacc_monthly_row_col_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection) 

mat_covid_associated_deaths_vacc <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_associated_death == TRUE) %>% 
  select(mother_upi, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(vaccination_status_at_infection) %>% 
  mutate(month = "Total") %>% 
  bind_rows(mat_covid_associated_deaths_vacc_monthly) %>% 
  bind_rows(mat_covid_associated_deaths_vacc_monthly_row_total) %>% 
  bind_rows(mat_covid_associated_deaths_vacc_monthly_row_col_total) %>% 
  mutate(indicator = "covid associated death")

# monthly infections in pregnancy with COVID subsequent death
mat_subsequent_deaths_vacc_monthly <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  select(mother_upi, covid_positive_test_date, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection)

mat_subsequent_deaths_vacc_monthly_row_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  select(mother_upi, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection) 

mat_subsequent_deaths_vacc_monthly_row_col_total <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(month, vaccination_status_at_infection) 

mat_subsequent_deaths_vacc <- infections_during_pregnancy_flagged_death %>% 
  filter(flag_covid_subsequent_death == TRUE) %>% 
  select(mother_upi, vaccination_status_at_infection) %>% 
  distinct() %>% 
  count(vaccination_status_at_infection) %>% 
  mutate(month = "Total") %>% 
  bind_rows(mat_subsequent_deaths_vacc_monthly) %>% 
  bind_rows(mat_subsequent_deaths_vacc_monthly_row_total) %>% 
  bind_rows(mat_subsequent_deaths_vacc_monthly_row_col_total) %>% 
  mutate(indicator = "subsequent maternal death")

# 
mat_subsequent_deaths_vacc %>% 
  bind_rows(mat_covid_associated_deaths_vacc) %>% write_rds(paste0(folder_temp_data, "infection_output_tables/maternal_deaths_by_vaccination.rds"))



# COMBINED MONTHLY INFECTIONS TABLE ####
out <- infections_during_pregnancy_monthly %>% 
  left_join(infections_during_pregnancy_flagged_admissions_monthly) %>% 
  left_join(infections_during_pregnancy_flagged_icu_monthly) %>% 
  left_join(infections_during_pregnancy_flagged_death_monthly) %>% 
  left_join(infections_during_pregnancy_flagged_subsequent_death_monthly) %>% 
  left_join(infections_during_pregnancy_flagged_oneyear_subsequent_death_monthly) %>% 
  mutate(across(starts_with("n_"), ~replace_na(., 0)))

out



#### Vaccination serious outcomes ####

vaccinations_during_pregnancy <- cohort %>%
  select(mother_upi, pregnancy_id, overall_outcome, est_conception_date, pregnancy_end_date, ends_with("vacc_occurence_date"), ends_with ("vacc_product_name")) %>%
  pivot_longer(!c(pregnancy_id, mother_upi, est_conception_date, pregnancy_end_date, overall_outcome),
               names_to = c("vacc_dose_number", ".value"), 
               names_pattern = "dose_(.)_(.*)", 
               values_drop_na = TRUE)  %>% 
  arrange(mother_upi, vacc_occurence_date) %>% 
  mutate(pregnancy_follow_up_date = if_else(is.na(pregnancy_end_date), end_date, pregnancy_end_date)) %>% 
  mutate(pregnancy_interval = interval(est_conception_date, pregnancy_follow_up_date)) %>% 
  mutate(vaccine_in_pregnancy = vacc_occurence_date %within% pregnancy_interval) %>% 
  filter(vaccine_in_pregnancy == TRUE) %>% 
  mutate(month = format(as.Date(vacc_occurence_date), "%Y-%m"),
         week_ending = floor_date(vacc_occurence_date, unit = "weeks", week_start = 1) + days(6)) %>% 
  mutate(gestation_days = 14 + (est_conception_date %--% vacc_occurence_date / days(1))) %>%
  mutate(trimester = trimester(est_conception_date, vacc_occurence_date))

  
# VACCINE RELATED ADMISSIONS ####
vaccine_associated_hospital_days <- 14

vaccine_during_pregnancy_flagged_admission <- vaccinations_during_pregnancy %>%
  left_join(combined_smr_stays) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(admission_to_vaccine_days = vacc_occurence_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_vaccine_associated = if_else(admission_to_vaccine_days >= 0 & admission_to_vaccine_days <= vaccine_associated_hospital_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 14 days before admission
  select(-admission_to_vaccine_days, -stay_interval)

# monthly vaccinations in pregnancy with associated admission
vaccine_during_pregnancy_flagged_admission_monthly <- vaccine_during_pregnancy_flagged_admission %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# monthly admissions in pregnancy by trimester ####
vaccine_admission_monthly_by_trimester <- vaccine_during_pregnancy_flagged_admission %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, trimester) %>% 
  distinct() %>% 
  group_by(month, trimester) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# add together into table by trimester
admission_monthly_by_trimester <- vaccine_during_pregnancy_flagged_admission_monthly %>% 
  mutate(trimester = "Total") %>% 
  bind_rows(vaccine_admission_monthly_by_trimester) %>% 
  mutate(indicator = "2 - any hospital admission")

admission_monthly_by_trimester %>% 
  rename(Stage_of_Preg = trimester, month_vaccine = month) %>% 
  write_rds(paste0(folder_temp_data, "vaccine_output_tables/admission_by_trimester.rds"))



# monthly admissions in pregnancy by product ####
vaccine_admission_monthly_by_product <- vaccine_during_pregnancy_flagged_admission %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, vacc_product_name) %>% 
  distinct() %>% 
  group_by(month, vacc_product_name) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# add together into table by product
admission_monthly_by_product <- vaccine_during_pregnancy_flagged_admission_monthly %>% 
  mutate(vacc_product_name = "Total - all types") %>% 
  bind_rows(vaccine_admission_monthly_by_product) %>% 
  mutate(indicator = "2 - any hospital admission")

admission_monthly_by_product %>% 
  rename(month_vaccine = month) %>% 
  write_rds(paste0(folder_temp_data, "vaccine_output_tables/admission_by_product.rds"))  

# VACCINE RELATED SICSAG ####
vaccine_associated_icu_days <- 21

vaccine_during_pregnancy_flagged_icu <- vaccinations_during_pregnancy %>%
  left_join(icu) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(admission_to_vaccine_days = vacc_occurence_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_vaccine_associated = if_else(admission_to_vaccine_days >= 0 & admission_to_vaccine_days <= vaccine_associated_icu_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 21 days before admission
  select(-admission_to_vaccine_days, -stay_interval)

# monthly vaccinations in pregnancy with associated ICU admission
vaccine_during_pregnancy_flagged_icu_monthly <- vaccine_during_pregnancy_flagged_icu %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month) %>% 
  distinct() %>% 
  group_by(month) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# monthly ICU admissions in pregnancy by trimester ####
vaccine_icu_monthly_by_trimester <- vaccine_during_pregnancy_flagged_icu %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, trimester) %>% 
  distinct() %>% 
  group_by(month, trimester) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# add together into table by trimester
icu_monthly_by_trimester <- vaccine_during_pregnancy_flagged_icu_monthly %>% 
  mutate(trimester = "Total") %>% 
  bind_rows(vaccine_icu_monthly_by_trimester) %>% 
  mutate(indicator = "3 - ICU")

icu_monthly_by_trimester %>% 
  rename(Stage_of_Preg = trimester, month_vaccine = month) %>% 
  write_rds(paste0(folder_temp_data, "vaccine_output_tables/icu_by_trimester.rds"))



# monthly ICU admissions in pregnancy by product ####
vaccine_icu_monthly_by_product <- vaccine_during_pregnancy_flagged_icu %>% 
  filter(flag_vaccine_associated == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, vacc_product_name) %>% 
  distinct() %>% 
  group_by(month, vacc_product_name) %>% 
  summarise(n = n()) %>% 
  arrange(month)

# add together into table by product
icu_monthly_by_product <- vaccine_during_pregnancy_flagged_icu_monthly %>% 
  mutate(vacc_product_name = "Total - all types") %>% 
  bind_rows(vaccine_icu_monthly_by_product) %>% 
  mutate(indicator = "3 - ICU")

icu_monthly_by_product %>% 
  rename(month_vaccine = month) %>% 
  write_rds(paste0(folder_temp_data, "vaccine_output_tables/icu_by_product.rds"))


# VACCINE RELATED MATERNAL DEATHS ####
vaccinations_during_pregnancy_flagged_death <- vaccinations_during_pregnancy %>% 
  left_join(deaths) %>% 
  mutate(vacc_associated_death_interval = interval(vacc_occurence_date, vacc_occurence_date + days(27))) %>% 
  mutate(follow_up_date = if_else(is.na(pregnancy_end_date), end_date, pregnancy_end_date)) %>% 
  mutate(vacc_subsequent_death_interval = interval(start = vacc_occurence_date, 
                                                    end =  follow_up_date + days(subsequent_maternal_death_days))) %>% 
  mutate(flag_vacc_associated_death = if_else(date_of_death %within% vacc_associated_death_interval, TRUE, FALSE)) %>%
  mutate(flag_vacc_subsequent_death = if_else(date_of_death %within% vacc_subsequent_death_interval, TRUE, FALSE))


# monthly vaccinations in pregnancy with associated death
vaccine_death_monthly_by_trimester_monthly <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester)

vaccine_death_monthly_by_trimester_monthly_row_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

vaccine_death_monthly_by_trimester_monthly_row_col_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

vacc_associated_death_monthly_by_trimester <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  select(mother_upi, trimester) %>% 
  distinct() %>% 
  count(trimester) %>% 
  mutate(month = "Total") %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly) %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly_row_total) %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly_row_col_total) %>% 
  mutate(indicator = "associated maternal death")

# monthly vaccinations in pregnancy with subsequent death
vaccine_death_monthly_by_trimester_monthly <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester)

vaccine_death_monthly_by_trimester_monthly_row_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

vaccine_death_monthly_by_trimester_monthly_row_col_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  mutate(trimester = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, trimester) %>% 
  distinct() %>% 
  count(month, trimester) 

vacc_subsequent_death_monthly_by_trimester <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  select(mother_upi, trimester) %>% 
  distinct() %>% 
  count(trimester) %>% 
  mutate(month = "Total") %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly) %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly_row_total) %>% 
  bind_rows(vaccine_death_monthly_by_trimester_monthly_row_col_total) %>% 
  mutate(indicator = "subsequent maternal death")


## save out
vacc_associated_death_monthly_by_trimester %>% 
  bind_rows(vacc_subsequent_death_monthly_by_trimester) %>% 
  rename(Stage_of_Preg = trimester, month_vaccine = month) %>% 
   write_rds(paste0(folder_temp_data, "vaccine_output_tables/maternal_deaths_by_trimester.rds"))


# monthly deaths in pregnancy by product ####

# monthly vaccinations in pregnancy with associated death
vaccine_death_monthly_by_product_monthly <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name)

vaccine_death_monthly_by_product_monthly_row_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  mutate(vacc_product_name = "Total") %>% 
  select(mother_upi, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name) 

vaccine_death_monthly_by_product_monthly_row_col_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  mutate(vacc_product_name = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name) 

vacc_associated_death_monthly_by_product <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_associated_death == TRUE) %>% 
  select(mother_upi, vacc_product_name) %>% 
  distinct() %>% 
  count(vacc_product_name) %>% 
  mutate(month = "Total") %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly) %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly_row_total) %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly_row_col_total) %>% 
  mutate(indicator = "associated maternal death")

# monthly vaccinations in pregnancy with subsequent death
vaccine_death_monthly_by_product_monthly <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  select(mother_upi, vacc_occurence_date, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name)

vaccine_death_monthly_by_product_monthly_row_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  mutate(vacc_product_name = "Total") %>% 
  select(mother_upi, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name) 

vaccine_death_monthly_by_product_monthly_row_col_total <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  mutate(vacc_product_name = "Total") %>% 
  mutate(month = "Total") %>% 
  select(mother_upi, month, vacc_product_name) %>% 
  distinct() %>% 
  count(month, vacc_product_name) 

vacc_subsequent_death_monthly_by_product <- vaccinations_during_pregnancy_flagged_death %>% 
  filter(flag_vacc_subsequent_death == TRUE) %>% 
  select(mother_upi, vacc_product_name) %>% 
  distinct() %>% 
  count(vacc_product_name) %>% 
  mutate(month = "Total") %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly) %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly_row_total) %>% 
  bind_rows(vaccine_death_monthly_by_product_monthly_row_col_total) %>% 
  mutate(indicator = "subsequent maternal death")

## save out
vacc_associated_death_monthly_by_product %>% 
  bind_rows(vacc_subsequent_death_monthly_by_product) %>% 
  rename(month_vaccine = month) %>% 
  write_rds(paste0(folder_temp_data, "vaccine_output_tables/maternal_deaths_by_product.rds"))


