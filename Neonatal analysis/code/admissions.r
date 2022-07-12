# run script 0b. setup.r in the main COPS code before this script

start_date <- as.Date("2020-03-01")
end_date <- as.Date("2022-01-31")

baby_level <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>%
  filter(outcome == "Live birth") %>%
  filter(x_full_cohort == T)

# check if baby has valid chi (baby upi) using phs methods package (chicheck function)
baby_level <- baby_level %>%
  mutate(baby_chi_validity = chi_check(baby_upi))


babies <- baby_level %>%
  group_by(baby_upi) %>%
  slice(1) %>%
  filter(baby_chi_validity == "Valid CHI") %>% # Only include babies with a valid CHI 
  select(-chi_validity) %>%
  ungroup()


## create df of babies who had covid
covid_babies <- babies %>% # create a data frame that includes only babies who tested positive in first 27 days 
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  filter(tests_baby_earliest_positive_test <= end_date, 
         days_old_had_covid_test >= 0 & days_old_had_covid_test <= 27)

covid_babies_upi <- covid_babies %>% 
  select(baby_upi)

covid_babies_gestation <- covid_babies %>% 
  select(baby_upi, x_gestation_at_outcome)

## read in SMR01 admissions data
# SMR01 ####
clear_temp_tables(SMRAConnection)
SMR01 <- SMRAConnection %>% tbl("SMR01_PI") # virtual table of all SMR01 episodes

extract_start_date <- (start_date - months(6)) %>% format("%d/%m/%Y") # CIS stays typically constructed from episodes up to six months prior
extract_end_date <- (end_date + months(2)) %>% format("%d/%m/%Y")

# extract all SMR01 episodes of interest (e.g. those in date range with matching mother UPI number) sorted into CIS order
df_eps <- SMR01 %>%
  filter(DISCHARGE_DATE >= TO_DATE(extract_start_date, "dd/mm/yyyy"),
         DISCHARGE_DATE <= TO_DATE(extract_end_date, "dd/mm/yyyy")) %>%
  inner_join(covid_babies_upi, copy = TRUE, by = c("UPI_NUMBER" = "baby_upi")) %>% 
  arrange(LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI) %>% 
  select(UPI_NUMBER, LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, SIGNIFICANT_FACILITY,
         MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5) %>% 
  collect() %>%
  clean_names() %>%
  mutate(across(contains("date"), as_date)) %>% 
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  mutate(picu = if_else("13" %in% significant_facility, 1, 0)) %>% 
  ungroup()


smr01_babies <- df_eps %>% 
  group_by(upi_number) %>% 
  slice(1)

smr01_admissions_tests <- covid_babies %>% 
  select(baby_upi, x_pregnancy_end_date, tests_baby_earliest_positive_test) %>% 
  right_join(df_eps, by = c("baby_upi" = "upi_number"))

# identify babies who had covid and any smr01 admission

any_smr01 <- smr01_admissions_tests %>% 
  mutate(age_at_admission = round(difftime(admission_date, x_pregnancy_end_date, units="days"))) %>%
  filter(age_at_admission <= 27) %>% 
  select(baby_upi) %>% 
  distinct()

# select only admissions that are associated with the infection either by time or by main condition
associated_admissions <- smr01_admissions_tests %>% 
  rowwise() %>% 
  mutate(admission_window = interval(start = (admission_date - days(7)), 
                                              end =  discharge_date)) %>% 
  mutate(age_at_admission = round(difftime(admission_date, x_pregnancy_end_date, units="days"))) %>%
  mutate(time_associated_paeds = if_else(as_date(tests_baby_earliest_positive_test) %within% admission_window, 1, 0)) %>% 
  mutate(any_code_associated_admission_neonate_period = if_else((main_condition == "U071" | main_condition == "U072") & age_at_admission <= 27, 1, 0)) %>% 
  mutate(time_associated_paeds_contains_code = if_else(time_associated_paeds == 1 & (main_condition == "U071" | main_condition == "U072"), 1, 0)) %>% 
  filter(time_associated_paeds == 1 | any_code_associated_admission_neonate_period == 1 | time_associated_paeds_contains_code == 1) %>% 
  mutate(time_associated_paeds_d28_to_d34 = if_else(age_at_admission > 27, 1, 0)) %>% 
  mutate(los = round(difftime(discharge_date, admission_date, units="days"))) %>% 
  mutate(nosocomial = case_when((tests_baby_earliest_positive_test < discharge_date & 
                                  round(difftime(tests_baby_earliest_positive_test, admission_date, units="days")) >= 7)
                                ~ "1 - Probable",
                                (tests_baby_earliest_positive_test < discharge_date & 
                                   round(difftime(tests_baby_earliest_positive_test, admission_date, units="days")) %in% 2:6)
                                 ~ "2 - Possible",
                                T ~ "3 - No")) %>% 
  ungroup() %>% 
  mutate(month_infection = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m") ) %>% 
  left_join(covid_babies_gestation)
  
tabyl(associated_admissions$time_associated_paeds)
tabyl(associated_admissions$any_code_associated_admission_neonate_period)
tabyl(associated_admissions$time_associated_paeds_contains_code)
tabyl(associated_admissions, time_associated_paeds, time_associated_paeds_contains_code)
upi_picu <- tabyl(associated_admissions,baby_upi, picu)
# identify list of admissions
smr01_admissions <- associated_admissions %>% 
  group_by(baby_upi, cis_marker) %>% 
  mutate(time_associated_paeds = max(time_associated_paeds),
         time_associated_paeds_contains_code = max(time_associated_paeds_contains_code),
         any_code_associated_admission_neonate_period = max(any_code_associated_admission_neonate_period),
         time_associated_paeds_d28_to_d34 = min(time_associated_paeds_d28_to_d34),
         los = max(los),
         nosocomial = min(nosocomial),
         picu = max(picu),
         x_gestation_at_outcome = max(x_gestation_at_outcome)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(baby_upi, month_infection, time_associated_paeds, time_associated_paeds_contains_code, any_code_associated_admission_neonate_period, time_associated_paeds_d28_to_d34,
         los, nosocomial, picu, x_gestation_at_outcome)

# identify list of UPI numbers
babies_with_covid_smr01_admissions <- associated_admissions %>% 
  group_by(baby_upi) %>% 
  mutate(time_associated_paeds = max(time_associated_paeds),
         time_associated_paeds_contains_code = max(time_associated_paeds_contains_code),
         any_code_associated_admission_neonate_period = max(any_code_associated_admission_neonate_period),
         time_associated_paeds_d28_to_d34 = min(time_associated_paeds_d28_to_d34),
         los = max(los),
         nosocomial = min(nosocomial),
         picu = max(picu),
         x_gestation_at_outcome = max(x_gestation_at_outcome)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(baby_upi, month_infection, time_associated_paeds, time_associated_paeds_contains_code, any_code_associated_admission_neonate_period, time_associated_paeds_d28_to_d34,
         los, nosocomial, picu, x_gestation_at_outcome)

# save out to process more in output script
babies_with_covid_smr01_admissions %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/babies_with_covid_smr01_admissions.rds"))
smr01_admissions %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/covid_smr01_admissions.rds"))

babies_with_covid_smr01_admissions <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/babies_with_covid_smr01_admissions.rds"))

## read in SBR admissions data
# SBR ####
data_sbr_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
    SELECT BABY_UPI_NUMBER, BABY_DATE_OF_BIRTH, BABY_TIME_OF_BIRTH, BABY_CHI_NUMBER,
    DERIVED_MOTHER_CHI, ICD10_MAIN, ICD10_OTHER, EPISODE_TYPE_OF_CARE, EPISODE_EVENT_DATE,
    EPISODE_TYPE, EPISODE_ID 
    FROM ANALYSIS.SBR_ANALYST_DATAMART SMR")
  )
) %>%
  clean_names()
# save out raw extract
data_sbr_temp_1 %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/sbr_raw_extract.rds"), compress = "gz")
data_sbr_temp_1 <- readRDS(paste0(folder_temp_data, "neonate_paper_temp/sbr_raw_extract.rds"))

# keep just records for neontatal admissions for babies with COVID
covid_babies_sbr <-  covid_babies %>% 
  select(baby_upi, tests_baby_earliest_positive_test, x_pregnancy_end_date) %>%  
  inner_join(data_sbr_temp_1, by = c("baby_upi" = "baby_upi_number")) %>% 
  mutate(episode_type_of_care = if_else(is.na(episode_type_of_care), 9, as.numeric(episode_type_of_care))) %>% 
  group_by(baby_upi) %>% 
  mutate(maximum_care_level = min(episode_type_of_care))%>% 
  mutate(nicu = if_else(1 %in% episode_type_of_care, 1, 0)) %>% 
  filter(maximum_care_level < 4) %>% 
  mutate(episode_event_date = as_date(episode_event_date, format = "%d/%m/%Y")) %>% 
  arrange(baby_upi, episode_id) %>% 
  ungroup() %>% 
  left_join(covid_babies_gestation)

any_sbr <- covid_babies_sbr %>% 
  mutate(age_at_admission = round(difftime(episode_event_date, x_pregnancy_end_date, units="days"))) %>%
  filter(age_at_admission <= 27) %>% 
  select(baby_upi) %>% 
  distinct()

## identify where the test falls in relation to stays and add flags for time and code association
associated_sbr <- covid_babies_sbr %>% 
  group_by(baby_upi) %>% 
  mutate(admission_date = min(episode_event_date),
         discharge_date = max(episode_event_date)) %>% 
  ungroup() %>% 
  mutate(next_event_date = if_else(lead(baby_upi) == baby_upi, lead(episode_event_date), NA_Date_)) %>% 
  mutate(neonatal_care = if_else(episode_type_of_care < 4, 1, 0)) %>% 
  mutate(episode_window = interval(start = (episode_event_date - days(7)), 
                                     end =  next_event_date)) %>% 
  rowwise() %>% 
  mutate(age_at_admission = round(difftime(episode_event_date, x_pregnancy_end_date, units="days"))) %>%
  mutate(test_associated_episode = if_else(as_date(tests_baby_earliest_positive_test) %within% episode_window, 1, 0)) %>% 
  mutate(time_associated_neonatal = if_else(test_associated_episode == 1 & episode_type_of_care < 4, 1, 0)) %>% 
  mutate(time_associated_neonatal_contains_code = if_else(time_associated_neonatal == 1 & (icd10_main == "U071" | icd10_main == "U072"), 1, 0)) %>% 
  mutate(any_code_associated_admission_neonate_period = if_else((icd10_main == "U071" | icd10_main == "U072") & age_at_admission <= 27, 1, 0)) %>% 
  filter(time_associated_neonatal == 1 | any_code_associated_admission_neonate_period == 1 | time_associated_neonatal_contains_code == 1) %>% 
  mutate(time_associated_neonatal_d28_to_d34 = if_else(age_at_admission > 27, 1, 0)) %>% 
  ungroup() %>% 
  mutate(month_infection = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m") )

# add in length of stay and nosocomial infection flags
associated_sbr <- associated_sbr %>% 
  mutate(los = round(difftime(discharge_date, admission_date, units="days"))) %>% 
  mutate(nosocomial = case_when((tests_baby_earliest_positive_test < discharge_date & 
                                   round(difftime(tests_baby_earliest_positive_test, admission_date, units="days")) >= 7)
                                ~ "1 - Probable",
                                (tests_baby_earliest_positive_test < discharge_date & 
                                   round(difftime(tests_baby_earliest_positive_test, admission_date, units="days")) %in% 2:6)
                                ~ "2 - Possible",
                                T ~ "3 - No"))
  

## gather list of UPIs
associated_sbr_babies <- associated_sbr %>% 
  group_by(baby_upi) %>% 
  summarise(month_infection = min(month_infection),
            time_associated_neonatal = max(time_associated_neonatal),
            time_associated_neonatal_contains_code = max(time_associated_neonatal_contains_code),
            any_code_associated_admission_neonate_period = max(any_code_associated_admission_neonate_period),
            time_associated_neonatal_d28_to_d34 = min(time_associated_neonatal_d28_to_d34),
            los = max(los),
            nosocomial = min(nosocomial),
            nicu = max(nicu),
            x_gestation_at_outcome = max(x_gestation_at_outcome)) 

# save out to process more in output script
associated_sbr_babies %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/associated_sbr_babies.rds"))


#### add in day zero tests ####
day_zero <- covid_babies %>% 
  filter(days_old_had_covid_test == 0) %>% 
  select(baby_upi) %>% 
  mutate(day_zero_test = 1)



#### add together SMR01 and SBR records ####
all_covid_admissions <- babies_with_covid_smr01_admissions %>% 
  full_join(associated_sbr_babies) %>% 
  full_join(day_zero) %>% 
  group_by(baby_upi) %>% 
  summarise(day_zero_test = max_(day_zero_test),
            time_associated_neonatal = max_(time_associated_neonatal),
            time_associated_neonatal_contains_code = max_(time_associated_neonatal_contains_code),
            time_associated_neonatal_d28_to_d34 = max_(time_associated_neonatal_d28_to_d34),
            time_associated_paeds = max_(time_associated_paeds),
            time_associated_paeds_contains_code = max_(time_associated_paeds_contains_code),
            time_associated_paeds_d28_to_d34 = max_(time_associated_paeds_d28_to_d34),
            any_code_associated_admission_neonate_period = max_(any_code_associated_admission_neonate_period)) %>% 
  mutate_all(~replace(., is.na(.), 0))


#create example dataset to send as test
all_admissions_non_confi <- all_covid_admissions %>% 
  mutate(baby_upi = 123456789 + row_number())

# read out chi list in CSV form
write_csv(all_covid_admissions, paste0(folder_temp_data, "neonate_paper_temp/covid_neonate_admissions.csv"))
write_csv(all_admissions_non_confi, paste0(folder_temp_data, "neonate_paper_temp/covid_neonate_admissions_no_chi.csv"))

# investigate flags
investigate <- all_covid_admissions %>% 
  rowwise() %>% 
  mutate(any_time_associated = max(time_associated_neonatal, time_associated_paeds),
         any_time_associated_with_code = max(time_associated_neonatal_contains_code, time_associated_paeds_contains_code))
tabyl(investigate$any_time_associated)
tabyl(investigate$any_time_associated_with_code)
tabyl(investigate, any_code_associated_admission_neonate_period, any_time_associated_with_code)

#### calculate covid admission rates ####

## Admission rate (per 1,000 neonates with covid) 

any_admission <- any_sbr %>% 
  full_join(any_smr01) 

covid_adm_numerator <- nrow(any_admission)
covid_adm_denominator <- nrow(covid_babies)
covid_admission_rate <- tibble( denominator = covid_adm_denominator,
                                 numerator = covid_adm_numerator,
                                 rate = 100*numerator/denominator) %>% 
  mutate(ci_lower = 100*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 100*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

covid_admission_rate %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/covid_admission_rate.rds"))

#### calculate background admission rates ####
## make list of babies with no covid
no_covid_babies <- babies %>% # create a data frame that includes only babies who did not test positive in first 27 days 
  mutate(days_old_had_covid_test = difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days")) %>%
  filter(x_pregnancy_end_date <= end_date &
         (days_old_had_covid_test > 27 | is.na(tests_baby_earliest_positive_test))) %>% 
  select(baby_upi, x_pregnancy_end_date, x_neonatal_death)

## read in SMR01 admissions data
# SMR01 ##
clear_temp_tables(SMRAConnection)
SMR01 <- SMRAConnection %>% tbl("SMR01_PI") # virtual table of all SMR01 episodes

extract_start_date <- (start_date - months(6)) %>% format("%d/%m/%Y") # CIS stays typically constructed from episodes up to six months prior
extract_end_date <- (end_date + months(2)) %>% format("%d/%m/%Y")

# extract all SMR01 episodes of interest (e.g. those in date range with matching mother UPI number) sorted into CIS order
df_eps <- SMR01 %>%
  filter(DISCHARGE_DATE >= TO_DATE(extract_start_date, "dd/mm/yyyy"),
         DISCHARGE_DATE <= TO_DATE(extract_end_date, "dd/mm/yyyy")) %>%
  inner_join(no_covid_babies, copy = TRUE, by = c("UPI_NUMBER" = "baby_upi")) %>% 
  arrange(LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI) %>% 
  select(UPI_NUMBER, LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, 
         MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5) %>% 
  collect() %>%
  clean_names() %>%
  mutate(across(contains("date"), as_date)) %>% 
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup() 

smr01_admissions_no_covid <- df_eps %>% 
  left_join(no_covid_babies, by = c("upi_number" = "baby_upi")) %>% 
  mutate(days_old_admitted = round(difftime(admission_date, x_pregnancy_end_date, units="days"))) %>%
  filter(days_old_admitted <= 27) %>% 
  group_by(upi_number) %>% 
  slice(1)

# SBR ##

# keep just records for neontatal admissions for babies without COVID
no_covid_babies_sbr <-  no_covid_babies  %>% 
  select(baby_upi, x_pregnancy_end_date) %>%  
  inner_join(data_sbr_temp_1, by = c("baby_upi" = "baby_upi_number")) %>% 
  mutate(episode_type_of_care = if_else(is.na(episode_type_of_care), 9, as.numeric(episode_type_of_care))) %>% 
  group_by(baby_upi) %>% 
  filter(episode_type_of_care < 4) %>% 
  mutate(episode_event_date = as_date(episode_event_date, format = "%d/%m/%Y")) %>% 
  arrange(baby_upi, episode_id) %>% 
  ungroup()

no_covid_sbr_admissions <- no_covid_babies_sbr %>% 
  mutate(days_old_admitted = round(difftime(episode_event_date, x_pregnancy_end_date, units="days"))) %>%
  filter(days_old_admitted <= 27) %>% 
  group_by(baby_upi) %>% 
  slice(1)

all_no_covid_admissions <- no_covid_sbr_admissions %>% 
  full_join(smr01_admissions_no_covid, by = c("baby_upi" = "upi_number")) %>% 
  group_by(baby_upi) %>% 
  slice(1)

## Background admission rate (per 1,000 neonates with covid) 
no_covid_adm_numerator <- nrow(all_no_covid_admissions)
no_covid_adm_denominator <- nrow(no_covid_babies)
no_covid_admission_rate <- tibble(denominator = no_covid_adm_denominator,
                                  numerator = no_covid_adm_numerator, 
                                  rate = 100*numerator/denominator) %>% 
  mutate(ci_lower = 100*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 100*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

no_covid_admission_rate %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/no_covid_admission_rate.rds"))


#### calculate neonatal death rates ####

# with covid
covid_neonatal_deaths <- covid_babies %>% 
  filter(x_neonatal_death != "Survived neonatal period")

covid_death_rate <- tibble(denominator = nrow(covid_babies),
                                  numerator = nrow(covid_neonatal_deaths), 
                                  rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))


# without covid
no_covid_neonatal_deaths <- no_covid_babies %>% 
  filter(x_neonatal_death != "Survived neonatal period")

no_covid_death_rate <- tibble(denominator = nrow(no_covid_babies),
                           numerator = nrow(no_covid_neonatal_deaths), 
                           rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

covid_death_rate %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/covid_death_rate.rds"))
no_covid_death_rate %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/no_covid_death_rate.rds"))



