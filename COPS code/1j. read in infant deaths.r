#### Read in Infant Deaths ####
data_infant_deaths <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
    "
    SELECT CHI, INFANT_DEATH_BIRTH_REG_YEAR as YEAR_OF_REGISTRATION, INFANT_DEATH_BIRTH_REG_DIST as REGISTRATION_DISTRICT,
    INFANT_DEATH_BIRTH_ENTRY_NO as ENTRY_NUMBER, DATE_OF_DEATH as DATE_OF_BABY_DEATH,
    UNDERLYING_CAUSE_OF_DEATH as UNDERLYING_CAUSE_OF_BABY_DEATH,
    CAUSE_OF_DEATH_CODE_0 AS CAUSE_OF_BABY_DEATH_0,
    CAUSE_OF_DEATH_CODE_1 AS CAUSE_OF_BABY_DEATH_1,
    CAUSE_OF_DEATH_CODE_2 AS CAUSE_OF_BABY_DEATH_2,
    CAUSE_OF_DEATH_CODE_3 AS CAUSE_OF_BABY_DEATH_3,
    CAUSE_OF_DEATH_CODE_4 AS CAUSE_OF_BABY_DEATH_4,
    CAUSE_OF_DEATH_CODE_5 AS CAUSE_OF_BABY_DEATH_5,
    CAUSE_OF_DEATH_CODE_6 AS CAUSE_OF_BABY_DEATH_6,
    CAUSE_OF_DEATH_CODE_7 AS CAUSE_OF_BABY_DEATH_7,
    CAUSE_OF_DEATH_CODE_8 AS CAUSE_OF_BABY_DEATH_8,
    CAUSE_OF_DEATH_CODE_9 AS CAUSE_OF_BABY_DEATH_9
    FROM ANALYSIS.GRO_INFANT_DEATHS_C GRO
    WHERE GRO.DATE_OF_DEATH >= TO_DATE('", cohort_start_date,"', 'yyyy-mm-dd')")
  )
) %>%
  clean_names() %>%
  mutate(nrs_triplicate_id = paste0(year_of_registration, "_", registration_district, "_", entry_number)) %>%
  select(chi, nrs_triplicate_id, date_of_baby_death, underlying_cause_of_baby_death, 
         cause_of_baby_death_0,
         cause_of_baby_death_1,
         cause_of_baby_death_2,
         cause_of_baby_death_3,
         cause_of_baby_death_4,
         cause_of_baby_death_5,
         cause_of_baby_death_6,
         cause_of_baby_death_7,
         cause_of_baby_death_8,
         cause_of_baby_death_9) %>% 
  mutate(chi = chi_pad(chi)) %>% 
  mutate(validity = chi_check(chi)) %>% 
  mutate(chi = case_when(validity == "Valid CHI" ~ chi,
                         T ~ NA_character_)) %>% 
  select(-validity)


#### Check for COVID-19 as cause of death ####
# Flag if Acute COVID-19 is the underlying cause of death
data_infant_deaths %<>% 
  mutate(underlying_cause_of_baby_death_acute_covid19 = case_when(underlying_cause_of_baby_death %in% icd10_acute_covid19 ~ T,
                                                             T ~ F))

# FLag if Acute COVID-19 is a contributing cause of death
data_infant_deaths %<>%
  mutate(contributory_cause_of_baby_death_acute_covid19 = case_when(cause_of_baby_death_0 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_1 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_2 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_3 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_4 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_5 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_6 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_7 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_8 %in% icd10_acute_covid19 ~ T,
                                                                    cause_of_baby_death_9 %in% icd10_acute_covid19 ~ T,
                                                                    T ~ F))

# FLag if History of COVID-19 is a cause of death
data_infant_deaths %<>%
  mutate(any_cause_of_baby_death_history_of_covid19 = case_when(underlying_cause_of_baby_death %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_0 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_1 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_2 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_3 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_4 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_5 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_6 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_7 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_8 %in% icd10_history_of_covid19 ~ T,
                                                                cause_of_baby_death_9 %in% icd10_history_of_covid19 ~ T,
                                                                T ~ F))

# Flag if Adverse Event Following COVID-19 Vaccination is a cause of death
data_infant_deaths %<>%
  mutate(any_cause_of_baby_death_adverse_event_following_covid19_vaccine = case_when(underlying_cause_of_baby_death %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_0 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_1 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_2 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_3 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_4 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_5 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_6 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_7 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_8 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     cause_of_baby_death_9 %in% icd10_adverse_event_after_covid19_vaccine ~ T,
                                                                                     T ~ F))


#### Write infant death file ####
write_rds(data_infant_deaths, paste0(folder_temp_data, "infant_deaths.rds"), compress = "gz")

#dates
dataset_dates("Infant deaths", data_infant_deaths$date_of_baby_death)

rm(data_infant_deaths)
