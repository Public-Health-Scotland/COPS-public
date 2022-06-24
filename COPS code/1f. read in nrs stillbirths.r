#### Read in NRS Stillbirths ####
data_nrs_stillbirths_raw <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
    "
    SELECT MOTHER_DERIVED_CHI, MOTHER_UPI_NUMBER, POSTCODE, DATE_OF_BIRTH,
    DURATION_OF_PREGNANCY, TOTAL_BIRTHS_LIVE_AND_STILL, SEX, WEIGHT_OF_FOETUS, 
    PRIMARY_CAUSE_OF_DEATH, SECONDARY_CAUSE_OF_DEATH_0, SECONDARY_CAUSE_OF_DEATH_1,
    SECONDARY_CAUSE_OF_DEATH_2, SECONDARY_CAUSE_OF_DEATH_3 
    FROM A.GRO_STILLBIRTHS_C
    WHERE DATE_OF_BIRTH >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')")
    )) %>% 
  clean_names()


  #### Add SIMD and HB ####
simd_healthboard_lookup <-
  readRDS(
    simd_hb_lookup
  ) %>%
  select(pc7, hb2019, simd2020v2_sc_quintile)


data_nrs_stillbirths <- data_nrs_stillbirths_raw %>% 
  left_join(simd_healthboard_lookup, by=c("postcode" = "pc7")) %>%  
  left_join(hb_lookup, by =  c("hb2019" = "healthboard_code")) %>% 
  mutate(hb2019 = healthboard) %>% 
  select(-healthboard) %>% 
  mutate(mother_upi_number = chi_pad(mother_upi_number)) %>% 
  mutate(validity = chi_check(mother_upi_number)) %>% 
  mutate(mother_upi_number = case_when(validity == "Valid CHI" ~ mother_upi_number,
                                       T ~ NA_character_)) %>%
  select(-validity) %>% 
  mutate(mother_upi_number = case_when(
    is.na(mother_upi_number) ~ paste0("46", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ mother_upi_number
  )) %>%
  mutate(
    termination = case_when(
      primary_cause_of_death == "P964" ~ T,
      secondary_cause_of_death_0 == "P964" ~ T,
      secondary_cause_of_death_1 == "P964" ~ T,
      secondary_cause_of_death_2 == "P964" ~ T,
      secondary_cause_of_death_3 == "P964" ~ T,
      T ~ F
    )
  ) %>%
  mutate(stillbirth = case_when(termination == T ~ F,
                                T ~ T)) %>%
  mutate(
    duration_of_pregnancy = case_when(duration_of_pregnancy == 99 ~ NA_real_,
                                      T ~ duration_of_pregnancy)
  ) %>%
  mutate(duration_of_pregnancy = if_else(duration_of_pregnancy %in% feasible_gestation_sb, duration_of_pregnancy, NA_real_)) %>% 
  mutate(assumed_gestation = if_else(is.na(duration_of_pregnancy), 1, 0)) %>% 
  mutate(duration_of_pregnancy = case_when(is.na(duration_of_pregnancy) ~ assumed_gestation_stillbirth,
                                           T ~ duration_of_pregnancy)) %>%
  mutate(estimated_date_of_conception = date_of_birth - (weeks(duration_of_pregnancy) - weeks(2))) %>%
  mutate(outcome_type = case_when(
    termination == T ~ "Termination",
    stillbirth == T ~ "Stillbirth"
  )) %>%
  mutate(total_births_live_and_still = case_when(is.na(total_births_live_and_still) ~ 1,
                                             total_births_live_and_still == 0 ~ 1,
                                             T ~ total_births_live_and_still)) %>%
  select(-c(mother_derived_chi)) %>% 
  select(
    mother_upi_number,
    date_of_birth,
    duration_of_pregnancy,
    estimated_date_of_conception,
    termination,
    stillbirth,
    everything()
  ) %>%
  filter(date_of_birth < Sys.Date()) %>% # We have a tiny number of stillbirths in the future. We need accurate dates, so remove any stillbirths which happen in the future.
  mutate(sex = case_when(sex == "1" ~ "M", sex == "2" ~ "F", T ~ NA_character_)) %>%
  rename_with( ~ paste0("nrssb_", .)) %>%
  replace_with_na_at(.vars = c("nrssb_sex"),
                     condition = ~.x == "9") %>% 
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_nrs_stillbirths, paste0(folder_temp_data, "nrs_stillbirths.rds"), compress = "gz")

#dates
dataset_dates("NRS stillbirths", data_nrs_stillbirths$nrssb_date_of_birth)

rm(data_nrs_stillbirths_raw, data_nrs_stillbirths)