#### Read in NRS Live Births ####
tic()
data_nrs_live_births_raw <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
    "
    SELECT MOTHER_UPI_NUMBER, CHILD_UPI_NUMBER, 
    YEAR_OF_REGISTRATION, REGISTRATION_DISTRICT, ENTRY_NUMBER, DATE_OF_BIRTH, SEX, 
    TOTAL_BIRTHS_LIVE_AND_STILL, POSTCODE
    FROM A.GRO_BIRTHS_C
    WHERE DATE_OF_BIRTH >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')")
  )) %>% 
  distinct() %>%  
  clean_names()


#### Add SIMD and HB ####
simd_healthboard_lookup <-
  readRDS(
    simd_hb_lookup
  ) %>%
  select(pc7, hb2019, simd2020v2_sc_quintile)


data_nrs_live_births <- data_nrs_live_births_raw %>% 
  left_join(simd_healthboard_lookup, by=c("postcode" = "pc7")) %>% 
  left_join(hb_lookup, by =  c("hb2019" = "healthboard_code")) %>% 
  mutate(hb2019 = healthboard) %>% 
  select(-healthboard) %>% 
  mutate(mother_upi_number = chi_pad(mother_upi_number)) %>% 
  mutate(validity = chi_check(mother_upi_number)) %>% 
  mutate(mother_upi_number = if_else(validity == "Valid CHI", mother_upi_number, NA_character_)) %>% 
  mutate(mother_upi_number = if_else(
    is.na(mother_upi_number),
    paste0(
      "43",
      str_pad(
        string = row_number(),
        width = 8,
        side = "left",
        pad = "0"
      )
    ),
    mother_upi_number
  )) %>%
  mutate(child_upi_number = chi_pad(child_upi_number)) %>% 
  mutate(validity = chi_check(child_upi_number)) %>% 
  mutate(child_upi_number = if_else(validity == "Valid CHI", child_upi_number, NA_character_)) %>%
  mutate(child_upi_number = if_else(
    is.na(child_upi_number),
    paste0(
      "53",
      str_pad(
        string = row_number(),
        width = 8,
        side = "left",
        pad = "0"
      )
    ),
    child_upi_number
  )) %>%
  mutate(estimated_gestation = assumed_gestation_live_birth) %>%
  mutate(estimated_conception_date = date_of_birth - (weeks(estimated_gestation) - weeks(2) )) %>%
  mutate(sex = case_when(sex == "1" ~ "M", sex == "2" ~ "F", T ~ NA_character_)) %>%
  mutate(outcome_type = "Live birth") %>%
  mutate(total_births_live_and_still = case_when(is.na(total_births_live_and_still) ~ 1,
                                             total_births_live_and_still == 0 ~ 1,
                                             T ~ total_births_live_and_still)) %>%
  select(-validity) %>%   
  rename_with( ~ paste0("nrslb_", .)) %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_nrs_live_births, paste0(folder_temp_data, "nrs_live_births.rds"), compress = "gz")


#dates
dataset_dates("NRS live births", data_nrs_live_births$nrslb_date_of_birth)
toc()

rm(data_nrs_live_births_raw, data_nrs_live_births)
gc()
