#### Read in AAS Terminations ####
data_aas_terminations <-
  read_csv(
    paste0(folder_data, "network_folder/COPS_ToP_extracted_25082022.csv"),
    col_types = cols(
      # name of the UPI_NUMBER field can change over subsequent extracts
      # make sure the name below matches what is read, and use a rename() step if necessary immediately
      # after read in, and before clean_names()
      
      # UPI_NUMBER = col_character(),
      DERIVED_UPI = col_character(),
      
      DATE_OF_BIRTH = col_datetime(format =
                                     "%Y-%m-%d"),
      DATE_OF_TERMINATION = col_datetime(format =
                                           "%Y-%m-%d"),
      ESTIMATED_GESTATION = col_number(),
      STATUTORY_GROUND_A = col_character(),
      STATUTORY_GROUND_B = col_character(),
      STATUTORY_GROUND_C = col_character(),
      STATUTORY_GROUND_D = col_character(),
      STATUTORY_GROUND_E = col_character(),
      STATUTORY_GROUND_F = col_character(),
      STATUTORY_GROUND_G = col_character(),
      SPECIFIC_INDICATIONS_1 = col_character(),
      SPECIFIC_INDICATIONS_2 = col_character(),
      SPECIFIC_INDICATIONS_3 = col_character(),
      SPECIFIC_INDICATIONS_4 = col_character(),
      SPECIFIC_INDICATIONS_5 = col_character(),
      SUSPECTED_CONDITION_IN_FOETUS = col_character(),
      simd2020v2_sc_quintile = col_number(),
      hb2019 = col_character(),
      hb2019name = col_character(),
      SELECTIVE_REDUCTION = col_number(),
      ORIGINAL_NUMBER_OF_FOETUSES = col_number(),
      REDUCED_TO = col_number()
    )
  ) %>%
  rename(UPI_NUMBER = DERIVED_UPI) %>% # can remove this line if the field in the .csv extract is already called UPI_NUMBER
  clean_names() %>%
  filter(date_of_termination >= cohort_start_date) %>% 
  mutate(pc7 = postcode(pc8)) %>% 
  left_join(hb_lookup, by =  c("hb2019" = "healthboard_code")) %>% 
  mutate(hb2019 = healthboard) %>% 
  select(-c(healthboard, pc8)) %>% 
  mutate(upi_number = chi_pad(upi_number)) %>% 
  mutate(validity = chi_check(upi_number)) %>% 
  mutate(upi_number = case_when(validity == "Valid CHI" ~ upi_number,
                                       T ~ NA_character_)) %>% 
  mutate(upi_number = case_when(
    is.na(upi_number) ~ paste0("45", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ upi_number
  )) %>%
  rename_with( ~ paste0("aas_", .)) %>%
  mutate(aas_estimated_gestation = if_else(aas_estimated_gestation %in% feasible_gestation_termination, 
                                           aas_estimated_gestation, NA_real_)) %>% 
  mutate(aas_assumed_gestation = if_else(is.na(aas_estimated_gestation), 1, 0)) %>% 
  mutate(aas_estimated_gestation = case_when(is.na(aas_estimated_gestation) ~ assumed_gestation_aas_termination,
                                             T ~ aas_estimated_gestation)) %>%
  mutate(aas_estimated_date_of_conception = aas_date_of_termination - (weeks(aas_estimated_gestation) - weeks(2) )) %>%
  arrange(aas_upi_number, aas_estimated_date_of_conception) %>%
  #It's more helpful to have these coded as something like 1 and 0 rather than Y or N
  mutate(aas_statutory_ground_a = case_when(aas_statutory_ground_a == "Y" ~ 1,
                                            aas_statutory_ground_a == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_b = case_when(aas_statutory_ground_b == "Y" ~ 1,
                                            aas_statutory_ground_b == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_c = case_when(aas_statutory_ground_c == "Y" ~ 1,
                                            aas_statutory_ground_c == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_d = case_when(aas_statutory_ground_d == "Y" ~ 1,
                                            aas_statutory_ground_d == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_e = case_when(aas_statutory_ground_e == "Y" ~ 1,
                                            aas_statutory_ground_e == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_f = case_when(aas_statutory_ground_f == "Y" ~ 1,
                                            aas_statutory_ground_f == "N" ~ 0)) %>%
  mutate(aas_statutory_ground_g = case_when(aas_statutory_ground_g == "Y" ~ 1,
                                            aas_statutory_ground_g == "N" ~ 0)) %>%
  mutate(aas_outcome_type = "Termination")


#### Assign each termination to a COPS Event, based on the date of termination ####
data_aas_terminations %<>%
  arrange(aas_upi_number, aas_date_of_termination)

data_aas_terminations <- 
  moving_index_deduplication(data_aas_terminations, aas_upi_number, aas_date_of_termination, dedupe_period)


#### Assign each termination to a COPS Event, based on the revised conception date ####
data_aas_terminations %<>%
  group_by(aas_upi_number, cops_event) %>%
  mutate(revised_conception_date = min(aas_estimated_date_of_conception)) %>%
  ungroup()

data_aas_terminations <- 
  moving_index_deduplication(data_aas_terminations, aas_upi_number, revised_conception_date, dedupe_period)


#### Set variable values based on each termination's COPS Event counter ####
data_aas_terminations %<>%
  ungroup() %>%
  group_by(aas_upi_number, cops_event) %>%
  mutate(
    aas_statutory_ground_a = max(aas_statutory_ground_a),
    aas_statutory_ground_b = max(aas_statutory_ground_b),
    aas_statutory_ground_c = max(aas_statutory_ground_c),
    aas_statutory_ground_d = max(aas_statutory_ground_d),
    aas_statutory_ground_e = max(aas_statutory_ground_e),
    aas_statutory_ground_f = max(aas_statutory_ground_f),
    aas_statutory_ground_g = max(aas_statutory_ground_g),
    aas_date_of_termination = min(aas_date_of_termination),
    aas_date_of_birth = first_(aas_date_of_birth),
    aas_pc7 = first_(aas_pc7),
    aas_hb2019 = first_(aas_hb2019),
    aas_suspected_condition_in_foetus = max(aas_suspected_condition_in_foetus),
    aas_selective_reduction = min_(aas_selective_reduction),
    aas_original_number_of_foetuses = max_(aas_original_number_of_foetuses),
    aas_reduced_to = max_(aas_reduced_to),
    aas_simd2020v2_sc_quintile = first_(aas_simd2020v2_sc_quintile)) %>%
  ungroup()

n_rows_before_slicing_to_cops_event_level <- nrow(data_aas_terminations)

#### Retain the indications associated with each termination before summarising to one row per event ####
aas_indications <- data_aas_terminations %>%
  select(aas_upi_number, cops_event, aas_specific_indications_1:aas_specific_indications_5) %>%
  pivot_longer(cols = aas_specific_indications_1:aas_specific_indications_5) %>%
  filter(!is.na(value)) %>%
  select(-name) %>%
  distinct() %>%
  group_by(aas_upi_number, cops_event) %>%
  mutate(indication = paste0("specific_indication_", row_number())) %>%
  pivot_wider(names_from = indication,
              values_from = value)

#### Cut terminations down to one row per COPS Event ####
data_aas_terminations %<>%
  select(-c(aas_specific_indications_1:aas_specific_indications_5)) %>%
  select(-c(aas_hb2019name, aas_validity, revised_conception_date)) %>%
  group_by(aas_upi_number, cops_event) %>%
  arrange(aas_assumed_gestation, aas_date_of_termination) %>% 
  slice(1) %>%
  ungroup() %>%
  left_join(aas_indications) %>%
  select(aas_upi_number,
         aas_estimated_date_of_conception,
         aas_outcome_type,
         everything()) %>%
  select(-cops_event) %>% 
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_aas_terminations, paste0(folder_temp_data, "aas.rds"), compress = "gz")

#record number filtered out
aas_filters <- data.frame(stage = 1,
                       den = n_rows_before_slicing_to_cops_event_level,
                       num = nrow(data_aas_terminations),
                       task = "Combine into COPS events") %>% 
  mutate(dataset ="aas")
write_rds(aas_filters, paste0(folder_temp_data, "aas_filters.rds"))

#dates
dataset_dates("AAS", data_aas_terminations$aas_date_of_termination)

rm(aas_filters, aas_indications)
rm(data_aas_terminations)
rm(n_rows_before_slicing_to_cops_event_level)

