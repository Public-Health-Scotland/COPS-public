#### Read in AAS Terminations ####
data_aas_terminations_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                                ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>%
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

data_aas_terminations_temp_2 <- data_aas_terminations_temp_1 %>%
  arrange(aas_upi_number, aas_date_of_termination) %>%
  mutate(cops_event = 1)


# Assign each event to a single pregnancy event. This allows us to group events which seem to be related and which probably should belong to the same pregnancy. It helps us overcome any issues with innacurate conception dates. 
repeat {
  data_aas_terminations_temp_2 <- data_aas_terminations_temp_2 %>%
    group_by(aas_upi_number, cops_event) %>%
    mutate(index_date = first(aas_date_of_termination)) %>% 
    mutate(days_since_index_event = difftime(aas_date_of_termination, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                  T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_aas_terminations_temp_2$days_since_index_event))
  
  if (max(data_aas_terminations_temp_2$days_since_index_event) <= dedupe_period) {
    data_aas_terminations_temp_2 <- data_aas_terminations_temp_2 %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 83 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}

data_aas_terminations_temp_2 <- data_aas_terminations_temp_2 %>%
  group_by(aas_upi_number, cops_event) %>%
  mutate(revised_conception_date = min(aas_estimated_date_of_conception)) %>%
  ungroup() %>%
  arrange(aas_upi_number, revised_conception_date) %>%
  mutate(cops_event = 1)

repeat {
  data_aas_terminations_temp_2 <- data_aas_terminations_temp_2 %>%
    group_by(aas_upi_number, cops_event) %>%
    mutate(index_date = first(revised_conception_date)) %>% # The first observed conception date for a woman becomes our initial index date, and then changes on every iteration to the first conception date which occurs > 83 days after the previous index date.
    mutate(days_since_index_event = difftime(revised_conception_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                  T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_aas_terminations_temp_2$days_since_index_event))
  
  if (max(data_aas_terminations_temp_2$days_since_index_event) <= dedupe_period) {
    data_aas_terminations_temp_2 <- data_aas_terminations_temp_2 %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 82 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}


data_aas_terminations_temp_3 <- data_aas_terminations_temp_2 %>%
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

aas_indications <- data_aas_terminations_temp_3 %>%
  select(aas_upi_number, cops_event, aas_specific_indications_1:aas_specific_indications_5) %>%
  pivot_longer(cols = aas_specific_indications_1:aas_specific_indications_5) %>%
  filter(!is.na(value)) %>%
  select(-name) %>%
  distinct() %>%
  group_by(aas_upi_number, cops_event) %>%
  mutate(indication = paste0("specific_indication_", row_number())) %>%
  pivot_wider(names_from = indication,
              values_from = value)

data_aas_terminations <- data_aas_terminations_temp_3 %>%
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

write_rds(data_aas_terminations, paste0(folder_temp_data, "aas.rds"))

#record number filtered out
aas_filters <- data.frame(stage = 1,
                       den = nrow(data_aas_terminations_temp_3),
                       num = nrow(data_aas_terminations),
                       task = "Combine into COPS events") %>% 
  mutate(dataset ="aas")
write_rds(aas_filters, paste0(folder_temp_data, "aas_filters.rds"))

#dates
dataset_dates("AAS", data_aas_terminations$aas_date_of_termination)

rm(aas_filters, aas_indications, data_aas_terminations_temp_1, data_aas_terminations_temp_2, data_aas_terminations_temp_3)
rm(data_aas_terminations)
