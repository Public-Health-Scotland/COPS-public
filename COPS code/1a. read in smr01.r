
### Read in SMR01 ####

data_smr01_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                     ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>%
  group_by(link_no, cis_marker) %>%
  mutate(admission_date = min(admission_date)) %>%
  mutate(discharge_date = max(discharge_date)) %>%
  ungroup() 

data_smr01_temp_2 <- data_smr01_temp_1 %>%
  pivot_longer(
    cols = main_condition:other_condition_5,
    names_to = "condition",
    values_to = "icd10"
  ) %>%
  filter(!is.na(icd10)) %>% # Remove any rows with no ICD10 code recorded, as they won't be required
  mutate(icd10_3 = str_sub(icd10, 1, 3)) %>% 
  rowwise() %>% 
  mutate(miscarriage = if_else(icd10 %in% icd10_miscarriage | icd10_3 %in% icd10_miscarriage, 1, 0)) %>% 
  mutate(ectopic_pregnancy = if_else(icd10 %in% icd10_ectopic | icd10_3 %in% icd10_ectopic, 1, 0)) %>% 
  mutate(molar_pregnancy = if_else(icd10 %in% icd10_molar | icd10_3 %in% icd10_molar, 1, 0)) %>% 
  group_by(link_no, cis_marker) %>%
  mutate(
    # If a miscarriage, molar pregnancy or ectopic pregnancy occurs anywhere in the stay, flag every episode as such
    miscarriage = max(miscarriage),
    ectopic_pregnancy = max(ectopic_pregnancy),
    molar_pregnancy = max(molar_pregnancy)
  ) %>%
  filter(miscarriage == 1 |
         ectopic_pregnancy == 1 |
         molar_pregnancy == 1) %>%
  mutate(
    ethnic_group = first(ethnic_group),
    # Some patients have multiple ethnic groups, postcodes etc recorded across different records. We need to choose one value and go with it, so choose the first observed value.
    dr_postcode = first(dr_postcode),
    hbres_currentdate = first(hbres_currentdate),
    hbtreat_currentdate = first(hbtreat_currentdate)
  ) %>%
  select(-c(condition, icd10_3)) %>%
  distinct() %>%
  mutate(condition = paste0("condition", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = condition,
              values_from = icd10) 


#record number filtered out
filter_1 <- data.frame(stage = 1,
                       den = nrow(data_smr01_temp_1),
                       num = nrow(data_smr01_temp_2),
                       task = "Keep only those with relevant ICD10 codes")
rm(data_smr01_temp_1)


data_smr01_temp_3 <- data_smr01_temp_2 %>%
  rename("mother_upi_number" = "upi_number") %>%
  mutate(outcome_type = case_when(ectopic_pregnancy == 1 ~ "Ectopic pregnancy",
                                  molar_pregnancy == 1 ~ "Molar pregnancy",
                                  miscarriage == 1 ~ "Miscarriage")) %>%
    mutate(gestation = case_when(outcome_type == "Molar pregnancy" | outcome_type == "Miscarriage"
                               ~ assumed_gestation_miscarriage,
                               outcome_type == "Ectopic pregnancy" 
                               ~ assumed_gestation_ectopic)) %>% 
  select(
    mother_upi_number,
    gestation,
    outcome_type,
    everything()
  ) %>%
  select(-c(miscarriage, molar_pregnancy, ectopic_pregnancy)) %>% 
  mutate(ethnic_group_mapped9= case_when(ethnic_group =="1A" ~ 1,
                                         ethnic_group =="1B" ~ 2,
                                         ethnic_group %in% c("1C","1K","1L","1Z")  ~ 3,
                                         ethnic_group %in% c("3F","3G","3H") ~ 5,
                                         ethnic_group =="3J" ~ 6,
                                         ethnic_group %in% c("5D","5C","5Y","4D","4Y")  ~ 7,
                                         ethnic_group %in% c("2A","6A","3Z","6Z")  ~ 8,
                                         T ~ NA_real_
  )) %>% 
  left_join(hb_lookup, by =  c("hbres_currentdate" = "healthboard_code")) %>% 
  mutate(hbres_currentdate = healthboard) %>% 
  select(-healthboard)


data_smr01_temp_4 <- data_smr01_temp_3 %>%
  mutate(cops_event = 1) # Start off the COPS Event counter - we'll allocate records to events in the repeat{} block which comes next

repeat {
  # This code groups SMR01 records into COPS Events.
  data_smr01_temp_4 <- data_smr01_temp_4 %>%
    group_by(link_no, cops_event) %>%
    mutate(index_date = first(admission_date)) %>% # The first observed admission date for a woman becomes our initial index date, and then changes on every iteration to the first admission date which occurs >83 days after the previous index date.
    mutate(days_since_index_event = difftime(admission_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                  T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_smr01_temp_4$days_since_index_event))
  
  if (max(data_smr01_temp_4$days_since_index_event) <= dedupe_period) {
    data_smr01_temp_4 <- data_smr01_temp_4 %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 83 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}

data_smr01_temp_5 <- data_smr01_temp_4 %>%
  group_by(link_no, cops_event) %>%
  mutate(outcome_type = case_when("Ectopic pregnancy" %in% outcome_type ~ "Ectopic pregnancy",
                                  "Molar pregnancy" %in% outcome_type ~ "Molar pregnancy",
                                  "Miscarriage" %in% outcome_type ~ "Miscarriage")) %>%
  mutate(
    cops_admission_date = min(admission_date),
    cops_discharge_date = max(discharge_date)
  ) %>%
  ungroup() %>%
  pivot_longer(col = starts_with("condition"),
               names_to = "condition",
               values_to = "icd10") %>%
  select(-c(admission_date, discharge_date, cis_marker, condition)) %>%
  filter(!is.na(icd10)) %>%
  distinct() %>%
  group_by(link_no, cops_event) %>%
  mutate(condition = paste0("conditon", row_number())) %>%
  mutate(
    ethnic_group = first(ethnic_group), # Some patients have multiple ethnic groups, postcodes etc recorded across different records. We need to choose one value and go with it, so choose the first observed value.
    ethnic_group_mapped9 = first(ethnic_group_mapped9), # Some patients have multiple ethnic groups, postcodes etc recorded across different records. We need to choose one value and go with it, so choose the first observed value.
    dr_postcode = first(dr_postcode),
    hbres_currentdate = first(hbres_currentdate),
    hbtreat_currentdate = first(hbtreat_currentdate)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = condition,
              values_from = icd10) %>%
  mutate(mother_upi_number = chi_pad(mother_upi_number)) %>% 
  mutate(validity = chi_check(mother_upi_number)) %>% 
  mutate(mother_upi_number = case_when(validity == "Valid CHI" ~ mother_upi_number,
                         T ~ NA_character_)) %>%
  select(-validity) %>% 
  mutate(mother_upi_number = case_when(
    # Create dummy UPI numbers for mothers who have no UPI recorded
    is.na(mother_upi_number) ~ paste0("41", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ mother_upi_number
  )) %>%
  mutate(baby_upi_number = paste0("51", str_pad(
    # SMR01 records don't contain baby UPI numbers, so let's create some dummy UPIs in case we need to track these specific cases in future
    string = row_number(),
    width = 8,
    side = "left",
    pad = "0"
  ))) %>%
  mutate(estimated_conception_date = cops_admission_date - (weeks(gestation) - weeks(2) )) %>%
  select(
    baby_upi_number,
    mother_upi_number,
    outcome_type,
    estimated_conception_date,
    gestation,
    everything()
  ) %>%
  rename_with( ~ paste0("smr01_", .)) %>%
  mutate(smr01 = T) %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

#### Add SIMD ####
simd_healthboard_lookup <-
  readRDS(
    simd_hb_lookup
  ) %>%
  select(pc7, simd2020v2_sc_quintile)

data_smr01 <- data_smr01_temp_5 %>% 
  left_join(simd_healthboard_lookup, by=c("smr01_dr_postcode" = "pc7")) %>% 
  rename(smr01_simd2020v2_sc_quintile = simd2020v2_sc_quintile)


write_rds(data_smr01, paste0(folder_temp_data, "smr01.rds"))

#record number filtered out
filter_2 <- data.frame(stage = 2,
                       den = nrow(data_smr01_temp_2),
                       num = nrow(data_smr01_temp_5),
                       task = "Combine episodes into COPS events")
smr01_filters <- bind_rows(filter_1, filter_2) %>% 
  mutate(dataset ="smr01")
write_rds(smr01_filters, paste0(folder_temp_data, "smr01_filters.rds"))

#dates
dataset_dates("SMR01", data_smr01$smr01_cops_discharge_date)

rm (data_smr01_temp_2, data_smr01_temp_3, data_smr01_temp_4, data_smr01_temp_5, filter_1, 
    filter_2, smr01_filters, data_smr01)

