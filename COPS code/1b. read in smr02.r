
#### Read in SMR02 ####
data_smr02_temp_raw <- ### EXTRACT/DATABASE CONNECTION DETAILS
                      ### REMOVED FOR PUBLIC RELEASE
  mutate(across(c(CONDITION_ON_DISCHARGE, OUTCOME_OF_PREGNANCY_BABY_1, 
                    OUTCOME_OF_PREGNANCY_BABY_2, OUTCOME_OF_PREGNANCY_BABY_3,
                    TYPE_OF_ABORTION, DIABETES, BOOKING_SMOKING_HISTORY,
                    NUM_OF_BIRTHS_THIS_PREGNANCY, BIRTHWEIGHT_BABY_1,
                    BIRTHWEIGHT_BABY_2, BIRTHWEIGHT_BABY_3, ESTIMATED_GESTATION), as.numeric)) %>% 
  clean_names()

#### Add SIMD and HB ####
simd_healthboard_lookup <-
  readRDS(
    simd_hb_lookup
  ) %>%
  select(pc7, hb2019, simd2020v2_sc_quintile)

data_smr02_temp_1 <- data_smr02_temp_raw %>% 
  left_join(simd_healthboard_lookup, by=c("dr_postcode" = "pc7")) %>% 
  filter(discharge_date >= as.Date("2019-05-01")) %>%
  select(
    upi_number,
    admission_date,
    discharge_date,
    estimated_gestation,
    baby_upi_number_1,
    baby_upi_number_2,
    baby_upi_number_3,
    baby_ci_chi_number_baby_1,
    baby_ci_chi_number_baby_2,
    baby_ci_chi_number_baby_3,
    presentation_at_delivery_b1,
    presentation_at_delivery_b2,
    presentation_at_delivery_b3,
    mode_of_delivery_baby_1,
    mode_of_delivery_baby_2,
    mode_of_delivery_baby_3,
    outcome_of_pregnancy_baby_1,
    outcome_of_pregnancy_baby_2,
    outcome_of_pregnancy_baby_3,
    birthweight_baby_1,
    birthweight_baby_2,
    birthweight_baby_3,
    apgar_5_minutes_baby_1,
    apgar_5_minutes_baby_2,
    apgar_5_minutes_baby_3,
    sex_baby_1,
    sex_baby_2,
    sex_baby_3,
    date_of_delivery,
    condition_on_discharge,
    type_of_abortion,
    dob,
    dr_postcode,
    simd2020v2_sc_quintile,
    hb2019,
    ethnic_group,
    total_previous_pregnancies,
    previous_spontaneous_abortions,
    previous_theraputic_abortions,
    total_previous_stillbirths,
    drug_misuse,
    weekly_alcohol_consumption,
    height,
    weight_of_mother,
    date_of_lmp,
    antenatal_steroids,
    diabetes,
    booking_smoking_history,
    num_of_births_this_pregnancy
  ) %>%
  mutate(num_of_outcomes_this_pregnancy = case_when(
    is.na(num_of_births_this_pregnancy) ~ 1,
    T ~ num_of_births_this_pregnancy
  )) %>% 
  mutate(baby_upi_number_2 = case_when(is.na(baby_ci_chi_number_baby_2) ~ NA_character_,
                                       T ~ baby_upi_number_2)) %>%
  mutate(baby_upi_number_3 = case_when(is.na(baby_ci_chi_number_baby_3) ~ NA_character_,
                                       T ~ baby_upi_number_3)) %>%
  distinct() 

data_smr02_temp_2 <- data_smr02_temp_1 %>%
  rename_with( ~ paste0("smr02_", .)) %>%
  unite(
    col = "baby1",
    "smr02_baby_upi_number_1",
    "smr02_baby_ci_chi_number_baby_1",
    "smr02_presentation_at_delivery_b1",
    "smr02_outcome_of_pregnancy_baby_1",
    "smr02_birthweight_baby_1",
    "smr02_apgar_5_minutes_baby_1",
    "smr02_sex_baby_1",
    "smr02_mode_of_delivery_baby_1"
  ) %>%
  unite(
    col = "baby2",
    "smr02_baby_upi_number_2",
    "smr02_baby_ci_chi_number_baby_2",
    "smr02_presentation_at_delivery_b2",
    "smr02_outcome_of_pregnancy_baby_2",
    "smr02_birthweight_baby_2",
    "smr02_apgar_5_minutes_baby_2",
    "smr02_sex_baby_2",
    "smr02_mode_of_delivery_baby_2"
  ) %>%
  unite(
    col = "baby3",
    "smr02_baby_upi_number_3",
    "smr02_baby_ci_chi_number_baby_3",
    "smr02_presentation_at_delivery_b3",
    "smr02_outcome_of_pregnancy_baby_3",
    "smr02_birthweight_baby_3",
    "smr02_apgar_5_minutes_baby_3",
    "smr02_sex_baby_3",
    "smr02_mode_of_delivery_baby_3"
  ) %>%
  mutate(smr02_upi_number = chi_pad(smr02_upi_number)) %>% 
  mutate(validity = chi_check(smr02_upi_number)) %>% 
  mutate(smr02_upi_number = case_when(validity == "Valid CHI" ~ smr02_upi_number,
                         T ~ NA_character_)) %>% 
  mutate(smr02_upi_number = case_when(
    is.na(smr02_upi_number) ~ paste0("42", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ smr02_upi_number
  )) %>%
  pivot_longer(c(baby1:baby3),
               names_to = "smr02_baby") %>%
  separate(
    value,
    c(
      "smr02_baby_upi_number",
      "smr02_baby_chi_number",
      "smr02_presentation_at_delivery",
      "smr02_outcome_of_pregnancy",
      "smr02_birthweight",
      "smr02_apgar_5_minutes",
      "smr02_sex",
      "smr02_mode_of_delivery"
    )
  ) 

#record number filtered out
filter_1 <- data.frame(stage = 1,
                       den = nrow(data_smr02_temp_1),
                       num = nrow(data_smr02_temp_2),
                       task = "Pivot so there is one record per potential baby: 3 per delivery")

data_smr02_temp_3 <- data_smr02_temp_2 %>%
  mutate(
    smr02_live_birth = case_when(
      smr02_condition_on_discharge == 3 &
        smr02_outcome_of_pregnancy %in% c(1, 3, 4, 5)
      ~ T,
      T ~ F
    )
  ) %>%
  mutate(
    smr02_miscarriage = case_when(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion %in% c(1, 2, 8, 9) &
        smr02_baby == "baby1"
      ~ T,
      T ~ F
    ) 
  ) %>%
      mutate(
        smr02_molar_pregnancy = case_when(
          smr02_condition_on_discharge == 2 &
            smr02_type_of_abortion == 3 &
            smr02_baby == "baby1"
          ~ T,
          T ~ F
        )    
  ) %>%
  mutate(
    smr02_ectopic_pregnancy = case_when(
      smr02_condition_on_discharge == 2 &
      smr02_type_of_abortion == 6 &
      smr02_baby == "baby1"
      ~ T,
      T ~ F
    )
  )  %>%
  mutate(
    smr02_stillbirth = case_when(
      smr02_condition_on_discharge == 3 &
        smr02_outcome_of_pregnancy == 2
      ~ T,
      T ~ F
    )
  ) %>%
  mutate(
    smr02_termination = case_when(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion == 4 &
        smr02_baby == "baby1"
      ~ T,
      T ~ F
    )
  ) %>%
  filter(
    smr02_live_birth == T |
      smr02_molar_pregnancy == T |
      smr02_miscarriage == T |
      smr02_ectopic_pregnancy == T |
      smr02_stillbirth == T |
      smr02_termination == T
  )

#record number filtered out
filter_2 <- data.frame(stage = 2,
                       den = nrow(data_smr02_temp_2),
                       num = nrow(data_smr02_temp_3),
                       task = "Select only those with a defined outcome")

data_smr02_temp_4 <- data_smr02_temp_3 %>%
  mutate(
    smr02_outcome_type = case_when(
      smr02_termination == T ~ "Termination",
      smr02_live_birth == T ~ "Live birth",
      smr02_stillbirth == T ~ "Stillbirth",
      smr02_ectopic_pregnancy == 1 ~ "Ectopic pregnancy",     
      smr02_molar_pregnancy == 1 ~ "Molar pregnancy",
      smr02_miscarriage == 1 ~ "Miscarriage"
    )
  ) %>%
  replace_with_na_at(
    .vars = c(
      "smr02_baby_upi_number",
      "smr02_baby_chi_number",
      "smr02_presentation_at_delivery",
      "smr02_outcome_of_pregnancy",
      "smr02_birthweight",
      "smr02_apgar_5_minutes",
      "smr02_sex",
      "smr02_mode_of_delivery"
    ),
    condition = ~ .x == "NA"
  ) %>%
  mutate(
    smr02_height = case_when(
      smr02_height < 10 ~ NA_real_,
      smr02_height > 200 ~ NA_real_,
      T ~ smr02_height
    )
  ) %>%
  mutate(
    smr02_weight_of_mother = case_when(
      smr02_weight_of_mother < 10 ~ NA_real_,
      smr02_weight_of_mother > 600 ~ NA_real_,
      T ~ smr02_weight_of_mother
    )
  ) %>%
  mutate(smr02_birthweight = as.numeric(smr02_birthweight)) %>%
  mutate(
    smr02_birthweight = case_when(
      smr02_birthweight < 10 ~ NA_real_,
      smr02_birthweight > 9000 ~ NA_real_,
      T ~ smr02_birthweight
    )
  ) %>%
  replace_with_na_at(.vars = c("smr02_sex"),
                     condition = ~.x %in% c("9", "0")) %>% 
  replace_with_na_at(.vars = c("smr02_booking_smoking_history", "smr02_diabetes"),
                     condition = ~.x == 9) %>%  
  mutate(smr02_ethnic_group_mapped9= case_when(smr02_ethnic_group =="1A" ~ 1,
                                               smr02_ethnic_group =="1B" ~ 2,
                                               smr02_ethnic_group %in% c("1C","1K","1L","1Z")  ~ 3,
                                               smr02_ethnic_group %in% c("3F","3G","3H") ~ 5,
                                               smr02_ethnic_group =="3J" ~ 6,
                                               smr02_ethnic_group %in% c("5D","5C","5Y","4D","4Y")  ~ 7,
                                               smr02_ethnic_group %in% c("2A","6A","3Z","6Z")  ~ 8,
                                               T ~ NA_real_
  )) %>% 
  mutate(smr02_baby_upi_number = chi_pad(smr02_baby_upi_number)) %>% 
  mutate(validity = chi_check(smr02_baby_upi_number)) %>% 
  mutate(smr02_baby_upi_number = case_when(validity == "Valid CHI" ~ smr02_baby_upi_number,
                                      T ~ NA_character_)) %>% 
  select(- validity) %>% 
  mutate(smr02_baby_upi_number = case_when(
    is.na(smr02_baby_upi_number) ~ paste0("52", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ smr02_baby_upi_number
  )) %>%
  mutate(smr02_sex = case_when(smr02_sex == "1" ~ "M", 
                               smr02_sex == "2" ~ "F", 
                               smr02_sex == "0" ~ "0", 
                               T ~ NA_character_)) %>%
  left_join(hb_lookup, by =  c("smr02_hb2019" = "healthboard_code")) %>% 
  mutate(smr02_hb2019 = healthboard) %>% 
  select(-healthboard) %>% 
  mutate(
    smr02_feasible_gestation = case_when(
      smr02_outcome_type == "Live birth" & smr02_estimated_gestation %in% c(feasible_gestation_lb) ~ T,
      smr02_outcome_type == "Stillbirth" &
        smr02_estimated_gestation %in% c(feasible_gestation_sb) ~ T,
      (
        smr02_outcome_type == "Miscarriage" |
        smr02_outcome_type == "Molar pregnancy" |
        smr02_outcome_type == "Ectopic pregnancy"
      ) & smr02_estimated_gestation %in% c(feasible_gestation_miscarriage) ~ T,
      smr02_outcome_type == "Termination" &
        smr02_estimated_gestation %in% c(feasible_gestation_termination) ~ T,
      T ~ F
    )
  )  %>% 
  mutate(smr02_estimated_gestation = case_when(smr02_feasible_gestation == T ~ smr02_estimated_gestation,
                                               smr02_feasible_gestation == F & !is.na(smr02_date_of_delivery) & !is.na(smr02_date_of_lmp)
                                                  ~ as.numeric(floor(difftime(smr02_date_of_delivery, smr02_date_of_lmp, units = "weeks"))),
                                               smr02_feasible_gestation == F & is.na(smr02_date_of_delivery) & !is.na(smr02_date_of_lmp)
                                                  ~ as.numeric(floor(difftime(smr02_admission_date, smr02_date_of_lmp, units = "weeks"))))) %>% 
  mutate(
    smr02_feasible_gestation = case_when(
      smr02_outcome_type == "Live birth" & smr02_estimated_gestation %in% c(feasible_gestation_lb) ~ T,
      smr02_outcome_type == "Stillbirth" &
        smr02_estimated_gestation %in% c(feasible_gestation_sb) ~ T,
      (
        smr02_outcome_type == "Miscarriage" |
          smr02_outcome_type == "Molar pregnancy" |
          smr02_outcome_type == "Ectopic pregnancy"
      ) & smr02_estimated_gestation %in% c(feasible_gestation_miscarriage) ~ T,
      smr02_outcome_type == "Termination" &
        smr02_estimated_gestation %in% c(feasible_gestation_termination) ~ T,
      T ~ F
    )
  ) 

#record number recoded
#height
denominator <- data_smr02_temp_3 %>% 
  select(smr02_height) %>% 
  filter(!is.na(smr02_height))
numerator <- data_smr02_temp_4 %>% 
  select(smr02_height) %>% 
  filter(!is.na(smr02_height))

recode_1 <- data.frame(variable = "height",
                       den = nrow(denominator),
                       num = nrow(numerator))
rm(numerator, denominator)
#weight
denominator <- data_smr02_temp_3 %>% 
  select(smr02_weight_of_mother) %>% 
  filter(!is.na(smr02_weight_of_mother))
numerator <- data_smr02_temp_4 %>% 
  select(smr02_weight_of_mother) %>% 
  filter(!is.na(smr02_weight_of_mother))

recode_2 <- data.frame(variable = "weight",
                       den = nrow(denominator),
                       num = nrow(numerator))
rm(numerator, denominator)
#birthweight
denominator <- data_smr02_temp_3 %>% 
  select(smr02_birthweight) %>% 
  filter(!is.na(smr02_birthweight))
numerator <- data_smr02_temp_4 %>% 
  select(smr02_birthweight) %>% 
  filter(!is.na(smr02_birthweight))

recode_3 <- data.frame(variable = "birthweight",
                       den = nrow(denominator),
                       num = nrow(numerator))
rm(numerator, denominator)
#gestation
denominator <- data_smr02_temp_3 %>% 
  select(smr02_estimated_gestation) %>% 
  filter(!is.na(smr02_estimated_gestation))
numerator <- data_smr02_temp_4 %>% 
  select(smr02_feasible_gestation) %>% 
  filter(smr02_feasible_gestation == T)

recode_4 <- data.frame(variable = "gestation",
                       den = nrow(denominator),
                       num = nrow(numerator))
rm(numerator, denominator)


data_smr02_temp_5 <- data_smr02_temp_4 %>%
  mutate(
    smr02_estimated_gestation = case_when(
      smr02_feasible_gestation == T ~ smr02_estimated_gestation,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Live birth" ~ assumed_gestation_live_birth,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Termination" ~ assumed_gestation_smr02_termination,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Stillbirth" ~ assumed_gestation_stillbirth,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Miscarriage" ~ assumed_gestation_miscarriage,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Molar pregnancy" ~ assumed_gestation_miscarriage,
      smr02_feasible_gestation == F &
        smr02_outcome_type == "Ectopic pregnancy"~ assumed_gestation_ectopic,
      T ~ smr02_estimated_gestation
    )
  ) %>%
  mutate(
    smr02_estimated_conception_date = case_when(
      smr02_live_birth == T ~ smr02_date_of_delivery - (weeks(smr02_estimated_gestation) - weeks(2)),
      smr02_live_birth == F ~ smr02_admission_date - (weeks(smr02_estimated_gestation) - weeks(2))
    )
  ) %>%
  mutate(
    smr02_pregnancy_end_date = case_when(
      !is.na(smr02_date_of_delivery) ~ smr02_date_of_delivery,
      T ~ smr02_admission_date
    )
  ) 

data_smr02_livebirths_and_stillbirths <- data_smr02_temp_5 %>%
  filter(smr02_outcome_type %in% c("Live birth", "Stillbirth")) %>%
  group_by(smr02_baby_upi_number) %>%
  # If a baby UPI appears more than once, but there was only one birth that pregnancy, the case is likely a duplicate
  # This can occur when more than one SMR02 record is generated during a birth due to the patient being transferred at some point
  # Note - this does not deduplicate multiples - but as of March 2021 there don't appear to be any multiples impacted by this issue
  mutate(likely_duplicate = case_when(row_number() > 1 & smr02_num_of_births_this_pregnancy == 1 ~ T,
                                      T ~ F)) %>%
  filter(likely_duplicate == F) %>%
  select(-likely_duplicate) 

data_smr02_miscarriage_ectopic_and_termination <- data_smr02_temp_5 %>%
  filter(smr02_outcome_type %in% c("Miscarriage", "Ectopic pregnancy", "Molar pregnancy", "Termination")) %>%
  arrange(smr02_upi_number, smr02_admission_date) %>%
  mutate(cops_event = 1)


# Assign each event to a single pregnancy event. This allows us to group events which seem to be related and which probably should belong to the same pregnancy. It helps us overcome any issues with innacurate conception dates. 
repeat {
  data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
    group_by(smr02_upi_number, cops_event) %>%
    mutate(index_date = first(smr02_admission_date)) %>% 
    mutate(days_since_index_event = difftime(smr02_admission_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                             T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_smr02_miscarriage_ectopic_and_termination$days_since_index_event))
  
  if (max(data_smr02_miscarriage_ectopic_and_termination$days_since_index_event) <= dedupe_period) {
    data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 83 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}

data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
  group_by(smr02_upi_number, cops_event) %>%
  mutate(revised_conception_date = min(smr02_estimated_conception_date)) %>%
  ungroup() %>%
  arrange(smr02_upi_number, revised_conception_date) %>%
  mutate(cops_event = 1)

repeat {
  data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
    group_by(smr02_upi_number, cops_event) %>%
    mutate(index_date = first(revised_conception_date)) %>% # The first observed conception date for a woman becomes our initial index date, and then changes on every iteration to the first conception date which occurs > 83 days after the previous index date.
    mutate(days_since_index_event = difftime(revised_conception_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                 T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_smr02_miscarriage_ectopic_and_termination$days_since_index_event))
  
  if (max(data_smr02_miscarriage_ectopic_and_termination$days_since_index_event) <= dedupe_period) {
    data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 82 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}


data_smr02_miscarriage_ectopic_and_termination_1 <- data_smr02_miscarriage_ectopic_and_termination %>%
  group_by(smr02_upi_number, cops_event) %>%
  mutate(smr02_outcome_type = case_when("Termination" %in% smr02_outcome_type ~ "Termination",
                                  "Ectopic pregnancy" %in% smr02_outcome_type ~ "Ectopic pregnancy",
                                  "Molar pregnancy" %in% smr02_outcome_type ~ "Molar pregnancy",
                                  "Miscarriage" %in% smr02_outcome_type ~ "Miscarriage")) %>%
  mutate(across(!smr02_feasible_gestation & !smr02_estimated_gestation & !smr02_outcome_type, 
                ~ first_(.x))) %>% 
  arrange(desc(smr02_feasible_gestation), .by_group = TRUE) %>% 
  slice(1) %>% 
  ungroup()

data_smr02 <- bind_rows(data_smr02_livebirths_and_stillbirths, data_smr02_miscarriage_ectopic_and_termination_1) %>% 
  select(
    smr02_baby_upi_number,
    smr02_baby_chi_number,
    smr02_upi_number,
    smr02_outcome_type,
    smr02_baby,
    smr02_estimated_conception_date,
    smr02_estimated_gestation,
    everything()) %>%
  mutate(smr02_assumed_gestation = if_else(smr02_feasible_gestation == F, 1, 0)) %>% 
  select(-c(smr02_feasible_gestation, cops_event, revised_conception_date)) %>% 
  arrange(smr02_upi_number, smr02_admission_date) %>%
  ungroup() %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_smr02, paste0(folder_temp_data, "smr02.rds"))

#record number filtered out
filter_3 <- data.frame(stage = 3,
                       den = nrow(data_smr02_temp_5),
                       num = nrow(data_smr02),
                       task = "Combine into COPS events")

#number filtered out
smr02_filters <- bind_rows(filter_1, filter_2, filter_3) %>% 
  mutate(dataset ="smr02")
write_rds(smr02_filters, paste0(folder_temp_data, "smr02_filters.rds"))

#number recoded
smr02_recoded <- bind_rows(recode_1, recode_2, recode_3, recode_4) %>% 
  mutate(dataset ="smr02")
write_rds(smr02_recoded, paste0(folder_temp_data, "smr02_recoded.rds"))

#dates
dataset_dates("SMR02", data_smr02$smr02_pregnancy_end_date)

rm (data_smr02_temp_1, data_smr02_temp_2, data_smr02_temp_3, data_smr02_temp_4, data_smr02_temp_5, 
    data_smr02_livebirths_and_stillbirths, data_smr02_miscarriage_ectopic_and_termination,
    data_smr02_miscarriage_ectopic_and_termination_1, filter_1, filter_2, filter_3, recode_1, 
    recode_2, recode_3, recode_4, data_smr02_temp_raw, smr02_filters, smr02_recoded)
rm(data_smr02)
