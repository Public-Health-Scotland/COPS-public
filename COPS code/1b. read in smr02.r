tic()
#### Read in SMR02 ####
data_smr02_temp_raw <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
    "
    SELECT UPI_NUMBER,
    BABY_CI_CHI_NUMBER_BABY_1 AS BABY_CHI_NUMBER_1, BABY_CI_CHI_NUMBER_BABY_2 AS BABY_CHI_NUMBER_2, BABY_CI_CHI_NUMBER_BABY_3 AS BABY_CHI_NUMBER_3,
    PRESENTATION_AT_DELIVERY_B1, PRESENTATION_AT_DELIVERY_B2, PRESENTATION_AT_DELIVERY_B3,
    MODE_OF_DELIVERY_BABY_1, MODE_OF_DELIVERY_BABY_2, MODE_OF_DELIVERY_BABY_3,
    OUTCOME_OF_PREGNANCY_BABY_1, OUTCOME_OF_PREGNANCY_BABY_2, OUTCOME_OF_PREGNANCY_BABY_3,
    BIRTHWEIGHT_BABY_1, BIRTHWEIGHT_BABY_2, BIRTHWEIGHT_BABY_3, 
    APGAR_5_MINUTES_BABY_1, APGAR_5_MINUTES_BABY_2, APGAR_5_MINUTES_BABY_3, 
    SEX_BABY_1, SEX_BABY_2, SEX_BABY_3,
    OFC_BABY_1, OFC_BABY_2, OFC_BABY_3,
    DOB, DR_POSTCODE, ETHNIC_GROUP, ADMISSION_DATE, DISCHARGE_DATE, 
    TOTAL_PREVIOUS_PREGNANCIES, PREVIOUS_SPONTANEOUS_ABORTIONS,
    PREVIOUS_THERAPUTIC_ABORTIONS, TOTAL_PREVIOUS_STILLBIRTHS, DRUG_MISUSE,
    WEEKLY_ALCOHOL_CONSUMPTION, HEIGHT, WEIGHT_OF_MOTHER, TYPE_OF_ABORTION,
    DATE_OF_LMP, ESTIMATED_GESTATION, ANTENATAL_STEROIDS, DIABETES,
    BOOKING_SMOKING_HISTORY, CONDITION_ON_DISCHARGE, 
    INDUCTION_OF_LABOUR, DURATION_OF_LABOUR,
    DATE_OF_DELIVERY, NUM_OF_BIRTHS_THIS_PREGNANCY, INDICATION_FOR_OPERATIVE_DEL,
    MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5
    FROM ANALYSIS.SMR02_PI SMR 
    WHERE (SMR.ADMISSION_DATE >= TO_DATE('", cohort_start_date, "', 'yyyy-mm-dd')) AND CONDITION_ON_DISCHARGE IN (2, 3)")
  )) %>% 
  # Make sure numeric fields are stored as numerics
  mutate(across(c(CONDITION_ON_DISCHARGE, 
                  TYPE_OF_ABORTION, DIABETES, BOOKING_SMOKING_HISTORY,
                  NUM_OF_BIRTHS_THIS_PREGNANCY, ESTIMATED_GESTATION, INDUCTION_OF_LABOUR,
                  OUTCOME_OF_PREGNANCY_BABY_1, OUTCOME_OF_PREGNANCY_BABY_2, OUTCOME_OF_PREGNANCY_BABY_3,
                  BIRTHWEIGHT_BABY_1,BIRTHWEIGHT_BABY_2, BIRTHWEIGHT_BABY_3,
                  OFC_BABY_1, OFC_BABY_2, OFC_BABY_3), as.numeric)) %>% 
  clean_names()

#### Add SIMD and HB ####
simd_healthboard_lookup <-
  readRDS(simd_hb_lookup) %>%
  select(pc7, hb2019, simd2020v2_sc_quintile)

#### Add postcode and rearrange variables #### 
data_smr02 <- data_smr02_temp_raw %>% 
  left_join(simd_healthboard_lookup, by=c("dr_postcode" = "pc7"))

#### Handle missing number of births
data_smr02 %<>%
  mutate(num_of_outcomes_this_pregnancy = case_when(
    is.na(num_of_births_this_pregnancy) ~ 1,
    T ~ num_of_births_this_pregnancy
  )) %>%
  distinct() 

nrow_temp1 <- nrow(data_smr02)


#### Pivot the data to one row per baby ####
# Unite the baby data so that each baby is held as a single variable.
# This ensures the baby remains associated with the mother after the pivot.
data_smr02 %<>% 
  rename_with(~ paste0("smr02_", .)) %>%
  unite(
    col = "baby1",
    #"smr02_baby_upi_number_1",
    "smr02_baby_chi_number_1",
    "smr02_presentation_at_delivery_b1",
    "smr02_outcome_of_pregnancy_baby_1",
    "smr02_birthweight_baby_1",
    "smr02_apgar_5_minutes_baby_1",
    "smr02_sex_baby_1",
    "smr02_mode_of_delivery_baby_1",
    "smr02_ofc_baby_1"
  ) %>%
  unite(
    col = "baby2",
    #"smr02_baby_upi_number_2",
    "smr02_baby_chi_number_2",
    "smr02_presentation_at_delivery_b2",
    "smr02_outcome_of_pregnancy_baby_2",
    "smr02_birthweight_baby_2",
    "smr02_apgar_5_minutes_baby_2",
    "smr02_sex_baby_2",
    "smr02_mode_of_delivery_baby_2",
    "smr02_ofc_baby_2"
  ) %>%
  unite(
    col = "baby3",
    #"smr02_baby_upi_number_3",
    "smr02_baby_chi_number_3",
    "smr02_presentation_at_delivery_b3",
    "smr02_outcome_of_pregnancy_baby_3",
    "smr02_birthweight_baby_3",
    "smr02_apgar_5_minutes_baby_3",
    "smr02_sex_baby_3",
    "smr02_mode_of_delivery_baby_3",
    "smr02_ofc_baby_3"
  ) 

# Check validity of mother UPI, if it's invaild then assign a dummy UPI
data_smr02 %<>% 
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
  )) 

# Pivot the data to one row per baby so that we can carry out some calculations at baby-level
data_smr02 %<>%
  pivot_longer(c(baby1:baby3),
               names_to = "smr02_baby") %>%
  separate(
    value,
    c(
      #"smr02_baby_upi_number",
      "smr02_baby_chi_number",
      "smr02_presentation_at_delivery",
      "smr02_outcome_of_pregnancy",
      "smr02_birthweight",
      "smr02_apgar_5_minutes",
      "smr02_sex",
      "smr02_mode_of_delivery",
      "smr02_ofc"
    )
  ) 

nrow_temp2 <- nrow(data_smr02)


#record number filtered out
filter_1 <- data.frame(stage = 1,
                       den = nrow_temp1,
                       num = nrow_temp2,
                       task = "Pivot so there is one record per potential baby: 3 per delivery")



#### Determine which fetuses fit within our outcome specifications ####
data_smr02 %<>%
  mutate(
    smr02_live_birth = if_else(
      smr02_condition_on_discharge == 3 &
        smr02_outcome_of_pregnancy %in% c(1, 3, 4, 5),
      T,
      F
    )
  ) %>%
  mutate(
    smr02_miscarriage = if_else(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion %in% c(1, 2, 8, 9) &
        smr02_baby == "baby1",
      T,
      F
    )
  ) %>%
  mutate(
    smr02_molar_pregnancy = if_else(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion == 3 &
        smr02_baby == "baby1",
      T,
      F
    )
  ) %>%
  mutate(
    smr02_ectopic_pregnancy = if_else(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion == 6 &
        smr02_baby == "baby1",
      T,
      F
    )
  )  %>%
  mutate(
    smr02_stillbirth = if_else(
      smr02_condition_on_discharge == 3 &
        smr02_outcome_of_pregnancy == 2,
      T,
      F
    )
  ) %>%
  mutate(
    smr02_termination = if_else(
      smr02_condition_on_discharge == 2 &
        smr02_type_of_abortion == 4 &
        smr02_baby == "baby1",
      T,
      F
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

nrow_temp3 <- nrow(data_smr02)

#record number filtered out
filter_2 <- data.frame(stage = 2,
                       den = nrow_temp2,
                       num = nrow_temp3,
                       task = "Select only those with a defined outcome")


#### Determine the overall outcome type of the fetus ####
data_smr02 %<>%
  mutate(
    smr02_outcome_type = case_when(
      smr02_termination == T ~ "Termination",
      smr02_live_birth == T ~ "Live birth",
      smr02_stillbirth == T ~ "Stillbirth",
      smr02_ectopic_pregnancy == 1 ~ "Ectopic pregnancy",     
      smr02_molar_pregnancy == 1 ~ "Molar pregnancy",
      smr02_miscarriage == 1 ~ "Miscarriage"
    )
  ) 



#### Replace NAs with R's NA type, and remove any heights and weights which are not feasible ####
data_smr02 %<>%
  mutate(smr02_baby_chi_number = case_when(
    smr02_baby_chi_number != "NA" ~ smr02_baby_chi_number,
    T ~ NA_character_
  )) %>%
  # mutate(smr02_baby_chi_number = case_when(
  #   smr02_baby_chi_number != "NA" ~ smr02_baby_chi_number,
  #   T ~ NA_character_
  # )) %>%
  mutate(
    smr02_presentation_at_delivery = case_when(
      smr02_presentation_at_delivery != "NA" ~ smr02_presentation_at_delivery,
      T ~ NA_character_
    )
  ) %>%
  mutate(
    smr02_outcome_of_pregnancy = case_when(
      smr02_outcome_of_pregnancy != "NA" ~ smr02_outcome_of_pregnancy,
      T ~ NA_character_
    )
  ) %>%
  mutate(smr02_birthweight = case_when(
    smr02_birthweight != "NA" ~ smr02_birthweight,
    T ~ NA_character_
  )) %>%
  mutate(smr02_apgar_5_minutes = case_when(
    smr02_apgar_5_minutes != "NA" ~ smr02_apgar_5_minutes,
    T ~ NA_character_
  )) %>%
  mutate(smr02_sex = case_when(smr02_sex != "NA" ~ smr02_sex,
                               T ~ NA_character_)) %>%
  mutate(smr02_mode_of_delivery = case_when(
    smr02_mode_of_delivery != "NA" ~ smr02_mode_of_delivery,
    T ~ NA_character_
  ))


not_na_height_before_recoding <- data_smr02 %>% select(smr02_height) %>% filter(!is.na(smr02_height)) %>% nrow()
not_na_weight_before_recoding <- data_smr02 %>% select(smr02_weight_of_mother) %>% filter(!is.na(smr02_weight_of_mother)) %>% nrow()
not_na_birthweight_before_recoding <- data_smr02 %>% select(smr02_birthweight) %>% filter(!is.na(smr02_birthweight)) %>% nrow()
not_na_gestation_before_recoding <- data_smr02 %>% select(smr02_estimated_gestation) %>% filter(!is.na(smr02_estimated_gestation)) %>% nrow()

data_smr02 %<>%
  mutate(
    smr02_height = if_else(
      smr02_height < 10 | smr02_height > 200,
      as.numeric(NA),
      smr02_height
    )
  ) %>%
  mutate(
    smr02_weight_of_mother = if_else(
      smr02_weight_of_mother < 10 | smr02_weight_of_mother > 600,
      as.numeric(NA),
      smr02_weight_of_mother
    )
  ) %>%
  mutate(smr02_birthweight = as.numeric(smr02_birthweight)) %>%
  mutate(
    smr02_birthweight = if_else(
      smr02_birthweight < 10 | smr02_birthweight > 9000,
      NA_real_,
      smr02_birthweight
    )
  ) %>%
  mutate(
    smr02_booking_smoking_history = case_when(
      smr02_booking_smoking_history == 9 ~ NA_real_,
      T ~ smr02_booking_smoking_history
    )
  ) %>%
  mutate(smr02_diabetes = case_when(smr02_diabetes == 9 ~ NA_real_,
                                    T ~ smr02_diabetes)) %>%
  mutate(smr02_ofc = as.numeric(smr02_ofc))



#### Replace baby CHI with UPI ####
# We were planning on replacing Baby CHI with Baby UPI, but this led to an issue where twins were both being assigned the
# same UPI number. They were being identified as duplicates and then they were having their CHIs removed as they seemed
# to be duplicates. For noe, just rename Baby CHI to Baby UPI. 
data_smr02 %<>%
  rename(smr02_baby_upi_number = smr02_baby_chi_number)


#### Check the validity of the baby UPIs. If they're invalid assign a dummy UPI ####
data_smr02 %<>%
  replace_with_na_at(.vars = c("smr02_sex"),
                     condition = ~.x %in% c("9", "0")) %>% 
  replace_with_na_at(.vars = c("smr02_booking_smoking_history", "smr02_diabetes"),
                     condition = ~.x == 9) %>%  
  mutate(smr02_baby_upi_number = chi_pad(smr02_baby_upi_number)) %>% 
  mutate(validity = chi_check(smr02_baby_upi_number)) %>% 
  mutate(smr02_baby_upi_number = if_else(validity == "Valid CHI", 
                                         smr02_baby_upi_number,
                                         NA_character_)) %>% 
  select(-validity) %>% 
  mutate(smr02_baby_upi_number = if_else(
    is.na(smr02_baby_upi_number),
    paste0(
      "52",
      str_pad(
        string = row_number(),
        width = 8,
        side = "left",
        pad = "0"
      )
    ),
    smr02_baby_upi_number
  )) %>% 
  mutate(smr02_sex = case_when(smr02_sex == "1" ~ "M", 
                               smr02_sex == "2" ~ "F", 
                               T ~ NA_character_))


#### Add on ethnicity and healthboard name ####
data_smr02 %<>%
  left_join(smr02_ethnicity_lookup) %>%
  left_join(hb_lookup, by =  c("smr02_hb2019" = "healthboard_code")) %>% 
  mutate(smr02_hb2019 = healthboard) %>% 
  select(-healthboard)


data_smr02 %<>%
  mutate(
    smr02_feasible_gestation = case_when(
      smr02_outcome_type == "Live birth" &
        smr02_estimated_gestation %in% c(feasible_gestation_lb) ~ T,
      smr02_outcome_type == "Stillbirth" &
        smr02_estimated_gestation %in% c(feasible_gestation_sb) ~ T,
      (
        smr02_outcome_type == "Miscarriage" |
          smr02_outcome_type == "Molar pregnancy" |
          smr02_outcome_type == "Ectopic pregnancy"
      ) &
        smr02_estimated_gestation %in% c(feasible_gestation_miscarriage) ~ T,
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
      smr02_outcome_type == "Live birth" &
        smr02_estimated_gestation %in% c(feasible_gestation_lb) ~ T,
      smr02_outcome_type == "Stillbirth" &
        smr02_estimated_gestation %in% c(feasible_gestation_sb) ~ T,
      (
        smr02_outcome_type == "Miscarriage" |
          smr02_outcome_type == "Molar pregnancy" |
          smr02_outcome_type == "Ectopic pregnancy"
      ) &
        smr02_estimated_gestation %in% c(feasible_gestation_miscarriage) ~ T,
      smr02_outcome_type == "Termination" &
        smr02_estimated_gestation %in% c(feasible_gestation_termination) ~ T,
      T ~ F
    )
  ) 

not_na_height_after_recoding <- data_smr02 %>% select(smr02_height) %>% filter(!is.na(smr02_height)) %>% nrow()
not_na_weight_after_recoding <- data_smr02 %>% select(smr02_weight_of_mother) %>% filter(!is.na(smr02_weight_of_mother)) %>% nrow()
not_na_birthweight_after_recoding <- data_smr02 %>% select(smr02_birthweight) %>% filter(!is.na(smr02_birthweight)) %>% nrow()
feasible_gestation_after_recoding <- data_smr02 %>% select(smr02_feasible_gestation) %>% filter(smr02_feasible_gestation == T) %>% nrow()

# Record the number recoded
recode_1 <- data.frame(variable = "height",
                       den = not_na_height_before_recoding,
                       num = not_na_height_after_recoding)

recode_2 <- data.frame(variable = "weight",
                       den = not_na_weight_before_recoding,
                       num = not_na_weight_after_recoding)

recode_3 <- data.frame(variable = "birthweight",
                       den = not_na_birthweight_before_recoding,
                       num = not_na_birthweight_after_recoding)

recode_4 <- data.frame(variable = "gestation",
                       den = not_na_gestation_before_recoding,
                       num = feasible_gestation_after_recoding)


#### Decide what the estimated gestation is based on the outcome type ####
data_smr02 %<>%
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
  mutate(smr02_pregnancy_end_date = if_else(
    !is.na(smr02_date_of_delivery),
    smr02_date_of_delivery,
    smr02_admission_date
  )) 

nrow_temp5 <- nrow(data_smr02)


#### Handle duplicate SMR02 events ####
data_smr02_livebirths_and_stillbirths <- data_smr02 %>%
  filter(smr02_outcome_type %in% c("Live birth", "Stillbirth")) %>%
  group_by(smr02_baby_upi_number) %>%
  # If a baby UPI appears more than once, but there was only one birth that pregnancy, the case is likely a duplicate
  # This can occur when more than one SMR02 record is generated during a birth due to the patient being transferred at some point
  # Note - this does not deduplicate multiples - but as of March 2021 there don't appear to be any multiples impacted by this issue
  mutate(likely_duplicate = case_when(row_number() > 1 & smr02_num_of_births_this_pregnancy == 1 ~ T,
                                      T ~ F)) %>%
  filter(likely_duplicate == F) %>%
  select(-likely_duplicate) 

data_smr02_miscarriage_ectopic_and_termination <- data_smr02 %>%
  filter(smr02_outcome_type %in% c("Miscarriage", "Ectopic pregnancy", "Molar pregnancy", "Termination"))

data_smr02_miscarriage_ectopic_and_termination <- 
  moving_index_deduplication(data_smr02_miscarriage_ectopic_and_termination, smr02_upi_number, smr02_admission_date, dedupe_period)

data_smr02_miscarriage_ectopic_and_termination <- data_smr02_miscarriage_ectopic_and_termination %>%
  group_by(smr02_upi_number, cops_event) %>%
  mutate(revised_conception_date = min(smr02_estimated_conception_date)) %>%
  ungroup()

data_smr02_miscarriage_ectopic_and_termination <-
  moving_index_deduplication(data_smr02_miscarriage_ectopic_and_termination, smr02_upi_number, revised_conception_date, dedupe_period)

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
    #smr02_baby_chi_number,
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


### Identify additional baby outcomes ####
# Premature Rupture of Membranes (PROM)
data_smr02 %<>%
  mutate(
    smr02_premature_rupture_of_membrane = case_when(
      str_sub(smr02_indication_for_operative_del, 1, 3) == "O42" ~ T,
      str_sub(smr02_main_condition, 1, 3) == "O42" ~ T,
      str_sub(smr02_other_condition_1, 1, 3) == "O42" ~ T,
      str_sub(smr02_other_condition_2, 1, 3) == "O42" ~ T,
      str_sub(smr02_other_condition_3, 1, 3) == "O42" ~ T,
      str_sub(smr02_other_condition_4, 1, 3) == "O42" ~ T,
      str_sub(smr02_other_condition_5, 1, 3) == "O42" ~ T,
      T ~ F
    )
  )

# Induction of Labour
data_smr02 %<>%
  mutate(
    smr02_induction_of_labour_category = case_when(
      smr02_induction_of_labour == 0 ~ "N",
      smr02_induction_of_labour >= 1 & smr02_induction_of_labour <= 8 ~ "Y",
      T ~ "U"
    )
  )

# Duration of Labour Category
data_smr02 %<>%
  mutate(
    smr02_duration_of_labour_category = case_when(
      smr02_duration_of_labour == 0 ~ "0",
      smr02_duration_of_labour >= 1 & smr02_duration_of_labour <= 98 ~ ">0",
      T ~ "U"
    )
  )


# Mode of Delivery Category
data_smr02 %<>%
  mutate(
    smr02_mode_of_delivery_category = case_when(
      smr02_mode_of_delivery %in% c("0", "1", "2", "5", "6", "A", "B", "C", "D", "E") ~ "Vaginal",
      smr02_mode_of_delivery == "7" ~ "Elective CS",
      smr02_mode_of_delivery == "8" ~ "Emergency CS",
      smr02_mode_of_delivery == "9" |
        smr02_mode_of_delivery == "NA" |
        is.na(smr02_mode_of_delivery) ~ "U"
    )
  )

# Onset of Delivery Process Classification
data_smr02 %<>%
  mutate(
    smr02_onset_of_delivery_process = case_when(
      smr02_premature_rupture_of_membrane == T ~ "Spontaneous",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category == "Y" ~ "Medically Indicated",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category %in% c("N", "U") & smr02_mode_of_delivery_category == "Vaginal" ~ "Spontaneous",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category %in% c("N", "U") & smr02_duration_of_labour_category == "0" & smr02_mode_of_delivery_category %in% c("Elective CS", "Emergency CS") ~ "Medically Indicated",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category %in% c("N", "U") & smr02_duration_of_labour_category == ">0" & smr02_mode_of_delivery_category %in% c("Elective CS", "Emergency CS") ~ "Spontaneous",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category %in% c("N", "U") & smr02_duration_of_labour_category == "U" & smr02_mode_of_delivery_category == "Elective CS" ~ "Medically Indicated",
      smr02_premature_rupture_of_membrane == F & smr02_induction_of_labour_category %in% c("N", "U") & smr02_duration_of_labour_category == "U" & smr02_mode_of_delivery_category == "Emergency CS" ~ "Spontaneous",
      T ~ "Undefined"
    )
  )


# Birthweight Centile
birthweight_centiles <- read_csv(paste0(folder_temp_data, "who_birthweight_centiles_lms.csv")) %>%
  clean_names() %>%
  rename_with(~ paste0("smr02_birthweight_", .)) %>%
  mutate(smr02_birthweight_sex = case_when(smr02_birthweight_sex == 1 ~ "M",
                                           smr02_birthweight_sex == 2 ~ "F"))

data_smr02 %<>% left_join(birthweight_centiles, by = c("smr02_sex" = "smr02_birthweight_sex", "smr02_estimated_gestation" = "smr02_birthweight_gestation"))

data_smr02 %<>% 
  mutate(smr02_birthweight_z = (((((smr02_birthweight/1000)/smr02_birthweight_m)^smr02_birthweight_l))-1)/(smr02_birthweight_l*smr02_birthweight_s) ) %>%
  mutate(smr02_birthweight_percentile = 100 * pnorm(smr02_birthweight_z, 0, 1))


# OFC LMS
ofc_lms <- read_csv(paste0(folder_temp_data, "who_ofc_lms.csv")) %>%
  clean_names() %>%
  rename_with(~ paste0("smr02_ofc_", .)) %>%
  mutate(smr02_ofc_sex = case_when(smr02_ofc_sex == 1 ~ "M",
                                           smr02_ofc_sex == 2 ~ "F"))

data_smr02 %<>% left_join(ofc_lms, by = c("smr02_sex" = "smr02_ofc_sex", "smr02_estimated_gestation" = "smr02_ofc_estimated_gestation"))

data_smr02 %<>% 
  mutate(smr02_ofc_z = (((((smr02_ofc/10)/smr02_ofc_m)^smr02_ofc_l))-1)/(smr02_ofc_l*smr02_ofc_s) ) %>%
  mutate(smr02_ofc_percentile = 100 * pnorm(smr02_ofc_z, 0, 1))


#### Write Data ####

write_rds(data_smr02, paste0(folder_temp_data, "smr02.rds"), compress = "gz")

toc()



#record number filtered out
filter_3 <- data.frame(stage = 3,
                       den = nrow_temp5,
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

rm (data_smr02, data_smr02_livebirths_and_stillbirths, data_smr02_miscarriage_ectopic_and_termination,
    data_smr02_miscarriage_ectopic_and_termination_1, filter_1, filter_2, filter_3, recode_1, 
    recode_2, recode_3, recode_4, data_smr02_temp_raw, smr02_filters, smr02_recoded,
    birthweight_centiles, ofc_lms)
gc()
