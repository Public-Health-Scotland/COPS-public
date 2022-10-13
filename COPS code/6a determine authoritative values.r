fetus_level_temp_1 <- read_rds(paste0(folder_temp_data, "script5_pregnancy_record_matched.rds"))

urban_rural_8_labels <- read_csv(paste0(folder_temp_data, "ur8_labels.csv"))

simd_pc <- readRDS(simd_hb_lookup) %>% select(pc7,simd2020v2_sc_quintile)
ur_pc <- haven::read_sav(ur_lookup) %>% select(pc7,UR6_2016, UR8_2016)

mother_upi_dobs_from_chi <- read_rds(paste0(folder_temp_data, "mother_upis_dob_from_CHI.rds"))

ethnicity_vacc_lookup_full <-  readRDS(ethnicity_vacc_lookup_path) %>% 
  clean_names() %>% 
  rename_with( ~ paste0("vacc_", .))

ethnicity_labels <- ethnicity_vacc_lookup_full %>% 
  select(vacc_ethnic_code, vacc_ethnic_code_desc_long) %>% 
  distinct() %>% 
  arrange(vacc_ethnic_code)

ethnicity_vacc_lookup <- ethnicity_vacc_lookup_full %>% 
  select(vacc_upi_number, vacc_ethnic_code)

#### Determine Pregnancy End Date ####
fetus_level_temp_2 <- fetus_level_temp_1 %>%
  rowwise() %>%
  mutate(x_pregnancy_end_date = first_(
    c(
      as.Date(nhslb_baby_dob),
      as.Date(nrslb_date_of_birth),
      as.Date(nrssb_date_of_birth),
      as.Date(smr02_pregnancy_end_date),
      as.Date(aas_date_of_termination),
      as.Date(smr01_cops_admission_date),
      as.Date(gp_losses_start_date)
    )
  )) %>%
  mutate(x_pregnancy_end_date = as_date(x_pregnancy_end_date)) # Remove any remaining datetime objects, converting to dates


#### Determine Gestation at Outcome Date ####
#pick gestations from records with recorded gestations first, and then calculated ones from an booking records, 
#and finally imputed gestations. 
fetus_level_temp_3 <- fetus_level_temp_2 %>%
  rowwise() %>%
  mutate(calc_gestation = case_when(
    !is.na(anbooking_gestationat_booking) & anbooking_assumed_gestation == FALSE
          ~ as.numeric(floor(difftime
                            (x_pregnancy_end_date, anbooking_booking_date, units="weeks")) + anbooking_gestationat_booking),
          T ~ NA_real_)) %>% 
  mutate(x_gestation_at_outcome = case_when(!is.na(nhslb_gest_period) & nhslb_assumed_gestation == 0
                                            ~ nhslb_gest_period,
                                            !is.na(nrssb_duration_of_pregnancy) & nrssb_assumed_gestation == 0
                                            ~ nrssb_duration_of_pregnancy,
                                            !is.na(smr02_estimated_gestation) & smr02_assumed_gestation == FALSE
                                            ~ smr02_estimated_gestation,
                                            !is.na(aas_estimated_gestation) & aas_assumed_gestation == 0
                                            ~ aas_estimated_gestation,
                                            outcome == "Live birth" & calc_gestation %in% feasible_gestation_lb
                                            ~ calc_gestation,
                                            outcome == "Stillbirth" & calc_gestation %in% feasible_gestation_sb
                                            ~ calc_gestation,
                                            outcome == "Termination" & calc_gestation %in% feasible_gestation_termination
                                            ~ calc_gestation,
                                            (outcome == "Molar pregnancy" | outcome == "Ectopic pregnancy" | outcome == "Miscarriage") 
                                             & calc_gestation %in% feasible_gestation_miscarriage
                                            ~ calc_gestation,
                                            !is.na(nhslb_gest_period) ~ nhslb_gest_period,
                                            !is.na(nrssb_duration_of_pregnancy) ~ nrssb_duration_of_pregnancy,
                                            !is.na(smr02_estimated_gestation) ~ smr02_estimated_gestation,
                                            !is.na(aas_estimated_gestation) ~ aas_estimated_gestation,
                                            !is.na(nrslb_estimated_gestation) ~ nrslb_estimated_gestation,
                                            !is.na(smr01_gestation) ~ smr01_gestation,
                                            !is.na(gp_losses_gestation) ~ gp_losses_gestation)) %>% 
  mutate(x_recorded_gestation = case_when(!is.na(nhslb_gest_period) & nhslb_assumed_gestation == 0 |
                                         !is.na(nrssb_duration_of_pregnancy) & nrssb_assumed_gestation == 0 |
                                         !is.na(smr02_estimated_gestation) & smr02_assumed_gestation == 0 |
                                         !is.na(aas_estimated_gestation) & aas_assumed_gestation == 0 ~ TRUE,
                                         T ~ FALSE)) %>% 
  mutate(x_an_calc_gest = case_when(x_recorded_gestation == FALSE &
                                    ((outcome == "Live birth" & calc_gestation %in% feasible_gestation_lb) |
                                    (outcome == "Stillbirth" & calc_gestation %in% feasible_gestation_sb) |
                                    (outcome == "Termination" & calc_gestation %in% feasible_gestation_termination) |
                                    ((outcome == "Molar pregnancy" | outcome == "Ectopic pregnancy" | outcome == "Miscarriage") 
                                    & calc_gestation %in% feasible_gestation_miscarriage)) 
                                    ~ TRUE,
                                    T~ FALSE)) %>% 
  mutate(x_assumed_gestation = case_when(outcome != "Ongoing" & x_recorded_gestation == FALSE & x_an_calc_gest == FALSE
                                         ~ TRUE,
                                         T ~ FALSE)
         ) %>% 
  mutate(x_gestation_ascertainment = case_when(x_recorded_gestation == TRUE ~ "Gestation recorded on end of pregnancy record",
                                               x_an_calc_gest == TRUE ~ "Gestation calculated from gestation at booking",
                                               x_assumed_gestation == TRUE | outcome == "Unknown" ~ "Gestation imputed based on outcome of pregnancy",
                                               outcome == "Ongoing" ~ "Ongoing pregnancy")) %>% 
  select(-c(x_recorded_gestation, x_an_calc_gest, x_assumed_gestation, calc_gestation))


#### Determine date of conception from end of pregnancy date and gestation ####
fetus_level_temp_4 <- fetus_level_temp_3 %>%
  rowwise() %>%
  mutate(x_est_conception_date = case_when(outcome == "Ongoing" 
                                           ~ as_date(anbooking_estimated_conception_date),
                                           outcome != "Ongoing" 
                                           ~ as_date(x_pregnancy_end_date - (weeks(x_gestation_at_outcome) - weeks(2))))) 


#### Determine Mother DOB ####
fetus_level_temp_5 <- fetus_level_temp_4 %>%
  left_join(mother_upi_dobs_from_chi %>% rename(mother_upi = upi)) %>% 
  rowwise() %>%
  mutate(x_mother_dob = first_(
    c(
      as.Date(chi_dob),
      as.Date(smr02_dob),
      as.Date(nhslb_mothers_dob),
      as.Date(aas_date_of_birth),
      as.Date(smr01_dob),
      as.Date(anbooking_mothers_dob),
      as.Date(gp_losses_date_of_birth)
    )
  ))


#### Determine Mother Age at Conception ####
fetus_level_temp_6 <- fetus_level_temp_5 %>%
  rowwise() %>%
  mutate(x_mother_age_at_conception = floor(as.numeric(difftime(x_est_conception_date, x_mother_dob, units="weeks"))
                                            / 52.25)) %>% 
  mutate(x_mother_age_at_conception = case_when(x_mother_age_at_conception %in% feasible_age ~ x_mother_age_at_conception,
                                                T ~ NA_real_))

tabyl(fetus_level_temp_6$x_mother_age_at_conception)


#### Determine Mother Age at Outcome ####
fetus_level_temp_7 <- fetus_level_temp_6 %>%
  rowwise() %>%
  mutate(x_mother_age_at_outcome = floor(as.numeric(difftime(x_pregnancy_end_date, x_mother_dob, units="weeks"))
                                         / 52.25))%>% 
  mutate(x_mother_age_at_outcome = case_when(x_mother_age_at_outcome %in% feasible_age ~ x_mother_age_at_outcome,
                                                T ~ NA_real_))

tabyl(fetus_level_temp_7$x_mother_age_at_outcome)

rm(fetus_level_temp_1, fetus_level_temp_2, fetus_level_temp_3, fetus_level_temp_4, fetus_level_temp_5, fetus_level_temp_6)

#### Determine postcode ####
fetus_level_temp_8 <- fetus_level_temp_7 %>%
  rowwise() %>%
  mutate(x_postcode = first_(
    c(
      nrslb_postcode,
      smr02_dr_postcode,
      aas_pc7,
      nrssb_postcode,
      smr01_dr_postcode
    )))

#Join geographical variables based on postcode ####

fetus_level_temp_8a <- fetus_level_temp_8 %>% left_join(ur_pc, by =c( "x_postcode" ="pc7")) %>%
  left_join(simd_pc , by =c( "x_postcode" ="pc7")) %>%
  rename(pc_simd = simd2020v2_sc_quintile,  pc_UR8 = UR8_2016)

#### Determine Healthboard of Residence ####
fetus_level_temp_9 <- fetus_level_temp_8a %>%
  rowwise() %>%
  mutate(x_hbres = first_(
    c(
      nhslb_hb2019,
      nrslb_hb2019,
      smr02_hb2019,
      aas_hb2019,
      nrssb_hb2019,
      smr01_hbres_currentdate,
      anbooking_hb2019,
      eave_hb
    )))

tabyl(fetus_level_temp_9$x_hbres)

#### Determine Mother SIMD ####
fetus_level_temp_10 <- fetus_level_temp_9 %>%
  rowwise() %>%
  mutate(x_simd = first_(
    c(
      nhslb_simd2020v2_sc_quintile,
      nrslb_simd2020v2_sc_quintile,
      smr02_simd2020v2_sc_quintile,
      aas_simd2020v2_sc_quintile,
      nrssb_simd2020v2_sc_quintile,
      smr01_simd2020v2_sc_quintile,
      anbooking_simd2020v2_sc_quintile,
      pc_simd, 
      eave_simd
    )
  ))

tabyl(fetus_level_temp_10$x_simd)


#### Determine Mother BMI ####
fetus_level_temp_11 <- fetus_level_temp_10 %>%
  rowwise() %>%
  mutate(smr02_bmi = round(smr02_weight_of_mother/((smr02_height/100) ^ 2))) %>% 
  mutate(
    smr02_bmi = case_when(smr02_bmi %in% feasible_bmi ~ smr02_bmi,
                          T ~ NA_real_)) %>% 
  mutate(x_bmi = first_(c(smr02_bmi, q_bmi)))

tabyl(fetus_level_temp_11$x_bmi)

#### Determine Mother Smoking Status ####
fetus_level_temp_12 <- fetus_level_temp_11 %>%
  rowwise() %>%
  mutate(x_booking_smoking_status = first_(c(anbooking_smoking_status, smr02_booking_smoking_history))) %>% 
  mutate(x_booking_smoking_status= as.character(x_booking_smoking_status)) %>% 
  mutate(x_booking_smoking_status = recode(x_booking_smoking_status, "0" = "non-smoker", 
                                                                      "1" = "smoker", 
                                                                      "2" = "ex-smoker")) %>% 
  mutate(x_gp_smoking_status = case_when(eave_non_smoker >= 1 & eave_smoker >= 1 & eave_ex_smoker >= 1 
                                         ~ NA_character_,
                                         eave_non_smoker == 0 & eave_smoker >= 1 & eave_ex_smoker >= 1 
                                         ~ NA_character_,
                                         eave_non_smoker >= 1 & eave_smoker >= 1 & eave_ex_smoker == 0 
                                         ~ NA_character_,
                                         eave_non_smoker >= 1 & eave_smoker == 0 & eave_ex_smoker >= 1 
                                         ~ "ex-smoker",
                                         eave_non_smoker >= 1 & eave_smoker == 0 & eave_ex_smoker == 0 
                                         ~ "non-smoker",
                                         eave_non_smoker == 0 & eave_smoker >= 1 & eave_ex_smoker == 0
                                         ~ "smoker",
                                         eave_non_smoker == 0 & eave_smoker == 0 & eave_ex_smoker >= 1
                                         ~ "ex-smoker",
                                         T ~ NA_character_)) %>% 
  mutate(x_overall_smoking_status = first_(c(x_booking_smoking_status, 
                                             x_gp_smoking_status))) %>% 
  mutate(x_overall_smoking_status = case_when ( (x_gp_smoking_status == "smoker" | x_gp_smoking_status == "ex-smoker") & x_booking_smoking_status == "non-smoker" ~ "ex-smoker",
                                                T ~ x_overall_smoking_status))

tabyl(fetus_level_temp_12$x_booking_smoking_status)
tabyl(fetus_level_temp_12$x_gp_smoking_status)
tabyl(fetus_level_temp_12$x_overall_smoking_status)


#### Determine Mother Ethnicity ####

fetus_level_temp_13 <- fetus_level_temp_12 %>%
  rowwise() %>%
  left_join(ethnicity_vacc_lookup, by = c("mother_upi" = "vacc_upi_number")) %>% 
  mutate(smr02_ethnic_group = if_else(smr02_ethnic_group == "98" | smr02_ethnic_group == "99", NA_character_, smr02_ethnic_group)) %>% 
  mutate(smr01_ethnic_group = if_else(smr01_ethnic_group == "98" | smr01_ethnic_group == "99", NA_character_, smr01_ethnic_group)) %>% 
  mutate(anbooking_ethnicity = if_else(anbooking_ethnicity == "98" | anbooking_ethnicity == "99", NA_character_, anbooking_ethnicity)) %>% 
  mutate(vacc_ethnic_code = if_else(vacc_ethnic_code == "98" | vacc_ethnic_code == "99", NA_character_, vacc_ethnic_code)) %>% 
  mutate(x_ethnicity_code = first_(c(smr02_ethnic_group, 
                                smr01_ethnic_group,
                                anbooking_ethnicity,
                                vacc_ethnic_code
  ))) %>% 
  left_join(ethnicity_labels, by = c("x_ethnicity_code" = "vacc_ethnic_code")) %>% 
  rename(x_ethnicity_desc = vacc_ethnic_code_desc_long)

tabyl(fetus_level_temp_13$x_ethnicity_code)
tabyl(fetus_level_temp_13$x_ethnicity_desc)

#### Determine Mother Urban/Rural Classification ####
fetus_level_temp_13a <- fetus_level_temp_13 %>%
  mutate(x_ur8 = first_(
    c(pc_UR8, 
      eave_ur8)
    )) %>%
left_join(urban_rural_8_labels, by=c("x_ur8"="ur8"))

tabyl(fetus_level_temp_13a$x_urban_rural_8_description)
#table(fetus_level_temp_13a$pc_UR8, fetus_level_temp_13a$eave_ur8)

#### Determine Baby Sex ####
fetus_level_temp_14 <- fetus_level_temp_13a %>%
  rowwise() %>%
  mutate(x_baby_sex = first_(
    c(
      nhslb_sex,
      nrslb_sex,
      nrssb_sex,
      smr02_sex
    )
  ))

tabyl(fetus_level_temp_14$x_baby_sex, show_na = F)

#### Determine Baby Weight ####
fetus_level_temp_15 <- fetus_level_temp_14 %>%
  rowwise() %>%
  mutate(x_baby_weight = first_(
    c(
      smr02_birthweight,
      nrssb_weight_of_foetus
    )
  ))

tabyl(fetus_level_temp_15$x_baby_weight, show_na = F)


#### Determine Baby DOB ####
fetus_level_temp_16 <- fetus_level_temp_15 %>%
  rowwise() %>%
  mutate(x_baby_dob = first_(
    c(
      as.Date(nhslb_baby_dob),
      as.Date(nrslb_date_of_birth),
      as.Date(nrssb_date_of_birth)
    )
  ))

rm(fetus_level_temp_7, fetus_level_temp_8, fetus_level_temp_9, fetus_level_temp_10, fetus_level_temp_11, fetus_level_temp_12,
   fetus_level_temp_13, fetus_level_temp_13a, fetus_level_temp_14, fetus_level_temp_15)

#### Determine Multiple/Singleton Status ####
fetus_level_temp_17 <- fetus_level_temp_16 %>%
  group_by(pregnancy_id) %>%
  mutate(x_births_this_pregnancy = max_(c(nrslb_total_births_live_and_still, nrssb_total_births_live_and_still, smr02_num_of_outcomes_this_pregnancy, nhslb_num_of_births))) %>%
  ungroup()

tabyl(fetus_level_temp_17$x_births_this_pregnancy)
tabyl(fetus_level_temp_17, outcome, x_births_this_pregnancy)


#### Determine which pregnancies have an unknown outcome #### 
latest_observed_end_date <- max_(fetus_level_temp_17$pregnancy_end_date)

fetus_level_temp_18 <- fetus_level_temp_17 %>%
  mutate(outcome = case_when( outcome == "Ongoing" & as_date(x_est_conception_date) + weeks(42) <= as_date(latest_observed_end_date) 
                              ~ "Unknown",
                              T ~ outcome )) %>% 
  mutate(x_pregnancy_end_date = case_when(outcome == "Unknown" ~ as_date(x_est_conception_date) + weeks(42),
                                          T ~ as_date(x_pregnancy_end_date  ))) %>% 
  mutate(x_gestation_at_outcome = case_when(outcome == "Unknown" ~ 44,
                               T ~ x_gestation_at_outcome))
  
#### Fix apparent mid-pregnancy terminations #### 
fetus_level_temp_28 <- fetus_level_temp_18 %>%
  mutate(days_between_top_end = if_else(!is.na(aas_date_of_termination), difftime(x_pregnancy_end_date, aas_date_of_termination, units = "days"),
                                        difftime(x_pregnancy_end_date, smr02_admission_date, units = "days"))) %>% 
  mutate(outcome = case_when(outcome == "Termination" & days_between_top_end >= 30 & 
                               (nhslb_outcome_type == "Live birth" | nrslb_outcome_type == "Live birth" | smr02_outcome_type == "Live birth") 
                             ~ "Live birth",
                             outcome == "Termination" & days_between_top_end >= 30 & 
                               (nrssb_outcome_type == "Stillbirth" | smr02_outcome_type == "Stillbirth") 
                             ~ "Stillbirth",
                              T ~ outcome )) %>% 
  select(-days_between_top_end)

#### Assign infant death status ####
data_infant_deaths_triplicate <- read_rds(paste0(folder_temp_data, "infant_deaths.rds")) %>% 
  select(
    nrs_triplicate_id,
    date_of_baby_death,
    underlying_cause_of_baby_death,
    cause_of_baby_death_0,
    cause_of_baby_death_1,
    cause_of_baby_death_2,
    cause_of_baby_death_3,
    cause_of_baby_death_4,
    cause_of_baby_death_5,
    cause_of_baby_death_6,
    cause_of_baby_death_7,
    cause_of_baby_death_8,
    cause_of_baby_death_9
  ) %>% 
  filter(nrs_triplicate_id != "NA_NA_NA") %>%
  filter(nrs_triplicate_id != "0_0_0") %>%
  rename(triplicate_date_of_baby_death = date_of_baby_death) %>%
  rename(triplicate_underlying_cause_of_baby_death = underlying_cause_of_baby_death) %>%
  rename(triplicate_cause_of_baby_death_0 = cause_of_baby_death_0) %>%
  rename(triplicate_cause_of_baby_death_1 = cause_of_baby_death_1) %>%
  rename(triplicate_cause_of_baby_death_2 = cause_of_baby_death_2) %>%
  rename(triplicate_cause_of_baby_death_3 = cause_of_baby_death_3) %>%
  rename(triplicate_cause_of_baby_death_4 = cause_of_baby_death_4) %>%
  rename(triplicate_cause_of_baby_death_5 = cause_of_baby_death_5) %>%
  rename(triplicate_cause_of_baby_death_6 = cause_of_baby_death_6) %>%
  rename(triplicate_cause_of_baby_death_7 = cause_of_baby_death_7) %>%
  rename(triplicate_cause_of_baby_death_8 = cause_of_baby_death_8) %>%
  rename(triplicate_cause_of_baby_death_9 = cause_of_baby_death_9)
  

data_infant_deaths_chi <- read_rds(paste0(folder_temp_data, "infant_deaths.rds")) %>% 
  select(
    chi,
    date_of_baby_death,
    underlying_cause_of_baby_death,
    cause_of_baby_death_0,
    cause_of_baby_death_1,
    cause_of_baby_death_2,
    cause_of_baby_death_3,
    cause_of_baby_death_4,
    cause_of_baby_death_5,
    cause_of_baby_death_6,
    cause_of_baby_death_7,
    cause_of_baby_death_8,
    cause_of_baby_death_9
  ) %>% 
  filter(!is.na(chi)) %>%
  rename(chi_date_of_baby_death = date_of_baby_death) %>%
  rename(chi_underlying_cause_of_baby_death = underlying_cause_of_baby_death) %>%
  rename(chi_cause_of_baby_death_0 = cause_of_baby_death_0) %>%
  rename(chi_cause_of_baby_death_1 = cause_of_baby_death_1) %>%
  rename(chi_cause_of_baby_death_2 = cause_of_baby_death_2) %>%
  rename(chi_cause_of_baby_death_3 = cause_of_baby_death_3) %>%
  rename(chi_cause_of_baby_death_4 = cause_of_baby_death_4) %>%
  rename(chi_cause_of_baby_death_5 = cause_of_baby_death_5) %>%
  rename(chi_cause_of_baby_death_6 = cause_of_baby_death_6) %>%
  rename(chi_cause_of_baby_death_7 = cause_of_baby_death_7) %>%
  rename(chi_cause_of_baby_death_8 = cause_of_baby_death_8) %>%
  rename(chi_cause_of_baby_death_9 = cause_of_baby_death_9)

fetus_level_temp_19 <- fetus_level_temp_28 %>% 
  mutate(x_nrs_lb_triplicate_id = case_when(!is.na(nrslb_year_of_registration) & !is.na(nrslb_registration_district) & !is.na(nrslb_entry_number) ~
                                        paste0(nrslb_year_of_registration, "_", nrslb_registration_district, "_", nrslb_entry_number)))

fetus_level_temp_20 <- fetus_level_temp_19 %>%
  left_join(data_infant_deaths_triplicate, by = c("x_nrs_lb_triplicate_id" = "nrs_triplicate_id")) %>%
  left_join(data_infant_deaths_chi, by = c("baby_upi" = "chi")) %>%
  rowwise() %>%
  mutate(x_date_of_baby_death = first_(c(triplicate_date_of_baby_death, chi_date_of_baby_death))) %>%
  mutate(x_underlying_cause_of_baby_death = first_(c(triplicate_underlying_cause_of_baby_death, chi_underlying_cause_of_baby_death))) %>%
  mutate(x_cause_of_baby_death_0 = first_(c(triplicate_cause_of_baby_death_0, chi_cause_of_baby_death_0))) %>%
  mutate(x_cause_of_baby_death_1 = first_(c(triplicate_cause_of_baby_death_1, chi_cause_of_baby_death_1))) %>%
  mutate(x_cause_of_baby_death_2 = first_(c(triplicate_cause_of_baby_death_2, chi_cause_of_baby_death_2))) %>%
  mutate(x_cause_of_baby_death_3 = first_(c(triplicate_cause_of_baby_death_3, chi_cause_of_baby_death_3))) %>%
  mutate(x_cause_of_baby_death_4 = first_(c(triplicate_cause_of_baby_death_4, chi_cause_of_baby_death_4))) %>%
  mutate(x_cause_of_baby_death_5 = first_(c(triplicate_cause_of_baby_death_5, chi_cause_of_baby_death_5))) %>%
  mutate(x_cause_of_baby_death_6 = first_(c(triplicate_cause_of_baby_death_6, chi_cause_of_baby_death_6))) %>%
  mutate(x_cause_of_baby_death_7 = first_(c(triplicate_cause_of_baby_death_7, chi_cause_of_baby_death_7))) %>%
  mutate(x_cause_of_baby_death_8 = first_(c(triplicate_cause_of_baby_death_8, chi_cause_of_baby_death_8))) %>%
  mutate(x_cause_of_baby_death_9 = first_(c(triplicate_cause_of_baby_death_9, chi_cause_of_baby_death_9))) %>%
  ungroup() %>%
  select(
    -c(
      triplicate_date_of_baby_death,
      triplicate_underlying_cause_of_baby_death,
      triplicate_cause_of_baby_death_0,
      triplicate_cause_of_baby_death_1,
      triplicate_cause_of_baby_death_2,
      triplicate_cause_of_baby_death_3,
      triplicate_cause_of_baby_death_4,
      triplicate_cause_of_baby_death_5,
      triplicate_cause_of_baby_death_6,
      triplicate_cause_of_baby_death_7,
      triplicate_cause_of_baby_death_8,
      triplicate_cause_of_baby_death_9,
      chi_date_of_baby_death,
      chi_underlying_cause_of_baby_death,
      chi_cause_of_baby_death_0,
      chi_cause_of_baby_death_1,
      chi_cause_of_baby_death_2,
      chi_cause_of_baby_death_3,
      chi_cause_of_baby_death_4,
      chi_cause_of_baby_death_5,
      chi_cause_of_baby_death_6,
      chi_cause_of_baby_death_7,
      chi_cause_of_baby_death_8,
      chi_cause_of_baby_death_9
    )
  )

rm(data_infant_deaths_chi, data_infant_deaths_triplicate)

fetus_level_temp_21 <- fetus_level_temp_20 %>%
  mutate(x_days_to_infant_death = difftime(x_date_of_baby_death, x_baby_dob, units = "days")) %>%
  mutate(x_neonatal_death = case_when(x_days_to_infant_death >= 0 & x_days_to_infant_death <= 6  ~ "Early neonatal death (d0-6)",
                                      x_days_to_infant_death >= 7 & x_days_to_infant_death <= 27 ~ "Late neonatal death (d7-27)",
                                      outcome == "Live birth" ~ "Survived neonatal period"))

#### Alter outcome based on infant death status ####
 # If a death record exists for an infant then we need to ensure the outcome is "live birth", regardless of what other sources may say.

print("Fetus outcomes before infant death reassignemnt")
tabyl(fetus_level_temp_21$outcome)

fetus_level_temp_22 <- fetus_level_temp_21 %>%
  mutate(outcome = case_when(!is.na(x_days_to_infant_death) ~ "Live birth",
                             T ~ outcome))

print("Fetus outcomes after infant death reassignemnt")
tabyl(fetus_level_temp_22$outcome)

#### Fix terminations with incorrect dates that have been matched to births #### 
fetus_level_temp_29 <- fetus_level_temp_22 %>%
  mutate(outcome = case_when(outcome == "Termination" & nhslb_outcome_type == "Live birth" & is.na(x_date_of_baby_death)
                             ~ "Live birth",
                             outcome == "Termination" & is.na(nhslb_outcome_type) 
                             ~ "Termination",
                             T ~ outcome))

#### Determine Shielding Status ####
#fetus_level_temp_22  <- left_join(fetus_level_temp_22 ,  data_shielding, by=c("mother_upi" = "shielding_chi"))
fetus_level_temp_23 <- fetus_level_temp_29 %>%
  rowwise() %>%
  mutate(shielding_group_any = max_(c(shielding_group1, shielding_group2, shielding_group3, shielding_group4, shielding_group5, shielding_group6, shielding_group7))) %>%
  ungroup()

#### Determine QCovid Status ####
fetus_level_temp_24 <- fetus_level_temp_23 %>%
  rowwise() %>%
  mutate(q_bmi_40_plus = case_when(q_bmi >= 40 ~ 1)) %>%
  mutate(q_diag_renal_failure = case_when(q_diag_ckd3 == 1 ~ 1,
                                          q_diag_ckd4 == 1 ~ 1,
                                          q_diag_ckd5 == 1 ~ 1)) %>%
  mutate(q_covid_diag_any = max_(c( q_diag_af,
                                    q_diag_asthma,
                                    q_diag_blood_cancer,
                                    q_diag_ccf,
                                    q_diag_cerebralpalsy,
                                    q_diag_chd,
                                    q_diag_cirrhosis,
                                    q_diag_renal_failure,
                                    q_diag_congen_hd,
                                    q_diag_copd,
                                    q_diag_dementia,
                                    q_diag_diabetes_1,
                                    q_diag_diabetes_2,
                                    q_diag_epilepsy,
                                    q_diag_fracture,
                                    q_diag_neuro,
                                    q_diag_parkinsons,
                                    q_diag_pulm_hyper,
                                    q_diag_pulm_rare,
                                    q_diag_pvd,
                                    q_diag_ra_sle,
                                    q_diag_resp_cancer,
                                    q_diag_sev_ment_ill,
                                    q_diag_sickle_cell,
                                    q_diag_stroke,
                                    q_diag_vte))) %>%
  mutate(q_covid_diag_any_excluding_diabetes = max_(c( q_diag_af,
                                                       q_diag_asthma,
                                                       q_diag_blood_cancer,
                                                       q_diag_ccf,
                                                       q_diag_cerebralpalsy,
                                                       q_diag_chd,
                                                       q_diag_cirrhosis,
                                                       q_diag_renal_failure,
                                                       q_diag_congen_hd,
                                                       q_diag_copd,
                                                       q_diag_dementia,
                                                       q_diag_epilepsy,
                                                       q_diag_fracture,
                                                       q_diag_neuro,
                                                       q_diag_parkinsons,
                                                       q_diag_pulm_hyper,
                                                       q_diag_pulm_rare,
                                                       q_diag_pvd,
                                                       q_diag_ra_sle,
                                                       q_diag_resp_cancer,
                                                       q_diag_sev_ment_ill,
                                                       q_diag_sickle_cell,
                                                       q_diag_stroke,
                                                       q_diag_vte))) %>%
  ungroup()


#### Determine Clinical Vulnerability ####
fetus_level_temp_25 <- fetus_level_temp_24 %>%
  mutate(cv_clinical_vulnerability_category = case_when(shielding_group_any == 1 ~ "clinically_extremely_vulnerable",
                                                        q_covid_diag_any_excluding_diabetes == 1 ~ "clinically_vulnerable",
                                                        T ~ "not_clinically_vulnerable"))

#### Determine Diabetes Status ####
fetus_level_temp_25a <- fetus_level_temp_25 %>%
  mutate(x_diabetes = case_when(smr02_diabetes == 2 ~ "gestational_diabetes",
                                smr02_diabetes == 1 | (q_diag_diabetes_1 == 1 | q_diag_diabetes_2 == 1) ~ "pre-existing_diabetes",
                                smr02_diabetes == 3 & q_diag_diabetes_1 == 0 & q_diag_diabetes_2 == 0 ~ "diabetes_onset_unknown",
                                smr02_diabetes %in% c(4, 9) ~ "confirmed_no_diabetes",
                                q_diag_diabetes_1 == 0 & q_diag_diabetes_2 == 0 ~ "assumed_no_diabetes",
                                T ~ "unknown"))

#### Identify and correct overlapping pregnancies ####
# Some women have several pregnancies which appear to overlap. This is mostly due to one pregnancy having a missing end date,
# where there pregnancy has ended but we don't know when it ended. Another pregnancy has occurred, but because we've set the 
# conception date for the previous pregnancy to conception_date + 44 weeks, it appears the pregnancies are concurrent.
fetus_level_temp_26 <- fetus_level_temp_25a %>%
  select(mother_upi, x_est_conception_date, x_pregnancy_end_date, x_gestation_at_outcome, everything()) %>%
  arrange(mother_upi, x_est_conception_date, x_pregnancy_end_date) %>%
  group_by(mother_upi) %>%
  mutate(next_pregnancy_is_embedded = case_when( outcome == "Unknown" & pregnancy_id != lead(pregnancy_id) & lead(x_est_conception_date) %within% interval(start = x_est_conception_date, end = x_pregnancy_end_date) ~ T,
                                                 outcome == "Ongoing" & lead(x_est_conception_date) > x_est_conception_date ~ T, # This line checks to see if a subsequent pregnancy exists
                                                 T ~ F)) %>%
  mutate(x_pregnancy_end_date = case_when(next_pregnancy_is_embedded == T ~ lead(x_est_conception_date) - weeks(4),
                                          T ~ x_pregnancy_end_date)) %>%
  mutate(x_gestation_at_outcome = case_when(next_pregnancy_is_embedded == T ~ floor(as.numeric(difftime(x_pregnancy_end_date, x_est_conception_date, units = "weeks"))) + 2,
                                            T ~ x_gestation_at_outcome)) %>%
  mutate(outcome = case_when(next_pregnancy_is_embedded == T ~ "Unknown",
                             T ~ outcome)) %>%
  select(-next_pregnancy_is_embedded) %>%
  mutate(x_gestation_ascertainment = case_when(outcome == "Unknown" ~ "Gestation imputed based on outcome of pregnancy",
                                               T ~ x_gestation_ascertainment)) %>% 
  ungroup()


#### Determine COVID wave ####
# First wave, second wave etc
fetus_level_temp_26a <- fetus_level_temp_26 %>% 
  mutate(x_first_wave = case_when( 
    x_est_conception_date <= as.Date("2020-06-30") & x_pregnancy_end_date >= as.Date("2020-03-01")  |  
      x_est_conception_date <= as.Date("2020-06-30") & outcome == "Ongoing"
    ~ T,
    T ~ F)) %>%
  mutate(x_full_cohort = case_when(
    x_first_wave == 1 | 
      x_est_conception_date >= as.Date("2020-03-31")
    ~ T,
    T ~ F))

sum_(fetus_level_temp_26a$x_first_wave)
sum_(fetus_level_temp_26a$x_full_cohort)
fetus_level_temp_26a %>% filter(outcome == "Live birth") %>% filter(x_first_wave == TRUE) %>% count()
fetus_level_temp_26a %>% filter(outcome == "Live birth") %>% filter(x_full_cohort == TRUE) %>% count()



#### Drop old variables and re-order file ###
fetus_level_temp_27 <- fetus_level_temp_26a %>% 
  select(-c(pregnancy_start_date, pregnancy_end_date)) %>% 
  dplyr::relocate(mother_upi, baby_upi, pregnancy_id, outcome, (starts_with('x', ignore.case = TRUE))) %>% 
  dplyr::relocate(smr02_bmi, .before = smr02_weight_of_mother)


### Add in GP data flags ###
fetus_level <- fetus_level_temp_27 %>%
  mutate(gp_only = case_when((is.na(smr01_1) &
                                is.na(aas_termination_1) & is.na(aas_termination_2) &
                                is.na(smr02_1) & is.na(smr02_2) & is.na(smr02_live_births) &
                                is.na(nrs_stillbirths_1) &
                                is.na(nhs_live_births) &
                                is.na(nrs_live_births) &
                                !is.na(gp_losses_1)) ~ 1,
                             T ~ 0)) %>% 
  mutate(gp_booked = case_when(gp_only == 1 & !is.na(antenatal_booking_1) ~ "Yes",
                               T ~ "No")) %>% 
  mutate(gp_data_status = "GP data included")

#### Flag cases with invalid mother UPI number ####
fetus_level <- fetus_level %>%
  mutate(chi_validity = chi_check(mother_upi))

#Sort SIMD NAs ####
fetus_level <- fetus_level %>% 
  mutate(x_simd = case_when(x_simd == "1" ~ "1=most deprived", 
                            x_simd == "5" ~ "5=least deprived", 
                            x_simd == "9" ~ NA_character_, 
                            TRUE ~ x_simd))

#### Write fetus-level files ####
fetus_level %>% write_rds(paste0(folder_temp_data, "script6_baby_level_record.rds"), compress = "gz")

rm(fetus_level_temp_16, fetus_level_temp_17, fetus_level_temp_18, fetus_level_temp_19, fetus_level_temp_20, fetus_level_temp_21,
   fetus_level_temp_22, fetus_level_temp_23, fetus_level_temp_24, fetus_level_temp_25, fetus_level,
   fetus_level_temp_26, fetus_level_temp_26a, fetus_level_temp_27, fetus_level_temp_28)
