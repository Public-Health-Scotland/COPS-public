
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record.rds")) 
fetuslevel_no_gp <- read_rds(paste0(folder_temp_data, "script6_baby_level_no_gp.rds")) 


pregnancies <- fetuslevel %>%
  rowwise() %>% 
  mutate(x_pregnancy_end_date = replace_na(x_pregnancy_end_date, as.Date("1970-01-01"))) %>% # summarise() won't alter NA date values, so use 1970-01-01 as a stand-in and change it back later
  ungroup() %>% 
  arrange(pregnancy_id) %>%
  group_by(pregnancy_id) %>%
  mutate(overall_outcome = case_when("Live birth" %in% outcome ~ "Live birth",
                                     "Termination" %in% outcome ~ "Termination",
                                     "Stillbirth" %in% outcome ~ "Stillbirth",
                                     "Ectopic pregnancy" %in% outcome ~ "Ectopic pregnancy",
                                     "Molar pregnancy" %in% outcome ~ "Molar pregnancy",
                                     "Miscarriage" %in% outcome ~ "Miscarriage",
                                     "Ongoing" %in% outcome ~ "Ongoing",
                                     "Unknown" %in% outcome ~ "Unknown"))  %>% 
  summarise(mother_upi = first_(mother_upi),
            gestation_at_outcome = max_(x_gestation_at_outcome),
            pregnancy_end_date = as.Date(max_(x_pregnancy_end_date)),
            est_conception_date = as.Date(min_(x_est_conception_date)),
            overall_outcome = first_(overall_outcome),
            first_wave = max_(x_first_wave),
            full_cohort = max_(x_full_cohort),
            mother_dob = first_(x_mother_dob),
            mother_age_at_conception = first_(x_mother_age_at_conception),
            mother_age_at_outcome = first_(x_mother_age_at_outcome),
            hbres = first_(x_hbres),
            postcode = first(x_postcode),
            simd = first_(x_simd),
            bmi = first_(x_bmi),
            booking_smoking_status = first_(x_booking_smoking_status),
            gp_smoking_status = first_(x_gp_smoking_status),
            overall_smoking_status = first_(x_overall_smoking_status),
            ethnicity9 = first_(x_ethnicity9),
            ethnicity_description = first_(x_ethnicity_description),
            urban_rural_description = first_(x_urban_rural_8_description),
            births_this_pregnancy = max_(x_births_this_pregnancy),
            shielding = max_(shielding_shield),
            shielding_group1 = max_(shielding_group1),
            shielding_group2 = max_(shielding_group2),
            shielding_group3 = max_(shielding_group3),
            shielding_group4 = max_(shielding_group4),
            shielding_group5 = max_(shielding_group5),
            shielding_group6 = max_(shielding_group6),
            shielding_group7 = max_(shielding_group7),
            shielding_group_any = max_(shielding_group_any),
            q_covid = max_(q_covid),
            q_bmi = first_(q_bmi),
            q_bmi_40_plus = max_(q_bmi_40_plus),
            q_diabetes_type = max_(q_diabetes_type),
            q_diag_af = max_(q_diag_af),
            q_diag_asthma = max_(q_diag_asthma),
            q_diag_blood_cancer = max_(q_diag_blood_cancer),
            q_diag_ccf = max_(q_diag_ccf),
            q_diag_cerebralpalsy = max_(q_diag_cerebralpalsy),
            q_diag_chd = max_(q_diag_chd),
            q_diag_cirrhosis = max_(q_diag_cirrhosis),
            q_diag_ckd_level = max_(q_diag_ckd_level),
            q_diag_congen_hd = max_(q_diag_congen_hd),
            q_diag_copd = max_(q_diag_copd),
            q_diag_dementia = max_(q_diag_dementia),
            q_diag_diabetes_1 = max_(q_diag_diabetes_1),
            q_diag_diabetes_2 = max_(q_diag_diabetes_2),
            q_diag_epilepsy = max_(q_diag_epilepsy),
            q_diag_fracture = max_(q_diag_fracture),
            q_diag_neuro = max_(q_diag_neuro),
            q_diag_parkinsons = max_(q_diag_parkinsons),
            q_diag_pulm_hyper = max(q_diag_pulm_hyper),
            q_diag_pulm_rare = max_(q_diag_pulm_rare),
            q_diag_pvd = max_(q_diag_pvd),
            q_diag_ra_sle = max_(q_diag_ra_sle),
            q_diag_resp_cancer = max_(q_diag_resp_cancer),
            q_diag_sev_ment_ill = max_(q_diag_sev_ment_ill),
            q_diag_sickle_cell = max_(q_diag_sickle_cell),
            q_diag_stroke = max_(q_diag_stroke),
            q_diag_vte = max_(q_diag_vte),
            q_diag_renal_failure = max_(q_diag_renal_failure),
            q_ethnicity = first_(q_ethnicity),
            q_ethnicity_mapped9 = first_(q_ethnicity_mapped9),
            q_home_cat = first_(q_home_cat),  # Should we use first_() or max_() here?
            q_learn_cat = first_(q_learn_cat),# Should we use first_() or max_() here?
            q_preexisting_diabetes = max_(q_preexisting_diabetes),
            cv_clinical_vulnerability_category = first_(cv_clinical_vulnerability_category),
            gp_data_status = "GP data included") %>%
  replace_with_na_at(.vars = c("pregnancy_end_date"),
                     condition = ~.x == as.Date("1970-01-01"))



pregnancies_no_gp <- fetuslevel_no_gp %>%
  rowwise() %>% 
  mutate(x_pregnancy_end_date = replace_na(x_pregnancy_end_date, "1970-01-01")) %>%
  ungroup() %>% 
  arrange(pregnancy_id) %>%
  group_by(pregnancy_id) %>%
  mutate(overall_outcome = case_when("Live birth" %in% outcome ~ "Live birth",
                                     "Termination" %in% outcome ~ "Termination",
                                     "Stillbirth" %in% outcome ~ "Stillbirth",
                                     "Ectopic pregnancy" %in% outcome ~ "Ectopic pregnancy",
                                     "Molar pregnancy" %in% outcome ~ "Molar pregnancy",
                                     "Miscarriage" %in% outcome ~ "Miscarriage",
                                     "Ongoing" %in% outcome ~ "Ongoing",
                                     "Unknown" %in% outcome ~ "Unknown"))  %>% 
  summarise(mother_upi = first_(mother_upi),
            gestation_at_outcome = max_(x_gestation_at_outcome),
            pregnancy_end_date = as.Date(max_(x_pregnancy_end_date)),
            est_conception_date = as.Date(min_(x_est_conception_date)),
            overall_outcome = first_(overall_outcome),
            first_wave = max_(x_first_wave),
            full_cohort = max_(x_full_cohort),
            mother_dob = first_(x_mother_dob),
            mother_age_at_conception = first_(x_mother_age_at_conception),
            mother_age_at_outcome = first_(x_mother_age_at_outcome),
            hbres = first_(x_hbres),
            postcode = first(x_postcode),
            simd = first_(x_simd),
            bmi = first_(x_bmi),
            booking_smoking_status = first_(x_booking_smoking_status),
            gp_smoking_status = first_(x_gp_smoking_status),
            overall_smoking_status = first_(x_overall_smoking_status),
            overall_smoking_status = first_(x_overall_smoking_status),
            ethnicity9 = first_(x_ethnicity9),
            ethnicity_description = first_(x_ethnicity_description),
            urban_rural_description = first_(x_urban_rural_8_description),
            births_this_pregnancy = max_(x_births_this_pregnancy),
            shielding = max_(shielding_shield),
            shielding_group1 = max_(shielding_group1),
            shielding_group2 = max_(shielding_group2),
            shielding_group3 = max_(shielding_group3),
            shielding_group4 = max_(shielding_group4),
            shielding_group5 = max_(shielding_group5),
            shielding_group6 = max_(shielding_group6),
            shielding_group7 = max_(shielding_group7),
            shielding_group_any = max_(shielding_group_any),
            q_covid = max_(q_covid),
            q_bmi = first_(q_bmi),
            q_bmi_40_plus = max_(q_bmi_40_plus),
            q_diabetes_type = max_(q_diabetes_type),
            q_diag_af = max_(q_diag_af),
            q_diag_asthma = max_(q_diag_asthma),
            q_diag_blood_cancer = max_(q_diag_blood_cancer),
            q_diag_ccf = max_(q_diag_ccf),
            q_diag_cerebralpalsy = max_(q_diag_cerebralpalsy),
            q_diag_chd = max_(q_diag_chd),
            q_diag_cirrhosis = max_(q_diag_cirrhosis),
            q_diag_ckd_level = max_(q_diag_ckd_level),
            q_diag_congen_hd = max_(q_diag_congen_hd),
            q_diag_copd = max_(q_diag_copd),
            q_diag_dementia = max_(q_diag_dementia),
            q_diag_diabetes_1 = max_(q_diag_diabetes_1),
            q_diag_diabetes_2 = max_(q_diag_diabetes_2),
            q_diag_epilepsy = max_(q_diag_epilepsy),
            q_diag_fracture = max_(q_diag_fracture),
            q_diag_neuro = max_(q_diag_neuro),
            q_diag_parkinsons = max_(q_diag_parkinsons),
            q_diag_pulm_hyper = max(q_diag_pulm_hyper),
            q_diag_pulm_rare = max_(q_diag_pulm_rare),
            q_diag_pvd = max_(q_diag_pvd),
            q_diag_ra_sle = max_(q_diag_ra_sle),
            q_diag_resp_cancer = max_(q_diag_resp_cancer),
            q_diag_sev_ment_ill = max_(q_diag_sev_ment_ill),
            q_diag_sickle_cell = max_(q_diag_sickle_cell),
            q_diag_stroke = max_(q_diag_stroke),
            q_diag_vte = max_(q_diag_vte),
            q_diag_renal_failure = max_(q_diag_renal_failure),
            q_covid_diag_any = max_(q_covid_diag_any),
            q_ethnicity = first_(q_ethnicity),
            q_ethnicity_mapped9 = first_(q_ethnicity_mapped9),
            q_home_cat = first_(q_home_cat),  # Should we use first_() or max_() here?
            q_learn_cat = first_(q_learn_cat),# Should we use first_() or max_() here?
            q_preexisting_diabetes = max_(q_preexisting_diabetes),
            cv_clinical_vulnerability_category = first_(cv_clinical_vulnerability_category),
            gp_data_status = "GP data excluded") %>%
  replace_with_na_at(.vars = c("pregnancy_end_date"),
                     condition = ~.x == as.Date("1970-01-01"))

#### Write pregnancy-level files ####
pregnancies %>% write_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds"))

pregnancies_no_gp %>% write_rds(paste0(folder_temp_data, "script6b_pregnancy_level_no_gp.rds"))



            