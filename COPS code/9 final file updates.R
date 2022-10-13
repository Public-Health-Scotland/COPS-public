######
#This file is to make minor changes to the final file for archiving. 



############################changes to baby file.
#read in baby file. 
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) 

#list the column we want to change. 
cols <- c("aas_statutory_ground_a"
          ,"aas_statutory_ground_b"
          ,"aas_statutory_ground_c"
          , "aas_statutory_ground_d"
          ,"aas_statutory_ground_e"
          ,"aas_statutory_ground_f"
          ,"aas_statutory_ground_g"
          ,"smr02_drug_misuse"
          ,"q_diag_af"
          ,"q_diag_asthma"
          ,"q_diag_blood_cancer"
          ,"q_diag_ccf"
          ,"q_diag_cerebralpalsy"
          ,"q_diag_chd"
          ,"q_diag_cirrhosis"
          ,"q_diag_ckd3"
          ,"q_diag_ckd4"
          ,"q_diag_ckd5"
          ,"q_diag_congen_hd"
          ,"q_diag_copd"
          ,"q_diag_dementia"
          ,"q_diag_diabetes_1"
          ,"q_diag_diabetes_2"
          ,"q_diag_epilepsy"
          ,"q_diag_fracture"
          ,"q_diag_neuro"
          ,"q_diag_parkinsons"
          ,"q_diag_pulm_hyper"
          ,"q_diag_pulm_rare"
          ,"q_diag_pvd"
          ,"q_diag_ra_sle"
          ,"q_diag_resp_cancer"
          ,"q_diag_sev_ment_ill"
          ,"q_diag_sickle_cell"
          ,"q_diag_stroke"
          ,"q_diag_vte"
          ,"q_preexisting_diabetes"
          ,"eave_non_smoker"
          ,"eave_smoker"
          ,"q_covid"
          ,"shielding_group_any"
          ,"shielding_group1"
          ,"shielding_group2"
          ,"shielding_group3"
          ,"shielding_group4"
          ,"shielding_group5"
          ,"shielding_group6"
          ,"shielding_group7"
          ,"q_bmi_40_plus"
          ,"q_diag_renal_failure"
          ,"tests_mother_positive_test_during_pregnancy")

#apply chANGE USING MUTATE IF FUNCTIONS. 
fetuslevel_2 <- fetuslevel %>% 
  mutate_if(names(.) %in% cols, 
            funs(as.logical(.)))



#recoding variables. 
fetuslevel_2["x_diabetes"][fetuslevel_2["x_diabetes"] == "unknown"] <- NA
fetuslevel_2["nhslb_fv_feed_2016"][fetuslevel_2["nhslb_fv_feed_2016"] == "Unknown/Invalid"] <- NA#

fetuslevel_2["nhslb_wk6_ethnicity"][fetuslevel_2["nhslb_wk6_ethnicity"] == 99] <- NA
fetuslevel_2["smr02_total_previous_pregnancies"][fetuslevel_2["smr02_total_previous_pregnancies"] == 99] <- NA
fetuslevel_2["smr02_previous_spontaneous_abortions"][fetuslevel_2["smr02_previous_spontaneous_abortions"] == 99] <- NA
fetuslevel_2["smr02_previous_theraputic_abortions"][fetuslevel_2["smr02_previous_theraputic_abortions"] == 99] <- NA
fetuslevel_2["smr02_total_previous_stillbirths"][fetuslevel_2["smr02_total_previous_stillbirths"] == 99] <- NA
fetuslevel_2["smr02_drug_misuse"][fetuslevel_2["smr02_drug_misuse"] == 9] <- NA
fetuslevel_2["smr02_weekly_alcohol_consumption"][fetuslevel_2["smr02_weekly_alcohol_consumption"] == 99] <- NA
fetuslevel_2["smr02_antenatal_steroids"][fetuslevel_2["smr02_antenatal_steroids"] == 9] <- NA
fetuslevel_2["smr02_induction_of_labour"][fetuslevel_2["smr02_induction_of_labour"] == 9] <- NA
fetuslevel_2["smr02_presentation_at_delivery"][fetuslevel_2["smr02_presentation_at_delivery"] == 9] <- NA
fetuslevel_2["smr02_apgar_5_minutes"][fetuslevel_2["smr02_apgar_5_minutes"] == "NR"] <- NA
fetuslevel_2["smr02_apgar_5_minutes"][fetuslevel_2["smr02_apgar_5_minutes"] == "RR"] <- NA


#### Write pregnancy-level file ####
fetuslevel_2 %>% write_rds(paste0(folder_temp_data, "fetus_level_record_final_file.rds"), compress = "gz")


####################################################################changes to pregnancy files. 


#read in PREGNANCY file. 
preg_file <- read_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds")) 

cols2 <- c("shielding_group1"
,"shielding_group2"
,"shielding_group3"
,"shielding_group4"
,"shielding_group5"
,"shielding_group6"
,"shielding_group7"
,"shielding_group_any"
,"q_covid"
,"q_bmi_40_plus"
,"q_diag_af"
,"q_diag_asthma"
,"q_diag_blood_cancer"
,"q_diag_ccf"
,"q_diag_cerebralpalsy"
,"q_diag_chd"
,"q_diag_cirrhosis"
,"q_diag_ckd3"
,"q_diag_ckd4"
,"q_diag_ckd5"
,"q_diag_congen_hd"
,"q_diag_copd"
,"q_diag_dementia"
,"q_diag_diabetes_1"
,"q_diag_diabetes_2"
,"q_diag_epilepsy"
,"q_diag_fracture"
,"q_diag_neuro"
,"q_diag_parkinsons"
,"q_diag_pulm_hyper"
,"q_diag_pulm_rare"
,"q_diag_pvd"
,"q_diag_ra_sle"
,"q_diag_resp_cancer"
,"q_diag_sev_ment_ill"
,"q_diag_sickle_cell"
,"q_diag_stroke"
,"q_diag_vte"
,"q_diag_renal_failure"
,"q_preexisting_diabetes"
,"mother_has_had_pcr_test_at_any_point"
,"mother_tested_positive_during_pregnancy"
,"first_wave"
,"full_cohort")

preg_file_2 <- preg_file %>% 
  mutate_if(names(.) %in% cols2, 
            funs(as.logical(.)))


#recoding variables. 
preg_file_2["diabetes"][preg_file_2["diabetes"] == "unknown"] <- NA

#### Write pregnancy-level file ####
preg_file_2 %>% write_rds(paste0(folder_temp_data, "pregnancy_level_record_final_file.rds"), compress = "gz")
