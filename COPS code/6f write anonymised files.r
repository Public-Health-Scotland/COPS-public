fetus_level <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection_anomaly_id.rds")) %>% 
  filter(chi_validity == "Valid CHI")
pregnancies <- read_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds")) %>% 
  filter(chi_validity == "Valid CHI")

## read in EAVE ID master file
master_id_file <- readRDS("/network_folder/MASTER_id_file.rds") %>% 
  select(EAVE_LINKNO, CHINumber)

#### fetus level: add in baby eave ID ####
fetus_level_anonymised <- fetus_level %>%
  left_join(master_id_file, by = c("baby_upi" = "CHINumber")) %>% 
  rename(baby_eave_linkno = EAVE_LINKNO) %>% 
  mutate(baby_chi_validity = chi_check(baby_upi)) %>% 
  mutate(baby_eave_linkno = if_else(baby_chi_validity == "Valid CHI", baby_eave_linkno, NA_character_)) %>% 
  select(-baby_chi_validity)

#### fetus level: Remove UPI numbers ####
fetus_level_anonymised <- fetus_level_anonymised %>%
  select(-mother_upi, 
         -baby_upi,
         -anbooking_upi,
         -nhslb_mother_chi,
         -nhslb_baby_chi,
         -nrslb_mother_upi_number,
         -nrslb_child_upi_number,
         -aas_upi_number,
         -nrssb_mother_upi_number,
         -smr01_mother_upi_number,
         -smr01_baby_upi_number,
         -smr01_link_no,
         -gp_losses_chi_number,
         -smr02_baby_upi_number,
         -smr02_upi_number)

#### fetus level: Remove NRS identifiers ####
fetus_level_anonymised <- fetus_level_anonymised %>%
  select(-x_nrs_lb_triplicate_id,
         -nrslb_year_of_registration,
         -nrslb_registration_district,
         -nrslb_entry_number)

#### fetus level: Remove postcodes ####
fetus_level_anonymised <- fetus_level_anonymised %>%
  select(-x_postcode,
         -nrslb_postcode,
         -aas_pc7,
         -nrssb_postcode,
         -smr01_dr_postcode,
         -smr02_dr_postcode)

#### fetus level: Remove linkage UUIDs ####
fetus_level_anonymised <- fetus_level_anonymised %>%
  select(-cops_births_1,
         -cops_births_2,
         -aas_termination_1,
         -aas_termination_2,
         -gp_losses_1,
         -smr01_1,
         -smr02_1,
         -nrs_stillbirths_1,
         -smr01_2,
         -aas_termination_2,
         -smr02_2,
         -gp_losses_2,
         -smr01_3,
         -nrs_stillbirths_2,
         -antenatal_booking_1,
         -antenatal_booking_2,
         -nhs_live_births,
         -nrs_live_births,
         -smr02_live_births,
         -gp_data_status
  )


fetus_level_anonymised %>%
  select(mother_eave_linkno, baby_eave_linkno, everything()) %>%
  write_rds(paste0(folder_outputs_anonymised, "cops_baby_level_record_anonymised.rds"), compress = "gz")


#### pregnancy level: Remove UPI numbers, postcodes, and GP data status ####
pregnancies_anonymised <- pregnancies %>%
  select(-mother_upi,
         -postcode,
         -gp_data_status)

pregnancies_anonymised %>%
  select(mother_eave_linkno, everything()) %>%
  write_rds(paste0(folder_outputs_anonymised, "cops_pregnancy_level_record_anonymised.rds"), compress = "gz")


#### Anonymised maternal outcomes files ####

## read in maternal outcomes files and EACE ID master file ####

## SMR01 ##
df_smr01 <- read_rds(paste0(folder_temp_data, "smr01_flagged_stays.rds"))


## SMR02 ##
df_smr02 <- read_rds(paste0(folder_temp_data, "smr02_flagged_episodes.rds"))


## DEATHS ##
df_deaths <- read_rds(paste0(folder_temp_data, "deaths_flagged.rds"))


## SICSAG ##
df_icu <- read_rds(paste0(folder_temp_data, "sicsag_episodes.rds"))

## test details ##
df_testdetails <- readRDS(paste0(folder_temp_data, "tests_details.rds") )

## create anonymised SMR01 ####

smr01_non_confi <- df_smr01 %>% 
  left_join(master_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-mother_upi) %>% 
  select(EAVE_LINKNO, everything())

## create anonymised SMR02 ####

smr02_non_confi <- df_smr02 %>% 
  left_join(master_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-mother_upi) %>% 
  select(EAVE_LINKNO, everything())

## create anonymised SICSAG ####

icu_non_confi <- df_icu %>% 
  left_join(master_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-mother_upi) %>% 
  select(EAVE_LINKNO, everything())

## create anonymised deaths ####

deaths_non_confi <- df_deaths %>% 
  left_join(master_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-mother_upi) %>% 
  select(EAVE_LINKNO, everything())

## create anonymised test details ####
testdetails_non_confi <- df_testdetails %>% 
  left_join(master_id_file, by = c("upi" = "CHINumber")) %>% 
  select(-upi) %>% 
  select(EAVE_LINKNO, everything())


## save non-confi files to non-confi area ####

smr01_non_confi %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/SMR01_hospital_stays_for_cohort.rds"), compress = "gz")

smr02_non_confi %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/SMR02_hospital_episodes_for_cohort.rds"), compress = "gz")

icu_non_confi %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/ICU_admissions_for_cohort.rds"), compress = "gz")

deaths_non_confi %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/deaths_for_cohort.rds"), compress = "gz")

testdetails_non_confi %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/tests_for_cohort.rds"), compress = "gz")


#### Anonymised anomalies file ####

## read in anomalies file  ##
df_anomalies <- readRDS(paste0(folder_temp_data, "script6_anomalies_matched_ids.rds")) %>% 
  clean_names() %>% 
  select(anomaly_id, date_end_of_pregnancy, starts_with("all_"), contains("icd10"), only_ground_e, genetic_anomaly)


## save out anonymised anomalies file to non-confi area ## 
df_anomalies %>% write_rds(paste0(folder_outputs_anonymised, "network_folder/anomalies.rds"), compress = "gz")


rm(smr01_non_confi, smr02_non_confi, icu_non_confi, deaths_non_confi, df_deaths,  df_icu, df_smr01, df_smr02, 
   fetus_level_anonymised,    pregnancies_anonymised)

