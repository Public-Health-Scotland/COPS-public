#### Read in COPS cohort ####
fetus_level <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>% 
  mutate(cohort = 1) %>% 
  mutate(baby_upi = case_when(
    is.na(baby_upi) ~ paste0("42", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ baby_upi
  )) %>% 
  group_by(baby_upi) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(pregnancy_id) %>% 
  mutate(cohort_records_in_pregnancy = sum(cohort)) %>% 
  ungroup() %>% 
  mutate(cohort_id = row_number())

fetus_level_limited <- fetus_level %>% 
  select(mother_upi, baby_upi, pregnancy_id, x_pregnancy_end_date, x_baby_sex, x_baby_weight, cohort_records_in_pregnancy,
         nrslb_year_of_registration, nrslb_registration_district, nrslb_entry_number, cohort, cohort_id) %>% 
  mutate(nrslb_year_of_registration = as.numeric(nrslb_year_of_registration),
         nrslb_registration_district = as.numeric(nrslb_registration_district),
         nrslb_entry_number = as.numeric(nrslb_entry_number))

#### Read in anomalies file ####
## and create row number id for matching later and number of records flag for multiples ##
df_anomalies <-
  read_csv(
    paste0(folder_temp_data, "Congenital Anomaly Linked Dataset.csv"),
    col_types = cols(
      AAS_ICD10.3 = col_character(),
      SMR01_ICD10.7 = col_character(),
      SMR01_ICD10.8 = col_character(),
      SMR02_ICD10.5 = col_character(),
      SBR_ICD10.11 = col_character(),
      SBR_ICD10.12 = col_character(),
      SBR_ICD10.13 = col_character(),
      SBR_ICD10.14 = col_character(),
      SBR_ICD10.15 = col_character(),
      SBR_ICD10.16 = col_character(),
      SBR_ICD10.17 = col_character(),
      AAS_ICD10.3 = col_character(),
      NRS_STILLBIRTH_ICD10.4 = col_character(),
      NRS_DEATH_ICD10.5 = col_character(),
      NRS_DEATH_ICD10.6 = col_character(),
      NRS_DEATH_ICD10.7 = col_character(),
      NRS_DEATH_ICD10.8 = col_character()
    )
  ) %>% 
  clean_names() %>% 
  mutate(date_end_of_pregnancy = format(as.Date(date_end_of_pregnancy, format = "%m/%d/%Y"), "%Y-%m-%d")) %>% 
  mutate(genetic_anomaly = if_else(all_13_chromosomal == 1 | all_12_1_skeletal_dysplasias == 1 | all_12_11_genetic_syndromes_and_microdeletions == 1, 1, 0)) %>% 
  mutate(anomalies = 1) %>% 
  group_by(cardriss_baby_upi) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(cardriss_mother_upi, date_end_of_pregnancy) %>% 
  mutate(anom_records_in_pregnancy = sum(anomalies)) %>% 
  ungroup() %>% 
  mutate(anomaly_id = row_number())

write_rds(df_anomalies, paste0(folder_temp_data, "anomalies_df.rds"), compress = "gz")


df_anomalies_limited <- df_anomalies %>% 
  select(cardriss_baby_upi, cardriss_mother_upi, date_end_of_pregnancy, sex, multiple_pregnancy, anom_records_in_pregnancy, anomalies, anomaly_id, birth_year_of_registration, birth_registration_district, birth_entry_number) %>% 
  mutate(cardriss_baby_upi = chi_pad(cardriss_baby_upi)) %>% 
  mutate(cardriss_mother_upi = chi_pad(cardriss_mother_upi))

#### Link using baby upi ####
fetus_level_baby_match <- fetus_level_limited %>% 
  full_join(df_anomalies_limited, by = c("baby_upi" = "cardriss_baby_upi"))


matched_1 <- fetus_level_baby_match %>% 
  filter(cohort == 1 & anomalies == 1)

# select only unmatched records for next stage
unmatched_anomalies_1 <- fetus_level_baby_match %>% 
  filter(is.na(cohort) & anomalies == 1) %>% 
  select(c(baby_upi, cardriss_mother_upi:last_col())) %>% 
  rename(cardriss_baby_upi = baby_upi)

unmatched_cohort_1 <- fetus_level_baby_match %>% 
  filter(is.na(anomalies) & cohort == 1) %>% 
  select(c(mother_upi:cohort_id))

#check for duplicates or missed records
nrow(unmatched_anomalies_1) + nrow(matched_1) == nrow(df_anomalies)
nrow(unmatched_cohort_1) + nrow(matched_1) == nrow(fetus_level)

#### link using NRS triplicate ids####
triplicate_id_match <- unmatched_cohort_1 %>% 
  filter(!is.na(nrslb_year_of_registration)) %>% 
  full_join(unmatched_anomalies_1, by = c("nrslb_year_of_registration" = "birth_year_of_registration", "nrslb_registration_district" = "birth_registration_district",
                                          "nrslb_entry_number" = "birth_entry_number"))

matched_2 <- triplicate_id_match %>% 
  filter(cohort == 1 & anomalies == 1)
# no further matches


#### link using mother upi and date of pregnancy end, singletons only ####

# match on all records by mother upi and create birth window
fetus_level_mother_match <- unmatched_cohort_1 %>% 
  mutate(birth_window = interval(start = x_pregnancy_end_date - days(35), 
                                 end =  x_pregnancy_end_date + days(35))) %>% 
  full_join(unmatched_anomalies_1, by = c("mother_upi" = "cardriss_mother_upi"))

# match based on mother upi and dob for singletons only
matched_3 <- fetus_level_mother_match %>% 
  filter(cohort == 1 & anomalies == 1 & (as.Date(date_end_of_pregnancy) %within% birth_window) & cohort_records_in_pregnancy <= 1) 

#### link using mother upi and date of pregnancy end, multiples ####
# select records of multiple pregnancies which have matched to a mother, and remove those who have mismatching sex

multiples_df <- fetus_level_mother_match %>% 
  filter(cohort == 1 & anomalies == 1 & (as.Date(date_end_of_pregnancy) %within% birth_window) & cohort_records_in_pregnancy > 1) %>% 
  mutate(baby_sex_num = case_when(x_baby_sex == "M" ~ 1, 
                                  x_baby_sex == "F" ~ 2,
                                  is.na(x_baby_sex) ~ 3)) %>%
  mutate(sex = replace_na(sex, 3)) %>% 
  filter(sex == baby_sex_num | (sex == 3 & baby_sex_num != 3) | (sex != 3 & baby_sex_num == 3))

# add together all previous matches so we can see the overall number of matches per pregnancy
all_matches <- matched_1 %>% 
  bind_rows(matched_2) %>% 
  bind_rows(matched_3) %>% 
  bind_rows(multiples_df) %>% 
  group_by(pregnancy_id, anomaly_id) %>% 
  slice(1) %>% 
  ungroup()

multiples_check <- all_matches %>% 
  group_by(pregnancy_id) %>% 
  mutate(matches_per_pregnancy = sum(cohort))

#### check numbers matched compared to expected number of anomalies ####
tabyl(multiples_check, matches_per_pregnancy, anom_records_in_pregnancy)

correct_number_matches <- multiples_check %>% 
  filter(matches_per_pregnancy == anom_records_in_pregnancy)

too_many_matches <- multiples_check %>% 
  filter(matches_per_pregnancy > anom_records_in_pregnancy) 
# manually checked: most appear to be correctly matched to pregnancies with multiple different end dates for different fetuses
# there are two cases of different anomaly records matching on to the same singleton cohort pregnancy: they look as if they are the same pregnancy. 
# pick the first anomaly record and keep

too_few_matches <- multiples_check %>% 
  filter(matches_per_pregnancy < anom_records_in_pregnancy) 
# only one, seems to be because of differently coded baby sex in cops vs cardriss. Will link on as extra record below.

## make list of matched IDs, picking only one of each 
matched_ids <- all_matches %>% 
  select(cohort_id, anomaly_id) %>% 
  arrange(cohort_id, anomaly_id) %>% 
  group_by(cohort_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(anomaly_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(matched = 1)

# check matching
anomaly_id_check <- matched_ids %>% 
  group_by(anomaly_id) %>% 
  slice(1)
nrow(anomaly_id_check) == nrow(matched_ids)

cohort_id_check <- matched_ids %>% 
  group_by(cohort_id) %>% 
  slice(1)
nrow(cohort_id_check) == nrow(matched_ids)
# all records matched once only now

## check unmatched anomalies
unmatched_anomalies <- df_anomalies %>% 
  left_join(matched_ids) %>% 
  filter(is.na(matched)) %>% 
  mutate(baby_chi_validity = chi_check(cardriss_baby_upi)) %>% 
  mutate(mother_chi_validity = chi_check(cardriss_mother_upi)) %>% 
  filter(mother_chi_validity == "Valid CHI" | baby_chi_validity == "Valid CHI" ) %>% 
  mutate(unmatched = 1) %>% 
  select(-cohort_id)

# double check that these records with valid chis really can't be matched onto the cohort
baby_match_check <- fetus_level_limited %>% 
  left_join(unmatched_anomalies, by = c("baby_upi" = "cardriss_baby_upi")) %>% 
  filter(unmatched == 1)
# no extra matches

mother_match_check <- fetus_level_limited %>% 
  left_join(unmatched_anomalies, by = c("mother_upi" = "cardriss_mother_upi")) %>% 
  filter(unmatched == 1) %>% 
  mutate(end_date_difference = difftime(x_pregnancy_end_date, as.Date(date_end_of_pregnancy), units = "days"))

# check those not yet matched that could
potential_extra_matches <- mother_match_check %>% 
  filter(is.na(end_date_difference) | (end_date_difference < 50 & end_date_difference > -50) )
# This includes the duplicate anomaly records dropped earlier, plus what looks like a multiple pregnancy, 
# male twin is in CARDRISS and only females in COPS. Maintain one so we have correct number of outcomes
# This is the same pregnancy that had too few matches above. Select just this extra match and keep
extra_match <- potential_extra_matches %>% 
  filter(is.na(nrslb_year_of_registration) & end_date_difference == 0) %>% 
  select(cohort_id, anomaly_id) %>% 
  mutate(matched = 1)

# Add extra match to pre-existing matches
final_matches <- matched_ids %>% 
  bind_rows(extra_match)

# double check matching
anomaly_id_check <- final_matches %>% 
  group_by(anomaly_id) %>% 
  slice(1)
nrow(anomaly_id_check) == nrow(final_matches)

cohort_id_check <- final_matches %>% 
  group_by(cohort_id) %>% 
  slice(1)
nrow(cohort_id_check) == nrow(final_matches)

# all records matched once only now
#### Add on anomaly ids to cohort and save ####
# and remove other alterations to fetus level file
fetus_level_matched_ids <- fetus_level %>% 
  left_join(final_matches) %>% 
  select(-c(matched, cohort, cohort_id, cohort_records_in_pregnancy)) %>% 
  mutate(baby_upi = if_else(str_starts(baby_upi, "42"), NA_character_, baby_upi))

fetus_level_matched_ids %>% write_rds(paste0(folder_temp_data, "script6_baby_level_record_infection_anomaly_id.rds"), compress = "gz")


#### Add on cohort ids to anomaly file and save ####
anomalies_matched_ids <- df_anomalies %>% 
  left_join(final_matches) %>% 
  select(-c(anomalies, cohort_id, anom_records_in_pregnancy))

anomalies_matched_ids %>% write_rds(paste0(folder_temp_data, "script6_anomalies_matched_ids.rds"), compress = "gz")

#### Identify final unmatched numbers for description ####
unmatched_anomalies_check <- anomalies_matched_ids %>% 
  filter(is.na(matched))

# Overall number unmatching anomaly records
nrow(unmatched_anomalies_check)

# Number anomaly records with valid UPI
unmatched_valid_upi <- unmatched_anomalies_check %>% 
  mutate(baby_chi_validity = chi_check(cardriss_baby_upi)) %>% 
  mutate(mother_chi_validity = chi_check(cardriss_mother_upi)) %>% 
  filter(mother_chi_validity == "Valid CHI" | baby_chi_validity == "Valid CHI" ) %>% 
  mutate(unmatched = 1)
nrow(unmatched_valid_upi)

# number anomaly records with no valid mother or baby upi
nrow(unmatched_anomalies_check) - nrow(unmatched_valid_upi)

# number anomaly records with UPI present in cohort
mother_match_check_final <- fetus_level_limited %>% 
  left_join(unmatched_valid_upi, by = c("mother_upi" = "cardriss_mother_upi")) %>% 
  filter(unmatched == 1) %>% 
  mutate(end_date_difference = difftime(x_pregnancy_end_date, as.Date(date_end_of_pregnancy), units = "days"))

upi_matching <- mother_match_check_final %>%
  group_by(anomaly_id) %>% 
  slice(1)
nrow(upi_matching)
# For these records we have the mother present in the cohort but for a different pregnancy, 
# or there are what look like duplicate anomaly records with different dates (2 cases)

# number anomaly records with valid UPI but no mother or baby UPI matching to cohort
nrow(unmatched_valid_upi) - nrow(upi_matching)
