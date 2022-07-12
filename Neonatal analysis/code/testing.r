# run script 0b. setup.r in the main COPS code before this script

end_date <- as.Date("2022-01-31")


# importin national testing data to find out how many neonates 
tic("extracting covid tests")
tests_pcr <- dbGetQuery(con,
                        "select distinct
                        subject_upi,
                        specimen_id,
                        test_result,
                        subject_date_of_birth,
                        date_ecoss_specimen,
                        flag_covid_symptomatic,
                        test_reason,
                        test_type,
                        test_ch1_result_ct_value, 
                        test_ch2_result_ct_value, 
                        test_ch3_result_ct_value, 
                        test_ch4_result_ct_value, 
                        test_ch1_target_gene, 
                        test_ch2_target_gene, 
                        test_ch3_target_gene, 
                        test_ch4_target_gene, 
                        test_ch1_target_gene_result, 
                        test_ch2_target_gene_result, 
                        test_ch3_target_gene_result, 
                        test_ch4_target_gene_result
                        from
                        covid_testing.lighthouse_and_ecoss_covid_testing
                        where
                        (test_type = 'PCR') AND
                        (test_result = 'POSITIVE' OR
                        (test_result = 'NEGATIVE' AND date_ecoss_specimen >= '2022-01-06')) AND
                        flag_test_result_transcribed = 0 AND
                        flag_test_result_denotified = 0") %>%
  distinct() 

tests_pcr <-tests_pcr %>%
  mutate(date_ecoss_specimen = as.Date(date_ecoss_specimen)) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi))) %>%
  mutate(test_type = "PCR")

tests_lfd <- dbGetQuery(
  con,
  "select
  subject_upi,
  specimen_id,
  date_specimen,
  test_result,
  subject_date_of_birth,
  test_reason
  from 
  covid_testing.covid_testing_antigen_results
  where 
  test_result = 'POSITIVE'
  and date_specimen >= '2022-01-06' ") %>%
  distinct()

tests_lfd <- tests_lfd %>% 
  mutate(date_ecoss_specimen = as.Date(date_specimen)) %>%
  select(-date_specimen) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi)))%>%
  mutate(test_type = "LFD")

tests <- tests_pcr %>%
  bind_rows(tests_lfd)

#### Number of neonates testing positive using only testing data ####
neonate_tests <- tests %>%
  mutate(age_in_days = round(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days"))) %>% 
  filter(age_in_days <= 29) %>% 
  filter(date_ecoss_specimen <= end_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE") %>% 
  mutate(age_in_days = round(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days"))) %>% 
  filter(age_in_days <= 27) %>% 
  group_by(upi) %>%
  slice(1)

neonate_tests %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/neonate_tests.rds"), compress = "gz")

post_neonate_tests <- tests %>%
  mutate(age_in_days = round(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days"))) %>% 
  filter(age_in_days > 27 & age_in_days <= 364) %>% 
  filter(date_ecoss_specimen <= end_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE")%>% 
  mutate(age_in_days = round(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days"))) %>% 
  filter(age_in_days > 27 & age_in_days <= 364) %>% 
  group_by(upi) %>%
  slice(1)

post_neonate_tests %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/post_neonate_tests.rds"), compress = "gz")


one_to_four_yo_tests <- tests %>%
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 0 & age_in_years <= 4) %>% 
  filter(date_ecoss_specimen <= end_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE")%>% 
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 0 & age_in_years <= 4) %>% 
  group_by(upi) %>%
  slice(1)

one_to_four_yo_tests %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/one_to_four_yo_tests.rds"), compress = "gz")


five_to_11_yo_tests <- tests %>%
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 4 & age_in_years <= 11) %>% 
  filter(date_ecoss_specimen <= end_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE")%>% 
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 4 & age_in_years <= 11) %>% 
  group_by(upi) %>%
  slice(1)

five_to_11_yo_tests %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/five_to_11_yo_tests.rds"), compress = "gz")


twelve_to_17_yo_tests <- tests %>%
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 11 & age_in_years <= 17) %>% 
  filter(date_ecoss_specimen <= end_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE")%>% 
  mutate(age_in_years = floor(difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")/365)) %>% 
  filter(age_in_years > 11 & age_in_years <= 17) %>% 
  group_by(upi) %>%
  slice(1)

twelve_to_17_yo_tests %>% write_rds(paste0(folder_temp_data, "neonate_paper_temp/twelve_to_17_yo_tests.rds"), compress = "gz")
