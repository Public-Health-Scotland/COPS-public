pregnancies <- read_rds(paste0(folder_temp_data, "script6_baby_level_record.rds"))
testing <- read_rds(paste0(folder_temp_data, "infections_all_test_dates.rds"))

testing <-testing %>% rename(positive_test_1_test_date = positive_test_1, positive_test_2_test_date = positive_test_2, 
                   positive_test_3_test_date = positive_test_3, positive_test_4_test_date = positive_test_4)

#test_details have now been saved separately (from script 4b). So select only date and specimen ID variables
testing <- testing %>% select(upi, contains("specimen_id"), contains("test_date"))
names(testing)
#### Add on data for mother COVID tests ####

pregnancy_covid_results <- pregnancies %>%
  select(mother_upi, pregnancy_id, x_est_conception_date, x_pregnancy_end_date) %>%
  group_by(pregnancy_id) %>%
  mutate(x_est_conception_date = min_(x_est_conception_date)) %>%
  mutate(x_pregnancy_end_date = case_when(is.na(x_pregnancy_end_date) ~ Sys.Date(),
                                          T ~ x_pregnancy_end_date)) %>%
  mutate(x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>%
  ungroup() %>%
  distinct() %>%
  left_join(testing, by = c("mother_upi" = "upi")) %>%
  pivot_longer(cols = contains("test_date")) %>%
  filter(!is.na(value)) %>%
  mutate(pregnancy_interval = interval(x_est_conception_date, x_pregnancy_end_date)) %>%
  mutate(test_result = case_when(# str_detect(name, "negative") ~ "negative",
                                  str_detect(name, "positive") ~ "positive")) %>%
  mutate(has_pos_test_at_any_point = T) %>%
  #mutate(negative_test_during_pregnancy = case_when(str_detect(name, "negative") & value %within% pregnancy_interval ~ T,
  #                                                  T ~ F)) %>%
   mutate(positive_test_during_pregnancy = case_when(as.Date(value) %within% pregnancy_interval ~ T,
                                                    T ~ F)) %>%
  group_by(pregnancy_id) %>%
  #mutate(negative_test_during_pregnancy = max(negative_test_during_pregnancy)) %>%
  mutate(positive_test_during_pregnancy = max(positive_test_during_pregnancy)) %>%
  ungroup()
#names(pregnancy_covid_results)
##summarise specimen ID
pregnancy_covid_results <- pregnancy_covid_results %>%
  mutate(specimen_id = case_when( str_detect(name, "test_1") ~ positive_test_1_specimen_id, 
                                  str_detect(name, "test_2") ~ positive_test_2_specimen_id,  
                                  str_detect(name, "test_3") ~ positive_test_3_specimen_id, 
                                  str_detect(name, "test_4") ~ positive_test_4_specimen_id )) %>%
  select(mother_upi,pregnancy_id, x_est_conception_date, x_pregnancy_end_date, name, value, pregnancy_interval,specimen_id,
         positive_test_during_pregnancy, has_pos_test_at_any_point) %>%
mutate(value = as.Date(value)) 

#summarise, include test type 
positive_test_dates <- pregnancy_covid_results %>%
  arrange(mother_upi, value) %>% 
#  filter(test_result == "positive") %>%
  group_by(pregnancy_id) %>%
  summarise(earliest_positive_test = min(value), 
            earliest_positive_test_id = first(specimen_id)) %>%
  ungroup()

earliest_positive_test_during_pregnancy <- pregnancy_covid_results %>%
  filter( value %within% pregnancy_interval) %>%
  arrange(mother_upi, value) %>% 
  group_by(pregnancy_id) %>%
  summarise(earliest_positive_test_during_pregnancy = min(value), 
            earliest_positive_test_id = first(specimen_id)) %>%
  ungroup()

all_positive_tests_during_pregnancy <- pregnancy_covid_results %>%
  filter( value %within% pregnancy_interval) %>%
  select(pregnancy_id, value, specimen_id) %>%
  arrange(value) %>%
  group_by(pregnancy_id) %>%
  mutate(test_number = paste0("positive_test_during_pregnancy_", row_number())) %>%
  pivot_wider(names_from = test_number, values_from = c(value, specimen_id))


total_positives_during_this_pregnancy <- pregnancy_covid_results %>%
  filter(value %within% pregnancy_interval) %>%
  group_by(pregnancy_id) %>%
  summarise(total_positives_during_this_pregnancy = n())

pregnancy_covid_results_all <- pregnancy_covid_results %>%
  #left_join(negatives_test_dates, by="pregnancy_id") %>%
  left_join(positive_test_dates, by="pregnancy_id") %>%
  #left_join(earliest_negative_test_during_pregnancy, by="pregnancy_id") %>%
  left_join(earliest_positive_test_during_pregnancy, by="pregnancy_id") %>%
  left_join(all_positive_tests_during_pregnancy, by="pregnancy_id") %>%
  left_join(total_positives_during_this_pregnancy, by="pregnancy_id") %>%
  select(pregnancy_id, 
         has_pos_test_at_any_point,
         total_positives_during_this_pregnancy,
         #earliest_negative_test, 
         earliest_positive_test, 
         #negative_test_during_pregnancy, 
         positive_test_during_pregnancy, 
         #earliest_negative_test_during_pregnancy, 
         earliest_positive_test_during_pregnancy,
         contains("positive_test_during_pregnancy")) %>%
  distinct() %>%
  rename_with( ~ paste0("tests_mother_", .)) %>%
  rename(pregnancy_id = tests_mother_pregnancy_id)

pregnancies <- pregnancies %>%
  left_join(pregnancy_covid_results_all, by="pregnancy_id")


#### Add on data for baby COVID tests ####
baby_covid_results <- pregnancies %>%
  select(baby_upi, x_date_of_baby_death) %>%
  filter(!is.na(baby_upi)) %>%
  left_join(testing, by = c("baby_upi" = "upi")) %>%
  pivot_longer(cols = contains("test_date")) %>%
  filter(!is.na(value)) %>%
  filter(value < x_date_of_baby_death | is.na(x_date_of_baby_death)) %>% # Remove any post-mortem tests
  select(-x_date_of_baby_death) %>%
  mutate(test_result = case_when( str_detect(name, "negative") ~ "negative",
                                  str_detect(name, "positive") ~ "positive")) %>%
  mutate(has_had_pcr_test_at_any_point = T) %>%
  mutate(specimen_id = case_when( str_detect(name, "test_1") ~ positive_test_1_specimen_id, 
                                  str_detect(name, "test_2") ~ positive_test_2_specimen_id,  
                                  str_detect(name, "test_3") ~ positive_test_3_specimen_id, 
                                  str_detect(name, "test_4") ~ positive_test_4_specimen_id )) %>%
  select(-c(positive_test_1_specimen_id, positive_test_2_specimen_id, positive_test_3_specimen_id, positive_test_4_specimen_id))


baby_positive_tests <- baby_covid_results %>%
  filter(test_result == "positive") %>%
  mutate(has_tested_positive = T) %>%
  group_by(baby_upi) %>%
  slice(1) %>%
  ungroup() %>%
  rename(earliest_positive_test = value) %>%
  rename(earliest_test_specimen_id = specimen_id) %>%
  select(baby_upi, has_tested_positive, earliest_positive_test,  earliest_test_specimen_id)

# baby_negative_tests <- baby_covid_results %>%
#   filter(test_result == "negative") %>%
#   mutate(has_tested_negative = T) %>%
#   group_by(baby_upi) %>%
#   slice(1) %>%
#   ungroup() %>%
#   rename(earliest_negative_test = value) %>%
#   select(baby_upi, has_tested_negative, earliest_negative_test)
  
baby_covid_results <- baby_covid_results %>%
  select(baby_upi, has_had_pcr_test_at_any_point) %>%
  distinct() %>%
  #left_join(baby_negative_tests) %>%
  left_join(baby_positive_tests) %>%
  rename_with( ~ paste0("tests_baby_", .)) %>%
  rename(baby_upi = tests_baby_baby_upi)

pregnancies <- pregnancies %>%
  left_join(baby_covid_results, by="baby_upi")

pregnancies %>% write_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds"), compress = "gz")

pregnancies <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds"))
