#### Read in ECOSS testing data ####
data_ecoss_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                     ### REMOVED FOR PUBLIC RELEASE
  select(subject_upi, date_ecoss_specimen, test_result) 

data_ecoss_temp_2 <- data_ecoss_temp_1 %>%
  filter(test_result %in% c("POSITIVE", "NEGATIVE")) %>%
  arrange(subject_upi, test_result, date_ecoss_specimen) %>%
  distinct() %>%
  group_by(subject_upi, test_result) %>%
  mutate("test" = paste0(test_result, "_COVID_TEST_", row_number())) %>%
  ungroup() %>%
  select(-test_result)

data_ecoss_temp_3 <- data_ecoss_temp_2 %>% 
  mutate(subject_upi = chi_pad(subject_upi)) %>% 
  mutate(validity = chi_check(subject_upi)) %>% 
  filter(validity == "Valid CHI")

#record number filtered out
filter_1 <- data.frame(stage = 1,
                                 den = nrow(data_ecoss_temp_1),
                                 num = nrow(data_ecoss_temp_2),
                                 task = "Select only positive or negative tests")

filter_2 <- data.frame(stage = 2,
                       den = nrow(data_ecoss_temp_2),
                       num = nrow(data_ecoss_temp_3),
                       task = "Remove invalid CHIs")

data_ecoss <- data_ecoss_temp_3 %>%
  pivot_wider(names_from = test, values_from = date_ecoss_specimen) %>%
  select(subject_upi, starts_with("POSITIVE"), starts_with("NEGATIVE")) %>%
  clean_names() %>%
  rename(mother_upi_number = subject_upi)

#record number filtered out
filter_3 <- data.frame(stage = 3,
                       den = nrow(data_ecoss_temp_3),
                       num = nrow(data_ecoss),
                       task = "Pivot data to one row per person")

write_rds(data_ecoss, paste0(folder_temp_data, "ecoss_tests.rds"))

#number filtered out
ecoss_filters <- bind_rows(filter_1, filter_2) %>% 
  mutate(dataset ="ecoss")
write_rds(ecoss_filters, paste0(folder_temp_data, "ecoss_filters.rds"))

rm(data_ecoss_temp_1, data_ecoss_temp_2, data_ecoss_temp_3, filter_1, filter_2, filter_3, ecoss_filters, data_ecoss)
