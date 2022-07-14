
########## read in infections data. 

### Connect to DVPROD ---------------------------------------------------
# 
con <- dbConnect(odbc() # REMOVED FOR PUBLIC RELEASE

# ### Extract data --------------------------------------------------------

### extract all pcr tests 
## restrict columns and time period as required
## exclude void, transcribed and de-notified results
tic("extracting covid tests")
tests_pcr <- dbGetQuery(con,
                        "select distinct
                        subject_upi,
                        subject_derived_age_years,
                        subject_residence_datazone_deprivation_decile,
                        test_result,
                        date_ecoss_specimen,
                        test_reason,
                        test_type
                        from
                        covid_testing.lighthouse_and_ecoss_covid_testing
                        where
                        (test_type = 'PCR') AND
                        subject_sex = 'Female' AND 
                        (test_result = 'POSITIVE' OR
                        (test_result = 'NEGATIVE' AND date_ecoss_specimen >= '2022-01-06')) AND
                        flag_test_result_transcribed = 0 AND
                        flag_test_result_denotified = 0") %>%
  distinct() %>% 
  mutate(date_ecoss_specimen = as.Date(date_ecoss_specimen)) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi))) %>%
  mutate(test_type = "PCR")

#lfdnames <- odbcPreviewObject(con,"covid_testing.covid_testing_antigen_results",rowLimit=1)
#odbcPreviewObject(TRANSFER, table="RESULTS_ORDERED_NSS", rowLimit=10)
tests_lfd <- dbGetQuery(
  con,
  "select
  subject_upi,
  subject_derived_age_years,
  subject_residence_postcode,
  date_specimen,
  test_result,
  test_reason
  from 
  covid_testing.covid_testing_antigen_results
  where 
  test_result = 'POSITIVE' AND
  subject_sex = 'Female'
  and date_specimen >= '2022-01-06' ") %>%
  distinct() %>% 
  mutate(date_ecoss_specimen = as.Date(date_specimen)) %>%
  select(-date_specimen) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi)))%>%
  mutate(test_type = "LFD")
toc()
# get simd for lfd tests as a derived field does not exist in the view.
pc_simd <- read_sav("/network_folder/postcode_2021_2_all_simd_carstairs.sav") 
pc_simd <- pc_simd %>% select(pc7, pc8, datazone2011_simd2020v2, simd2020v2_sc_decile)
#subject_residence_datazone_deprivation_decile

tests_lfd <- tests_lfd %>% mutate(pc7 = postcode(subject_residence_postcode,format = "pc7"))
tests_lfd <- left_join(tests_lfd, pc_simd)
tests_lfd <- tests_lfd %>% select(-pc7, -subject_residence_postcode, -datazone2011_simd2020v2, 
  -pc8) %>%
  rename(subject_residence_datazone_deprivation_decile = simd2020v2_sc_decile )


tests <- tests_pcr %>%
  bind_rows(tests_lfd)
#### Cut the data down to one test per day and apply the rule to flip positive LFDs to negative if followed by a negative PCR within 2 days ####
# Favour PCR tests over LFDs, and favour positive tests over negative or indeterminate
tic("applying Negative-PCR-after-Positive-LFD rule")

tests <- tests %>%
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
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything())

toc()


#### make sure we only pick positive tests that are attached to a valid chi ####
tests %<>%
  filter(upi_check == "Valid CHI") %>% 
  select(-upi_check) %>%
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>%
  filter(test_result == "POSITIVE")


#### Apply PHS Positive reporting method ###
# This requires that any positive tests within 90 days of a person's first positive test will be discarded.
# After 90 days a person may have another positive test, but further positive tests within 90 days of that test will be discarded.
# This ensures that we pick up one positive result per infection, rather than multiple positive tests referring to the same COVID infection.
# The method used here is identical to that used to group SMR01, SMR02 etc. records for COPS.
tests_grouped <- tests %>%
  moving_index_deduplication(., upi, date_ecoss_specimen, 90) %>%
  rename(covid_infection = cops_event) %>%
  arrange(upi, covid_infection, date_ecoss_specimen) %>%
  group_by(upi, covid_infection) %>%
  slice(1) %>%
  ungroup() %>%
  select(upi, covid_infection, everything())

## Filter by age####
tests_by_age <- tests_grouped %>%
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>% 
  mutate(date_ecoss_specimen = as.Date(date_ecoss_specimen)) %>%
  mutate(subject_derived_age_years = as.numeric(subject_derived_age_years)) %>%
  filter(subject_derived_age_years %in% 18:44) %>% 
  mutate(age_group = cops_age_group(subject_derived_age_years)) %>% 
  mutate(age_group = case_when(age_group == " 1 <= 19" ~ "1 18-19",
                               age_group == "7 >=40" ~ "7 40-44",
                               T ~ age_group)) %>% 
  mutate(month_infection = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  mutate(simd_quintile = case_when(subject_residence_datazone_deprivation_decile %in% 1:2 ~ 1,
                                   subject_residence_datazone_deprivation_decile %in% 3:4 ~ 2,
                                   subject_residence_datazone_deprivation_decile %in% 5:6 ~ 3,
                                   subject_residence_datazone_deprivation_decile %in% 7:8 ~ 4,
                                   subject_residence_datazone_deprivation_decile %in% 9:10 ~ 5))

##group by age and simd####
count_by_age_group <- tests_by_age %>% 
  tabyl(age_group, month_infection) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  select(-age_group)

count_by_simd <- tests_by_age %>% 
  tabyl(simd_quintile, month_infection) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  filter(!is.na(simd_quintile)) %>% 
  select(-simd_quintile)


### population estimates by deprivation from Bob

simd2020_quin_pop_raw <- read_rds("/network_folder/DataZone2011_pop_est_2011_2020.rds")
simd2020_quin_pop <- simd2020_quin_pop_raw %>%
  filter(year == 2020) %>%
  select(simd2020v2_sc_quintile, sex, starts_with("age")) %>%
  filter(sex == "f") %>% 
  rename(simd2020_quin = simd2020v2_sc_quintile) %>%
  pivot_longer(starts_with("age"), names_to = "age", values_to = "pop") %>%
  mutate(age = str_remove(age, "age") %>% str_remove("plus") %>% as.integer()) %>%
  filter(age %in% 18:44) %>% 
  mutate(pop_age_group = cops_age_group(age)) %>% 
  mutate(pop_age_group = case_when(pop_age_group == " 1 <= 19" ~ "1 18-19",
                                   pop_age_group == "7 >=40" ~ "7 40-44",
                               T ~ pop_age_group)) %>% 
  group_by(pop_age_group, simd2020_quin) %>% 
  summarise(pop = sum(pop))

pop_by_simd_and_age <- simd2020_quin_pop %>% 
  pivot_wider(id_cols = pop_age_group, names_from = simd2020_quin, values_from = pop) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  select(-pop_age_group)

saveRDS(count_by_simd, paste0(folder_temp_data, "network_folder/female_count_simd.rds"))
saveRDS(count_by_age_group, paste0(folder_temp_data, "network_folder/female_count_age.rds"))
#### read out into Excel template xxxx
template <- openxlsx::loadWorkbook(file.path(paste(folder_templates, "Infection comparison and denominator figures_template.xlsx", sep = "/")))

writeData(template, "Infection comparison data", count_by_age_group, startCol = 2, startRow = 7, colNames = FALSE)
writeData(template, "Infection comparison data", count_by_simd, startCol = 2, startRow = 19, colNames = FALSE)
writeData(template, "Infection comparison data", pop_by_simd_and_age, startCol = 2, startRow = 29, colNames = FALSE)

#### Save out workbook ####
saveWorkbook(template, (paste0(folder_outputs, "network_folder/Infection_comparison_data_", Sys.Date(), ".xlsx")), overwrite =TRUE)

pop_by_simd_and_age$females_only <- c("18-19", "20-24", "25-29", "30-34", "35-39", "40-44", "Total")
pop_by_simd_and_age <- pop_by_simd_and_age %>% select(females_only, `1`, `2`, `3`, `4`, `5`, Total)

saveRDS(pop_by_simd_and_age, paste0(folder_temp_data, "network_folder/female_pop_simd_age.rds"))
saveRDS(pop_by_simd_and_age, paste0(folder_temp_data, "network_folder/female_pop_simd_age.rds"))