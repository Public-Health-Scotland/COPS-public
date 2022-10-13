#### Read in vaccine data ####
data_vaccine <- read.csv("/network_folder/dash_extract_Cohorts_preg_data_2022-0-25.csv")


data_vaccine_processed <- data_vaccine %>% 
  mutate(upi = chi_pad(as.character(patient_derived_upi_number))) %>% 
  mutate(upi_sex_character = as.numeric(substr(upi, 9, 9))) %>% 
  mutate(patient_sex = if_else(upi_sex_character %% 2 == 0, "FEMALE", "MALE")) %>% 
  mutate(vacc_occurence_date = as_date(substr(vacc_occurence_date, 1, 10))) %>% 
  mutate(upi_dob = as.Date(substr(upi, 1, 6), "%d%m%y")) %>% 
  mutate(age = trunc((upi_dob %--% vacc_occurence_date) / years(1)))

data_vaccine_by_age <- data_vaccine_processed %>%
  filter(patient_sex == "FEMALE") %>% 
  filter(age %in% 18:44) %>% 
  filter(vacc_occurence_date <= publication_latest_vacc_date) %>% 
  mutate(age_group = cops_age_group(age)) %>% 
  mutate(age_group = case_when(age_group == " 1 <= 19" ~ "1 18-19",
                               age_group == "7 >=40" ~ "7 40-44",
                               T ~ age_group)) %>% 
  mutate(month_vaccination = format(as.Date(vacc_occurence_date), "%Y-%m")) 

overall_count_by_age_group <- data_vaccine_by_age %>% 
  count(age_group, month_vaccination) %>% 
  pivot_wider(id_cols = c(age_group), names_from = month_vaccination, values_from = n) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  mutate(vacc_dose_number = "All")

overall_count_by_dose_num <- data_vaccine_by_age %>% 
  count(vacc_dose_number, month_vaccination) %>% 
  pivot_wider(id_cols = c(vacc_dose_number), names_from = month_vaccination, values_from = n) %>% 
  adorn_totals(where = "col") %>% 
  mutate(age_group = "All") %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number))

count_by_age_group <- data_vaccine_by_age %>% 
  count(age_group, vacc_dose_number, month_vaccination) %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number)) %>% 
  pivot_wider(id_cols = c(age_group, vacc_dose_number), names_from = month_vaccination, values_from = n) %>% 
  adorn_totals(where = "col") %>% 
  bind_rows(overall_count_by_age_group) %>%
  bind_rows(overall_count_by_dose_num) %>% 
  arrange(age_group)
#save for use in later checks
saveRDS(overall_count_by_dose_num, paste0(folder_temp_data, "vaccine_output_tables/compare_overall_count_by_dose_num.rds"))
saveRDS(count_by_age_group, paste0(folder_temp_data, "vaccine_output_tables/compare_count_by_age_group.rds"))
### population estimates

pop_raw <- read_rds("//conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2020.rds")
population <- pop_raw %>%
  filter(year == 2020) %>%
  select(sex, starts_with("age")) %>%
  filter(sex == "f") %>% 
  pivot_longer(starts_with("age"), names_to = "age", values_to = "pop") %>%
  mutate(age = str_remove(age, "age") %>% str_remove("plus") %>% as.integer()) %>%
  filter(age %in% 18:44) %>% 
  summarise(pop = sum(pop))

### population coverage
count <- data_vaccine_by_age %>% 
  count(vacc_dose_number) %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number))

population_coverage <- count %>% 
  mutate(percent_coverage = n/population[[1,1]])

saveRDS(population_coverage, paste0(folder_temp_data, "vaccine_output_tables/population_coverage.rds"))

#### create title ####
dates <- readRDS(paste0(folder_temp_data, "all_dates.rds"))
title <- paste0("COVID-19 vaccination comparison figures for COPS, ", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"))

#### read out into Excel template ####
template <- openxlsx::loadWorkbook(file.path(paste(folder_templates, "Vaccination comparison figures_template.xlsx", sep = "/")))

writeData(template, "Vaccination comparison data", title, startCol = 1, startRow = 1)
writeData(template, "Vaccination comparison data", select(count_by_age_group, -c(age_group, vacc_dose_number)), startCol = 3, startRow = 6, colNames = FALSE)
writeData(template, "Vaccination comparison data", select(population_coverage, -c(vacc_dose_number, n)), startCol = 2, startRow = 46, colNames = FALSE)

#### Save out workbook ####
saveWorkbook(template, (paste0(folder_outputs, "Vaccines/Vaccination_comparison_data_", Sys.Date(), ".xlsx")), overwrite =TRUE)
