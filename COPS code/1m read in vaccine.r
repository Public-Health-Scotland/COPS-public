#### Read in vaccine data ####

data_vaccine <- read_csv("/network_folder/dash_extract_Cohorts_preg_data_2022-08-25.csv")


data_vaccine_COPS_temp <- data_vaccine %>% 
  mutate(mother_upi = chi_pad(as.character(patient_derived_upi_number))) %>% 
  mutate(mother_upi_check = chi_check(as.character(mother_upi))) 

rm(data_vaccine)
gc()

tabyl(data_vaccine_COPS_temp$mother_upi_check)

data_vaccine_COPS_temp_1 <- data_vaccine_COPS_temp %>% 
  mutate(vacc_occurence_date = as.Date(substr(vacc_occurence_date, 1, 10), "%Y-%m-%d")) %>% 
  filter(vacc_occurence_date >= vacc_start_date)

rm(data_vaccine_COPS_temp)
gc()

chi_completeness_check <- data_vaccine_COPS_temp_1 %>% 
  mutate(week_ending = ceiling_date(vacc_occurence_date, unit = "week", change_on_boundary = F)) %>% 
  count(week_ending, mother_upi_check) %>% 
  pivot_wider(names_from = mother_upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate(percentage = round(missing / (valid_chi + missing) * 100, digits = 2))

overall_chi_completeness <- data_vaccine_COPS_temp_1 %>% 
  count(mother_upi_check) %>% 
  pivot_wider(names_from = mother_upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate(percentage = round(valid_chi / (valid_chi + missing) * 100, digits = 1))

overall_chi_completeness_perc <- overall_chi_completeness$percentage[1]
saveRDS(overall_chi_completeness_perc, paste0(folder_temp_data, "overall_vacc_chi_completeness_perc.rds"))

theme_set(theme_classic())
chi_completeness_check %>% 
  ggplot(aes(x=week_ending, y=percentage)) +
  geom_line(aes()) +
  xlab("Week ending") +
  ylab("Percentage vaccination records missing CHI")

template <- loadWorkbook(paste0(folder_templates, "Vaccine CHI check.xlsx"))
writeData(template, "Vaccine CHI completeness", chi_completeness_check, colNames = FALSE, startCol = 1, startRow = 2)
insertPlot(template, "Vaccine CHI completeness", width = 6, startCol = 6, startRow = 2,
           height = 3.5, fileType = "png", units = "in")
saveWorkbook(template, (paste0(folder_outputs, "network_folder/Vaccine_CHI_check_", Sys.Date(), ".xlsx")), overwrite =TRUE)

data_vaccine_COPS_temp_1 <- data_vaccine_COPS_temp_1 %>% 
  filter(mother_upi_check == "Valid CHI") %>% 
  filter(vacc_occurence_date <= publication_latest_vacc_date) %>% 
  filter(vacc_occurence_date >= vacc_start_date) %>% 
  select(vacc_dose_number, vacc_occurence_date, vacc_product_name, mother_upi,
         vacc_location_health_board_name) 
#
#### identify and fix those with wrong number of records ####
data_vaccine_COPS_temp_2 <- data_vaccine_COPS_temp_1 %>% 
  distinct() %>% 
  mutate(vacc_dose_number = as.numeric(substr(vacc_dose_number, 6, 6))) %>% 
  arrange(mother_upi, vacc_dose_number, vacc_occurence_date) %>% 
  group_by(mother_upi) %>% 
  mutate(n_records = n()) %>% 
  ungroup() %>% 
  group_by(mother_upi, vacc_dose_number) %>% 
  mutate(n_each_dose = as.numeric(n())) %>% 
  ungroup()

#dates
dataset_dates("Vaccines", data_vaccine_COPS_temp_1$vacc_occurence_date)
rm(data_vaccine_COPS_temp_1)
gc()

# take out those that have the wrong number of records
data_vaccine_COPS_for_fixing_1 <- data_vaccine_COPS_temp_2 %>% 
  filter(n_records > max(vacc_dose_number) | n_each_dose > 1)
# this should contain zero rows. If not, these records need fixing.

if(nrow(data_vaccine_COPS_for_fixing_1) != 0) 
{print(paste0(nrow(data_vaccine_COPS_for_fixing_1), " vaccine records have an unexpected number of doses: intervention needed."))
  } else {
  print("All vaccine records have expected number of doses: no action needed.")}
##Run fix. 
#this deals with duplicated doses ie 2x dose 1s on different dates.#the first date is selected.
#if other issues are identified in future, this section may need to be added to to fix other problems
if(nrow(data_vaccine_COPS_for_fixing_1) != 0) {
fixed <- data_vaccine_COPS_for_fixing_1 %>% arrange(mother_upi, vacc_dose_number, vacc_occurence_date) %>%
  group_by(mother_upi, vacc_dose_number) %>% slice(1) %>% 
  ungroup()
data_vaccine_COPS_temp_2 <- data_vaccine_COPS_temp_2 %>% filter(!mother_upi %in% data_vaccine_COPS_for_fixing_1$mother_upi) 
data_vaccine_COPS_temp_2 <- rbind(data_vaccine_COPS_temp_2, fixed)}

#### create dataset with one line per person ####
data_vaccine_COPS <- data_vaccine_COPS_temp_2 %>% 
  select(mother_upi, vacc_dose_number, vacc_occurence_date, vacc_product_name, vacc_location_health_board_name) %>% 
  mutate(vacc_dose_number = as.character(paste0("dose_", vacc_dose_number))) %>% 
  mutate(vacc_occurence_date = as.character(vacc_occurence_date)) %>% 
  pivot_longer(!c(mother_upi, vacc_dose_number), names_to = "indicator", values_to = "value") %>% 
  pivot_wider(id_cols = mother_upi,
              names_from = c(vacc_dose_number, indicator),
              values_from = value,
              names_sep = "_") %>% 
  mutate_at(vars(matches("vacc_occurence_date")), as_date)

rm(data_vaccine_COPS_temp_2)
gc()

write_rds(data_vaccine_COPS, paste0(folder_temp_data, "vaccine_cops.rds"), compress = "gz")


rm(data_vaccine_COPS_for_fixing_1, data_vaccine_COPS, chi_completeness_check)
