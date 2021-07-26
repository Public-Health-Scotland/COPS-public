#### Read in vaccine data ####

###
### FOLDER LOCATIONS REMOVED FOR PUBLIC RELEASE
###

data_vaccine <- ### EXTRACT/DATABASE CONNECTION DETAILS
                ### REMOVED FOR PUBLIC RELEASE
data_vaccine_COPS <- data_vaccine %>% 
  mutate(mother_upi = chi_pad(as.character(patient_derived_upi_number))) %>% 
  select(vacc_dose_number, vacc_occurence_date, vacc_product_name, mother_upi,
         vacc_location_name, vacc_location_health_board_name, vaccination_known_pregnant) %>% 
  distinct()
  

write_rds(data_vaccine_COPS, paste0(folder_temp_data, "vaccine_cops.rds"))

#dates
dataset_dates("Vaccines", data_vaccine_COPS$vacc_occurence_date)