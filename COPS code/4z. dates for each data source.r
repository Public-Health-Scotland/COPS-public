
# dates for each dataset
smr01_dates <- read_rds(paste0(folder_temp_data, "SMR01_dates.rds"))
smr02_dates <- read_rds(paste0(folder_temp_data, "SMR02_dates.rds"))
nrslb_dates <- read_rds(paste0(folder_temp_data, "NRS live births_dates.rds"))
nhslb_dates <- read_rds(paste0(folder_temp_data, "NHS live births_dates.rds"))
aas_dates <- read_rds(paste0(folder_temp_data, "AAS_dates.rds"))
nrssb_dates <- read_rds(paste0(folder_temp_data, "NRS stillbirths_dates.rds"))
gp_losses_dates <- read_rds(paste0(folder_temp_data, "GP losses_dates.rds"))
an_booking_dates <- read_rds(paste0(folder_temp_data, "AN booking_dates.rds"))
testing_dates <- read_rds(paste0(folder_temp_data, "Testing_dates.rds"))
infant_deaths_dates <- read_rds(paste0(folder_temp_data, "Infant deaths_dates.rds"))
vaccines <- read_rds(paste0(folder_temp_data, "Vaccines_dates.rds"))

dates <- bind_rows(an_booking_dates, gp_losses_dates, smr01_dates, smr02_dates, aas_dates, nrslb_dates, nrssb_dates, 
                   nhslb_dates, infant_deaths_dates, vaccines, testing_dates)

write_rds(dates, paste0(folder_temp_data, "all_dates.rds"))

dates <- readRDS(paste0(folder_temp_data, "all_dates.rds"))
old_dates <- readRDS(paste0(folder_temp_data, "network_folder/all_dates.rds"))

rm(an_booking_dates, gp_losses_dates, smr01_dates, smr02_dates, aas_dates, nrslb_dates, nrssb_dates, 
   nhslb_dates, infant_deaths_dates, vaccines, testing_dates, dates)
