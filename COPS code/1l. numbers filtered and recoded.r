
# numbers filtered
smr01_filters <- read_rds(paste0(folder_temp_data, "smr01_filters.rds"))
smr02_filters <- read_rds(paste0(folder_temp_data, "smr02_filters.rds"))
aas_filters <- read_rds(paste0(folder_temp_data, "aas_filters.rds"))
gp_losses_filters <- read_rds(paste0(folder_temp_data, "gp_losses_filters.rds"))
an_booking_filters <- read_rds(paste0(folder_temp_data, "an_booking_filters.rds"))
ecoss_filters <- read_rds(paste0(folder_temp_data, "ecoss_filters.rds"))

all_filters <- bind_rows(smr01_filters, smr02_filters, aas_filters, gp_losses_filters, an_booking_filters, ecoss_filters) %>% 
  mutate(number_filtered = den-num) %>% 
  mutate(percentage_removed = ((number_filtered/den)*100)) %>% 
  select(c(dataset, stage, task, den, num, number_filtered, percentage_removed))

write_rds(all_filters, paste0(folder_temp_data, "all_filters.rds"))
rm(smr01_filters, smr02_filters, aas_filters, gp_losses_filters, an_booking_filters, ecoss_filters, all_filters)

# numbers recoded
smr02_recoded <- read_rds(paste0(folder_temp_data, "smr02_recoded.rds"))
qcovid_recoded <- read_rds(paste0(folder_temp_data, "qcovid_recoded.rds"))


all_recoded <- bind_rows(smr02_recoded, qcovid_recoded) %>% 
  mutate(number_recoded = den-num) %>% 
  mutate(percentage_recoded = ((number_recoded/den)*100)) %>% 
  select(c(dataset, variable, den, num, number_recoded, percentage_recoded))

write_rds(all_recoded, paste0(folder_temp_data, "all_recoded.rds"))
rm(smr02_recoded, qcovid_recoded, all_recoded)

