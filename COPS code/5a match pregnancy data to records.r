
data_smr01               <- read_rds(paste0(folder_temp_data, "smr01.rds")) %>% select(-smr01)
data_smr02               <- read_rds(paste0(folder_temp_data, "smr02.rds")) 
data_cops_births         <- read_rds(paste0(folder_temp_data, "cops_births.rds")) %>%
                              select(baby_id, nhs_live_births, nrs_live_births, smr02_live_births, baby_upi)
data_aas                 <- read_rds(paste0(folder_temp_data, "aas.rds")) 
data_nrs_stillbirths     <- read_rds(paste0(folder_temp_data, "nrs_stillbirths.rds"))
data_gp_losses           <- read_rds(paste0(folder_temp_data, "gp_losses.rds"))
data_an_booking          <- read_rds(paste0(folder_temp_data, "antenatal_booking.rds")) %>% select(-c(anbooking_event_type, anbooking))

data_nhs_live_births <-      read_rds(paste0(folder_temp_data, "nhs_live_births.rds"))
data_nrs_live_births <-      read_rds(paste0(folder_temp_data, "nrs_live_births.rds"))

data_q_covid         <-      read_rds(paste0(folder_temp_data, "qcovid.rds"))
data_shielding       <-      read_rds(paste0(folder_temp_data, "shielding.rds"))
data_vaccine         <-      read_rds(paste0(folder_temp_data, "vaccine_cops.rds"))

#### Read in Pregnancy Record ####
pregnancies1 <- read_rds(paste0(folder_temp_data, "script3_pregnancy_record.rds"))

pregnancies2 <- pregnancies1 %>% 
  left_join(data_cops_births, by=c("cops_births_1" = "baby_id")) %>%
  left_join(data_an_booking, by=c("antenatal_booking_1" = "event_id")) %>%
  left_join(data_nhs_live_births, by=c("nhs_live_births" = "event_id")) %>% 
  left_join(data_nrs_live_births, by=c("nrs_live_births" = "event_id")) %>%
  left_join(data_aas, by=c("aas_termination_1" = "event_id")) %>%
  left_join(data_nrs_stillbirths, by=c("nrs_stillbirths_1" = "event_id")) %>%
  left_join(data_smr01, by=c("smr01_1" = "event_id")) %>%
  left_join(data_gp_losses, by=c("gp_losses_1" = "event_id"))

#Join SMR02 data separately to avoid variable name collisions, just in case a tiny number of fetuses have been matched to multiple SMR02 records
pregnancies_live_birth <- pregnancies2 %>%
  filter(outcome == "Live birth") %>%
  left_join(data_smr02, by=c("smr02_live_births" = "event_id"))

pregnancies_other <- pregnancies2 %>%
  filter(outcome != "Live birth") %>%
  left_join(data_smr02, by=c("smr02_1" = "event_id"))

pregnancies3 <- pregnancies_live_birth %>%
  bind_rows(pregnancies_other) %>%
  arrange(mother_upi, pregnancy_start_date)
