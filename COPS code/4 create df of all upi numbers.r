###
### Create a csv of all CHI numbers which appear in our cohort.
### This is required when requesting certain data extracts
### such as COVID test results and the shielding patient list.
###


babies <- read_rds(paste0(folder_temp_data, "cops_births.rds")) %>% select(baby_upi) %>%
  rename(upi = baby_upi)

mothers <- read_rds(paste0(folder_temp_data, "script3_pregnancy_record.rds")) %>% select(mother_upi) %>%
  rename(upi = mother_upi)

all_upis <- bind_rows(babies, mothers) %>%
  distinct() %>%
  filter(!str_starts(upi,"4")) %>%
  filter(!str_starts(upi,"5")) %>%
  filter(!str_starts(upi,"6")) %>%
  filter(!str_starts(upi,"7")) %>%
  filter(!str_starts(upi,"8"))
  
write_csv(all_upis, paste0(folder_cohorts, "all_upis.csv"))

rm(all_upis, mothers, babies)
