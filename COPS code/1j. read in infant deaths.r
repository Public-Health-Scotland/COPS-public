#### Read in Infant Deaths ####
data_infant_deaths <- ### EXTRACT/DATABASE CONNECTION DETAILS
                      ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>%
  mutate(nrs_triplicate_id = paste0(year_of_registration, "_", registration_district, "_", entry_number)) %>%
  select(chi, nrs_triplicate_id, date_of_baby_death, underlying_cause_of_baby_death, 
         cause_of_baby_death_0,
         cause_of_baby_death_1,
         cause_of_baby_death_2,
         cause_of_baby_death_3,
         cause_of_baby_death_4,
         cause_of_baby_death_5,
         cause_of_baby_death_6,
         cause_of_baby_death_7,
         cause_of_baby_death_8,
         cause_of_baby_death_9) %>% 
  mutate(chi = chi_pad(chi)) %>% 
  mutate(validity = chi_check(chi)) %>% 
  mutate(chi = case_when(validity == "Valid CHI" ~ chi,
                         T ~ NA_character_)) %>% 
  select(-validity)


write_rds(data_infant_deaths, paste0(folder_temp_data, "infant_deaths.rds"))

#dates
dataset_dates("Infant deaths", data_infant_deaths$date_of_baby_death)

rm(data_infant_deaths)
