#### Read in Shielding Data ####
data_shielding <- ### EXTRACT/DATABASE CONNECTION DETAILS
                  ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>%
  rename_with( ~ paste0("shielding_", .)) %>%
  mutate(shielding_shield = case_when(is.na(shielding_shield) ~ 0,
                                      T ~ shielding_shield)) %>%
  filter(shielding_shield == 1 | shielding_group6 == 1)
  
write_rds(data_shielding, paste0(folder_temp_data, "shielding.rds"))

# CHI: CHI Number
# Shield: 1 = shielding; 0 = not shielding
# Removal: R = individual who has been flagged for removal; D = shielding individual who has died; blank = active shielder
# Group 1: Transplant group
# Group 2: Cancer group
# Group 3: Respiratory group
# Group 4: Rare disease group
# Group 5: Immunosuppression group
# Group 6: Pregnant with Heart Disease group
# Group 7: Clinician identified group