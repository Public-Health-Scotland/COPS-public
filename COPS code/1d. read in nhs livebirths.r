
#### Read in NHS Live Births ####
data_nhs_live_births <- ### EXTRACT/DATABASE CONNECTION DETAILS
                        ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>%
  select(-hb_cypher) %>%
  rename("mothers_dob" = "mothers_do_b") %>%
  rename("baby_chi" = "chi") %>%
  distinct() %>%
  filter(baby_dob >= as.Date("2019-05-01")) %>%
  mutate(mother_chi = chi_pad(mother_chi)) %>% 
  mutate(validity = chi_check(mother_chi)) %>% 
  mutate(mother_chi = case_when(validity == "Valid CHI" ~ mother_chi,
                                       T ~ NA_character_)) %>% 
  mutate(mother_chi = case_when(
    is.na(mother_chi) ~ paste0("44", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ mother_chi
  )) %>%
  mutate(baby_chi = chi_pad(baby_chi)) %>% 
  mutate(validity = chi_check(baby_chi)) %>% 
  mutate(baby_chi = case_when(validity == "Valid CHI" ~ baby_chi,
                                T ~ NA_character_)) %>% 
  mutate(gest_period = if_else(gest_period %in% feasible_gestation_lb, gest_period, NA_integer_)) %>% 
  mutate(assumed_gestation = if_else(is.na(gest_period), 1, 0)) %>% 
  mutate(gest_period = case_when(is.na(gest_period) ~ assumed_gestation_live_birth,
                                 T ~ as.double(gest_period))) %>%
  mutate(estimated_conception_date = baby_dob - (weeks(gest_period) - weeks(2) ) ) %>%
  group_by(mother_chi, baby_dob) %>% mutate(num_of_births = n()) %>% ungroup() %>%
  left_join(hb_lookup, by =  c("hb2019" = "healthboard_code")) %>% 
  mutate(hb2019 = healthboard) %>% 
  select(-c(validity, healthboard)) %>% 
  mutate(outcome_type = "Live birth") %>%
  rename_with( ~ paste0("nhslb_", .)) %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_nhs_live_births, paste0(folder_temp_data, "nhs_live_births.rds"))

#dates
dataset_dates("NHS live births", data_nhs_live_births$nhslb_baby_dob)

rm(data_nhs_live_births)

