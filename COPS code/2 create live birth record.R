
start_date <- as.Date("2010-01-01") # For testing purposes, just set these to silly numbers to ensure we pick up everyone
end_date <- as.Date("2030-01-01")

data_smr02 <- read_rds(paste0(folder_temp_data, "smr02.rds")) %>% filter(smr02_date_of_delivery >= start_date & smr02_date_of_delivery <= end_date)
data_nrs_live_births <- read_rds(paste0(folder_temp_data, "nrs_live_births.rds")) %>% filter(nrslb_date_of_birth >= start_date & nrslb_date_of_birth <= end_date)
data_nhs_live_births <- read_rds(paste0(folder_temp_data, "nhs_live_births.rds")) %>% filter(nhslb_baby_dob >= start_date & nhslb_baby_dob <= end_date)

temp_smr02_births <- data_smr02 %>% filter(smr02_live_birth == T) %>% select(smr02_upi_number, smr02_baby_upi_number, smr02_date_of_delivery, smr02_estimated_conception_date, event_id) %>% mutate(source = "smr02")
temp_nhs_live_births <- data_nhs_live_births %>% select(nhslb_mother_chi, nhslb_baby_chi, nhslb_baby_dob, nhslb_estimated_conception_date, event_id) %>% mutate(source = "nhs_live_births")
temp_nrs_live_birth <- data_nrs_live_births %>% select(nrslb_mother_upi_number, nrslb_child_upi_number, nrslb_date_of_birth, nrslb_estimated_conception_date, event_id) %>% mutate(source = "nrs_live_births")

colnames(temp_smr02_births) <-        c("mother_upi", "baby_upi", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_nhs_live_births) <-     c("mother_upi", "baby_upi", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_nrs_live_birth) <-      c("mother_upi", "baby_upi", "event_date", "conception_date", "event_id", "data_source")

births1 <- temp_nhs_live_births %>%
  bind_rows(temp_smr02_births) %>%
  bind_rows(temp_nrs_live_birth)
  
births1 %>%
  mutate(validity = chi_check(mother_upi)) %>%
  tabyl(validity)


#### Identify consistent mother UPIs ####
births1 <- births1 %>%
  group_by(baby_upi) %>%
  mutate(mother_upi = case_when(str_starts(mother_upi, "4") ~ NA_character_,
                                T ~ mother_upi)) %>%
  mutate(consistent_mother_upi = case_when(n_distinct(mother_upi, na.rm=T) > 1 ~ F,
                                           T ~ T)) %>% 
  ungroup()



#### Fill in missing mother UPIs where possible
births_corrected_mother_upis <- births1 %>%
  group_by(baby_upi) %>%
  filter(consistent_mother_upi == T) %>%
  mutate(mother_upi = first_non_na(mother_upi)) %>%
  ungroup()

births2 <- births1 %>%
  filter(consistent_mother_upi == F) %>%
  bind_rows(births_corrected_mother_upis) %>%
  select(-consistent_mother_upi)

#### Fill in missing baby UPIs where possible
births3 <- births2 %>%
  mutate(baby_upi = case_when(str_starts(baby_upi, "5") ~ NA_character_,
                              T ~ baby_upi)) %>%
  group_by(mother_upi, event_date) %>%
  mutate(multiple_baby_upis = case_when(min_(baby_upi) != max_(baby_upi) ~ T,
                                        T ~F)) %>%
  ungroup() %>%
  mutate(correctable_baby_upi = case_when(multiple_baby_upis == F & !is.na(mother_upi) ~ T,
                                          T ~ F))

births_corrected_baby_upis <- births3 %>%
  filter(correctable_baby_upi == T) %>%
  group_by(mother_upi, event_date) %>%
  mutate(baby_upi = first_non_na(baby_upi)) %>%
  ungroup()

births4 <- births3 %>%
  filter(correctable_baby_upi != T) %>%
  bind_rows(births_corrected_baby_upis) %>%
  select(-c(multiple_baby_upis, correctable_baby_upi)) %>%
  arrange(mother_upi, baby_upi)


#### Some of our baby UPIs appear more than once. This makes it impossible to accurately link these babies, so for linkage purposes delete their numbers.
births5 <- births4 %>% 
  group_by(data_source, baby_upi) %>%
  mutate(duplicate_baby_upi = case_when( n() > 1 ~ T,     # If a baby UPI appears more than once in a single datasource then there must be a mistake. THe babies are real but one of the UPIs is wrong. 
                                         T ~ F)) %>%
  ungroup() %>%
  mutate(baby_upi = case_when(duplicate_baby_upi == T ~ paste0("72", str_pad(string = row_number(), width = 8, side = "left", pad = "0")), # If a baby UPI is a duplicate, assign a temporary UPI just until the next stage
                              T ~ baby_upi)) %>%
  select(-duplicate_baby_upi)


# This block reconciles all of our records into one row per baby as far as possible based on the data we've got, with the UUIDs of their relevant birth records
# Undoubtedly some babies are in here two or more times because their UPI is not consistently recorded across datasets.
births_grouped <- births5 %>%
  group_by(mother_upi, baby_upi) %>%
  mutate(conception_date = min(conception_date)) %>%
  mutate(event_date = min(event_date)) %>%
  ungroup() %>% group_by(baby_upi) %>%
  pivot_wider(names_from="data_source", values_from="event_id") %>%
  ungroup() %>%
  mutate(mother_upi = case_when(is.na(mother_upi) ~ paste0("82", str_pad(string = row_number(), width = 8, side = "left", pad = "0")),
                                T ~ mother_upi))

# As noted previously, some of our babies are in here more than once because their UPI may not be recorded consistently across datasets.
# However, for any pregnancy in SMR02 and NRS Live Births, we know how many babies should have been born. Therefore, we can cut our records down
# based on this and simply hope that as time goes on accurate UPIs are filled in and the inconsistencies are resolved.
num_of_births_smr02 <- data_smr02 %>% select(event_id, smr02_num_of_births_this_pregnancy)
num_of_births_nrslb <- data_nrs_live_births %>% select(event_id, nrslb_total_births_live_and_still)
num_of_births_nhslb <- data_nhs_live_births %>% select(event_id, nhslb_num_of_births)

births_limited <- births_grouped %>%
  filter(!is.na(mother_upi)) %>%
  left_join(num_of_births_smr02, by = c("smr02" = "event_id")) %>%
  left_join(num_of_births_nrslb, by = c("nrs_live_births" = "event_id")) %>%
  left_join(num_of_births_nhslb, by = c("nhs_live_births" = "event_id")) %>%
  rowwise() %>%
  mutate(number_of_births = max_(na.omit(c(smr02_num_of_births_this_pregnancy, nrslb_total_births_live_and_still, nhslb_num_of_births)))) %>%
  group_by(mother_upi, event_date) %>%
  mutate(number_of_births = max_(na.omit(number_of_births))) %>%
  mutate(observed_babies = n() ) %>%
  arrange(baby_upi, nhs_live_births, smr02) %>%
  mutate(excess_birth = case_when(row_number() > number_of_births ~ T,
                                  T ~ F)) %>%
  ungroup() %>%
  filter(excess_birth == F) %>%
  select(-c(observed_babies, excess_birth, smr02_num_of_births_this_pregnancy, nrslb_total_births_live_and_still, nhslb_num_of_births))

births6 <- births_grouped %>%
  filter(is.na(mother_upi)) %>%
  bind_rows(births_limited) %>%
  arrange(mother_upi, baby_upi) %>%
  rename(copsbirths_number_of_births = number_of_births) %>%
  rename(smr02_live_births = smr02) %>%
  rowwise() %>% mutate(baby_id = UUIDgenerate()) %>% ungroup()

write_rds(births6, paste0(folder_temp_data, "cops_births.rds"), compress = "gz")

rm(data_nhs_live_births, data_nrs_live_births, data_smr02, temp_nhs_live_births, temp_nrs_live_birth, temp_smr02_births,
   births1, births2, births3, births4, births5, births6, births_limited, births_grouped, births_corrected_baby_upis,
   births_corrected_mother_upis, num_of_births_nhslb, num_of_births_nrslb, num_of_births_smr02)