

data_smr01               <- read_rds(paste0(folder_temp_data, "smr01.rds"))
data_smr02               <- read_rds(paste0(folder_temp_data, "smr02.rds"))
data_cops_births         <- read_rds(paste0(folder_temp_data, "cops_births.rds")) # Including all births for now, will remove all those not on the spine of NHS records later
data_aas                 <- read_rds(paste0(folder_temp_data, "aas.rds"))
data_nrs_stillbirths     <- read_rds(paste0(folder_temp_data, "nrs_stillbirths.rds"))
data_gp_losses           <- read_rds(paste0(folder_temp_data, "gp_losses.rds"))
data_an_booking          <- read_rds(paste0(folder_temp_data, "antenatal_booking.rds"))

#### Create a pregnancy df ####
temp_smr01_events       <- data_smr01 %>% select(smr01_mother_upi_number, smr01_outcome_type, smr01_cops_admission_date, smr01_estimated_conception_date, event_id) %>% mutate(data_source = "smr01")
temp_smr02_events       <- data_smr02 %>% select(smr02_upi_number, smr02_outcome_type, smr02_pregnancy_end_date, smr02_estimated_conception_date, event_id) %>% mutate(data_source = "smr02") %>% filter(smr02_outcome_type != "Live birth") #SMR02 live births are already capture in COPS Births
temp_copsbirths_events  <- data_cops_births %>% select(mother_upi, event_date, conception_date, baby_id, nhs_live_births) %>% mutate(data_source = "cops_births", outcome_type = "Live birth")
temp_aas_events         <- data_aas %>% select(aas_upi_number, aas_outcome_type, aas_date_of_termination, aas_estimated_date_of_conception, event_id) %>% mutate(data_source = "aas_termination")
temp_nrssb_events       <- data_nrs_stillbirths %>% select(nrssb_mother_upi_number, nrssb_outcome_type, nrssb_date_of_birth, nrssb_estimated_date_of_conception, event_id) %>% mutate(data_source = "nrs_stillbirths")
temp_gplosses_events    <- data_gp_losses %>% select(gp_losses_chi_number, gp_losses_outcome_type, gp_losses_start_date, gp_losses_estimated_conception_date, event_id) %>% mutate(data_source = "gp_losses")
temp_anbooking_events   <- data_an_booking %>% select(anbooking_upi, anbooking_event_type, anbooking_booking_date, anbooking_estimated_conception_date, event_id) %>% mutate(data_source = "antenatal_booking")

colnames(temp_smr01_events)      <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_smr02_events)      <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_copsbirths_events) <-  c("mother_chi", "event_date","conception_date", "event_id", "nhs_live_births", "data_source", "event_type")
colnames(temp_aas_events)        <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_nrssb_events)      <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_gplosses_events)   <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")
colnames(temp_anbooking_events)  <-  c("mother_chi", "event_type", "event_date", "conception_date", "event_id", "data_source")


pregnancies1 <- bind_rows(temp_smr01_events, 
                         temp_smr02_events, 
                         temp_copsbirths_events,
                         temp_aas_events, 
                         temp_nrssb_events ,
                         temp_gplosses_events,
                         temp_anbooking_events
                         ) %>%
  arrange(mother_chi, conception_date, event_date)

#### Replace mother CHI with UPI ####
chi_to_upi <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      "
      SELECT CHI_NUMBER as mother_chi, UPI_NUMBER as mother_upi
      FROM UPIP.L_UPI_DATA")
  )) %>%
  clean_names() %>%
  group_by(mother_chi) %>%
  slice(1) %>%
  ungroup()

pregnancies1 %<>%
  left_join(chi_to_upi)

pregnancies1 %>%
  select(mother_chi, mother_upi) %>%
  mutate(chi_matches_upi = mother_chi == mother_upi) %>%
  tabyl(chi_matches_upi)

pregnancies1 %<>%
  select(-mother_chi)

rm(chi_to_upi)

pregnancies1 %>%
  mutate(validity = chi_check(mother_upi)) %>%
  tabyl(validity)

#### Add dummy UPI for any woman without a number ####
pregnancies1 %<>%
  mutate(mother_upi = if_else(is.na(mother_upi), paste0( # For the purposes of grouping pregnancies, it's useful for each woman to have a UPI
    "82",
    str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )
  ),
  mother_upi)) 


#### Group by event date ####
# Assign each event to a single pregnancy event. This allows us to group events which seem to be related and which probably should belong to the same pregnancy. It helps us overcome any issues with innacurate conception dates. 
pregnancies2 <- 
  moving_index_deduplication(pregnancies1, mother_upi, event_date, dedupe_period)

pregnancies3 <- pregnancies2 %>%
  group_by(mother_upi, cops_event) %>%
  mutate(revised_conception_date = min(conception_date)) %>%
  ungroup() 

#### Group by conception date ####
pregnancies3 <- 
  moving_index_deduplication(pregnancies3, mother_upi, revised_conception_date, dedupe_period)

pregnancies4 <- pregnancies3 %>% 
  select(-c(revised_conception_date)) %>% # This variable was based only on grouping by event date - pregnancy_start_date takes into account our final pregnancy groupings
  rename(pregnancy = cops_event)
  
#### Determine the start and end date of each pregnancy and check that bookings are grouped correctly ####
pregnancies5 <- pregnancies4 %>%
  group_by(mother_upi, pregnancy) %>%
  mutate(pregnancy_start_date = min_(conception_date)) %>%
  mutate(pregnancy_end_date = max_(event_date)) %>%
  ungroup() %>% 
  arrange(mother_upi, event_date) %>% 
  mutate(pregnancy = case_when(event_type == "booking" & 
                                  mother_upi == lag(mother_upi) &
                                  event_date >= lag(pregnancy_start_date) & 
                                  event_date <= lag(pregnancy_end_date) ~ lag(pregnancy),
                                  event_type == "booking" & 
                                  mother_upi == lead(mother_upi) &
                                  event_date >= lead(pregnancy_start_date) & 
                                  event_date <= lead(pregnancy_end_date) ~ lead(pregnancy),
                                  T ~ pregnancy)) %>% 
  group_by(mother_upi, pregnancy) %>%
  mutate(pregnancy_id = UUIDgenerate()) %>% 
  ungroup()

write_rds(pregnancies5, paste0(folder_temp_data, "temp_pregnancies.rds"))
rm(pregnancies1, pregnancies2, pregnancies3, pregnancies4, pregnancies5)
#pick up point
pregnancies5 <- read_rds(paste0(folder_temp_data, "temp_pregnancies.rds"))
pregnancies6 <- pregnancies5 %>% filter(event_date <= Sys.Date()) # Delete any records with events which occur in the future

#### Fix terminations or early losses that happen soon after a live birth and have been grouped with live birth ####
pregnancies6 <- pregnancies6 %>% 
  mutate(birth_date = if_else(event_type == "Live birth" | event_type ==  "Stillbirth", event_date, as.POSIXct("1970-01-01"))) %>% 
  group_by(pregnancy_id) %>% 
  mutate(birth_date = max_(birth_date)) %>% 
  mutate(termination_loss_birth = if_else(("Live birth" %in% event_type | "Stillbirth" %in% event_type) & 
                                            ("Termination" %in% event_type | "Molar pregnancy" %in% event_type | "Ectopic pregnancy" %in% event_type | "Miscarriage" %in% event_type), 1, 0)) %>% 
  ungroup() %>% 
  mutate(post_preg_termination_loss = if_else(termination_loss_birth == 1 & (event_type == "Termination" | event_type == "Molar pregnancy" | event_type == "Ectopic pregnancy" | event_type == "Miscarriage") 
                                              & difftime(birth_date, event_date, units = "days") < - 30, 1, 0)) %>% 
  rowwise() %>% 
  mutate(pregnancy_id = if_else(post_preg_termination_loss == 1, UUIDgenerate(), pregnancy_id)) %>% 
  ungroup() %>% 
  group_by(pregnancy_id) %>% 
  mutate(pregnancy_start_date = min_(conception_date)) %>%
  mutate(pregnancy_end_date = max_(event_date)) %>% 
  ungroup() %>% 
  select(-c(birth_date, termination_loss_birth, post_preg_termination_loss))

#### Figure out how many outcomes each pregnancy should have had ####
temp_livebirths_num_of_outcomes    <- data_cops_births %>% select(baby_id, copsbirths_number_of_births) %>% rename(number_of_outcomes = copsbirths_number_of_births) %>% rename(event_id = baby_id)
temp_stillbirths_num_of_outcomes   <- data_nrs_stillbirths %>% select(event_id, nrssb_total_births_live_and_still) %>% rename(number_of_outcomes = nrssb_total_births_live_and_still)
temp_aas_num_of_outcomes           <- data_aas %>% select(event_id, aas_original_number_of_foetuses) %>% rename(number_of_outcomes = aas_original_number_of_foetuses) %>% mutate(number_of_outcomes = case_when(is.na(number_of_outcomes) ~ 1, T ~ number_of_outcomes))
temp_smr02_num_of_outcomes         <- data_smr02 %>% select(event_id, smr02_num_of_outcomes_this_pregnancy) %>% rename(number_of_outcomes = smr02_num_of_outcomes_this_pregnancy)

temp_num_of_outcomes <- temp_livebirths_num_of_outcomes %>%
  bind_rows(temp_stillbirths_num_of_outcomes) %>%
  bind_rows(temp_aas_num_of_outcomes) %>%
  bind_rows(temp_smr02_num_of_outcomes)

pregnancies_an_booking <- pregnancies6 %>% # Store booking data separately and match it on later
  filter(event_type == "booking")

pregnancies7 <- pregnancies6 %>%
  filter(event_type != "booking") %>%
  left_join(temp_num_of_outcomes, by="event_id") %>%
  mutate(number_of_outcomes = case_when(is.na(number_of_outcomes) ~ 1,
                                        T ~ number_of_outcomes)) %>%
  group_by(mother_upi, pregnancy) %>%
  mutate(number_of_outcomes = max(number_of_outcomes)) %>%
  ungroup()

pregnancies_singleton1 <- pregnancies7 %>% filter(number_of_outcomes == 1)
pregnancies_multiple1  <- pregnancies7 %>% filter(number_of_outcomes >  1)

pregnancies_singleton1 <- pregnancies_singleton1 %>%
  group_by(pregnancy_id) %>%
  mutate(exceptional_singleton = case_when(n() > 1 ~ T,
                                           T ~ F)) %>%
  ungroup()

pregnancies_singleton_exceptional <- pregnancies_singleton1 %>%
  filter(exceptional_singleton == T) %>%
  group_by(pregnancy_id) %>%
  mutate(outcome = case_when("Termination" %in% event_type ~ "Termination",
                             "Live birth" %in% event_type ~ "Live birth",
                             "Stillbirth" %in% event_type ~ "Stillbirth",
                             "Ectopic pregnancy" %in% event_type ~ "Ectopic pregnancy",
                             "Molar pregnancy" %in% event_type ~ "Molar pregnancy",
                             "Miscarriage" %in% event_type ~ "Miscarriage")) %>%
  arrange(nhs_live_births) %>% # Make sure records with an NHS Live Births record come first
  mutate(nhs_live_births = as.character(max_(nhs_live_births))) %>% 
  select(c(mother_upi, event_id, data_source, pregnancy_id, pregnancy_start_date, pregnancy_end_date, outcome, nhs_live_births)) %>%
  group_by(pregnancy_id, data_source) %>%
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  ungroup() %>%
  pivot_wider(values_from = event_id, names_from = data_source)

pregnancies_singleton2 <- pregnancies_singleton1 %>%
  filter(exceptional_singleton == F) %>%
  rename(outcome = event_type) %>%
  select(c(mother_upi, event_id, data_source, pregnancy_id, pregnancy_start_date, pregnancy_end_date, outcome, nhs_live_births)) %>%
  group_by(pregnancy_id, data_source) %>%
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  ungroup() %>%
  pivot_wider(values_from = event_id, names_from=data_source) %>%
  bind_rows(pregnancies_singleton_exceptional)
#Singletons are now done!
  
# Identify multiple pregnancies where we have more outcomes than expected
pregnancies_multiple1 <- pregnancies_multiple1 %>%
  group_by(pregnancy_id) %>%
  mutate(exceptional_multiple = case_when(n() > number_of_outcomes ~ T,
                                          T ~ F)) %>%
  ungroup()

pregnancies_multiple_non_exceptional <- pregnancies_multiple1 %>%
  filter(exceptional_multiple == F) %>%
  group_by(pregnancy_id) %>%
  mutate(outcome_no = row_number()) %>%
  ungroup() %>%
  group_by(event_id) %>%
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  ungroup() %>%
  pivot_wider(values_from = event_id, names_from=data_source) %>%
  rename(outcome = event_type) %>%
  select(-c(pregnancy, number_of_outcomes, exceptional_multiple, outcome_no, event_date, conception_date)) %>%
  select(mother_upi, pregnancy_id, pregnancy_start_date, pregnancy_end_date, outcome, everything())
#Non-exceptional multiples are now done. Time to tackle the exceptional multiples, then bind all the rows together for a final outcome file. 

#### Create an interim record of resolved pregnancies ####
#i.e. pregnancies where we're happy we've got the outcomes sorted
pregnancies_all_outcomes <- pregnancies_singleton2 %>%
  bind_rows(pregnancies_multiple_non_exceptional)


#### Resolve exceptional multiples ####
pregnancies_multiple_exceptional <- pregnancies_multiple1 %>%
  filter(exceptional_multiple == T)


#Some of our stillborn babies appear in both SMR02 and NRS Stillbirths. Combine so we have one row per stillborn baby. 
temp_stillbirths <- pregnancies_multiple_exceptional %>%
  filter(event_type == "Stillbirth") %>%
  left_join(data_smr02) %>%
  left_join(data_nrs_stillbirths) %>%
  rowwise() %>%
  mutate(baby_weight = first(na.omit(c(nrssb_weight_of_foetus, smr02_birthweight)))) %>%
  mutate(baby_sex = first(na.omit(c(nrssb_sex, smr02_sex)))) %>%
  #select(c(mother_upi:exceptional_multiple), baby_weight, baby_sex) %>%  # This line seems to be causing errors
  arrange(mother_upi, pregnancy_id, baby_sex, baby_weight) %>%
  group_by(pregnancy_id) %>%
  mutate(different_baby = case_when( row_number() == 1 ~ T,
                                     data_source == lag(data_source) ~ T,
                                     baby_sex == lag(baby_sex) & baby_weight == lag(baby_weight) ~ F,
                                     n() == 2 & "smr02" %in% data_source & "nrs_stillbirths" %in% data_source ~ F,
                                     T ~ T
                                     )) %>%
  mutate(baby_no = 0 + cumsum(different_baby)) %>%
  select(mother_upi, pregnancy_id, baby_no, pregnancy_start_date, pregnancy_end_date, event_type, data_source, event_id, nhs_live_births) %>%
  group_by(pregnancy_id, baby_no, data_source) %>%
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  group_by(pregnancy_id, baby_no) %>%
  pivot_wider(names_from = data_source, values_from = event_id) %>%
  ungroup() %>%
  select(-baby_no) %>%
  rename(outcome = event_type)
  
  
# Deal with threatened early losses - i.e. pregnancies with an SMR01 or GP Loss early on, but which then goes on to produce a live birth
temp_threatened_early_losses <- pregnancies_multiple_exceptional %>%
  group_by(pregnancy_id) %>%
  filter("Live birth" %in% event_type & ("Ectopic pregnancy" %in% event_type | 
                                           "Molar pregnancy" %in% event_type | 
                                           "Miscarriage" %in% event_type)) %>%
  ungroup()

temp_threatened_early_losses_loss_records <- temp_threatened_early_losses %>%
  filter(event_type == "Ectopic pregnancy" |
         event_type == "Molar pregnancy" |
         event_type == "Miscarriage") %>%
  select(pregnancy_id, data_source, event_id) %>%
  group_by(pregnancy_id) %>%
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  pivot_wider(names_from = data_source, values_from = event_id) %>%
  ungroup()

final_threatened_early_losses <- temp_threatened_early_losses %>% # This df can be added on to pregnancies_all_outcomes
  filter(event_type != "Ectopic pregnancy" &
           event_type != "Molar pregnancy" &  
           event_type != "Miscarriage" ) %>%
  select(mother_upi, pregnancy_id, pregnancy_start_date, pregnancy_end_date, event_type, event_id, nhs_live_births) %>%
  rename(outcome = event_type, cops_births_1 = event_id) %>%
  left_join(temp_threatened_early_losses_loss_records, by = "pregnancy_id")


#Finally, deal with the remaining exceptional multiples
temp_remaining_exceptional_multiples <- pregnancies_multiple_exceptional %>%
  filter(event_type != "Stillbirth") %>%
  group_by(pregnancy_id) %>%
  mutate(exclude = case_when("Live birth" %in% event_type & ("Ectopic pregnancy" %in% event_type | 
                                                               "Molar pregnancy" %in% event_type | 
                                                               "Miscarriage" %in% event_type) ~ T,
                             T ~F)) %>%
  filter(exclude == F) %>% select(-exclude) %>%
  select(mother_upi, pregnancy_id, pregnancy_start_date, pregnancy_end_date, event_type, event_id, data_source, nhs_live_births) %>%
  mutate(data_source = paste0(data_source, "_1")) %>%
  mutate(event = row_number()) %>% #Create a dummy event ID variable so that each event is pivoted individually
  pivot_wider(names_from = data_source, values_from = event_id) %>%
  select(-c(event)) %>%
  ungroup() %>%
  rename(outcome = event_type)

# Add on our remaining outcomes
pregnancies_all_outcomes <- pregnancies_all_outcomes %>%
  bind_rows(temp_stillbirths) %>%
  bind_rows(final_threatened_early_losses) %>%
  bind_rows(temp_remaining_exceptional_multiples)




#### Resolve live birth discrepancies ####
# We have a small number of cases where live births have been incorrectly pivoted. Each baby should occupy a single row, and there should not be two live babies on one row.
# This issue arises especially when a birth occurs close to midnight, with one baby being born on one day and another on the next day.
#  APRIL 2022 - Why did we decide to favour the COPS Births 2 over COPS Births 1 in these situation? A change has now been made to favour whatever COPS Births
#  record hasd NHS Live Births on it - but we need to revisit this. 
#
# temp_second_babies <- pregnancies_all_outcomes %>%
#   filter(!is.na(cops_births_2)) %>%
#   select(-cops_births_1) %>%
#   rename(cops_births_1 = cops_births_2)
# 
# pregnancies_all_outcomes <- pregnancies_all_outcomes %>%
#   filter(is.na(cops_births_2)) %>%
#   select(-cops_births_2) %>%
#   bind_rows(temp_second_babies)


#Pivot antenatal bookings to one row per pregnancy
pregnancies_an_booking_wide <- pregnancies_an_booking %>%
  select(mother_upi, pregnancy_id, event_id, data_source, pregnancy_start_date) %>%
  rename(an_mother_upi = mother_upi, an_pregnancy_start_date = pregnancy_start_date) %>%
  group_by(pregnancy_id) %>%
  mutate(an_pregnancy_start_date = min(an_pregnancy_start_date)) %>% # We need to ensure that each pregnancy has a consistent AN Booking conception date, otherwise we'll end up with multiple lines per pregnancy after the pivot
  mutate(data_source = paste0(data_source, "_", row_number())) %>%
  pivot_wider(names_from = data_source, values_from = event_id)

#Add antenatal booking back on to our main df
pregnancies_all_outcomes_and_ongoing <- pregnancies_all_outcomes %>%
  full_join(pregnancies_an_booking_wide, by = "pregnancy_id") %>%
  mutate(mother_upi = case_when(is.na(mother_upi) ~ an_mother_upi,
                                T ~ mother_upi)) %>%
  mutate(pregnancy_start_date = case_when(is.na(pregnancy_start_date) ~ an_pregnancy_start_date,
                                          T ~ pregnancy_start_date)) %>%
  mutate(outcome = case_when(is.na(outcome) & !is.na(antenatal_booking_1) ~ "Ongoing",
                             T ~ outcome)) %>%
  select(-c(an_mother_upi, an_pregnancy_start_date))

#### Drop any babies which are not observed on NHS Live Births ####
pregnancies_nhs_spine <- pregnancies_all_outcomes_and_ongoing %>% 
  filter(outcome != "Live birth" | (outcome == "Live birth" & !is.na(nhs_live_births))) %>% 
  select(-nhs_live_births)

#Save out final pregnancy record
write_rds(pregnancies_nhs_spine, paste0(folder_temp_data, "script3_pregnancy_record.rds"), compress = "gz")

rm(data_aas, data_an_booking, data_cops_births, data_gp_losses, data_nrs_stillbirths, data_smr01, data_smr02, final_threatened_early_losses,
   pregnancies_all_outcomes, pregnancies_an_booking, pregnancies_an_booking_wide, pregnancies_multiple1,
   pregnancies_singleton1, pregnancies_singleton2, pregnancies5, pregnancies6, pregnancies7, temp_aas_events, temp_aas_num_of_outcomes,
   temp_anbooking_events, temp_copsbirths_events, temp_gplosses_events, temp_nrssb_events, temp_num_of_outcomes, pregnancies_multiple_exceptional,
   pregnancies_multiple_non_exceptional, pregnancies_singleton_exceptional, temp_livebirths_num_of_outcomes, temp_remaining_exceptional_multiples,
   temp_second_babies, temp_stillbirths, temp_threatened_early_losses_loss_records, temp_threatened_early_losses,
   temp_smr01_events, temp_smr02_events, temp_smr02_num_of_outcomes, temp_stillbirths_num_of_outcomes, pregnancies_all_outcomes_and_ongoing,
   pregnancies_nhs_spine)

