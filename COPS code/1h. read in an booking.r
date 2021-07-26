#### Read in Antenatal Bookings ####
data_an_booking_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                          ### REMOVED FOR PUBLIC RELEASE
  mutate(BookingDate = as.Date(BookingDate)) %>%
  mutate(MothersDob = as.Date(MothersDob)) %>%
  mutate(LMP = as.Date(LMP)) %>%
  clean_names() %>%
  mutate(upi = chi_pad(upi),
         chi = chi_pad(chi)) %>% 
  mutate(upi = case_when(!is.na(upi) ~ upi,
                         is.na(upi) ~ chi)) %>% 
  mutate(validity = chi_check(upi)) %>% 
  mutate(upi = case_when(validity == "Valid CHI" ~ upi,
                         T ~ NA_character_)) %>% 
  select(-c(chi, hb2019name, smoking_status_name, validity)) %>%
  left_join(hb_lookup, by =  c("hb2019" = "healthboard_code")) %>% 
  mutate(hb2019 = healthboard) %>% 
  select(-healthboard) %>% 
  select(upi, everything()) %>%
  mutate(feasible_gestation = case_when(gestationat_booking %in% feasible_gestation_booking ~ T,
                                        T ~ F)) %>% 
  mutate(gestationat_booking = case_when(feasible_gestation == T ~ gestationat_booking,
                                         feasible_gestation == F & !is.na(lmp)
                                         ~ as.numeric(floor(difftime(booking_date, lmp, units = "weeks"))),
                                         T ~ NA_real_)) %>% 
  mutate(feasible_gestation = case_when(gestationat_booking %in% feasible_gestation_booking ~ T,
                                        T ~ F)) %>% 
  mutate(assumed_gestation = case_when(feasible_gestation == F ~ T,
                                       T ~ F)) %>% 
  mutate(gestationat_booking = case_when(feasible_gestation == T ~ gestationat_booking,
                                         feasible_gestation == F ~ assumed_gestation_booking
                                         )) %>% 
  mutate(upi = case_when(
    is.na(upi) ~ paste0("47", str_pad(
      string = row_number(),
      width = 8,
      side = "left",
      pad = "0"
    )),
    T ~ str_pad( # For some reason, our latest data dump is missing leading 0s, so add them back in
      string = upi,
      width = 10,
      side = "left",
      pad = "0"
    ))) %>%
  mutate(estimated_conception_date = booking_date - (weeks(gestationat_booking) - weeks(2) )) %>%
  mutate(event_type = "booking") %>%
  select(booking_date, everything()) %>%
  rename_with( ~ paste0("anbooking_", .)) %>% 
  replace_with_na_at(.vars = c("anbooking_smoking_status"),
                     condition = ~.x == 9)

data_an_booking_temp_2 <- data_an_booking_temp_1 %>%
  distinct() %>%
  arrange(anbooking_upi, anbooking_booking_date) %>%
  mutate(cops_event = 1)


# Assign each event to a single pregnancy event. This allows us to group events which seem to be related and which probably should belong to the same pregnancy. It helps us overcome any issues with innacurate conception dates. 
repeat {
  data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
    group_by(anbooking_upi, cops_event) %>%
    mutate(index_date = first(anbooking_booking_date)) %>% 
    mutate(days_since_index_event = difftime(anbooking_booking_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                  T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_an_booking_temp_2$days_since_index_event))
  
  if (max(data_an_booking_temp_2$days_since_index_event) <= dedupe_period) {
    data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 83 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}

data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
  group_by(anbooking_upi, cops_event) %>%
  mutate(revised_conception_date = min(anbooking_estimated_conception_date)) %>%
  ungroup() %>%
  arrange(anbooking_upi, revised_conception_date) %>%
  mutate(cops_event = 1)

repeat {
  data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
    group_by(anbooking_upi, cops_event) %>%
    mutate(index_date = first(revised_conception_date)) %>% # The first observed conception date for a woman becomes our initial index date, and then changes on every iteration to the first conception date which occurs > 83 days after the previous index date.
    mutate(days_since_index_event = difftime(revised_conception_date, index_date, units = "days")) %>%
    mutate(cops_event = case_when(days_since_index_event > dedupe_period ~ cops_event + 1,
                                  T ~ cops_event)) %>%
    ungroup()
  
  print(Sys.time())
  print("Max days since index event:")
  print(max(data_an_booking_temp_2$days_since_index_event))
  
  if (max(data_an_booking_temp_2$days_since_index_event) <= dedupe_period) {
    data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
      select(-c(index_date, days_since_index_event))
    break # If no records take place more than 82 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
  }
  print("Running another loop...")
}


data_an_booking <- data_an_booking_temp_2 %>%
  group_by(anbooking_upi, cops_event) %>%
  arrange(anbooking_assumed_gestation, anbooking_booking_date) %>% 
  mutate(anbooking_mothers_dob= first_(anbooking_mothers_dob), 
          anbooking_simd2020v2_sc_quintile= first_(anbooking_simd2020v2_sc_quintile),
          anbooking_hb2019= first_(anbooking_hb2019),
          anbooking_lmp= first_(anbooking_lmp),
          anbooking_smoking_status= first_(anbooking_smoking_status)) %>% 
  slice(1) %>%
  ungroup() %>%
  select(-c(cops_event, revised_conception_date, anbooking_feasible_gestation)) %>%
  mutate(anbooking = T) %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

write_rds(data_an_booking, paste0(folder_temp_data, "antenatal_booking.rds"))

#dates
dataset_dates("AN booking", data_an_booking$anbooking_booking_date)

#record number filtered out
an_booking_filters <- data.frame(stage = 1,
                          den = nrow(data_an_booking_temp_1),
                          num = nrow(data_an_booking),
                          task = "Combine into COPS events") %>% 
  mutate(dataset ="AN booking")

write_rds(an_booking_filters, paste0(folder_temp_data, "an_booking_filters.rds"))

rm(data_an_booking_temp_1, data_an_booking_temp_2, an_booking_filters)
rm(data_an_booking)
