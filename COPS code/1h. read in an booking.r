#### Read in Antenatal Bookings ####
data_an_booking_temp_1 <-
  read_csv(
    paste0(
      folder_data,
      "network_folder/COPS_Antenatalextract_25082022.csv"
    ),
    col_types = cols(
      .default = "?",
      chi = col_character(),
 #     upi = col_character(),
      # BookingDate = col_datetime(format =
      #                              "%m/%d/%Y"),
      # MothersDob = col_datetime(format =
      #                              "%m/%d/%Y"),
      # LMP = col_datetime(format =
      #                      "%m/%d/%Y"),
      GestationatBooking = col_number(),
      Ethnicity = col_character()
    )
  ) %>%
  mutate(BookingDate = as.Date(BookingDate)) %>%
  filter(BookingDate >= cohort_start_date) %>% 
  mutate(MothersDob = as.Date(MothersDob)) %>%
  mutate(LMP = as.Date(LMP)) %>%
  clean_names() %>%
  mutate(#upi = chi_pad(upi),
         chi = chi_pad(chi)) %>% 
 # mutate(upi = case_when(!is.na(upi) ~ upi,
#                         is.na(upi) ~ chi)) %>% 
mutate(upi = chi) %>% # because there is no UPI in march extract
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
  mutate(anbooking_smoking_status = if_else(anbooking_smoking_status == 9, NA_real_, anbooking_smoking_status))

data_an_booking_temp_2 <- data_an_booking_temp_1 %>%
  distinct()

data_an_booking_temp_2 <- 
  moving_index_deduplication(data_an_booking_temp_2, anbooking_upi, anbooking_booking_date, dedupe_period)

data_an_booking_temp_2 <- data_an_booking_temp_2 %>%
  group_by(anbooking_upi, cops_event) %>%
  mutate(revised_conception_date = min(anbooking_estimated_conception_date))

data_an_booking_temp_2 <- 
  moving_index_deduplication(data_an_booking_temp_2, anbooking_upi, revised_conception_date, dedupe_period)



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

write_rds(data_an_booking, paste0(folder_temp_data, "antenatal_booking.rds"), compress = "gz")

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


