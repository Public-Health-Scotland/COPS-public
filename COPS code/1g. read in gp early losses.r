#### Read in new GP Early Losses ####
TRANSFER = dbConnect(odbc() ### DATABASE CONNECTION DETAILS REMOVED FOR PUBLIC RELEASE

data_gp_losses_temp_raw_1 <- dbGetQuery(TRANSFER,statement = 'SELECT DISTINCT * FROM COPS_DATA') %>%
    clean_names() %>% 
    filter(start_date >= cohort_start_date)

data_gp_losses_temp_raw_2 <- data_gp_losses_temp_raw_1 %>% 
    mutate(chi_number = chi_pad(as.character(chi_number))) %>% 
    mutate(validity = chi_check(chi_number)) %>% 
    filter(validity == "Valid CHI") %>% 
    select(-validity)


#### Keep records in previous file that have disappeared from new one ####
#Read in previous GP early losses file
data_gp_losses_old_1 <- readRDS(paste0(folder_temp_data, "gp_losses_raw.rds")) %>% clean_names()%>% 
  filter(start_date >= as.Date("2015-01-01")) %>% 
  mutate(chi_number = chi_pad(as.character(chi_number))) %>% 
  mutate(validity = chi_check(chi_number)) %>% 
  filter(validity == "Valid CHI") %>% 
  select(-validity)


#create a list of old start dates and CHIs
data_gp_losses_old_2 <- data_gp_losses_old_1 %>% 
  select(c(chi_number, start_date)) %>% 
  distinct() %>% 
  mutate(old_record = 1)    

#create a list of old start dates and CHIs
data_gp_losses_new_1 <- data_gp_losses_temp_raw_2 %>% 
  select(c(chi_number, start_date)) %>% 
  distinct() %>% 
  mutate(new_record = 1)  

#keep only those CHIs and start dates that do not exist in new file
data_gp_losses_lost <- left_join(data_gp_losses_old_2, data_gp_losses_new_1, by = c("chi_number" = "chi_number",
                                                                                     "start_date" = "start_date")) %>% 
  filter(is.na(new_record) & old_record == 1) %>% 
  select(-c(new_record, old_record)) %>% 
  mutate(keep = 1)

#fetch those relevant records from the old file
data_gp_losses_lost_records <- left_join(data_gp_losses_old_1, data_gp_losses_lost, by = c("chi_number" = "chi_number",
                                                                                           "start_date" = "start_date")) %>% 
  filter(keep == 1) %>% 
  select(-keep)

#add old records to new ones
data_gp_losses_temp_combined <- bind_rows(data_gp_losses_temp_raw_2, data_gp_losses_lost_records)

#write out full raw file for next time
write_rds(data_gp_losses_temp_combined, paste0(folder_temp_data, "gp_losses_raw.rds"))


#### Clean data ####

data_gp_losses_temp_1 <- data_gp_losses_temp_combined  %>% 
    mutate(age = floor(interval( ymd(date_of_birth),ymd(start_date))/years(1) ))

data_gp_losses_temp_2 <- data_gp_losses_temp_1 %>% filter(age %in% feasible_age) 


#record number filtered out
filter_1 <- data.frame(stage = 1,
                       den = nrow(data_gp_losses_temp_raw_1),
                       num = nrow(data_gp_losses_temp_raw_2),
                       task = "Filter out invalid CHIs")

filter_2 <- data.frame(stage = 2,
                       den = nrow(data_gp_losses_temp_raw_2),
                       num = nrow(data_gp_losses_temp_combined),
                       task = "Add in lost records")

filter_3 <- data.frame(stage = 3,
                       den = nrow(data_gp_losses_temp_1),
                       num = nrow(data_gp_losses_temp_2),
                       task = "Filter out those under 10 or over 55")

data_gp_losses_temp_3 <- data_gp_losses_temp_2 %>%
  mutate(miscarriage = if_else(read_code %in% read_miscarriage$code, 1, 0)) %>% 
  mutate(ectopic_pregnancy = if_else(read_code %in% read_ectopic$code, 1, 0)) %>% 
  mutate(molar_pregnancy = if_else(read_code %in% read_molar$code, 1, 0)) %>% 
  filter(sex == "F") %>%
  select(chi_number, date_of_birth, start_date, read_code, miscarriage, ectopic_pregnancy, molar_pregnancy) 

#record number filtered out
filter_4 <- data.frame(stage = 4,
                       den = nrow(data_gp_losses_temp_2),
                       num = nrow(data_gp_losses_temp_3),
                       task = "Filter out males")
#Remove BB codes
data_gp_losses_temp_4 <- data_gp_losses_temp_3 %>%
  filter(str_starts(read_code,"BB", negate = TRUE))

#record number filtered out
filter_5 <- data.frame(stage = 5,
                       den = nrow(data_gp_losses_temp_3),
                       num = nrow(data_gp_losses_temp_4),
                       task = "Remove BB codes")

#Remove irrelevant codes
data_gp_losses_temp_5 <- data_gp_losses_temp_4 %>%
  filter (miscarriage == 1 | ectopic_pregnancy == 1 | molar_pregnancy == 1) %>% 
  mutate(outcome_type = case_when(ectopic_pregnancy == 1 ~ "Ectopic pregnancy",
                                  molar_pregnancy == 1 ~ "Molar pregnancy",
                                  miscarriage == 1 ~ "Miscarriage")) %>% 
  select(-c(miscarriage, ectopic_pregnancy, molar_pregnancy))
 
#record number filtered out
filter_6 <- data.frame(stage = 6,
                      den = nrow(data_gp_losses_temp_4),
                      num = nrow(data_gp_losses_temp_5),
                      task = "Filter out irrelevant codes")

data_gp_losses_temp_6 <- 
  moving_index_deduplication(data_gp_losses_temp_5, chi_number, start_date, dedupe_period)

data_gp_losses <- data_gp_losses_temp_6 %>%
  group_by(chi_number, cops_event) %>%
  mutate(outcome_type = case_when("Ectopic pregnancy" %in% outcome_type ~ "Ectopic pregnancy",
                                  "Molar pregnancy" %in% outcome_type ~ "Molar pregnancy",
                                  "Miscarriage" %in% outcome_type ~ "Miscarriage")) %>%
  mutate(
    start_date = min(start_date),
    date_of_birth = first(date_of_birth)
  ) %>%
  distinct() %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>%
  pivot_wider(id_cols = c(chi_number, cops_event, start_date, date_of_birth, outcome_type),
              names_from = row,
              names_prefix = "read_code_",
              values_from = read_code) %>% 
  mutate(gestation = case_when(outcome_type == "Ectopic pregnancy" ~ assumed_gestation_ectopic,
                               outcome_type == "Molar pregnancy" |
                               outcome_type == "Miscarriage" ~ assumed_gestation_miscarriage)) %>%
  mutate(estimated_conception_date = start_date - (weeks(gestation) - weeks(2) )) %>%
  select(-cops_event) %>%  
  rename_with( ~ paste0("gp_losses_", .)) %>%
  mutate(gp_loss = T) %>%
  rowwise() %>% mutate(event_id = UUIDgenerate()) %>% ungroup()

#record number filtered out
filter_7 <- data.frame(stage = 7,
                       den = nrow(data_gp_losses_temp_4),
                       num = nrow(data_gp_losses),
                       task = "Combine into COPS events")


write_rds(data_gp_losses, paste0(folder_temp_data, "gp_losses.rds"), compress = "gz")

#number filtered out
gp_losses_filters <- bind_rows(filter_1, filter_2, filter_3, filter_4, filter_5, filter_6, filter_7) %>% 
  mutate(dataset ="GP losses")
write_rds(gp_losses_filters, paste0(folder_temp_data, "gp_losses_filters.rds"), compress = "gz")

#dates
dataset_dates("GP losses", data_gp_losses$gp_losses_start_date)

rm(filter_1, filter_2, filter_3, filter_4, filter_5, filter_6, filter_7, data_gp_losses_temp_1, 
   data_gp_losses_temp_2, data_gp_losses_temp_3, gp_losses_filters,
   data_gp_losses_temp_4, data_gp_losses_temp_5, data_gp_losses_temp_6,
   data_gp_losses_temp_raw_1, data_gp_losses_temp_raw_2, data_gp_losses_lost_records, data_gp_losses_temp_combined,
   data_gp_losses_lost, data_gp_losses_new_1, data_gp_losses_old_1, data_gp_losses_old_2)
rm(data_gp_losses)
