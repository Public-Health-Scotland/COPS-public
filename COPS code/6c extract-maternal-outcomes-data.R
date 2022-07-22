library(haven) # required to process SICSAG SPSS data

start_date <-  dmy("01/05/2019") 
end_date <- today() 



# DIAGNOSIS CODES ####
hypertensive_codes <- c("O11", 
                        "O13",
                        "O140", "O141", "O142", "O149",
                        "O150", "O151", "O152", "O159")

vte_codes <- c("I260", "I269",
               "I801", "I802", "I803",
               "O082",
               "O223",
               "O871",
               "O882",
               "I808", "I809",
               "I81",
               "I820", "I821", "I822", "I823", "I828", "I829",
               "O229",
               "O879",
               "G08",
               "I636",
               "I676",
               "O225",
               "O873")

acute_covid19_codes <- c("U071", "U072", "U075")
history_of_covid19_codes <- c("U073", "U074")
adverse_covid19_vax_event_codes <- c("U077")

all_codes_list <- list(hypertensive_codes = hypertensive_codes,
                       vte_codes = vte_codes,
                       acute_covid19_codes = acute_covid19_codes,
                       history_of_covid19_codes = history_of_covid19_codes,
                       adverse_covid19_vax_event_codes = adverse_covid19_vax_event_codes)

all_codes <- as.character(unlist(all_codes_list))




# MOTHER UPIS ####
# list of mother UPIs to extract data for
mother_upis <- read_rds(file.path(folder_temp_data, "script6b_pregnancy_level_record.rds")) %>%
  select(mother_upi, chi_validity) %>%
  filter(chi_validity == "Valid CHI") %>% 
  select(-chi_validity) %>% 
  distinct() %>%
  rename(UPI_NUMBER = mother_upi)

write_rds(mother_upis, file.path(folder_temp_data, "mother_upis.rds"), compress = "gz")
mother_upis <- read_rds(file.path(folder_temp_data, "mother_upis.rds"))




# SMR01 ####
clear_temp_tables(SMRAConnection)
SMR01 <- SMRAConnection %>% tbl("SMR01_PI") # virtual table of all SMR01 episodes

extract_start_date <- (start_date - months(6)) %>% format("%d/%m/%Y") # CIS stays typically constructed from episodes up to six months prior
extract_end_date <- end_date %>% format("%d/%m/%Y")

# extract all SMR01 episodes of interest (e.g. those in date range with matching mother UPI number) sorted into CIS order
df_eps <- SMR01 %>%
  filter(DISCHARGE_DATE >= TO_DATE(extract_start_date, "dd/mm/yyyy"),
         DISCHARGE_DATE <= TO_DATE(extract_end_date, "dd/mm/yyyy")) %>%
  inner_join(mother_upis, copy = TRUE) %>% 
  arrange(LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI) %>% 
  select(UPI_NUMBER, LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, 
         # ADMISSION, DISCHARGE, URI, 
         MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5) %>% 
  collect() %>% 
  clean_names() %>% 
  mutate(across(contains("date"), as_date)) %>% mutate(across(contains("date"), as_date))


## SMR01 FLAGS ####
# flag episodes for various conditions:
#   for each condition: flag as TRUE  if any 4 character diagnostic code matches any 4 character code for the condition OR
#                                     if any 3 character diagnostic code matches any 3 character code for the condition
df_eps <- df_eps %>% 
  add_flag_columns(all_codes_list) %>% 
  mutate(flag_any = if_any(starts_with("flag"))) %>% # flag for if any condition flag is TRUE
  # additional flags indicating if COVID19 was main condition or not
  mutate(flag_acute_covid19_main = is_code_in_codevector(main_condition, acute_covid19_codes)) %>% 
  mutate(flag_acute_covid19_other = if_else(flag_acute_covid19 == TRUE & flag_acute_covid19_main == FALSE, TRUE, FALSE))

# pull out all the relevant codes for each CIS stay
# each condition will have all the relevant codes for that condition recorded -- from all diagnosis positions in all 
# episodes forming the stay. If >1 relevant code is detected, it is concatenated with comma separator
df_relcodes <- df_eps %>% 
  select(link_no, cis_marker, contains("condition")) %>% 
  pivot_longer(contains("condition"), names_to = "position", values_to = "condition_") %>% 
  filter(is.na(condition_) == FALSE) %>% 
  filter(is_code_in_codevector(condition_, all_codes)) %>% 
  select(-position) %>% 
  distinct() %>% 
  add_flag_columns(all_codes_list, output = "names") %>% 
  mutate(condition = coacross(starts_with("flag"))) %>% 
  group_by(link_no, cis_marker, condition) %>% 
  summarise(codes = paste(condition_, collapse = ","), .groups = "drop") %>% 
  pivot_wider(id_cols = c(link_no, cis_marker), names_from = condition, values_from = codes)

# tibble of CIS stays -- flags now represent condition being present in any episode in the stay
df_stays <- df_eps %>% 
  group_by(link_no, cis_marker) %>% 
  summarise(upi_number = first(upi_number),
            admission_date = min(admission_date),
            discharge_date = max(discharge_date),
            across(starts_with("flag"), ~any(.)), .groups = "drop")

## SMR01 OUTPUT ####
# CIS stay info (admission/discharge dates, flags for various conditions, codes for various conditions)
df_smr01 <- df_stays %>% 
  left_join(df_relcodes) %>% 
  filter(discharge_date >= start_date) %>%
  select(upi_number, admission_date, discharge_date, starts_with("flag"), ends_with("codes")) %>% 
  mutate(data_source = "SMR01") %>% 
  rename(mother_upi = upi_number) %>% ungroup()

# df_smr01 %>% filter(flag_any) %>% View() # inspect stays with any flagged condition
write_rds(df_smr01, paste0(folder_temp_data, "smr01_flagged_stays.rds"), compress = "gz")




# SMR02 ####
clear_temp_tables(SMRAConnection)
SMR02 <- SMRAConnection %>% tbl("SMR02_PI") # virtual table of all SMR02 episodes

extract_start_date <- start_date %>% format("%d/%m/%Y")
extract_end_date <- end_date %>% format("%d/%m/%Y")

# extract all SMR02 episodes of interest (e.g. those in date range with matching mother UPI number) 
df_eps <- SMR02 %>%
  filter(DISCHARGE_DATE >= TO_DATE(extract_start_date, "dd/mm/yyyy"),
         DISCHARGE_DATE <= TO_DATE(extract_end_date, "dd/mm/yyyy")) %>%
  inner_join(mother_upis, copy = TRUE) %>% 
  arrange(UPI_NUMBER, ADMISSION_DATE, DISCHARGE_DATE) %>% 
  select(UPI_NUMBER, ADMISSION_DATE, DISCHARGE_DATE, 
         MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5) %>% 
  collect() %>% 
  clean_names() %>% 
  mutate(across(contains("date"), as_date)) %>% mutate(across(contains("date"), as_date))

## SMR02 FLAGS ####
# flag episodes for various conditions:
#   for each condition: flag as TRUE  if any 4 character diagnostic code matches any 4 character code for the condition OR
#                                     if any 3 character diagnostic code matches any 3 character code for the condition
df_eps <- df_eps %>% 
  add_flag_columns(all_codes_list) %>% 
  mutate(flag_any = if_any(starts_with("flag"))) %>% # flag for if any condition flag is TRUE
  # additional flags indicating if COVID19 was main condition or not
  mutate(flag_acute_covid19_main = is_code_in_codevector(main_condition, acute_covid19_codes)) %>% 
  mutate(flag_acute_covid19_other = if_else(flag_acute_covid19 == TRUE & flag_acute_covid19_main == FALSE, TRUE, FALSE)) %>% 
  # as no link/cis identifiers, will add in episode number for each UPI
  group_by(upi_number) %>% 
  mutate(ep_num = row_number()) %>% 
  ungroup() 

# pull out all the relevant codes for each episode
# each condition will have all the relevant codes for that condition recorded -- from all diagnosis positions in all 
# episodes forming the stay. If >1 relevant code is detected, it is concatenated with comma separator
df_relcodes <- df_eps %>% 
  select(upi_number, ep_num, contains("condition")) %>% 
  pivot_longer(contains("condition"), names_to = "position", values_to = "condition_") %>% 
  filter(is.na(condition_) == FALSE) %>% 
  filter(is_code_in_codevector(condition_, all_codes)) %>% 
  select(-position) %>% 
  distinct() %>% 
  add_flag_columns(all_codes_list, output = "names") %>% 
  mutate(condition = coacross(starts_with("flag"))) %>% 
  group_by(upi_number, ep_num, condition) %>% 
  summarise(codes = paste(condition_, collapse = ","), .groups = "drop") %>% 
  pivot_wider(id_cols = c(upi_number, ep_num), names_from = condition, values_from = codes)


## SMR02 OUTPUT ####
# CIS stay info (admission/discharge dates, flags for various conditions, codes for various conditions)
df_smr02 <- df_eps %>% 
  left_join(df_relcodes) %>% 
  filter(admission_date >= start_date,
         discharge_date <= end_date) %>%
  select(upi_number, admission_date, discharge_date, starts_with("flag"), ends_with("codes")) %>% 
  mutate(data_source = "SMR02") %>% 
  rename(mother_upi = upi_number) %>% ungroup()

# df_smr02 %>% filter(flag_any) %>% View() # inspect stays with any flagged condition
write_rds(df_smr02, paste0(folder_temp_data, "smr02_flagged_episodes.rds"), compress = "gz")




# SICSAG ####
sicsag_dir <- "/network_folder/ICU_extracts/"
sicsag_latest <- sort(dir(sicsag_dir)[str_starts(dir(sicsag_dir), "episode")], decreasing = TRUE)[[1]]

message("using SICSAG extract created on ", file.info(paste0(sicsag_dir, sicsag_latest))$ctime)

df_ep <- read_rds(paste0(sicsag_dir, sicsag_latest)) %>% 
  clean_names()

## SICSAG RECODES ####
# for mapping values to labels...as much of the SICSAG data is numeric (with descriptive labels)
unit_outcome_recodes <- generate_recodes(df_ep, "unit_outcome")
unit_outcome_derived_recodes <- generate_recodes(df_ep, "unit_outcome_derived")
ap_diag_recodes <- generate_recodes(df_ep, "ap_diag")
corr_apii_diag_recodes <- generate_recodes(df_ep, "corr_apii_diag")
diag_recodes <- generate_recodes(df_ep, "prime_diag_hosp") # same for prime_diag_hosp, prime_diag_unit and all the oth_diagX columns 
covid_ic_uor_hdu_recodes <- generate_recodes(df_ep, "covid_ic_uor_hdu")

## SICSAG TIDY DATA ####
# remove SPSS labels, select columns we need, filter for only UPIs we need, recode some columns with SPSS label data
df_ep <- df_ep %>%
  zap_labels() %>%
  zap_label() %>%
  select(chi_no, admit_unit, disc_date, preg_status, 
         unit_outcome, 
         unit_outcome_derived, 
         ap_diag, # APACHE diagnosis
         corr_apii_diag, # corresponding APACHE II diagnosis
         prime_diag_hosp,
         prime_diag_unit,
         starts_with("oth_diag"),
         covid_ic_uor_hdu) %>%
  rename(mother_upi = chi_no) %>%
  inner_join(mother_upis %>% rename(mother_upi = UPI_NUMBER)) %>%
  # recode various fields to their label values
  mutate(unit_outcome = recode(unit_outcome, !!!unit_outcome_recodes)) %>%
  mutate(unit_outcome_derived = recode(unit_outcome_derived, !!!unit_outcome_derived_recodes)) %>%
  mutate(ap_diag = recode(ap_diag, !!!ap_diag_recodes)) %>%
  mutate(corr_apii_diag = recode(corr_apii_diag, !!!corr_apii_diag_recodes)) %>%
  mutate(across(starts_with("oth_diag"), ~recode(., !!!diag_recodes))) %>%
  mutate(covid_ic_uor_hdu = recode(covid_ic_uor_hdu, !!!covid_ic_uor_hdu_recodes)) %>%
  #
  arrange(mother_upi, admit_unit, disc_date) %>%
  rename(admission_date = admit_unit,
         discharge_date = disc_date) %>%
  filter(admission_date >= start_date,
         discharge_date <= end_date | is.na(discharge_date)) %>%
  group_by(mother_upi) %>%
  mutate(ep_num = row_number()) %>%
  ungroup() %>%
  mutate(data_source = "SICSAG")

## SICSAG -- NA DISCHARGE DATES ####
no_discharge_date_eps <- df_ep %>% filter(is.na(discharge_date))

# inspect how many in each month
no_discharge_date_eps %>% select(admission_date) %>% mutate(admission_month = format(as.Date(admission_date), "%Y-%m")) %>% group_by(admission_month) %>% summarise(n = n())
df_ep <- df_ep %>% filter(is.na(discharge_date) == FALSE) # filter out those with NA discharge dates

# SICSAG OUTPUT ####
write_rds(df_ep, paste0(folder_temp_data, "sicsag_episodes.rds"), compress = "gz")



# DEATHS ####
## DIAGNOSIS CODES ####
# just the COVID related codes for deaths...
all_codes_list <- list(acute_covid19_codes = acute_covid19_codes,
                       history_of_covid19_codes = history_of_covid19_codes,
                       adverse_covid19_vax_event_codes = adverse_covid19_vax_event_codes)

all_codes <- as.character(unlist(all_codes_list))

clear_temp_tables(SMRAConnection)
DEATHS <- SMRAConnection %>% tbl("DEATHS_C") 

extract_start_date <- start_date %>% format("%d/%m/%Y")
extract_end_date <- end_date %>% format("%d/%m/%Y")

# extract all death records of interest (e.g. those in date range with matching mother UPI number)
df_deaths <- DEATHS %>% 
  filter(DATE_OF_DEATH >= TO_DATE(extract_start_date, "dd/mm/yyyy"),
         DATE_OF_DEATH <= TO_DATE(extract_end_date, "dd/mm/yyyy")) %>%
  inner_join(mother_upis, copy = TRUE) %>% 
  select(UPI_NUMBER, DATE_OF_DEATH, UNDERLYING_CAUSE_OF_DEATH, starts_with("CAUSE_OF_DEATH_CODE")) %>% 
  collect() %>% 
  clean_names() %>% 
  mutate(across(contains("date"), as_date)) %>% mutate(across(contains("date"), as_date))

length(unique(df_deaths$upi_number)) == nrow(df_deaths) # TRUE if one death record per UPI

# DEATHS FLAGS ####
# flag death records for various conditions:
#   for each condition: flag as TRUE  if any 4 character diagnostic code matches any 4 character code for the condition OR
#                                     if any 3 character diagnostic code matches any 3 character code for the condition
df_deaths <- df_deaths %>% 
  add_flag_columns(all_codes_list, col_contains = "cause_of_death") %>% 
  mutate(flag_any = if_any(starts_with("flag"))) %>%  # flag for if any condition flag is TRUE
  # additional flags indicating if COVID19 was underlying cause or not
  mutate(flag_acute_covid19_underlying = is_code_in_codevector(underlying_cause_of_death, acute_covid19_codes)) %>% 
  mutate(flag_acute_covid19_contributory = if_else(flag_acute_covid19 == TRUE & flag_acute_covid19_underlying == FALSE, TRUE, FALSE)) %>% 
  ungroup() %>% 
  rename(mother_upi = upi_number) %>% 
  mutate(data_source = "deaths")

## DEATHS OUTPUT ####
write_rds(df_deaths, paste0(folder_temp_data, "deaths_flagged.rds"), compress = "gz")

rm(list = (c("SMR01", "SMR02", "DEATHS", ls()[str_starts(ls(), "df_")], ls()[str_ends(ls(), "_recodes")]))) # remove data etc no longer needed
