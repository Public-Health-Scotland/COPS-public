
mother_upis_to_link <- read_rds(paste0(folder_temp_data, "mother_upis_dob_from_CHI.rds")) %>%
  select(upi) %>%
  filter(chi_check(upi) == "Valid CHI") %>% 
  distinct() %>%
  rename(UPI_NUMBER = upi)

df <- read_rds(full_ethnicity_lookup_path)

df <- df %>% 
  rename(UPI_NUMBER = patient_derived_chi_number) %>% 
  inner_join(mother_upis_to_link)

message(paste0("Ethnicity data found for ", nrow(df), " out of ", nrow(mother_upis_to_link), " mother UPIs. This represents ", signif(100*nrow(df)/nrow(mother_upis_to_link), 3), "%\n"))
message(paste0("Excluding \'not known' and \'refused\' leaves ", nrow(df %>% filter(!(ethnic_code %in% c("98", "99")))), ", i.e. ",  signif(100*nrow(df %>% filter(!(ethnic_code %in% c("98", "99")))) / nrow(mother_upis_to_link), 3), "%"))

write_rds(df, ethnicity_vacc_lookup_path, compress = "gz")

rm(df, mother_upis_to_link)
