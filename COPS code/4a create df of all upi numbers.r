###
### Create a csv of all CHI numbers which appear in our cohort.
### This is required when requesting certain data extracts
### such as COVID test results and the shielding patient list.
###
### Extract date of birth info for mother CHI numbers from the 
### CHI database and create a lookup. This will be used as a 
### source for mother dobs in script "6a determine authoritative values.r"


babies <- read_rds(paste0(folder_temp_data, "cops_births.rds")) %>% select(baby_upi) %>%
  rename(upi = baby_upi)

mothers <- read_rds(paste0(folder_temp_data, "script3_pregnancy_record.rds")) %>% select(mother_upi) %>%
  rename(upi = mother_upi)

all_upis <- bind_rows(babies, mothers) %>%
  distinct() %>%
  filter(chi_check(upi) == "Valid CHI")

write_csv(all_upis, paste0(folder_cohorts, "all_upis.csv"))


# extract dates of birth for these UPIs from CHI ####
# connect to SMRA ####
smra <- odbc::dbConnect(odbc::odbc() ### DATABASE CONNECTION DETAILS REMOVED FOR PUBLIC RELEASE


# mother DATES OF BIRTH from CHI database ####
mothers <- mothers %>% 
  distinct() %>%
  filter(chi_check(upi) == "Valid CHI")

upi_dobs <- smra %>% tbl(dbplyr::in_schema("UPIP", "L_UPI_DATA")) %>% 
  inner_join(mothers %>% rename(CHI_NUMBER = upi), copy = TRUE) %>% 
  filter(is.na(DELETION_INDICATOR)) %>% # remove any that have been marked as deleted
  select(CHI_NUMBER, DATE_OF_BIRTH) %>% 
  distinct() %>% 
  collect() %>% 
  rename(upi = CHI_NUMBER,
         chi_dob = DATE_OF_BIRTH) %>% 
  mutate(chi_dob = as_date(chi_dob))

# filter out any with multiple dates of birth (if there are any)
upi_dobs <- upi_dobs %>% group_by(upi) %>% filter(n() == 1) %>% ungroup()

# join the dobs back onto our original list of UPIs
out <- mothers %>% left_join(upi_dobs)

message(sum(is.na(out$chi_dob)), " UPIs out of ", nrow(out), " do not have a date of birth in CHI.")

# write out the mother dates of birth as sourced from CHI database
write_rds(out, paste0(folder_temp_data, "mother_upis_dob_from_CHI.rds"), compress = "gz")

# clean up
rm(mothers, babies, all_upis, upi_dobs, out)
