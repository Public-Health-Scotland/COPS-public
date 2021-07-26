## Add on any new CHIs to the EAVE master link file
#Run this only whe
# select one each of valid mother CHIs
pregnancies3_upi <- pregnancies3 %>% 
  select(mother_upi) %>% 
  distinct() %>% 
  filter(!str_starts(mother_upi,"4")) %>%
  filter(!str_starts(mother_upi,"5")) %>%
  filter(!str_starts(mother_upi,"6")) %>%
  filter(!str_starts(mother_upi,"7")) %>%
  filter(!str_starts(mother_upi,"8"))


###Code below from Emily Moore to produce EAVE IDs
today <- Sys.Date()

#set file name & name of chi field
file_name <- "COPS"
chi_field <- "mother_upi"


MASTER_id_file <- ### EXTRACT/DATABASE CONNECTION DETAILS
                  ### REMOVED FOR PUBLIC RELEASE

preexisting_eave_id <- pregnancies3_upi %>% 
  left_join(MASTER_id_file, by=c("mother_upi" = "CHINumber")) %>% 
  filter(!is.na(EAVE_LINKNO))

no_eave_id <- pregnancies3_upi %>% 
  left_join(MASTER_id_file, by=c("mother_upi" = "CHINumber")) %>% 
  filter(is.na(EAVE_LINKNO)) %>% 
  select(mother_upi)

df <- no_eave_id
str(df)
df <- df %>% mutate(validchi = phsmethods::chi_check(mother_upi))
df <- df %>% filter(validchi == "Valid CHI")

### REFERENCE TO FUNCTIONS FILE REMOVED FOR PUBLIC RELEASE

add_chi_to_linkfile(df, chi = "mother_upi", source = "COPS")


MASTER_id_file <- MASTER_id_file %>%  select(CHINumber, EAVE_LINKNO)

df <- left_join(df, MASTER_id_file, by = c("mother_upi" = "CHINumber"))
table(is.na(df$EAVE_LINKNO), df$mother_upi=="")

df <- df %>% select(-mother_upi) %>% filter(!is.na(EAVE_LINKNO))
saveRDS(df, paste0(file_name, today, ".rds"))
