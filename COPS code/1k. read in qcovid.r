
qcovid_data_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                      ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>% 
  distinct() %>% 
  mutate(q_ethnicity_mapped9= case_when(q_ethnicity %in% c("1","2","3")  ~ 4,
                                       q_ethnicity %in% c("8","9","10") ~ 5,
                                       q_ethnicity == "15" ~ 6,
                                       q_ethnicity %in% c("12","13","14") ~ 7,
                                       q_ethnicity %in% c("4","5","6","7","11","16") ~ 8,
                                       q_ethnicity %in% c("17","0") ~ NA_real_,
                                       T ~ NA_real_
                                       )) %>% 
  mutate(q_preexisting_diabetes= case_when(q_diag_diabetes_1 == 1 ~ 1,
                               q_diag_diabetes_2 == 1 ~ 1)) %>% 
  mutate(q_diabetes_type= case_when(q_diag_diabetes_1 == 1 ~ 1,
                                           q_diag_diabetes_2 == 2 ~ 2)) %>% 
  mutate(q_bmi = as.numeric(q_bmi))
  
qcovid_data <- qcovid_data_temp_1 %>%  
  mutate(
    q_bmi = case_when(q_bmi %in% feasible_bmi ~ q_bmi,
                      T ~ NA_real_))
  
# Record BMI recoded  
denominator <- qcovid_data_temp_1 %>% 
    select(q_bmi) %>% 
    filter(!is.na(q_bmi))
numerator <- qcovid_data %>% 
    select(q_bmi) %>% 
    filter(!is.na(q_bmi))
  
qcovid_recoded <- data.frame(variable = "BMI",
                         den = nrow(denominator),
                         num = nrow(numerator)) %>% 
  mutate(dataset ="qcovid")
rm(numerator, denominator)
write_rds(qcovid_recoded, paste0(folder_temp_data, "qcovid_recoded.rds"))

#Fetch extra smoking data
smoking_data_temp_1 <- ### EXTRACT/DATABASE CONNECTION DETAILS
                       ### REMOVED FOR PUBLIC RELEASE
  distinct() %>% 
  filter(diag == "EAVE_NON_SMOKER" |
         diag == "EAVE_EX_SMOKER" |
         diag == "EAVE_SMOKER") %>% 
  mutate(flag = 1)

smoking_data_temp_2 <- smoking_data_temp_1 %>%
  distinct() %>% 
  group_by(EAVE_LINKNO) %>% 
  pivot_wider(id_cols = EAVE_LINKNO, 
              names_from = diag,
              values_from = flag,
              values_fill = 0) %>% 
  clean_names()

#Add smoking data to qcovid data
data_qcovid_smoking <- qcovid_data %>% 
  full_join(smoking_data_temp_2, by=c("eave_linkno" = "eave_linkno"))
  

#Read in extra GP demographics data
gp_demographics <- ### EXTRACT/DATABASE CONNECTION DETAILS
                   ### REMOVED FOR PUBLIC RELEASE
  clean_names() %>% 
  distinct() %>%
  rename("linkno" = "eave2") %>% 
  select(c(linkno, simd, hb, ur6, ur8)) %>% 
  mutate(hb = str_sub(hb, 5)) %>% 
  mutate(hb = str_replace(hb, "&", "and")) %>% 
  rename_with( ~ paste0("eave_", .)) 

data_qcovid_smoking_demographics <- gp_demographics %>%
  full_join(data_qcovid_smoking, by=c("eave_linkno" = "eave_linkno"))  %>% 
  mutate(q_covid = 1) %>%
  retype()
  

write_rds(data_qcovid_smoking_demographics, paste0(folder_temp_data, "qcovid.rds"))

rm(qcovid_data, qcovid_data_temp_1, smoking_data_temp_1, smoking_data_temp_2, gp_demographics, data_qcovid_smoking,
   data_qcovid_smoking_demographics, qcovid_recoded)

