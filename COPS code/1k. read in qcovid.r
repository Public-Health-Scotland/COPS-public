
##Start from here - deduplicated march 2020 file with all variables####
qcovid1 <- readRDS(paste0(folder_data, "network_folder/qcovid_march_2020.rds"))
qcovid1 <- qcovid1 %>% ungroup() 
names(qcovid1)
#load jan 2021 update
qcovid2 <- readRDS(paste0(folder_data, "network_folder/qcovid_jan2021_wide.rds"))
names(qcovid2)

  
qcovid2 <- qcovid2 %>% rename(BMI = Q_BMI) %>%
  mutate(Q_LEARN_CAT = case_when(Q_LEARN_CAT_1 ==1 ~1,
                                 Q_LEARN_CAT_2==1 ~ 2,
                                 TRUE ~ 0 )) %>%
  mutate(Q_HOME_CAT = case_when(Q_HOME_CAT_1 == 1 ~ 1, 
                                Q_HOME_CAT_2 == 1 ~2, 
                                TRUE ~0))

qcovid2 <- qcovid2  %>% mutate(Qversion = "Jan2021")
qcovid1 <- qcovid1  %>% mutate(Qversion = "March2020")

#qcovid_demog1 <- qcovid1 %>% select(EAVE_LINKNO, datazone2011, simd2020v2_sc_quintile, Qversion) 
#urban_rural <- read_sav("network_folder/datazone2011_urban_rural_2016.sav")
#urban_rural <- urban_rural %>% select(Datazone2011, UR6_2016, UR8_2016)
#SPD <- readRDS("network_folder/Scottish_Postcode_Directory_2021_2.rds")
#SPD <- SPD %>% select(datazone2011, hb2019name) %>% unique()
#qcovid_demog1 <- qcovid_demog1 %>% left_join(SPD) %>% left_join(urban_rural, by = c("datazone2011" = "Datazone2011")) %>%
#  rename(ur6 = UR6_2016, ur8 = UR8_2016, hb = hb2019name,  simd = simd2020v2_sc_quintile) %>% select(-datazone2011)
#simd, hb, ur6, ur8
#qcovid_demog2 <- qcovid2 %>% select(EAVE_LINKNO, hb2019name, simd2020, UR8_2016, UR6_2016, Qversion  ) %>%
#  rename(ur6 = UR6_2016, ur8 = UR8_2016, hb =hb2019name,  simd= simd2020 )#
#qcovid_demog <- rbind(qcovid_demog1, qcovid_demog2)
#rm(qcovid_demog1, qcovid_demog2)
#saveRDS(qcovid_demog, paste0(folder_data, "network_folder/all_qcovid_demographics.rds"))

#rbind the 2 files
#names(qcovid2)[which( !(names(qcovid2) %in% names(qcovid1)))]
qcovid2 <- qcovid2 %>% select(-c(  Q_HOME_CAT_1,Q_HOME_CAT_2, Q_LEARN_CAT_1, Q_LEARN_CAT_2, Postcode, 
                                 DateOfBirth, EventDate, pc7, hb2019name, simd2020, UR8_2016, UR6_2016, UR6_2016))
qcovid1 <- qcovid1 %>% select(-c(Q_RX_IMMUNO, Q_RX_LABA, Q_RX_PRED, SG_ASTRX_COD, SG_IMMRX_COD, DeductionDate,Age,
simd2020v2_sc_quintile, UPI_NUMBER, CHINumber, valid_BMI, Q_BMI  ))

qcovid_all <- rbind(qcovid1, qcovid2) %>%
  retype()

qcovid_all <-qcovid_all  %>% mutate(q_ethnicity = case_when(Q_ETHNICITY_1 ==1  ~1 , 
                                                            Q_ETHNICITY_2 ==1  ~2 ,
                                                            Q_ETHNICITY_3 ==1  ~3 ,
                                                            Q_ETHNICITY_4 ==1  ~4 ,
                                                            Q_ETHNICITY_5 ==1  ~5 ,
                                                            Q_ETHNICITY_6 ==1  ~6 ,
                                                            Q_ETHNICITY_7 ==1  ~7 ,
                                                            Q_ETHNICITY_8 ==1  ~8 ,
                                                            Q_ETHNICITY_9 ==1  ~9 ,
                                                            Q_ETHNICITY_10 ==1  ~10 ,
                                                            Q_ETHNICITY_11 ==1  ~11 ,
                                                            Q_ETHNICITY_12 ==1  ~12 ,
                                                            Q_ETHNICITY_13 ==1  ~13 ,
                                                            Q_ETHNICITY_14 ==1  ~14 ,
                                                            Q_ETHNICITY_15 ==1  ~15, 
                                                            Q_ETHNICITY_16 ==1  ~16, 
                                                            Q_ETHNICITY_0 ==1  ~0, 
                                                            TRUE ~0))

#cleaning data, derived variables ####
qcovid_data_temp_1 <-
  qcovid_all  %>%
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
  mutate(q_bmi = as.numeric(bmi))
  
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
write_rds(qcovid_recoded, paste0(folder_temp_data, "qcovid_recoded2.rds"))

#Fetch extra smoking data
smoking_data_temp_1 <-
  readRDS(
    "/network_folder/cohort_diags_2020-09-15_SK.rds"
  )  %>%
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
  left_join(smoking_data_temp_2, by=c("eave_linkno" = "eave_linkno"))


#Read in extra GP demographics data
gp_demographics <-
  readRDS(
    paste0(folder_data, "network_folder/all_qcovid_demographics.rds")) %>%
  clean_names() %>% 
  distinct() %>%
#  rename("linkno" = "eave2") %>% 
  select(c(eave_linkno, simd, hb, ur6, ur8, qversion)) %>% 
  mutate(hb = str_sub(hb, 5)) %>% 
  mutate(hb = str_replace(hb, "&", "and")) %>% 
  rename(eave_simd = simd,eave_hb = hb, eave_ur6 = ur6, eave_ur8 = ur8)

data_qcovid_smoking_demographics <- gp_demographics %>%
  left_join(data_qcovid_smoking, by=c("eave_linkno" =  "eave_linkno", "qversion" = "qversion"))  %>% 
  mutate(q_covid = 1) %>%
  retype(eave_ur6, eave_ur8)

data_qcovid_smoking_demographics <- data_qcovid_smoking_demographics %>%
  mutate(q_diag_diabetes_1 = case_when(is.na(q_diag_diabetes_1) ~ 0,
                                       T ~ 1)) %>%
  mutate(q_diag_diabetes_2 = case_when(is.na(q_diag_diabetes_2) ~ 0,
                                       T ~ 1))

write_rds(data_qcovid_smoking_demographics, paste0(folder_temp_data, "qcovid.rds"), compress = "gz")

rm(qcovid_data, qcovid_data_temp_1, smoking_data_temp_1, smoking_data_temp_2, gp_demographics, data_qcovid_smoking,
   data_qcovid_smoking_demographics, qcovid_recoded)

rm(qcovid1, qcovid2, qcovid_all)