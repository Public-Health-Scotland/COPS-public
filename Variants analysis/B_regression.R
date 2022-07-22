##Regression of main outcomes by time period####
#- in relation to dominance of Delta/Omicron variants
#setup####
library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(janitor)
library(haven)
library(survival)
library(survey)
library(jtools)
#packageurl <- "http://cran.r-project.org/src/contrib/Archive/jtools/jtools_2.1.2.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
# had to reinstall pander first
path_to_scripts <- file.path(paste(here::here(), "Analysis code and metadata", "COPS Code/", sep = "/"))

#Setup, read in packages and get folder directories
source(file.path(paste0(path_to_scripts,"Ad-Hoc-Analysis/Outcomes_by_variant/0.Setup for variants analysis.R"))) 

vaccination_status <- function(dose_1_date, dose_2_date, dose_3_date, reference_date){
  one_dose_date <- dose_1_date + days(21)
  
  two_dose_date <- dose_2_date + days(14)
  
  three_dose_date <- dose_3_date + days(14) # grouping 3/subsequent as one group as vv few 4s at this time
  
  case_when(reference_date < one_dose_date | is.na(dose_1_date) ~ "0 - Unvaccinated",
            reference_date >= one_dose_date & 
              (reference_date < two_dose_date | is.na(two_dose_date)) ~ "1 - One dose",
            reference_date >= two_dose_date & 
              (reference_date < three_dose_date | is.na(three_dose_date)) ~ "2 - Two doses",
            reference_date >= three_dose_date  ~ "3 - Three doses"
  )
  
}
cops_reporting_ethnicity <- function(ethnicity_code) {case_when(str_starts(ethnicity_code, "1") ~ "1 White",
                                                                str_detect( toupper(ethnicity_code), "WHITE") ~ "1 White",
                                                                substr(toupper(ethnicity_code),1,2) %in% c("3F", "3G", "3H") ~ "2 South Asian",
                                                                str_detect( toupper(ethnicity_code), "INDIAN") | str_detect( toupper(ethnicity_code), "PAKISTANI") ~ "2 South Asian",
                                                                substr(toupper(ethnicity_code),1,2) %in% c("5D", "5C", "5Y", "4D", "4Y", "5X") ~ "3 Black/Caribbean/African" ,
                                                                str_detect( toupper(ethnicity_code), "BLACK") ~ "3 Black/Caribbean/African" ,
                                                                str_detect( toupper(ethnicity_code), "AFRICAN") ~ "3 Black/Caribbean/African" ,
                                                                substr(toupper(ethnicity_code),1,2) %in% c("3J", "3X", "2A", "6A", "3Z", "6Z", "OT") ~ "4 Other or mixed ethnicity",
                                                                str_detect( toupper(ethnicity_code), "MIXED") |str_detect( toupper(ethnicity_code), "OTHER") ~ "4 Other or mixed ethnicity",
                                                                is.na(ethnicity_code) ~ "5 Unknown/missing", 
                                                                substr(toupper(ethnicity_code),1,2) %in% c("99", "98") ~ "5 Unknown/missing", 
                                                                toupper(ethnicity_code)=="UNKNOWN" | str_detect( toupper(ethnicity_code), "REFUSED") ~ "5 Unknown/missing")} 



delta_start <- as.Date("2021-05-17") #>90% S+ (changing from S- alpha )  - actually think it was earlier than this but need to check
delta_end <- as.Date("2021-12-14")
#transition_start <- as.Date("2021-12-09")# Date where >10% were S-
#transition_end <- as.Date("2022-01-04")
omicron_start <- as.Date("2021-12-15") #date of >90% S-
infections_cutoff <- as.Date("2022-01-31") #end of infection period included in analyses.

####Data prep####
# long format - 1 infection per row
# prep vacc status and time to end of pregnancy, from infection date.
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) 


##################
fetus_infections <- fetuslevel %>% filter(tests_mother_positive_test_during_pregnancy==1 & 
                                                   (tests_mother_value_positive_test_during_pregnancy_1 >=delta_start |
                                                      tests_mother_value_positive_test_during_pregnancy_1>=delta_start ) ) %>%
  select(pregnancy_id, mother_upi, baby_upi, tests_mother_value_positive_test_during_pregnancy_1, tests_mother_value_positive_test_during_pregnancy_2) %>% 
  pivot_longer(cols = c(tests_mother_value_positive_test_during_pregnancy_1, tests_mother_value_positive_test_during_pregnancy_2), 
               names_to = "test_number", values_to = "mother_positive_test_during_pregnancy_date") %>%
  filter(!is.na(mother_positive_test_during_pregnancy_date)) %>% 
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date >=omicron_start & mother_positive_test_during_pregnancy_date < infections_cutoff ~ "Omicron",
                                    mother_positive_test_during_pregnancy_date >=delta_start & mother_positive_test_during_pregnancy_date <= delta_end ~ "Delta")) %>%
  filter(variant_period== "Delta" |variant_period== "Omicron") %>%
  unique() # unique values - for multiple pregnancies with stillbirths, 
           #no baby upi can end up with multiple identical records when reduced to prg.mother,baby ID plus the infection date.


#join back to main fetus level data.
fetuslevel <- fetuslevel %>% 
  select(-c(tests_mother_value_positive_test_during_pregnancy_1, tests_mother_value_positive_test_during_pregnancy_2))

fetus_infections <- left_join(fetus_infections, fetuslevel, by=c("pregnancy_id", "mother_upi", "baby_upi"))
fetus_infections$ethnicity_reporting <- cops_reporting_ethnicity(fetus_infections$x_ethnicity_code)

#vacc status, gest at infection, time from infection to end preg.
fetus_infections  <- fetus_infections  %>% 
  mutate(vaccination_status_at_infection =
           vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy_date)) %>% 
  mutate(infection_to_end=  difftime(x_pregnancy_end_date, mother_positive_test_during_pregnancy_date, units="days")) %>%
  mutate(gestation_days = 14 + (x_est_conception_date %--% mother_positive_test_during_pregnancy_date/ days(1))) %>%
  mutate(gestation_at_infection = floor(gestation_days/7))



#pregnancies####
pregnancies <-readRDS(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds"))

#select pregnancies w infections only. limit to infection > delta start date
pregnancies_infections <- pregnancies %>% filter(mother_tested_positive_during_pregnancy==1 & 
                                                   (mother_positive_test_during_pregnancy_1 >=delta_start |
                                                      mother_positive_test_during_pregnancy_2 >=delta_start ) ) %>%
  select(pregnancy_id, mother_upi, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>% 
  pivot_longer(cols = c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2), 
               names_to = "test_number", values_to = "mother_positive_test_during_pregnancy_date") %>%
  filter(!is.na(mother_positive_test_during_pregnancy_date)) %>% 
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date >=omicron_start & mother_positive_test_during_pregnancy_date <= infections_cutoff ~ "Omicron",
                                    mother_positive_test_during_pregnancy_date >=delta_start & mother_positive_test_during_pregnancy_date <= delta_end ~ "Delta")) %>%
  filter(variant_period== "Delta" |variant_period== "Omicron")%>% unique


#join back to main pregnancies data.
pregnancies <- pregnancies %>% select(-c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2))

pregnancies_infections <- left_join(pregnancies_infections, pregnancies)

table(pregnancies_infections$variant_period, useNA="always")
#

#vaccination status
pregnancies_infections <- pregnancies_infections %>% 
  mutate(infection_to_end=  difftime(pregnancy_end_date, mother_positive_test_during_pregnancy_date, units="days")) %>%
  mutate(gestation_days = 14 + (est_conception_date %--% mother_positive_test_during_pregnancy_date/ days(1))) %>%
  mutate(gestation_at_infection = floor(gestation_days/7)) %>%
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy_date))

pregnancies_infections$ethnicity_reporting <- cops_reporting_ethnicity(pregnancies_infections$ethnicity_code)

################################
##ICU admission - for covid ####
################################
# COVID RELATED SICSAG ####
mother_upis <- read_rds(file.path(folder_temp_data, "mother_upis.rds"))
end_date  <- today()
# SICSAG ####

sicsag_latest <- sort(dir(sicsag_dir)[str_starts(dir(sicsag_dir), "episode")], decreasing = TRUE)[[1]]

message("using SICSAG extract created on ", file.info(paste0(sicsag_dir, sicsag_latest))$ctime)

df_ep <- read_rds(paste0(sicsag_dir, sicsag_latest)) %>% 
  clean_names()

## SICSAG RECODES ####

generate_recodes <- function(df_labelled, colname, output = "vector"){
  # Takes a column from a labelled dataframe (e.g from have SPSS import) and returns a vector or tibble that can be used to recode e.g. the values to the labels or vice-versa.
  # Useful if data values are numeric and the labels are more informative strings.
  # By default returns a named vector (easier for use in dplyr::recode) but can optionally return a tibble instead (set e.g. output = "tibble")
  labels <- attr((df_labelled[[{{colname}}]]), which = "labels")
  
  out <- tibble(codes = labels,
                values = labels %>% names())
  
  if(output == "vector"){
    out <- deframe(out)
  }
  
  return(out)
  
}
# for mapping values to labels...as much of the SICSAG data is numeric (with descriptive labels)
unit_outcome_recodes <- generate_recodes(df_ep, "unit_outcome")
unit_outcome_derived_recodes <- generate_recodes(df_ep, "unit_outcome_derived")
ap_diag_recodes <- generate_recodes(df_ep, "ap_diag")
corr_apii_diag_recodes <- generate_recodes(df_ep, "corr_apii_diag")
diag_recodes <- generate_recodes(df_ep, "prime_diag_hosp") # same for prime_diag_hosp, prime_diag_unit and all the oth_diagX columns 
covid_ic_uor_hdu_recodes <- generate_recodes(df_ep, "covid_ic_uor_hdu")


## SICSAG TIDY DATA ####
# remove SPSS labels, select columns we need, filter for only UPIs we need, recode some columns with SPSS label data
start_date <- as.Date("2021-04-01")
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
  mutate(across(starts_with("prime_diag"), ~recode(., !!!diag_recodes))) %>%
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


#check ICU data for covid admissions without an associated test
covid_associated_icu_days <- 21
icu <- df_ep %>% 
  select(mother_upi, admission_date, discharge_date, covid_ic_uor_hdu, prime_diag_unit, preg_status, data_source, 
         oth_diag1, oth_diag2, oth_diag3, oth_diag4, oth_diag5, oth_diag6 )

covid_diags <- c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")
icu_diags <- df_ep %>% 
  select(mother_upi, admission_date, discharge_date, covid_ic_uor_hdu, prime_diag_unit, preg_status, data_source, 
         oth_diag1, oth_diag2, oth_diag3, oth_diag4, oth_diag5, oth_diag6 ) %>%
  mutate(flag_covid_diag_in_stay = case_when(prime_diag_unit %in% covid_diags ~1,
                                            oth_diag1  %in% covid_diags ~1,
                                            oth_diag2  %in% covid_diags ~1,
                                            oth_diag3  %in% covid_diags ~1,
                                            oth_diag4  %in% covid_diags ~1,
                                            oth_diag5  %in% covid_diags ~1,
                                            oth_diag6  %in% covid_diags ~1,
                                            TRUE~0)
  ) %>% filter(flag_covid_diag_in_stay==1)

preg_icu_diag <- left_join(pregnancies, icu_diags) %>% 
  mutate(admission_in_preg = case_when(admission_date >est_conception_date & is.na(pregnancy_end_date) ~1,
                                       admission_date >est_conception_date & admission_date <= pregnancy_end_date ~1,
                                        TRUE ~0)) %>%
  mutate(admission_assoc_preg = case_when( admission_date > pregnancy_end_date & admission_date <= pregnancy_end_date+21 ~1,
                                           TRUE ~0)) %>%
  filter(admission_in_preg ==1 | admission_assoc_preg==1) %>%
 filter(admission_date >= delta_start) %>%
  mutate(variant_period = case_when(admission_date >=delta_start & admission_date <= delta_end ~ "Delta",
                                    admission_date >=omicron_start & admission_date <= infections_cutoff ~ "Omicron",
                                    TRUE ~ "after_cutoff"))

  
pregnancies_icu <- pregnancies_infections %>% left_join(icu) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(flag_covid_test_in_stay = mother_positive_test_during_pregnancy_date %within% stay_interval) %>% # TRUE if +ve test taken during the stay
  mutate(flag_covid_diag_in_stay = case_when(prime_diag_unit %in% covid_diags ~1,
                                             oth_diag1  %in% covid_diags ~1,
                                             oth_diag2  %in% covid_diags ~1,
                                             oth_diag3  %in% covid_diags ~1,
                                             oth_diag4  %in% covid_diags ~1,
                                             oth_diag5  %in% covid_diags ~1,
                                             oth_diag6  %in% covid_diags ~1,
                                             TRUE~0)
         ) %>%
  mutate(admission_to_test_days = mother_positive_test_during_pregnancy_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_covid_temporal_stay = if_else(admission_to_test_days >= 0 & admission_to_test_days <= covid_associated_icu_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 21 days before admission
  mutate(flag_covid_associated = if_else(flag_covid_temporal_stay == TRUE | flag_covid_test_in_stay == TRUE, TRUE, FALSE))  # TRUE if either previous flag TRUE

pregnancies_icu_1 <- pregnancies_icu %>% filter(is.na(admission_date))
pregnancies_icu_2 <- pregnancies_icu %>% filter(!is.na(admission_date)) 

pregnancies_icu_associated <- pregnancies_icu_2 %>% 
  arrange(pregnancy_id, mother_positive_test_during_pregnancy_date, admission_date) %>%
  group_by(pregnancy_id, mother_positive_test_during_pregnancy_date, flag_covid_associated) %>% 
  slice_head(n=1) %>% ungroup %>% #select first in group
  arrange(pregnancy_id, mother_positive_test_during_pregnancy_date, -flag_covid_associated) %>%
  group_by(pregnancy_id, mother_positive_test_during_pregnancy_date) %>% slice_max(flag_covid_associated) %>%
  ungroup()
  

#can only be one covid associated stay per test,so 
pregnancies_icu_21 <- rbind(pregnancies_icu_1, pregnancies_icu_associated)
##sort NA values
pregnancies_icu_21 <- pregnancies_icu_21 %>% 
  mutate(flag_covid_associated = ifelse(is.na(flag_covid_associated), FALSE, flag_covid_associated)) %>%
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  mutate(simd = ifelse(is.na(simd), 9, simd)) #%>%

saveRDS(pregnancies_icu_21, paste0(folder_temp_data, "variants_analysis/", "icu_model_data.rds"))

##ICU regression 21 days####

vars <- c("variant_period", "simd", "gestation_at_infection",
          "vaccination_status_at_infection", "mother_age_at_outcome","icu_21", "reporting_ethnicity")


design <- svydesign(ids=~mother_upi,  data=pregnancies_icu_21)

model1_1 <- svyglm(flag_covid_associated ~ variant_period,
                  design = design, data=pregnancies_icu_21, 
                  family= binomial(link = "logit") )
summ(model1_1, exp=T, confint = T)

saveRDS(summ(model1_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_unadj_OR.rds" ))
table(pregnancies_icu_21$flag_covid_associated, pregnancies_icu_21$variant_period)


model1b <- svyglm(flag_covid_associated~ variant_period + simd +gestation_at_infection+ 
                    vaccination_status_at_infection + mother_age_at_conception + ethnicity_reporting, 
                  design = design, data=pregnancies_icu_21, 
                  family= binomial(link = "logit") )


summ(model1b, exp=T, confint = T)

saveRDS(summ(model1b, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_model_summary_ethn.rds" ))

#interaction check (probably too small numbers to show anything)
pregnancies_icu_21 <- pregnancies_icu_21 %>% 
  mutate(any_vacc = ifelse(vaccination_status_at_infection != "0 - Unvaccinated", 1,0))
model1bi <- svyglm(flag_covid_associated~ variant_period*any_vacc + 
                      simd +gestation_at_infection+ 
                      mother_age_at_conception + ethnicity_reporting, 
                    design = design, data=pregnancies_icu_21, 
                    family= binomial(link = "logit") )


summ(model1bi, exp=T, confint = T)
saveRDS(summ(model1bi, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_model_interactions2_summary_ethn.rds" ))

table(pregnancies_icu_21$flag_covid_associated, pregnancies_icu_21$vaccination_status_at_infection, useNA="always")
table(pregnancies_icu_21$flag_covid_associated, pregnancies_icu_21$ethnicity_reporting, useNA="always")
table(pregnancies_icu_21$flag_covid_associated, pregnancies_icu_21$variant_period, useNA="always")

## ICU regression diagnosis####
#covid diagnosis in icu stay (ie leave out incidental admission to ICU following a test.)####
pregnancies_icu <- pregnancies_infections %>% left_join(icu) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(flag_covid_test_in_stay = mother_positive_test_during_pregnancy_date %within% stay_interval) %>% # TRUE if +ve test taken during the stay
  mutate(flag_covid_diag_in_stay = case_when(prime_diag_unit %in% covid_diags ~1,
                                             oth_diag1  %in% covid_diags ~1,
                                             oth_diag2  %in% covid_diags ~1,
                                             oth_diag3  %in% covid_diags ~1,
                                             oth_diag4  %in% covid_diags ~1,
                                             oth_diag5  %in% covid_diags ~1,
                                             oth_diag6  %in% covid_diags ~1,
                                             TRUE~0)
  ) %>%
  mutate(admission_to_test_days = mother_positive_test_during_pregnancy_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_covid_temporal_stay = if_else(admission_to_test_days >= 0 & admission_to_test_days <= covid_associated_icu_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 21 days before admission
  mutate(flag_covid_associated = if_else(flag_covid_temporal_stay == TRUE | flag_covid_test_in_stay == TRUE, TRUE, FALSE))  # TRUE if either previous flag TRUE

pregnancies_icu_1 <- pregnancies_icu %>% filter(is.na(admission_date))
pregnancies_icu_2 <- pregnancies_icu %>% filter(!is.na(admission_date)) 

pregnancies_icu_diag <- pregnancies_icu_2 %>% 
  arrange(pregnancy_id, mother_positive_test_during_pregnancy_date, admission_date) %>%
  group_by(pregnancy_id, mother_positive_test_during_pregnancy_date, flag_covid_diag_in_stay) %>% 
  slice_head(n=1) %>% ungroup %>% #select first in group
  arrange(pregnancy_id, mother_positive_test_during_pregnancy_date, -flag_covid_diag_in_stay) %>%
  group_by(pregnancy_id, mother_positive_test_during_pregnancy_date) %>% slice_max(flag_covid_diag_in_stay) %>%
  ungroup()

pregnancies_icu <- rbind(pregnancies_icu_1, pregnancies_icu_diag)
##sort NA values
pregnancies_icu <- pregnancies_icu %>% 
  mutate(flag_covid_associated = ifelse(is.na(flag_covid_diag_in_stay), FALSE, flag_covid_diag_in_stay)) %>%
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  mutate(simd = ifelse(is.na(simd), 9, simd)) #%>%


saveRDS(pregnancies_icu, paste0(folder_temp_data, "variants_analysis/", "icu_diag_model_data.rds"))

vars <- c("variant_period", "simd", "gestation_at_infection",
          "vaccination_status_at_infection", "mother_age_at_conception","icu_21", "reporting_ethnicity")

vars %in% names(pregnancies_infections)

design <- svydesign(ids=~mother_upi,  data=pregnancies_icu)
#univariate
model1.1_1 <- svyglm(flag_covid_diag_in_stay ~ variant_period,
                   design = design, data=pregnancies_icu, 
                   family= binomial(link = "logit") )
summ(model1.1_1, exp=T, confint = T)



saveRDS(summ(model1.1_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_diag_unadj_OR.rds" ))

#multivariate model
#incl ethnicity
model1.1b <- svyglm(flag_covid_associated~ variant_period + simd +gestation_at_infection+ 
                    vaccination_status_at_infection + mother_age_at_conception + ethnicity_reporting, 
                  design = design, data=pregnancies_icu, 
                  family= binomial(link = "logit") )


summ(model1.1b, exp=T, confint = T)
saveRDS(summ(model1.1b, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_diag_model_summary_ethn.rds" ))


##interacitons ####

pregnancies_icu <- pregnancies_icu %>% 
  mutate(any_vacc = ifelse(vaccination_status_at_infection != "0 - Unvaccinated", 1,0))

design <- svydesign(ids=~mother_upi,  data=pregnancies_icu)

model1.1bii <- svyglm(flag_covid_diag_in_stay~ variant_period*any_vacc + simd +gestation_at_infection + 
                      mother_age_at_conception + ethnicity_reporting, 
                    design = design, data=pregnancies_icu, 
                    family= binomial(link = "logit") )
summary(model1.1bii)
summ(model1.1bii, exp=T, confint = T)

table(pregnancies_icu$flag_covid_associated,pregnancies_icu$vaccination_status_at_infection, pregnancies_icu$variant_period )
saveRDS(summ(model1.1bii, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "icu_diag_model_summary_interaction2.rds" ))


##########################
#pregnancy outcomes ####
#########################
#preterm####
#counting live preterm only
#limit denominator to 20-36 wks infection
#limit denominator to those with pregnancies surviving 28+ beyond infection (i.e. live outcome or ongoing)
fetus_preterm_denom <- fetus_infections %>% 
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  filter(gestation_at_infection %in% 20:36) %>% # select only infectiosn in gestation window for preterm
  mutate(indenom = case_when(outcome=="Ongoing" ~1 ,# remove deaths witihn 28 days of infection
                             infection_to_end >28 ~1, 
                             infection_to_end <=28 & outcome %in% c("Live birth")  ~1,
                             infection_to_end <=28 & outcome %in% c("Stillbirth", "Termination","Miscarriage", "Unknown")  ~0,
                             TRUE~3)) %>% filter(indenom==1) %>%
  mutate(flag_preterm = ifelse(x_gestation_at_outcome %in% 20:36 & outcome=="Live birth",1,0)) %>%
  mutate(flag_covid_assoc_preterm = ifelse(infection_to_end %in% 0:27 & flag_preterm==1,1,0))

saveRDS(fetus_preterm_denom , paste0(folder_temp_data, "variants_analysis/", "preterm_model_data.rds"))


design <- svydesign(data=fetus_preterm_denom, 
                    ids=~pregnancy_id)

model2_1 <- svyglm(flag_covid_assoc_preterm ~ variant_period , data=fetus_preterm_denom, design = design,
         family= binomial(link = "logit") )
summ(model2_1)
saveRDS(summ(model2_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "preterm_unadj_OR.rds" ))



model2a <- 
  svyglm(flag_covid_assoc_preterm ~ variant_period + x_simd + vaccination_status_at_infection  +gestation_at_infection  + 
           x_mother_age_at_conception + ethnicity_reporting, design = design, data=fetus_preterm_denom, 
         family= binomial(link = "logit") )
summ(model2a,  exp=T, confint = T)

saveRDS(summ(model2a, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "preterm_model_summary_eth.rds" ))

##interactions
fetus_preterm_denom  <-fetus_preterm_denom  %>% 
  mutate(any_vacc = ifelse(vaccination_status_at_infection != "0 - Unvaccinated", 1,0))
design <- svydesign(data=fetus_preterm_denom, 
                    ids=~pregnancy_id)

model2bii <- svyglm(flag_covid_assoc_preterm ~ variant_period*any_vacc +
                        x_simd +
                       gestation_at_infection  + 
                       x_mother_age_at_conception + ethnicity_reporting, design = design, data=fetus_preterm_denom, 
                     family= binomial(link = "logit") )


  summ(model2bii, exp=T, confint = T)
saveRDS(summ(model2bii, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "preterm_model_interactions2_summary.rds" ))



#Stillbirths####
all_births <- fetus_infections %>% 
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  filter(outcome== "Live birth" | outcome=="Stillbirth" |(outcome=="Miscarriage" & x_gestation_at_outcome >=20) ) %>% # births only
  filter(mother_positive_test_during_pregnancy_date >=delta_start & mother_positive_test_during_pregnancy_date <= infections_cutoff ) %>%   
  filter(infection_to_end %in% 0:27 ) %>%# only outcome within 28 days of test
  mutate(stillbirth = case_when(outcome=="Live birth" ~0,
                                outcome=="Stillbirth" ~1, 
                                outcome=="Miscarriage" ~1) )#

saveRDS(all_births , paste0(folder_temp_data, "variants_analysis/", "stillbirth_model_data.rds"))
linked_stillbirths<- all_births %>% filter(stillbirth==1)


table(all_births$ethnicity_reporting, all_births$outcome) # all stillbirths white, so we wont get good estimates for ethnicity

design <- svydesign(ids=~pregnancy_id,  data=all_births)
model3_1 <-  svyglm(stillbirth~ variant_period,
                    design = design, data=all_births, 
                    family= binomial(link = "logit") )
summ(model3_1, exp=T)
saveRDS(summ(model3_1,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "stillb_unadj_OR.rds" ))

model3 <-  svyglm(stillbirth~ variant_period + x_simd + vaccination_status_at_infection +gestation_at_infection 
                   + x_mother_age_at_conception,
                   design = design, data=all_births, 
                   family= binomial(link = "logit") )

saveRDS(summ(model3,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "stillbirth_model_summary.rds" ))


#neonatal deaths####

#all live births
all_live_births <- fetus_infections %>% filter(outcome== "Live birth" ) %>% # live births only
  filter(mother_positive_test_during_pregnancy_date >=delta_start) %>% # test in time period
  filter(infection_to_end %in% 0:27 ) %>% # only births within 28 days of test
  mutate(NND = case_when(x_neonatal_death %in% c("Early neonatal death (d0-6)", "Late neonatal death (d7-27)" ) ~1, # flag nnd y/n
                         x_neonatal_death=="Survived neonatal period" ~0))
#only possible for ONE test to be within 28 days so this is unambiguous
saveRDS(all_live_births , paste0(folder_temp_data, "variants_analysis/", "NND_model_data.rds"))

table(all_live_births$NND, all_live_births$variant_period, useNA="always")
table(all_live_births$NND, all_live_births$ethnicity_reporting, useNA="always")#

design <- svydesign(ids=~pregnancy_id,  data=all_live_births)

model4_1 <-  svyglm(NND~ variant_period,
                  design = design, data=all_live_births, 
                  family= binomial(link = "logit") )
summ(model4_1)
saveRDS(summ(model4_1,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "NND_unadj_OR.rds" ))


##excl. ethnicity numbers too low
model4a <-  svyglm(NND~ variant_period + x_simd + vaccination_status_at_infection +gestation_at_infection  + x_mother_age_at_outcome,
                  design = design, data=all_live_births, 
                  family= binomial(link = "logit") )
summary(model4a)
saveRDS(summ(model4a,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "NND_model_summary.rds" ))

#Low apgar####
#only live births at term included
#denominator = all live term births up to 28 days after infection 

term_births <- fetus_infections %>%  filter(infection_to_end %in% 0:27) %>% # only biths within 28 days of infection
  filter(outcome== "Live birth" & x_gestation_at_outcome >=37 ) %>% # live term births only
  filter(mother_positive_test_during_pregnancy_date >= delta_start) %>% # test in time period
  mutate(smr02_apgar_5_minutes = if_else(smr02_apgar_5_minutes == "NR" | smr02_apgar_5_minutes == "RR", NA_character_, smr02_apgar_5_minutes))%>%
  mutate(low_apgar = ifelse(as.numeric(smr02_apgar_5_minutes) < 7,1,0)) # low apgar flag if <7

table(is.na(term_births$low_apgar), term_births$variant_period)
table(is.na(term_births$smr02_admission_date), term_births$variant_period)
table(term_births$low_apgar, term_births$ethnicity_reporting)

saveRDS(term_births  , paste0(folder_temp_data, "variants_analysis/", "apgar_model_data.rds"))

design <- svydesign(ids=~pregnancy_id,  data=term_births)
model5_1 <-  svyglm(low_apgar~ variant_period,
                    design = design, data=term_births, 
                    family= binomial(link = "logit") )
summ(model5_1, exp=T,  confint = T)
saveRDS(summ(model5_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "apgar_unadj_OR.rds" ))


model5 <-  svyglm(low_apgar ~ variant_period + x_simd + vaccination_status_at_infection +
                    gestation_at_infection  + x_mother_age_at_outcome+
                    ethnicity_reporting,
                  design = design, data=term_birth, 
                  family= binomial(link = "logit") )
summ(model5, exp=T, confint = T)
saveRDS(summ(model5,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "apgar_model_summary_ethn.rds" ))

readRDS(paste0(folder_temp_data, "variants_analysis/", "apgar_model_summary_ethn.rds" ))
model5a <-  svyglm(low_apgar ~ variant_period + x_simd + vaccination_status_at_infection +
                    gestation_at_infection  + x_mother_age_at_outcome,
                  design = design, data=term_birth, 
                  family= binomial(link = "logit") )
summ(model5a, exp=T, confint = T)
saveRDS(summ(model5a, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "apgar_model_summary.rds" ))

table( lubridate::month(term_births$x_pregnancy_end_date, label=T), is.na(term_births$low_apgar)) 
table( lubridate::month(term_births$x_pregnancy_end_date, label=T), is.na(term_births$smr02_admission_date)) 
##higher % missingness in omicron era.

#
missing_apgars <- term_births %>% filter(is.na(low_apgar))
table(missing_apgars$x_baby_dob[is.na(missing_apgars$smr02_live_birth)], missing_apgars$variant_period[is.na(missing_apgars$smr02_live_birth)])
table(is.na(missing_apgars$smr02_live_birth), missing_apgars$variant_period)

### neonatal infections####
#denominator = all live births up to 28 days after infection.
live_births <- fetus_infections %>%  filter(infection_to_end %in% 0:27) %>% # only biths within 28 days of infection
  filter(outcome== "Live birth" ) %>% # live term births only
  filter(mother_positive_test_during_pregnancy_date >= delta_start) %>% # test in time period
  mutate(baby_infection_time = as.Date(tests_baby_earliest_positive_test)-x_pregnancy_end_date ) %>%
  mutate(neonate_infection = case_when(baby_infection_time %in% 0:27 ~1,
                                       TRUE~0)) %>%
  mutate(early_neonate_infection = case_when(baby_infection_time %in% 0:6 ~1,  #early neonate infections as well
                                             TRUE~0))

table(live_births$neonate_infection,live_births$variant_period, useNA="always")
table(live_births$neonate_infection,live_births$ethnicity_reporting, useNA="always")
table(live_births$neonate_infection,live_births$vaccination_status_at_infection, useNA="always")

#neonatal infections
nninfections <- live_births %>% filter(neonate_infection==1) %>% group_by(variant_period) %>% summarise(n_assoc_NNIs = n())
early_nninfections <- live_births %>% filter(early_neonate_infection==1) %>% group_by(variant_period) %>% summarise(n_assoc_early_NNIs = n())
#denominator (n live births witihn 28 days of infection)
NNI_denominator <- live_births %>% group_by(variant_period) %>% summarise(n_covid_assoc_LB = n())

NNI_table <- left_join(nninfections,early_nninfections)
NNI_table <- left_join(NNI_table, NNI_denominator)

saveRDS(NNI_table, paste0(folder_temp_data, "variants_analysis/", "NNI_table.rds"))

table(live_births$neonate_infection,live_births$variant_period, useNA="always")
table(live_births$neonate_infection,live_births$ethnicity_reporting, useNA="always")
saveRDS(live_births  , paste0(folder_temp_data, "variants_analysis/", "neonate_infect_model_data.rds"))

design <- svydesign(ids=~pregnancy_id,  data=live_births)
model6_1 <-  svyglm(neonate_infection~ variant_period,
                    design = design, data=live_births, 
                    family= binomial(link = "logit") )
summ(model6_1, exp=T, confint = T)
saveRDS(summ(model6_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "NNI_unadj_OR.rds" ))


model6 <-  svyglm(neonate_infection ~ variant_period + x_simd + vaccination_status_at_infection +
                    gestation_at_infection  + x_mother_age_at_outcome,
                  design = design, data=live_birth, 
                  family= binomial(link = "logit") )
summ(model6, exp=T, confint = T)

saveRDS(summ(model6,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "NNI_model_summary.rds" ))

### EARLY neonatal infections####

table(live_births$early_neonate_infection,live_births$variant_period, useNA="always")
table(live_births$early_neonate_infection,live_births$ethnicity_reporting, useNA="always")

design <- svydesign(ids=~pregnancy_id,  data=live_births)
model7_1 <-  svyglm(early_neonate_infection~ variant_period,
                    design = design, data=live_births, 
                    family= binomial(link = "logit") )
summ(model7_1, exp=T, confint = T)
saveRDS(summ(model7_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "earlyNNI_unadj_OR.rds" ))

#numbers too low for MV analysis
model7 <-  svyglm(early_neonate_infection ~ variant_period + x_simd + vaccination_status_at_infection +
                    gestation_at_infection  + x_mother_age_at_outcome,
                  design = design, data=live_birth, 
                  family= binomial(link = "logit") )
summ(model7, exp=T, confint = T)

#saveRDS(summ(model7,  exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "earlyNNI_model_summary.rds" ))
