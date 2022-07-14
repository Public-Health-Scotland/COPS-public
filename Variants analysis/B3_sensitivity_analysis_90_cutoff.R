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
path_to_scripts <- file.path(paste(here::here(), "Analysis code and metadata", "COPS Code/", sep = "/"))

#Setup, read in packages and get folder directories
source(file.path(paste0(path_to_scripts,"Ad-Hoc-Analysis/Outcomes_by_variant/0.Setup for variants analysis.R"))) 

#packageurl <- "http://cran.r-project.org/src/contrib/Archive/jtools/jtools_2.1.2.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
# had to reinstall pander first

vaccination_status <- function(dose_1_date, dose_2_date, dose_3_date, reference_date){
  one_dose_date <- dose_1_date + days(21)
  
  two_dose_date <- dose_2_date + days(14)
  
  three_dose_date <- dose_3_date + days(14)
  
  #four_dose_date <- dose_4_date + days(14)
  
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




###Sensitivity analysis 90% cutoffs####
delta_start90 <- as.Date("2021-06-12")
delta_end90 <- as.Date("2021-12-07")

omicron_start90 <- as.Date("2021-12-27")
omicron_end <- as.Date("2022-01-31")

##setup####
####Data prep - long format - each infection (in pregnancy) as a line
# prep vacc status and time to end of pregnancy, from infection date.
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) 


##################
fetus_infections <- fetuslevel %>% filter(tests_mother_positive_test_during_pregnancy==1 & 
                                            (tests_mother_value_positive_test_during_pregnancy_1 >=delta_start90 |
                                               tests_mother_value_positive_test_during_pregnancy_1>=delta_start90 ) ) %>%
  select(pregnancy_id, mother_upi, baby_upi, tests_mother_value_positive_test_during_pregnancy_1, tests_mother_value_positive_test_during_pregnancy_2) %>% 
  pivot_longer(cols = c(tests_mother_value_positive_test_during_pregnancy_1, tests_mother_value_positive_test_during_pregnancy_2), 
               names_to = "test_number", values_to = "mother_positive_test_during_pregnancy_date") %>%
  filter(!is.na(mother_positive_test_during_pregnancy_date)) %>% 
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date >=omicron_start90 & mother_positive_test_during_pregnancy_date < infections_cutoff ~ "Omicron",
                                    mother_positive_test_during_pregnancy_date >=delta_start90 & mother_positive_test_during_pregnancy_date <= delta_end ~ "Delta")) %>%
  filter(variant_period== "Delta" |variant_period== "Omicron") %>%
  unique() # unique values - for multiple pregnancies with stillbirths, no baby upi can end up with multiple identical records when reduced to prg.mother,baby ID plus the infection date.
# need to reduce to unqiue lines before adding back on , else ends up duplicaiting rows


summary(fetus_infections$mother_positive_test_during_pregnancy_date)
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

#select pregnancies w infections only. limit to infection >June 2021
#and make into long file with 2nd row for those with 2 infections in prengancy 
pregnancies_infections <- pregnancies %>% filter(mother_tested_positive_during_pregnancy==1 & 
                                                   (mother_positive_test_during_pregnancy_1 >=delta_start90 |
                                                      mother_positive_test_during_pregnancy_2 >=delta_start90 ) ) %>%
  select(pregnancy_id, mother_upi, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>% 
  pivot_longer(cols = c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2), 
               names_to = "test_number", values_to = "mother_positive_test_during_pregnancy_date") %>%
  filter(!is.na(mother_positive_test_during_pregnancy_date)) %>% 
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date >=omicron_start90 & mother_positive_test_during_pregnancy_date <= infections_cutoff ~ "Omicron",
                                    mother_positive_test_during_pregnancy_date >=delta_start90 & mother_positive_test_during_pregnancy_date <= delta_end90 ~ "Delta")) %>%
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


#######################################
#demographics ####
########################################
#gestation  @ infection
pregnancies_infections <-  pregnancies_infections %>% 
  mutate(trimester = trimester(est_conception_date, mother_positive_test_during_pregnancy_date)) %>%
  mutate(gestation_days = 14 + (est_conception_date %--% mother_positive_test_during_pregnancy_date / days(1))) %>%
  mutate(gestation_at_infection = floor(gestation_days/7))

pregnancies_infections <- pregnancies_infections %>% mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  filter(variant_period == "Delta" | variant_period=="Omicron")

age_summary <- pregnancies_infections %>% group_by(variant_period, maternal_age_group) %>% summarise(n_infections = n()) %>% 
  ungroup() %>%
  mutate(var="Maternal age group") %>%
  rename(level = maternal_age_group)
#ethnicity
pregnancies_infections <-pregnancies_infections %>%  mutate(ethnicity_desc_reporting = cops_reporting_ethnicity(ethnicity_code)) 

eth_summary <- pregnancies_infections %>% group_by(variant_period, ethnicity_desc_reporting) %>% summarise(n_infections = n()) %>% 
  ungroup() %>%
  mutate(var="Maternal ethnicity") %>%
  rename(level = ethnicity_desc_reporting)

#simd
simd_summary <- pregnancies_infections %>% 
  mutate(simd = ifelse(!is.na(simd) & simd==9, NA, simd)) %>%
  group_by(variant_period, simd) %>% summarise(n_infections = n()) %>%   ungroup() %>%
  mutate(var="SIMD") %>%
  rename(level = simd)

#comorbs
cv_summary <- pregnancies_infections %>% group_by(variant_period, cv_clinical_vulnerability_category) %>% summarise(n_infections = n())%>%   ungroup() %>%
  mutate(var="clinical vulnerability") %>%
  rename(level = cv_clinical_vulnerability_category)
diabetes <- pregnancies_infections %>% group_by(variant_period, diabetes) %>% summarise(n_infections = n())%>%   ungroup() %>%
  mutate(var="Diabetes") %>%
  rename(level = diabetes)

#urban rural
UR <-  pregnancies_infections %>%
  group_by(variant_period, urban_rural_description) %>% 
  summarise(n_infections = n())%>%   ungroup() %>%
  mutate(var="Urban rural") %>%
  rename(level = urban_rural_description)

### Pregnancy attributes####
#outcomes
outcome_summary <-  pregnancies_infections %>%
  group_by(variant_period, overall_outcome) %>% 
  summarise(n_infections = n()) %>% ungroup() %>%
  mutate(var="outcome") %>%
  rename(level = overall_outcome)
# n fetuses
n_babies <-  pregnancies_infections %>%
  group_by(variant_period, births_this_pregnancy) %>% 
  summarise(n_infections = n()) %>%ungroup() %>%
  mutate(var="N fetuses") %>%
  rename(level = births_this_pregnancy)
#vacc status @ infection
pregnancies_infections <- pregnancies_infections %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy_date))

vacc_status <-  pregnancies_infections %>%
  group_by(variant_period, vaccination_status_at_infection) %>% 
  summarise(n_infections = n()) %>% ungroup() %>%
  mutate(var="Vaccination status") %>%
  rename(level = vaccination_status_at_infection)

weeks_summary <-  pregnancies_infections %>%
  group_by(variant_period, gestation_at_infection) %>% 
  summarise(n_infections = n())  %>% ungroup() %>%
  mutate(var="gestation_at_infection") %>%
  rename(level =gestation_at_infection)

trimester_summary <-  pregnancies_infections %>%
  group_by(variant_period, trimester) %>% 
  summarise(n_infections = n())  %>% ungroup() %>%
  mutate(var="trimester_at_infection") %>%
  rename(level = trimester)




demographics_summaries <- rbind(simd_summary,eth_summary , UR, age_summary, cv_summary, diabetes,
                                trimester_summary, weeks_summary,  vacc_status, n_babies, outcome_summary) 
demographics_summaries <-
  demographics_summaries %>% 
  pivot_wider(id_cols= c(var,level), names_from = variant_period, values_from = n_infections)# %>% select(-`NA`)
saveRDS(demographics_summaries, paste0(folder_temp_data,  "variants_analysis/sensistivity_analysis/","sens90_variant_demogs1_no_transition.rds"))



##ICU admission - for covid ####
#cox or just logistic? don't really care about time-to-failure, just Y/N within 21 days - so logistic may be more appropriate.
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


#check ICU data for covid admissions without an assocuated test
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
  filter(admission_date >= delta_start90) %>%
  mutate(variant_period = case_when(admission_date >=delta_start90 & admission_date <= delta_end90 ~ "Delta",
                                    admission_date >=omicron_start90 & admission_date <= infections_cutoff ~ "Omicron",
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


check <- pregnancies_icu_associated %>% group_by(pregnancy_id, mother_positive_test_during_pregnancy_date) %>% 
  summarise(count= sum(flag_covid_associated))
table(check$count)
#can only be one covid associated stay per test,so 
pregnancies_icu_21 <- rbind(pregnancies_icu_1, pregnancies_icu_associated)
##sort NA values
pregnancies_icu_21 <- pregnancies_icu_21 %>% 
  mutate(flag_covid_associated = ifelse(is.na(flag_covid_associated), FALSE, flag_covid_associated)) %>%
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  mutate(simd = ifelse(is.na(simd), 9, simd)) #%>%

pregnancies_icu_21 <- pregnancies_icu_21 %>% 
  filter(mother_positive_test_during_pregnancy_date >= delta_start90 & mother_positive_test_during_pregnancy_date <= omicron_end) %>%
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date <= delta_end90 ~ "Delta", 
                                    mother_positive_test_during_pregnancy_date >= omicron_start90 &
                                      mother_positive_test_during_pregnancy_date <= omicron_end ~ "Omicron", 
                                    TRUE ~ "transition")) %>% 
  filter(variant_period !="transition")
#%>%

##ICU regression 21 days####
saveRDS(pregnancies_icu_21, paste0(folder_temp_data, "variants_analysis/", "sens90_icu_model_data.rds"))

model1 <- glm(flag_covid_associated ~ variant_period + simd + vaccination_status_at_infection  +gestation_at_infection+
                mother_age_at_conception, data=pregnancies_icu_21, 
              family= binomial(link = "logit") )
summary(model1)
plot(model1)
table(pregnancies_icu_21$flag_covid_associated , pregnancies_icu_21$variant_period, useNA="always")

vars <- c("variant_period", "simd", "gestation_at_infection",
          "vaccination_status_at_infection", "mother_age_at_outcome","icu_21", "reporting_ethnicity")


design <- svydesign(ids=~mother_upi,  data=pregnancies_icu_21)

model1_1 <- svyglm(flag_covid_associated ~ variant_period,
                   design = design, data=pregnancies_icu_21, 
                   family= binomial(link = "logit") )

summ(model1_1, exp=T, confint = T)

saveRDS(summ(model1_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90icu_unadj_OR.rds" ))
table(pregnancies_icu_21$flag_covid_associated, pregnancies_icu_21$variant_period)

model1a <- svyglm(flag_covid_associated~ variant_period + simd +gestation_at_infection+ 
                    vaccination_status_at_infection + mother_age_at_conception, 
                  design = design, data=pregnancies_icu_21, 
                  family= binomial(link = "logit") )

summ(model1a, exp=T, confint = T)
saveRDS(summ(model1a, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90icu_model_summary.rds" ))

model1b <- svyglm(flag_covid_associated~ variant_period + simd +gestation_at_infection+ 
                    vaccination_status_at_infection + mother_age_at_conception + ethnicity_reporting, 
                  design = design, data=pregnancies_icu_21, 
                  family= binomial(link = "logit") )

summ(model1b, exp=T, confint = T)
saveRDS(summ(model1b, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90icu_model_summary_ethn.rds" ))

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


check <- pregnancies_icu_diag %>% group_by(pregnancy_id, mother_positive_test_during_pregnancy_date) %>% 
  summarise(count= sum(flag_covid_diag_in_stay))
table(check$count)
#can only be one covid associated stay per test,so 
pregnancies_icu <- rbind(pregnancies_icu_1, pregnancies_icu_diag)
##sort NA values
pregnancies_icu <- pregnancies_icu %>% 
  mutate(flag_covid_associated = ifelse(is.na(flag_covid_diag_in_stay), FALSE, flag_covid_diag_in_stay)) %>%
  mutate(ethnicity_reporting = ifelse(is.na(ethnicity_reporting), "5 Unknown/missing", ethnicity_reporting)) %>%
  mutate(simd = ifelse(is.na(simd), 9, simd)) #%>%


table(pregnancies_icu$flag_covid_diag_in_stay, pregnancies_icu$variant_period, useNA="always")

saveRDS(pregnancies_icu, paste0(folder_temp_data, "variants_analysis/", "sens90_icu_diag_model_data.rds"))

model1.1 <- glm(flag_covid_diag_in_stay ~ variant_period + simd + vaccination_status_at_infection  +gestation_at_infection+
                  mother_age_at_conception, data=pregnancies_icu, 
                family= binomial(link = "logit") )

#plot(model1)
table(pregnancies_icu$flag_covid_diag_in_stay , pregnancies_icu$variant_period, useNA="always")

vars <- c("variant_period", "simd", "gestation_at_infection",
          "vaccination_status_at_infection", "mother_age_at_conception","icu_21", "reporting_ethnicity")

vars %in% names(pregnancies_infections)
design <- svydesign(ids=~mother_upi,  data=pregnancies_icu)

model1.1_1 <- svyglm(flag_covid_diag_in_stay ~ variant_period,
                     design = design, data=pregnancies_icu, 
                     family= binomial(link = "logit") )
summ(model1.1_1, exp=T, confint = T)



saveRDS(summ(model1.1_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90_icu_diag_unadj_OR.rds" ))
table(pregnancies_icu$flag_covid_diag_in_stay, pregnancies_icu$variant_period)

model1.1a <- svyglm(flag_covid_diag_in_stay~ variant_period + simd +gestation_at_infection+ 
                      vaccination_status_at_infection + mother_age_at_conception, 
                    design = design, data=pregnancies_icu, 
                    family= binomial(link = "logit") )
summary(model1.1a)
names(summary(model1.1a))
summ(model1.1a, exp=T, confint = T)
saveRDS(summ(model1.1a, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90_sens90icu_diag_model_summary.rds" ))

model1.1b <- svyglm(flag_covid_associated~ variant_period + simd +gestation_at_infection+ 
                      vaccination_status_at_infection + mother_age_at_conception + ethnicity_reporting, 
                    design = design, data=pregnancies_icu, 
                    family= binomial(link = "logit") )
summary(model1.1b)
names(summary(model1.1b))
summ(model1.1b, exp=T, confint = T)
saveRDS(summ(model1.1b, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90icu_diag_model_summary_ethn.rds" ))





#pregnancy outcomes ####

#preterm####
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

fetus_preterm_denom <- fetus_preterm_denom %>%
  filter(mother_positive_test_during_pregnancy_date >= delta_start90 & mother_positive_test_during_pregnancy_date <= omicron_end) %>%
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date <= delta_end90 ~ "Delta", 
                                    mother_positive_test_during_pregnancy_date >= omicron_start90 &
                                      mother_positive_test_during_pregnancy_date <= omicron_end ~ "Omicron", 
                                    TRUE ~ "transition")) %>% 
  filter(variant_period !="transition")
saveRDS(fetus_preterm_denom , paste0(folder_temp_data, "variants_analysis/", "sens90preterm_model_data.rds"))

table(fetus_preterm_denom$ethnicity_reporting, fetus_preterm_denom$flag_covid_assoc_preterm)
table(fetus_preterm_denom$x_births_this_pregnancy)
table(fetus_preterm_denom$flag_covid_assoc_preterm, fetus_preterm_denom$variant_period)

design <- svydesign(data=fetus_preterm_denom, 
                    ids=~pregnancy_id)

model2_1 <- svyglm(flag_covid_assoc_preterm ~ variant_period , data=fetus_preterm_denom, design = design,
                   family= binomial(link = "logit") )
summ(model2_1, exp=T, confint = T)
saveRDS(summ(model2_1, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90preterm_unadj_OR.rds" ))

#RDS::summary(model2_1, odds=TRUE)

model2 <- 
  svyglm(flag_covid_assoc_preterm ~ variant_period + x_simd + vaccination_status_at_infection
         +gestation_at_infection  + x_mother_age_at_conception, design = design, data=fetus_preterm_denom, 
         family= binomial(link = "logit") )
summ(model2,exp=T, confint = T)
saveRDS(summ(model2,exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90preterm_model_summary.rds" ))


model2a <- 
  svyglm(flag_covid_assoc_preterm ~ variant_period + x_simd + vaccination_status_at_infection  +gestation_at_infection  + 
           x_mother_age_at_conception + ethnicity_reporting, design = design, data=fetus_preterm_denom, 
         family= binomial(link = "logit") )
summ(model2a,  exp=T, confint = T)

saveRDS(summ(model2a, exp=T, confint = T),paste0(folder_temp_data, "variants_analysis/", "sens90preterm_model_summary_eth.rds" ))

