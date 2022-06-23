 ###Demographics of infections in cohort by time period####
#- in relation to dominance of Delta/Omicron variants
#setup####
 path_to_scripts <- file.path(paste(here::here(), "Analysis code and metadata", "COPS Code/", sep = "/"))
 
 #Setup, read in packages and get folder directories
source(file.path(paste0(path_to_scripts,"Ad-Hoc-Analysis/Outcomes_by_variant/0.Setup for variants analysis.R"))) 

library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(haven)
 library(hablar)
 delta_start <- as.Date("2021-05-17") #>90% S+ (changing from S- alpha )  - actually think it was earlier than this but need to check
 delta_end <- as.Date("2021-12-14")
 #transition_start <- as.Date("2021-12-09")# Date where >10% were S-
 #transition_end <- as.Date("2022-01-04")
 omicron_start <- as.Date("2021-12-15") #date of >90% S-
 omicron_end <- as.Date("2022-01-31") 
 
 publication_latest_vacc_date <- lubridate::ymd("2022-01-31")  

 publication_latest_vacc_date <- as.Date("2022-03-31")
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
 
 
 cops_age_group <- function(age_variable) {case_when(age_variable >= 11 & age_variable <= 19 ~ " 1 <= 19", 
                                                     age_variable >= 20 & age_variable <= 24 ~ "2 20-24", 
                                                     age_variable >= 25 & age_variable <= 29 ~ "3 25-29",
                                                     age_variable >= 30 & age_variable <= 34 ~ "4 30-34",
                                                     age_variable >= 35 & age_variable <= 39 ~ "5 35-39",
                                                     age_variable >= 40 & age_variable <= 55 ~ "7 >=40", TRUE ~ "8 Unknown")}
 
 vaccination_status <- function(dose_1_date, dose_2_date, dose_3_date, reference_date){
  one_dose_date <- dose_1_date + days(21)
  
  two_dose_date <- dose_2_date + days(14)
  
  three_dose_date <- dose_3_date + days(14)
  
  case_when(reference_date < one_dose_date | is.na(dose_1_date) ~ "0 - Unvaccinated",
            reference_date >= one_dose_date & 
              (reference_date < two_dose_date | is.na(two_dose_date)) ~ "1 - One dose",
            reference_date >= two_dose_date & 
              (reference_date < three_dose_date | is.na(three_dose_date)) ~ "2 - Two doses",
            reference_date >= three_dose_date ~ "3 - Three doses")
  
}
trimester <- function(est_conception_date, reference_date){
  # assigns trimester category based on estimated conception date and reference date.
  # Note: est_conception_date is defined as two weeks gestation
  # Note: will return NA if e.g. reference date is NA so take care using with ongoing pregnancies etc
  
  gestation_days <- 14 + (est_conception_date %--% reference_date / days(1))
  
  trimester <- case_when(gestation_days < 14 ~ "invalid",  # indicates reference date is before est_conception_date
                         gestation_days < (14*7) ~ "1 - first-trimester",
                         gestation_days < (28*7) ~ "2 - second-trimester",
                         gestation_days >= (28*7) ~ "3 - third-trimester",
                         TRUE ~ NA_character_)
  return(trimester)
}

##check current status of tests, sgene etc####
wgs_tests <- readRDS(paste0(folder_temp_data,  "wgs_tests.rds")) 



str(tests_pcr)# this is sourced in setup


tests_pcr <-tests_pcr %>%
  mutate(date_ecoss_specimen = as.Date(date_ecoss_specimen)) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi))) %>%
  mutate(test_type = "PCR")

wgs_with_date <- left_join(wgs_tests, select(tests_pcr, specimen_id, date_ecoss_specimen))

lineage_series <- wgs_with_date %>% group_by(date_ecoss_specimen, genomic_lineage) %>% summarise(count=n()) %>% 
  pivot_wider(names_from =genomic_lineage,  values_from = count)

variant_series <- wgs_with_date %>% group_by(date_ecoss_specimen, variant_of_interest)%>% 
  summarise(count=n()) %>% 
  pivot_wider(names_from = variant_of_interest,  values_from = count)
variant_series <- variant_series %>% filter(!is.na(date_ecoss_specimen))
saveRDS(variant_series, paste0(folder_temp_data, "wgs_variants_by_date.rds"))

##
variant_series <- readRDS(paste0(folder_temp_data, "wgs_variants_by_date.rds"))
#further development - include details from PCR following an LFD
tests_sgene <- tests_pcr %>%
  mutate(across(ends_with("target_gene"), ~gsub("[^[:alnum:]]", "", .))) %>% 
  mutate(across(ends_with("target_gene"), ~case_when(str_detect(., "ORF1AB") == TRUE ~ "ORF1AB",
                                                     str_detect(., "NGENE") == TRUE ~ "NGENE",
                                                     str_detect(., "SGENE") == TRUE ~ "SGENE",
                                                     str_detect(., "MS2") == TRUE ~ "MS2",
                                                     TRUE ~ .))) %>%
  distinct() %>% 
  pivot_longer(starts_with("test_ch")) %>%
  # for matching ct values to gene names, extract e.g. number 1 from "test_ch1_result_ct_value" and "test_ch1_target_gene"
 mutate(gene_test_id = str_extract(name, "\\d+")) %>%
  mutate(type = case_when(str_detect(name, "ct_value") == TRUE ~ "ct_value",
                          (str_detect(name, "target_gene") == TRUE & str_detect(name, "result") == FALSE) ~ "gene_type",
                          str_detect(name, "target_gene_result") == TRUE ~ "gene_result",
                          TRUE ~ NA_character_)) %>%
  pivot_wider(c(subject_upi,specimen_id,date_ecoss_specimen,  gene_test_id), names_from = type, values_from = value) %>% 
  filter(is.na(gene_type) == FALSE) %>% 
  pivot_wider(c(subject_upi, date_ecoss_specimen,  specimen_id), names_from = c(gene_type), values_from = c(ct_value, gene_result)) %>%
  mutate(sgene_classification = case_when(gene_result_SGENE == "NEGATIVE" & (ct_value_ORF1AB <= 30 | ct_value_NGENE <= 30) ~ "True S Gene Dropout",
                                          gene_result_SGENE == "NEGATIVE" & (is.na(ct_value_ORF1AB) & is.na(ct_value_NGENE)) ~ "No Ct Values",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_ORF1AB > 30 & ct_value_NGENE > 30 ~ "Weak Positive",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_ORF1AB > 30 & is.na(ct_value_NGENE) ~ "Weak Positive",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_NGENE > 30 & is.na(ct_value_ORF1AB) ~ "Weak Positive",
                                          gene_result_SGENE == "POSITIVE" ~ "Positive S Gene",
                                          TRUE ~ "other"))
#tests_sgene <- tests_sgene %>%
#select(upi, specimen_id, covid_infection, sgene_classification)

sgene <- tests_sgene %>% 
group_by(as.Date(date_ecoss_specimen), sgene_classification) %>%
summarise(count=n())#

sgene <-sgene %>% pivot_wider(`as.Date(date_ecoss_specimen)`, names_from = sgene_classification, values_from = count)
sgene$perc_pos <- sgene$`Positive S Gene`/( sgene$`Positive S Gene`+sgene$`True S Gene Dropout`)

saveRDS(sgene, paste0(folder_temp_data, "sgene_Apr21Apr22.rds"))

#setup####
sgene <- readRDS(paste0(folder_temp_data, "sgene_Apr21Apr22.rds"))
sgene <- sgene %>% replace_na(list(other=0, `Positive S Gene`=0, `True S Gene Dropout`=0, `Weak Positive`=0, `No Ct Values`=0)) %>%
  mutate(per_ct_values = 1- (`No Ct Values`/(other+ `Positive S Gene` +`True S Gene Dropout`+ `Weak Positive`+ `No Ct Values`))) %>%
  mutate(per_no_sgene_status = 1- ((`Weak Positive`+ `No Ct Values`+other)/ 
                                     (other+ `Positive S Gene` +`True S Gene Dropout`+ `Weak Positive`+ `No Ct Values`) ) ) 

sgene_summary <- sgene %>%
  filter(`as.Date(date_ecoss_specimen)` >= delta_start & `as.Date(date_ecoss_specimen)`  <= omicron_end ) %>%
  mutate(group=1) %>% group_by(group) %>%
  summarise(other = sum(other) , `Positive S Gene` = sum(`Positive S Gene`), `True S Gene Dropout` = sum(`True S Gene Dropout`), 
           `Weak Positive` = sum(`Weak Positive`), `No Ct Values`  = sum(`No Ct Values`) ) %>%
  mutate(per_ct_values = 1- (`No Ct Values`/(other+ `Positive S Gene` +`True S Gene Dropout`+ `Weak Positive`+ `No Ct Values`))) %>%
  mutate(per_no_sgene_status = 1- ((`Weak Positive`+ `No Ct Values`+other)/ 
                                     (other+ `Positive S Gene` +`True S Gene Dropout`+ `Weak Positive`+ `No Ct Values`) ) ) 

                                  
folder_temp_data              <- "//PHI_conf/COPS/99-temp/"
#set dates for periods
delta_start <- as.Date("2021-05-17") #>90% S+ (changing from S- alpha )  - actually think it was earlier than this but need to check
delta_end <- as.Date("2021-12-14")
#transition_start <- as.Date("2021-12-09")# Date where >10% were S-
#transition_end <- as.Date("2022-01-04")
omicron_start <- as.Date("2021-12-15") #date of >90% S-

infections_cutoff <- as.Date("2022-02-01")

####
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) 
pregnancies <-readRDS(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds"))

#select pregnancies w infections only. limit to infection >June 2021
#and make into long file with 2nd row for those with 2 infections in prengancy 
pregnancies_infections <- pregnancies %>% filter(mother_tested_positive_during_pregnancy==1) %>%
    select(pregnancy_id, mother_upi, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>% 
  pivot_longer(cols = c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2), 
               names_to = "test_number", values_to = "mother_positive_test_during_pregnancy_date") %>%
  filter(!is.na(mother_positive_test_during_pregnancy_date)) %>% 
  mutate(variant_period = case_when(mother_positive_test_during_pregnancy_date >=omicron_start & mother_positive_test_during_pregnancy_date< infections_cutoff~ "Omicron",
                                    mother_positive_test_during_pregnancy_date >=infections_cutoff~ "After cutoff",
    mother_positive_test_during_pregnancy_date >=delta_start & mother_positive_test_during_pregnancy_date <= delta_end ~ "Delta"))
table(pregnancies_infections$variant_period)
#join back to main pregnancies data.

pregnancies <- pregnancies %>% select(-c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) 

pregnancies_infections <- left_join(pregnancies_infections, pregnancies)
#gestation  @ infection
pregnancies_infections <-  pregnancies_infections %>% 
  mutate(trimester = trimester(est_conception_date, mother_positive_test_during_pregnancy_date)) %>%
  mutate(gestation_days = 14 + (est_conception_date %--% mother_positive_test_during_pregnancy_date / days(1))) %>%
  mutate(gestation_at_infection = floor(gestation_days/7))



trimester_monthly_summary <-  pregnancies_infections %>%
  mutate(week_end = ceiling_date(mother_positive_test_during_pregnancy_date, unit = "week", change_on_boundary = F)) %>%
  mutate(month_in_words = format(as.Date(week_end), "%b %Y")) %>%
  mutate(month = format(as.Date(week_end), "%Y-%m")) %>%
  group_by( month,month_in_words, trimester) %>% 
  summarise(n_infections = n())  %>% ungroup() %>%
  mutate(var="trimester") %>%
  rename(level = trimester)

saveRDS(trimester_monthly_summary, paste0(folder_temp_data, "trimester_month_infect_summary.rds"))

#pregnancies_infections <-pregnancies_infections  %>% 
#  filter( mother_positive_test_during_pregnancy_1 >=delta_start | mother_positive_test_during_pregnancy_2 >=delta_start  ) 

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


##NNI rates####
neonate_infections <- fetuslevel %>% 
  filter(outcome== "Live birth" ) %>% # live births only
  filter(x_pregnancy_end_date >= as.Date("2021-04-01")) %>% #start at 1st june rather than delta start
  mutate(baby_infection_time = as.Date(tests_baby_earliest_positive_test)-x_pregnancy_end_date ) %>%
  mutate(neonate_infection = case_when(baby_infection_time %in% 0:27 ~1,
                                       TRUE~0)) %>%
  mutate(variant_period_birth = case_when(x_pregnancy_end_date >= delta_start & x_pregnancy_end_date<= delta_end ~ "Delta", 
                                          x_pregnancy_end_date > delta_end & x_pregnancy_end_date<= infections_cutoff ~ "Omicron" ))

NNI_rate <- neonate_infections %>%
  mutate(week_end_birth = ceiling_date(x_pregnancy_end_date, unit = "week", change_on_boundary = F)) %>%
  mutate(month_in_words = format(as.Date(week_end_birth), "%b %Y")) %>%
  mutate(month_born = format(as.Date(week_end_birth), "%Y-%m")) %>%
  group_by( month_born, month_in_words) %>% 
  summarise(count_live_births = n(), neonate_infections = sum(neonate_infection)) %>% ungroup() %>%
  mutate(NNI_rate_1000 = neonate_infections/count_live_births * 1000)
 
saveRDS(NNI_rate,paste0(folder_temp_data, "variants_analysis/", "Neonate_infect_monthly_table.rds"))
 ###NNI rate cohort only

neonate_infections_cohort <- fetus_infections %>% 
  filter(outcome== "Live birth" ) %>% # live births only
  filter(mother_positive_test_during_pregnancy_date >=delta_start & mother_positive_test_during_pregnancy_date <=infections_cutoff) %>% # live births only
   mutate(baby_infection_time = as.Date(tests_baby_earliest_positive_test)-x_pregnancy_end_date ) %>%
  mutate(neonate_infection = case_when(baby_infection_time %in% 0:27 ~1,
                                       TRUE~0)) %>%
  mutate(variant_period_birth = case_when(x_pregnancy_end_date >= delta_start & x_pregnancy_end_date<= delta_end ~ "Delta", 
                                          x_pregnancy_end_date > delta_end & x_pregnancy_end_date<= infections_cutoff ~ "Omicron" ))

NNI_rate_cohort <- neonate_infections_cohort %>%
  mutate(week_end_birth = ceiling_date(x_pregnancy_end_date, unit = "week", change_on_boundary = F)) %>%
  mutate(month_in_words = format(as.Date(week_end_birth), "%b %Y")) %>%
  mutate(month_born = format(as.Date(week_end_birth), "%Y-%m")) %>%
  group_by( month_born, month_in_words) %>% 
  summarise(count_live_births = n(), neonate_infections = sum(neonate_infection)) %>% ungroup() %>%
  mutate(NNI_rate_1000 = neonate_infections/count_live_births * 1000)

saveRDS(NNI_rate_cohort,paste0(folder_temp_data, "variants_analysis/", "Neonate_infect_cohort_monthly_table.rds"))


  
table(neonate_infections$neonate_infection, useNA = "always")
table(neonate_infections$neonate_infection,neonate_infections$variant_period_birth, useNA = "always")
table(neonate_infections$tests_baby_earliest_positive_test[neonate_infections$neonate_infection==1], useNA = "always")

#gestation  @ infection
pregnancies_infections <-  pregnancies_infections %>% 
  mutate(trimester = trimester(est_conception_date, mother_positive_test_during_pregnancy_date)) %>%
  mutate(gestation_days = 14 + (est_conception_date %--% mother_positive_test_during_pregnancy_date / days(1))) %>%
  mutate(gestation_at_infection = floor(gestation_days/7))

## monthly numbers ####
trimester_monthly_summary <-  pregnancies_infections %>%
  mutate(week_end = ceiling_date(mother_positive_test_during_pregnancy_date, unit = "week", change_on_boundary = F)) %>%
  mutate(month_in_words = format(as.Date(week_end), "%b %Y")) %>%
  mutate(month = format(as.Date(week_end), "%Y-%m")) %>%
  group_by( month,month_in_words, trimester) %>% 
  summarise(n_infections = n())  %>% ungroup() %>%
  mutate(var="trimester") %>%
  rename(level = trimester)

saveRDS(trimester_monthly_summary, paste0(folder_temp_data, "trimester_month_infect_summary.rds"))

ggplot(trimester_monthly_summary, aes(x=month,y=n_infections,fill=level)) +
  geom_bar(position="stack", stat="identity")

##Monthly infection rates####
#### Rate in Pregnancy ####
#calculate numbers pregnant each month - take trimester at start of month.
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= as.Date("2020-03-01")) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start) %>% 
  select(month, month_start)

months_total_pregnant <- months %>% 
  mutate(number_pregnancies_t1 = 0) %>% 
  mutate(number_pregnancies_t2 = 0) %>% 
  mutate(number_pregnancies_t3 = 0) %>% 
  select(month, number_pregnancies_t1, number_pregnancies_t2, number_pregnancies_t3)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>%  
    mutate(trimester_month_start = trimester(est_conception_date,  months$month_start[i]))%>%
    mutate(flag = case_when(pregnancy_end_date <= months$month_start[i] ~  NA_real_,  #this is needed otherwise all flagged as t3 with no end date.
                            trimester_month_start=="1 - first-trimester" ~ 1,
                            trimester_month_start=="2 - second-trimester" ~ 2,
                            trimester_month_start=="3 - third-trimester" ~ 3,
                            T ~ NA_real_)) %>% 
    count(flag) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies_t1 <- number[1,]$n
  months_total_pregnant[i,]$number_pregnancies_t2 <- number[2,]$n
  months_total_pregnant[i,]$number_pregnancies_t3 <- number[3,]$n
  
}


months_total_pregnant <- months_total_pregnant %>% pivot_longer(cols=c(number_pregnancies_t1, number_pregnancies_t2, number_pregnancies_t3)) %>%
  rename(n_pregnancies = value, trimester = name) %>% mutate(trimester= case_when(trimester== "number_pregnancies_t1"~ "1 - first-trimester", 
                                                                       trimester== "number_pregnancies_t2" ~ "2 - second-trimester", 
                                                                       trimester==  "number_pregnancies_t3"~ "3 - third-trimester" )) %>% filter(month >= "2021-01")
trimester_monthly_summary

monthly_summary <- left_join(months_total_pregnant, trimester_monthly_summary, by=c("month"= "month", "trimester"="level")) %>%
  filter(month >= "2021-04") %>%
  mutate(rate_per_100 = n_infections/n_pregnancies*100)

saveRDS(monthly_summary, paste0(folder_temp_data, "variants_analysis/", "trimester_month_infect_summary.rds"))

ggplot(monthly_summary, aes(x=month,y=n_infections,fill=trimester)) +
  geom_bar(position="stack", stat="identity")

ggplot(monthly_summary, aes(x=month,y=rate_per_100,fill=trimester)) +
  geom_bar(position="stack", stat="identity")
ggplot(monthly_summary, aes(x=month,y=rate_per_100,fill=trimester)) +
  geom_bar(position="dodge", stat="identity")
#infection rates in pregnant womend over the time period
#total rate for omicrons and delta periods, 
#### Rate in Pregnancy ####

variant_period <- c("Delta", "Omicron")
period_start <- c(delta_start, omicron_start)
period_end <- c(delta_end, infections_cutoff-1)
var_period <- as.data.frame(cbind(variant_period,
                                  period_start  = as.character(period_start), 
                                  period_end = as.character(period_end)))
var_period$period_start <- as.Date(var_period$period_start)

var_period$period_end <- as.Date(var_period$period_end)


period_total_pregnant <- var_period %>% 
  mutate(number_pregnancies = 0) %>% 
  select(variant_period, number_pregnancies)

# count number pregnant in each time period
for(i in 1:nrow(var_period)){
  number <-  pregnancies %>% 
    mutate(flag = case_when((pregnancy_end_date >= var_period$period_start[i] | overall_outcome == "Ongoing") 
                            & est_conception_date < var_period$period_end[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag) %>% 
    filter(!is.na(flag))
  
  period_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}

#write_rds(months_total_pregnant, paste0(folder_temp_data, "infection_output_tables/total_preg_by_month.rds"))
# count onset of covid cases in each period (total)####
period_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = everything()) %>%
  select(value) %>%
  filter(!is.na(value)) %>%
  mutate(variant_period = case_when(value >=delta_start & value <= delta_end ~ "Delta", 
                                value >=omicron_start & value <= infections_cutoff ~ "Omicron",
                                TRUE ~ "NA")) %>%
  select(variant_period) %>%
  group_by(variant_period) %>%
  count() %>% 
  filter(variant_period != "NA")%>% 
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy <- period_numerators %>%
  left_join(period_total_pregnant) %>%
  mutate("% women with COVID-19 during pregnancy whole time period" = (covid_infections_during_pregnancy / number_pregnancies) * 100)# %>%
  #pivot_longer(!month, names_to = "indicator", values_to = "value") %>% 
  #left_join(month_lookup) %>% 
  #select(-month) %>% 
  #pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

total_pregnancies <- pregnancies %>%
  group_by(mother_upi) %>%
  mutate(mother_total_pregnancies = n()) %>%
  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(total_pregnancies = n(),
            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000) %>%
  pivot_longer(cols = total_pregnancies:percent, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
  select(-indicator)

data_rate_in_pregnancy = data_rate_in_pregnancy %>%
  bind_cols(total_pregnancies)





#######################################
#demographics ####
########################################
#that dont change over time
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
saveRDS(demographics_summaries, paste0(folder_temp_data,  "variants_analysis/", "temp_variant_demogs1_no_transition.rds"))


####neonates####
#neonate infections in babies born within 28 days ofmother infection
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
NNI_table <- left_join(nninfections , early_nninfections )
NNI_table <- left_join(NNI_table, NNI_denominator)
NNI_table


saveRDS(NNI_table, paste0(folder_temp_data, "variants_analysis/", "NNI_table.rds"))

vac_nnis <- live_births %>% filter(neonate_infection==1) %>% group_by(variant_period, vaccination_status_at_infection) %>% 
  summarise(neonate_infections = n())
saveRDS(vac_nnis, paste0(folder_temp_data, "variants_analysis/", "NNI_vacc_table.rds"))

ethn_nnis <- live_births %>% filter(neonate_infection==1) %>% group_by(variant_period, ethnicity_reporting) %>% 
  summarise(neonate_infections = n())

age_nnis <- live_births %>% filter(neonate_infection==1) %>% group_by(variant_period,x_mother_age_at_outcome) %>% 
  summarise(neonate_infections = n())
 live_births %>% filter(neonate_infection==1) %>% group_by(variant_period,x_gestation_at_outcome) %>% 
  summarise(neonate_infections = n())

###############################################################################
####Descriptive - outcomes of infection####
#############################################################################
# COVID RELATED SICSAG ####
mother_upis <- read_rds(file.path(folder_temp_data, "mother_upis.rds"))
end_date  <- today()
# SICSAG ####

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
start_date <- as.Date("2021-01-01")
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
pregnancies_icu <- pregnancies %>% left_join(df_ep) %>% filter(!is.na(admission_date)) %>%
  filter(admission_date >= est_conception_date & admission_date <= (pregnancy_end_date+28)) 

table(pregnancies_icu$prime_diag_unit %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected"), pregnancies_icu$mother_has_had_pcr_test_at_any_point)

pregnancies_icu <- pregnancies_icu %>% filter(prime_diag_unit %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")|
                                                oth_diag1 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")| 
                                                oth_diag2 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")| 
                                                oth_diag3 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")| 
                                                oth_diag4 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")| 
                                                oth_diag5 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected")| 
                                                oth_diag6 %in% c("Chest infection-Covid-19 confirmed", "Chest infection-Covid-19 suspected") ) %>%
  arrange(pregnancy_id, admission_date) %>%
  group_by(pregnancy_id) %>% mutate(admit_diff = admission_date - lag(admission_date)) %>% ungroup() %>%
  filter(is.na(admit_diff) | admit_diff >=90) %>%
  filter(admission_date >= delta_start) %>%
  mutate(admission_period = case_when(admission_date >= delta_start & admission_date <=delta_end ~ "Delta", 
                                      admission_date >= omicron_start & admission_date <=infections_cutoff+28 ~ "Omicron" ))

table(pregnancies_icu$admission_period)

table(pregnancies_icu$mother_tested_positive_during_pregnancy, useNA="always")
table(pregnancies_icu$mother_has_had_pcr_test_at_any_point, useNA="always")

pregnancies_icu <-pregnancies_icu %>% select(pregnancy_id, mother_upi, admission_date, discharge_date, prime_diag_unit:oth_diag6, admission_period)
pregnancies_icu_diags <- left_join(pregnancies, pregnancies_icu)


#df_icu <- read_sav(paste0(icu_folder ,"episode_level_data_20220228.sav"))
#df_icu <- read_rds(paste0(folder_temp_data, "sicsag_episodes.rds"))
icu <- df_ep %>% 
  select(mother_upi, admission_date, discharge_date, covid_ic_uor_hdu, prime_diag_unit, preg_status, data_source, 
         oth_diag1, oth_diag2, oth_diag3, oth_diag4, oth_diag5, oth_diag6 )

covid_associated_icu_days <- 21

infections_during_pregnancy_flagged_icu <- pregnancies_infections %>%
  left_join(icu) %>% 
  mutate(stay_interval = interval(admission_date, discharge_date)) %>% 
  mutate(flag_covid_in_stay = mother_positive_test_during_pregnancy_date %within% stay_interval) %>% # TRUE if +ve test taken during the stay
  mutate(admission_to_test_days = mother_positive_test_during_pregnancy_date %--% admission_date / days(1)) %>% # days between +ve test and admission date
  mutate(flag_covid_temporal_stay = if_else(admission_to_test_days >= 0 & admission_to_test_days <= covid_associated_icu_days, TRUE, FALSE)) %>% # TRUE if +ve test <= 21 days before admission
  mutate(flag_covid_associated = if_else(flag_covid_temporal_stay == TRUE | flag_covid_in_stay == TRUE, TRUE, FALSE)) %>%  # TRUE if either previous flag TRUE
  select(-admission_to_test_days, -stay_interval) %>% 
# mutate(month = format(as.Date(covid_positive_test_date), "%Y-%m"),
#         week_ending = floor_date(covid_positive_test_date, unit = "weeks", week_start = 1) + days(6)) %>% 
#  mutate(gestation_days = 14 + (est_conception_date %--% covid_positive_test_date / days(1))) %>%
#  mutate(trimester = trimester(est_conception_date, covid_positive_test_date)) %>% 
mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy_date))

infections_icu_table <- infections_during_pregnancy_flagged_icu %>% filter(flag_covid_associated==TRUE) %>%
  group_by(variant_period) %>% summarise(n_icu21 = n()) 

infections_icu_table_diags <- infections_during_pregnancy_flagged_icu %>% filter(flag_covid_associated==TRUE) %>%
  mutate(primary_diag_grouped = 
           ifelse(prime_diag_unit=="Chest infection-Covid-19 confirmed" | prime_diag_unit=="Chest infection-Covid-19 suspected", "Covid infection", "Other diagnosis"))%>% 
  group_by(variant_period, primary_diag_grouped) %>% summarise(n_icu21 = n())

covid_diags <- c("Chest infection-Covid-19 confirmed" , "Chest infection-Covid-19 suspected")
infections_icu_table_diags <- infections_during_pregnancy_flagged_icu %>% filter(flag_covid_associated==TRUE) %>%
  mutate(primary_diag_grouped = 
           ifelse(prime_diag_unit=="Chest infection-Covid-19 confirmed" | prime_diag_unit=="Chest infection-Covid-19 suspected", "Covid infection", "Other diagnosis"))%>% 
  mutate(other_diag_grouped = case_when(oth_diag1 %in% covid_diags | oth_diag2 %in% covid_diags |
                                          oth_diag3 %in% covid_diags | oth_diag4 %in% covid_diags |
                                          oth_diag5 %in% covid_diags | oth_diag6 %in% covid_diags  ~ "Covid infection", 
                                          TRUE ~ "Other diagnosis")) %>% 
  group_by(variant_period, primary_diag_grouped, other_diag_grouped) %>% summarise(n_icu21 = n()) %>% ungroup() %>%
  mutate(any_covid_diag = case_when(primary_diag_grouped=="Covid infection" | other_diag_grouped=="Covid infection" ~ "Covid infection", 
                                    TRUE ~ "Other diagnosis"))


icu_summaries <-
  infections_icu_table_diags %>% ungroup() %>%
  group_by(variant_period, any_covid_diag) %>%
  summarise(n_icu21 = sum(n_icu21)) %>% ungroup() %>%
  pivot_wider(id_cols= c(any_covid_diag), names_from = variant_period, values_from = n_icu21) %>% 
  mutate(Delta_perc = percent(Delta/sum(Delta), accuracy =0.1), Omicron_per = percent(Omicron/sum(Omicron), accuarcy=0.1))
  
saveRDS(icu_summaries, paste0(folder_temp_data, "variants_analysis/", "table_icu_by_diag.rds"))

##ICU by vacc status
infections_icu_table_vacc <- infections_during_pregnancy_flagged_icu %>% filter(!is.na(variant_period)) %>%
  mutate(covid_assoc_icu = ifelse(is.na(flag_covid_associated) | flag_covid_associated==FALSE,"No","Yes")) %>%
  mutate(primary_diag_grouped = 
           ifelse(prime_diag_unit=="Chest infection-Covid-19 confirmed" | prime_diag_unit=="Chest infection-Covid-19 suspected", 
                  "Covid infection", "Other diagnosis")) %>%# filter(flag_covid_associated==TRUE) %>%
  group_by(variant_period, vaccination_status_at_infection) %>% 
  summarise(total_cases = n(), N_icu21 = sum(covid_assoc_icu=="Yes"), N_icu_covid_diag = sum(primary_diag_grouped=="Covid infection", na.rm=T))%>% 
  pivot_wider(id_cols= c(vaccination_status_at_infection), names_from = variant_period, values_from = c( total_cases, N_icu21, N_icu_covid_diag)) %>% 
  mutate(perc_icu_Delta = percent(N_icu21_Delta/total_cases_Delta), 
         perc_icu_Omicron = percent(N_icu21_Omicron/total_cases_Omicron)) %>%
  mutate(perc_icu_diag_Delta = percent(N_icu_covid_diag_Delta/total_cases_Delta),
         perc_icu_diag_Omicron = percent(N_icu_covid_diag_Omicron/total_cases_Omicron)) 

saveRDS(infections_icu_table_vacc, paste0(folder_temp_data,  "variants_analysis/", "variants_icu_by_vacc.rds"))

## DEATHS ####
df_deaths <- read_rds(paste0(folder_temp_data, "deaths_flagged.rds"))

deaths <- df_deaths %>% 
  select(mother_upi, date_of_death, flag_acute_covid19, data_source)
rm(df_icu, df_deaths)

# COVID RELATED DEATHS ####
maternal_covid_death_days <- 28 # "Maternal death within 28 days is defined as death of the woman from any cause occurring on or up to 27 days following the date of onset of COVID-19"
subsequent_maternal_death_days <- 41 # "Any subsequent maternal death is defined as death of the woman from any cause occurring on or or at any point following the date of onset of COVID-19, up to 41 days following the end date of pregnancy"
one_year_maternal_death_days <- 365
end_date <- today() 
infections_during_pregnancy_flagged_death <-pregnancies_infections %>% 
  left_join(deaths) %>% 
  mutate(covid_associated_death_interval = interval(mother_positive_test_during_pregnancy_date, mother_positive_test_during_pregnancy_date + days(27))) %>% 
  mutate(follow_up_date = if_else(is.na(pregnancy_end_date), end_date, pregnancy_end_date)) %>% 
   mutate(covid_subsequent_death_interval = interval(start = mother_positive_test_during_pregnancy_date, 
                                                      end =  follow_up_date + days(subsequent_maternal_death_days))) %>% 
  #  mutate(covid_oneyear_death_interval = interval(start = covid_positive_test_date, 
  #                                                   end =  follow_up_date + days(one_year_maternal_death_days))) %>% 
  mutate(flag_covid_associated_death = if_else(!is.na(date_of_death) & date_of_death %within% covid_associated_death_interval, TRUE, FALSE)) %>%
 mutate(flag_covid_subsequent_death = if_else(!is.na(date_of_death) & date_of_death %within% covid_subsequent_death_interval, TRUE, FALSE)) %>% 
  #  mutate(flag_covid_oneyear_subsequent_death = if_else(date_of_death %within% covid_oneyear_death_interval, TRUE, FALSE)) %>% 
  mutate(gestation_days = 14 + (est_conception_date %--% mother_positive_test_during_pregnancy_date / days(1))) %>%
  mutate(trimester = trimester(est_conception_date, mother_positive_test_during_pregnancy_date)) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy_date))

infections_death_table <- infections_during_pregnancy_flagged_death %>% #filter(flag_covid_subsequent_death ==TRUE) %>%
  group_by(variant_period, flag_covid_subsequent_death ) %>% summarise(n_subs_death = n()) %>% filter(!is.na(variant_period)) %>% 
  pivot_wider(id_cols = variant_period, values_from = n_subs_death, names_from = flag_covid_subsequent_death) %>%
  select(-`FALSE`) %>% rename(subsequent_mat_death = `TRUE`)

infections_death28_table <- infections_during_pregnancy_flagged_death %>% #filter(flag_covid_subsequent_death ==TRUE) %>%
  group_by(variant_period, flag_covid_associated_death ) %>% summarise(n_death = n()) %>% filter(!is.na(variant_period)) %>% 
  pivot_wider(id_cols = variant_period, values_from = n_death, names_from = flag_covid_associated_death) %>% 
  mutate(n_mat_deaths = 0) %>%
  select(-`FALSE`) 

infections_death_tableall <- left_join(infections_death_table, infections_death28_table)
names(infections_death_tableall) <- c("variant_period", "subsequent maternal death", "maternal death within 28 days") 

saveRDS(infections_death_tableall, paste0(folder_temp_data, "table_mat_dths.rds"))
infections_death_tableall <- readRDS( paste0(folder_temp_data, "table_mat_dths.rds"))
## Pregnancies ending in preterm birth ####
####
fetuslevel <- read_rds(paste0(folder_temp_data, "script6_baby_level_record_infection.rds"))  %>% 
  rename(tests_mother_has_had_pcr_test_at_any_point = tests_mother_has_pos_test_at_any_point) %>%
  rename(tests_mother_positive_test_during_pregnancy_1 =tests_mother_value_positive_test_during_pregnancy_1, 
         tests_mother_positive_test_during_pregnancy_2 = tests_mother_value_positive_test_during_pregnancy_2 )

end_infection_date <- as.Date("2022-01-31")

fetus_level_processed <- fetuslevel %>%
  filter(chi_validity == "Valid CHI") %>% 
  filter(x_est_conception_date <= end_infection_date) %>%   # only include data for befoer end of infection period considered
  select(-chi_validity) %>% 
  mutate(month_pregnancy_end = format(x_pregnancy_end_date, "%Y-%m")) %>% 
  group_by(pregnancy_id) %>% 
  mutate(fetus_number = row_number()) %>% 
  ungroup() 

fetus_level_processed_long <- fetus_level_processed %>% 
  pivot_longer(cols = c(tests_mother_positive_test_during_pregnancy_1, tests_mother_positive_test_during_pregnancy_2), names_to = "indicator",
               values_to = "mother_positive_test_during_pregnancy") %>% 
  select(-indicator) %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(month = format(as.Date(mother_positive_test_during_pregnancy), "%Y-%m") ) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy))


data_preterm_preg <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  select(mother_upi, month, pregnancy_id, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, vaccination_status_at_infection, x_gestation_at_outcome) %>% 
  mutate(covid_associated_preterm_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                      end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_preterm = if_else(x_pregnancy_end_date %within% covid_associated_preterm_interval, TRUE, FALSE)) %>% 
  mutate(flag_preterm = if_else(x_gestation_at_outcome < 37, TRUE, FALSE)) %>% 
  mutate(flag_very_preterm = if_else(x_gestation_at_outcome < 32, TRUE, FALSE)) %>% 
  select(-x_gestation_at_outcome) %>% 
  filter(flag_preterm == TRUE) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date),
         flag_preterm = max(flag_preterm),
         flag_very_preterm = max(flag_very_preterm)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

data_preterm_preg <- data_preterm_preg %>% 
  left_join(select(pregnancies_infections, c(pregnancy_id, mother_positive_test_during_pregnancy_date, variant_period)),
            by=c("pregnancy_id","mother_positive_test_during_pregnancy" = "mother_positive_test_during_pregnancy_date" ) ) %>%
  filter(!is.na(variant_period))

knitr::kable(data_preterm_preg  %>% filter(flag_covid_associated_preterm==TRUE) %>%
               group_by(variant_period) %>% summarise(N_covid_associatesd_preterm=n() , n_very_preterm = sum(flag_very_preterm==TRUE)))


data_preterm_preg  %>% filter(flag_covid_associated_preterm==TRUE) %>%
  group_by(variant_period) %>% summarise(N_covid_associatesd_preterm=n() , n_very_preterm = sum(flag_very_preterm==TRUE))


preterm_by_vacc <- data_preterm_preg  %>% #filter(flag_covid_associated_preterm==TRUE) %>%
  group_by(variant_period, vaccination_status_at_infection) %>% 
  summarise(N_covid_associatesd_preterm=sum(flag_covid_associated_preterm==TRUE), n_lb_post_infect = n())
#saveRDS(preterm_by_vacc, paste0(folder_temp_data, "variants_preterm_vacc.rds"))


data_preterm_denominator <- fetus_level_processed_long %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(gestation_at_infection = floor((mother_positive_test_during_pregnancy - x_est_conception_date)/7))%>%
  filter(gestation_at_infection < 37 & gestation_at_infection >= 20) %>%
  select(mother_upi, month, pregnancy_id, x_est_conception_date, x_pregnancy_end_date, gestation_at_infection,mother_positive_test_during_pregnancy, vaccination_status_at_infection, x_gestation_at_outcome) %>% 
  mutate(covid_associated_preterm_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                      end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_preterm = if_else(x_gestation_at_outcome < 37, TRUE, FALSE)) %>%
  mutate(flag_covid_associated_preterm =
           if_else(x_pregnancy_end_date %within% covid_associated_preterm_interval & flag_preterm==TRUE, TRUE, FALSE)) 
#  mutate(flag_very_preterm = if_else(x_gestation_at_outcome < 32, TRUE, FALSE))# %>% 
 # select(-x_gestation_at_outcome) %>% 
  #filter(flag_preterm == TRUE) %>% 
 
table(data_preterm_denominator$vaccination_status_at_infection, data_preterm_denominator$flag_covid_associated_preterm, 
      data_preterm_denominator$mother_positive_test_during_pregnancy > as.Date("2021-12-15"))



