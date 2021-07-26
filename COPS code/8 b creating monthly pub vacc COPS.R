########################## Publication script to create COPS vaccination information ############################

library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(naniar)
library(hablar)
library(janitor)
library(knitr)
library(ggplot2)
library(phsmethods)
library(openxlsx)
library(tidylog)
library(glue)
library(openxlsx)
library(data.table)
library(readxl)

cops_age_group <- function(age_variable) {case_when(age_variable <= 19 ~ " 1 <= 19", 
                                                    age_variable >= 20 & age_variable <= 24 ~ "2 20-24", 
                                                    age_variable >= 25 & age_variable <= 29 ~ "3 25-29",
                                                    age_variable >= 30 & age_variable <= 34 ~ "4 30-34",
                                                    age_variable >= 35 & age_variable <= 39 ~ "5 35-39",
                                                    age_variable >= 40 ~ "7 >=40", TRUE ~ "Unknown")}


###
### FOLDER LOCATIONS REMOVED FOR PUBLIC RELEASE
###

#vaccine date 

vaccine_date <- "2020-12-08"


################################## read in data 
#COPS cohort 
babies <- read_rds(paste0(folder_temp_data, "script6_baby_level_record.rds"))
#vaccines cohort 
vaccines <- read_rds(paste0(folder_temp_data, "vaccine_cops.rds"))
#pregnancy cohort 
pregnancies_1 <- read_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds"))



################ invalid CHI's in pregnancies file - remove them 121,741 (1,114)
pregnancies<- pregnancies_1 %>% 
  mutate(mother_upi_check = chi_check(as.character(mother_upi))) %>% 
  filter(mother_upi_check == "Valid CHI") %>% 
  mutate(pregnancy_end_date =case_when(is.na(pregnancy_end_date) ~ as.Date("1970-01-01"),
                                       T ~ pregnancy_end_date))


###### The eligible for vaccination sub-cohort would include women pregnant on 8 Dec 2020
##(the date the first vaccine was delivered outwith a clinical trial in Scotland) plus those subsequently becoming pregnant

################ invalid CHI's in vaccine file - remove them
vaccines<- vaccines %>% 
  mutate(mother_upi_check = chi_check(as.character(mother_upi))) %>% 
  filter(mother_upi_check == "Valid CHI") %>% 
  filter(vacc_occurence_date < as.Date("2021-06-01"))

###### link dataset using the mother UPI
preg_vacc_data_initial <- pregnancies %>% 
  left_join(vaccines, by = "mother_upi") 

#Number of pregnancies in COPS vaccinated with D1 or D2
preg_vacc_data1_2 <- preg_vacc_data_initial %>% 
  filter(vacc_dose_number==1 |vacc_dose_number==2) %>% 
  mutate(postcondays=difftime(vacc_occurence_date, est_conception_date, units = "days")) %>%
  mutate(postcondays = as.numeric(postcondays))

preg_vacc_data <- preg_vacc_data1_2 %>% 
  mutate(vaccine_preg = case_when(vacc_occurence_date >= est_conception_date 
                                  & vacc_occurence_date <= pregnancy_end_date  ~ 1,
                                  vacc_occurence_date >= est_conception_date 
                                  & pregnancy_end_date == "1970-01-01" ~1,
                                  T ~ NA_real_)) %>% 
  mutate(vaccineb4_preg=case_when(vacc_occurence_date < est_conception_date~ 1, T ~ NA_real_)) %>%
  mutate(vaccineafter_preg=case_when(vacc_occurence_date > pregnancy_end_date 
                                     & pregnancy_end_date != "1970-01-01" ~ 1, T ~ NA_real_)) %>% 
  mutate(week_ending = ceiling_date(vacc_occurence_date, unit = "week", change_on_boundary = F)) %>% 
  mutate(month_vaccine = format(as.Date(vacc_occurence_date), "%Y-%m"))

preg_vacc_data_main<- preg_vacc_data %>% 
  filter(vaccine_preg== 1) %>% 
  mutate(postcondays=difftime(vacc_occurence_date, est_conception_date, units = "days")) %>%
  mutate(postcondays = as.numeric(postcondays)) %>% 
  filter(postcondays < 308)


preg_vacc_data_main_check <- preg_vacc_data_main %>% 
  count(mother_upi)
preg_vacc_data_main_check_type <- preg_vacc_data_main %>% 
  count(vacc_product_name )

########################table 1#########################


preg_vacc_data6 <- preg_vacc_data_main %>% 
  count(week_ending,  vacc_dose_number)

preg_vacc_data7 <- preg_vacc_data_main %>% 
  mutate(vacc_dose_number= 99) %>% 
  count(week_ending, vacc_dose_number)


Table1 <-  preg_vacc_data6 %>% 
  bind_rows(preg_vacc_data7) %>% 
  mutate(Dose_Number= case_when (vacc_dose_number==99  ~ "Any type",
                                 vacc_dose_number==1  ~ "Dose 1",
                                 vacc_dose_number==2  ~ "Dose 2" )) %>% 
  pivot_wider(names_from = "week_ending", values_from = "n") %>%  
  select(-vacc_dose_number) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-1])) 

ggplot(data=preg_vacc_data6, aes(x=week_ending, y=n, group=vacc_dose_number)) +
  geom_line(aes(colour=vacc_dose_number))




########################table 1 HB #########################


preg_vacc_data6HB <- preg_vacc_data_main %>% 
  count(month_vaccine, hbres, vacc_dose_number)


preg_vacc_data6HB_SCOT <- preg_vacc_data_main %>% 
  count(month_vaccine, vacc_dose_number) %>% 
  mutate(hbres="1 Scotland")

preg_vacc_data7HB <- preg_vacc_data_main %>% 
  mutate(vacc_dose_number= 99) %>% 
  count(month_vaccine, hbres, vacc_dose_number)

preg_vacc_data7_SCOT <- preg_vacc_data_main %>% 
  mutate(vacc_dose_number= 99) %>% 
  count(month_vaccine, vacc_dose_number) %>% 
  mutate(hbres="1 Scotland")

Table1HB <-  preg_vacc_data6HB %>% 
  bind_rows(preg_vacc_data7HB,preg_vacc_data6HB_SCOT, preg_vacc_data7_SCOT) %>% 
  mutate(Dose_Number= case_when (vacc_dose_number==99  ~ "Any type",
                                 vacc_dose_number==1  ~ "Dose 1",
                                 vacc_dose_number==2  ~ "Dose 2" )) %>% 
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  select(-vacc_dose_number) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1:2)])) %>% 
  arrange(hbres)

ggplot(data=preg_vacc_data7HB, aes(x=month_vaccine, y=n, group=hbres)) +
  geom_line(aes(colour=hbres))

ggplot(data=preg_vacc_data_main, aes(x=hbres)) +
  geom_bar()+ theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

########################table 1 LA #########################



########################table 2 gestattion#########################


preg_vacc_data2 <- preg_vacc_data_main %>% 
  mutate(Stage_of_Preg= case_when(postcondays >= 0 & postcondays <= 83 
                                  ~ "First Trimester",
                                  postcondays >= 84 & postcondays <= 181 
                                  ~ "Second Trimester", 
                                  postcondays >= 182 & postcondays <= 294  
                                  ~ "Third Trimester",
                                  postcondays > 294 
                                  ~" #unlikely to be during preg")) %>% 
  count(month_vaccine, Stage_of_Preg)

preg_vacc_data3 <- preg_vacc_data_main %>%  
  mutate(Stage_of_Preg= "Total during pregnancy") %>% 
  count(month_vaccine, Stage_of_Preg)


Table2 <-  preg_vacc_data2 %>% 
  bind_rows(preg_vacc_data3) %>% 
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1)])) %>%
  arrange(Stage_of_Preg)

ggplot(data=preg_vacc_data2, aes(x=month_vaccine, y=n, group=Stage_of_Preg)) +
  geom_line(aes(colour=Stage_of_Preg))

preg_vacc_data_main %>% 
  mutate(Stage_of_Preg= case_when(postcondays >= 0 & postcondays <= 83 
                                  ~ "First Trimester",
                                  postcondays >= 84 & postcondays <= 181 
                                  ~ "Second Trimester", 
                                  postcondays >= 182 & postcondays <= 294  
                                  ~ "Third Trimester",
                                  postcondays >= 294 
                                  ~" #unlikely to be during preg")) %>% 
  ggplot(aes(x=Stage_of_Preg)) +
  geom_bar()




########################table 3 - vaccine type #########################

preg_vacc_data4 <- preg_vacc_data_main %>%  
  count(month_vaccine, vacc_product_name)


preg_vacc_data5 <- preg_vacc_data_main %>% 
  mutate(vacc_product_name= "Total - all types") %>% 
  count(month_vaccine, vacc_product_name)


Table3 <-  preg_vacc_data4 %>% 
  bind_rows(preg_vacc_data5) %>% 
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1)])) %>% 
  arrange(vacc_product_name)

ggplot(data=preg_vacc_data4, aes(x=month_vaccine, y=n, group=vacc_product_name)) +
  geom_line(aes(colour=vacc_product_name))

ggplot(data=preg_vacc_data_main, aes(x=vacc_product_name)) +
  geom_bar()


####adding an extra section for dose Vs Product 

preg_vacc_data_dose_product <- preg_vacc_data_main %>%  
  count(month_vaccine, vacc_product_name, vacc_dose_number)

preg_vacc_data_dose_product_all <- preg_vacc_data_main %>% 
  mutate(vacc_product_name= "Total - all types") %>% 
  count(month_vaccine, vacc_product_name, vacc_dose_number)


Table3_dose_type <-  preg_vacc_data_dose_product %>% 
  bind_rows(preg_vacc_data_dose_product_all) %>% 
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1)])) %>% 
  arrange(vacc_product_name)


########################table 4 vaccination age #########################

preg_vacc_data8 <- preg_vacc_data_main %>%  
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month_vaccine, maternal_age_group)


preg_vacc_data9 <- preg_vacc_data_main %>%  
  mutate(maternal_age_group= "Total") %>% 
  count(month_vaccine, maternal_age_group)


Table4 <- preg_vacc_data8 %>% 
  bind_rows(preg_vacc_data9) %>%  
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1)])) %>% 
  arrange(maternal_age_group)

ggplot(data=preg_vacc_data8, aes(x=month_vaccine, y=n, group=maternal_age_group)) +
  geom_line(aes(colour=maternal_age_group))

preg_vacc_data_main %>%  
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  ggplot(aes(x=maternal_age_group)) +
  geom_bar()

########################table 5 SIMD #########################

preg_vacc_data10 <- preg_vacc_data_main %>%  
  count(month_vaccine, simd)


preg_vacc_data11 <- preg_vacc_data_main %>% 
  mutate(simd=99) %>% 
  count(month_vaccine, simd)


Table5 <-  preg_vacc_data10 %>% 
  bind_rows(preg_vacc_data11) %>% 
  pivot_wider(names_from = "month_vaccine", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total_todate = rowSums(.[-(1)])) %>% 
  arrange(simd)

ggplot(data=preg_vacc_data10, aes(x=month_vaccine, y=n, group=simd)) +
  geom_line(aes(colour=simd))

ggplot(data=preg_vacc_data_main, aes(x=simd)) +
  geom_bar()

########################table 6 uptake  #########################



#dec
preg_vacc_data_uptake_dec <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2020-12-01"~ "Dec-20" )) %>% 
  count(month) %>% 
  filter(!is.na(month))
#jan
preg_vacc_data_uptake_jan <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-01-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-01-01"~ "Jan-21" )) %>% 
  count(month) %>% 
  filter(!is.na(month))

#feb
preg_vacc_data_uptake_feb <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-02-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-02-01"~ "Feb-21" )) %>% 
  count(month) %>% 
  filter(!is.na(month))

#mar
preg_vacc_data_uptake_mar <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-03-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-03-01"~ "Mar-21" )) %>% 
  count(month) %>% 
  filter(!is.na(month))

#apr
preg_vacc_data_uptake_apr <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-04-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-04-01"~ "Apr-21" )) %>% 
  count(month) %>% 
  filter(!is.na(month))

#may
preg_vacc_data_uptake_may <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-05-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-05-01"~ "May-21" )) %>% 
  count(month) %>% 
  filter(!is.na(month))


preg_vacc_data_uptake2 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%b-%y")) %>% 
  count(month_vacc, mother_upi) %>% 
  count(month_vacc) %>% 
  rename(month=month_vacc)



Table6<-  preg_vacc_data_uptake_dec %>% 
  bind_rows(preg_vacc_data_uptake_jan,preg_vacc_data_uptake_feb,
            preg_vacc_data_uptake_mar, preg_vacc_data_uptake_apr, preg_vacc_data_uptake_may) %>% 
  rename(Number_wom_preg_mon =n) %>% 
  left_join(preg_vacc_data_uptake2) %>% 
  mutate(percen_vacc=(n/Number_wom_preg_mon)*100)


Table6_total_1 <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-06-01"~ "Total Preg" )) %>% 
  count(month) %>% 
  filter(!is.na(month))

Table6_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi) %>% 
  mutate(month = "Total Vaccinated") %>% 
  count(month)


Table6_total <- Table6_total_1 %>% 
  bind_rows(Table6_total_2)


########################table 6 uptake age   #########################



#dec
preg_vacc_data_uptake_dec_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2020-12-01"~ "Dec-20" )) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>% 
  filter(!is.na(month))

#jan
preg_vacc_data_uptake_jan_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-01-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-01-01"~ "Jan-21" )) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>%
  filter(!is.na(month))

#feb
preg_vacc_data_uptake_feb_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-02-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-02-01"~ "Feb-21" )) %>%
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>%
  filter(!is.na(month))

#mar
preg_vacc_data_uptake_mar_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-03-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-03-01"~ "Mar-21" )) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>% 
  filter(!is.na(month))

#apr
preg_vacc_data_uptake_apr_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-04-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-04-01"~ "Apr-21" )) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>% 
  filter(!is.na(month))

#may
preg_vacc_data_uptake_may_age <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-05-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-05-01"~ "May-21" )) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month, maternal_age_group) %>% 
  filter(!is.na(month))

preg_vacc_data_uptake2_age <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%b-%y")) %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month_vacc, maternal_age_group, mother_upi) %>%
  count(month_vacc, maternal_age_group) %>% 
  rename(vacc_month=n) %>% 
  rename(month=month_vacc)



Table6age<-  preg_vacc_data_uptake_dec_age %>% 
  bind_rows(preg_vacc_data_uptake_jan_age, preg_vacc_data_uptake_feb_age,
            preg_vacc_data_uptake_mar_age, preg_vacc_data_uptake_apr_age,
            preg_vacc_data_uptake_may_age) %>% 
  rename(Number_wom_preg_mon =n) %>% 
  left_join(preg_vacc_data_uptake2_age) %>% 
  mutate(percen_vacc = (vacc_month/Number_wom_preg_mon)*100)


#################################age group total 

Table6_totalage_1 <- pregnancies %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-06-01"~ "Total Preg" )) %>% 
  count(month, maternal_age_group) %>% 
  filter(!is.na(month))

Table6_totalage_2 <- preg_vacc_data_main %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(mother_upi, maternal_age_group) %>% 
  mutate(month = "Total Vaccinated") %>% 
  count(month, maternal_age_group)


Table6_agetotal <- Table6_totalage_1 %>% 
  bind_rows(Table6_totalage_2)



########################table 6 uptake SIMD #########################



#dec
preg_vacc_data_uptake_dec_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2020-12-01"~ "Dec-20" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))
#jan
preg_vacc_data_uptake_jan_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-01-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-01-01"~ "Jan-21" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

#feb
preg_vacc_data_uptake_feb_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-02-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-02-01"~ "Feb-21" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

#mar
preg_vacc_data_uptake_mar_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-03-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-03-01"~ "Mar-21" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

#apr
preg_vacc_data_uptake_apr_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-04-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-04-01"~ "Apr-21" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

#may
preg_vacc_data_uptake_may_simd <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2021-05-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-05-01"~ "May-21" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

preg_vacc_data_uptake2_simd <- preg_vacc_data_main %>%  
  mutate(month_vacc = format(vacc_occurence_date, format = "%b-%y")) %>% 
  count(month_vacc,simd, mother_upi) %>% 
  count(month_vacc, simd) %>% 
  rename(vacc_month=n) %>% 
  rename(month=month_vacc) 



Table6simd<-  preg_vacc_data_uptake_dec_simd %>% 
  bind_rows(preg_vacc_data_uptake_jan_simd,
            preg_vacc_data_uptake_feb_simd, 
            preg_vacc_data_uptake_mar_simd,
            preg_vacc_data_uptake_apr_simd,
            preg_vacc_data_uptake_may_simd) %>% 
  rename(Number_wom_preg_mon =n) %>% 
  left_join(preg_vacc_data_uptake2_simd) %>% 
  mutate(percen_vacc = (vacc_month/Number_wom_preg_mon)*100)


###########################simd total 

Table6_totalsimd_1 <- pregnancies %>% 
  mutate(month = case_when(pregnancy_end_date >="2020-12-01" | pregnancy_end_date == "1970-01-01" 
                           & est_conception_date < "2021-06-01"~ "Total Preg" )) %>% 
  count(month, simd) %>% 
  filter(!is.na(month))

Table6_totalsimd_2 <- preg_vacc_data_main %>% 
  count(mother_upi, simd) %>% 
  mutate(month = "Total Vaccinated") %>% 
  count(month, simd)


Table6_simdtotal <- Table6_totalsimd_1 %>% 
  bind_rows(Table6_totalsimd_2)



########################table 7 coverage #########################


preg_vacc_data_table7a <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  count(month_delivered)

preg_vacc_data_table7b <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(atleastone= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, atleastone) %>% 
  distinct() %>% 
  filter(atleastone==1) %>% 
  count(month_delivered) %>% 
  rename(onevacc=n)


preg_vacc_data_table7c <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(flag= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, flag) %>% 
  group_by(mother_upi, month_delivered) %>% 
  mutate(twoormore= sum(flag)) %>% 
  ungroup() %>%
  distinct() %>% 
  filter(twoormore==2) %>% 
  count(month_delivered) %>% 
  rename(twovacc=n)


Table7 <- preg_vacc_data_table7a %>% 
  left_join(preg_vacc_data_table7b) %>% 
  left_join(preg_vacc_data_table7c) %>% 
  rename(delivered=n)


########################table 7 coverage - Age Breakdown #########################


preg_vacc_data_table7a_age <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  count(month_delivered, maternal_age_group)


preg_vacc_data_table7b_age <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  mutate(atleastone= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, atleastone, maternal_age_group) %>% 
  distinct() %>% 
  filter(atleastone==1) %>% 
  count(month_delivered, maternal_age_group) %>% 
  rename(onevacc=n)


preg_vacc_data_table7c_age <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  mutate(flag= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, flag, maternal_age_group) %>% 
  group_by(mother_upi, month_delivered) %>% 
  mutate(twoormore= sum(flag)) %>% 
  ungroup() %>%
  distinct() %>% 
  filter(twoormore==2) %>% 
  count(month_delivered, maternal_age_group) %>% 
  rename(twovacc=n)


Table7_age <- preg_vacc_data_table7a_age %>% 
  left_join(preg_vacc_data_table7b_age) %>% 
  left_join(preg_vacc_data_table7c_age) %>% 
  rename(delivered=n)


########################table 7 coverage - SIMD Breakdown #########################


preg_vacc_data_table7a_simd <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  count(month_delivered, simd)


preg_vacc_data_table7b_simd <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(atleastone= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, atleastone, simd) %>% 
  distinct() %>% 
  filter(atleastone==1) %>% 
  count(month_delivered, simd) %>% 
  rename(onevacc=n)


preg_vacc_data_table7c_simd <- preg_vacc_data_initial %>% 
  filter(overall_outcome=="Stillbirth" | overall_outcome=="Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(month_delivered>"2020-11") %>% 
  mutate(flag= case_when(vacc_occurence_date<= pregnancy_end_date~ 1, T ~ NA_real_)) %>% 
  select(mother_upi, month_delivered, flag, simd) %>% 
  group_by(mother_upi, month_delivered) %>% 
  mutate(twoormore= sum(flag)) %>% 
  ungroup() %>%
  distinct() %>% 
  filter(twoormore==2) %>% 
  count(month_delivered, simd) %>% 
  rename(twovacc=n)


Table7_simd <- preg_vacc_data_table7a_simd %>% 
  left_join(preg_vacc_data_table7b_simd) %>% 
  left_join(preg_vacc_data_table7c_simd) %>% 
  rename(delivered=n)


output<- list(Table1,Table1HB, Table2, Table3,Table3_dose_type, Table4, Table5, Table6, Table6age, Table6simd, Table7, Table7_age, Table7_simd, Table6_agetotal, Table6_simdtotal, Table6_total)

write.xlsx(output,(paste0(folder_temp_data, "preg_vacc.xlsx")))
