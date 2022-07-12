# run script 0b. setup.r in the main COPS code before this script

#### Total live births in our cohort ####

start_date <- as.Date("2020-03-01") 
end_date <- as.Date("2022-01-31")

data_nrs_live_births <- read_rds(paste0(folder_temp_data, "nrs_live_births.rds")) %>% filter(nrslb_date_of_birth >= start_date & nrslb_date_of_birth <= end_date)
data_nhs_live_births <- read_rds(paste0(folder_temp_data, "nhs_live_births.rds")) %>% filter(nhslb_baby_dob >= start_date & nhslb_baby_dob <= end_date)

total_births_nrs <- data_nrs_live_births %>%# calculate the total number of live births each month 
  filter(nrslb_date_of_birth >= "2020-03-01" & nrslb_date_of_birth <= "2022-01-31") %>%
  summarise(nrs_live_births = n())

total_births_nhslb <- data_nhs_live_births %>%# calculate the total number of live births each month 
  filter(nhslb_baby_dob >= "2020-03-01" & nhslb_baby_dob <= "2022-01-31") %>%
  summarise(nhs_live_births = n())

total_births_COPS_with_baby_chi <- babies %>%
  summarise(cops_births_valid_baby_chi = n())

total_births_COPS_with_mother_and_baby_chi <- group3_babies %>%
  summarise(cops_births_valid_baby_mother_chi = n())

total_births <- total_births_nrs %>%
  bind_cols(., total_births_nhslb, total_births_COPS_with_baby_chi, total_births_COPS_with_mother_and_baby_chi) %>%
  pivot_longer(cols=everything(), names_to = "Total Live Births", values_to = "n")


##### Overall Rate ##### 

months_total_births <- babies %>% # calculate the total number of live births each month 
  mutate(month = format(as.Date(x_pregnancy_end_date), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_live_births = n())

# covid babies in testing data
months_total_covid_national_testing <- neonate_tests %>% 
  mutate(month = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_national_testing <- months_total_births %>% # group 2 
  left_join(., months_total_covid_national_testing, by=c("month")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%
  arrange(month) %>% 
  adorn_totals()

neonates_covid_rates_national_testing <- neonates_covid_national_testing %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)

# babies with valid chi
months_total_covid <- covid_babies %>% # calculates total number of neonates in our cohort with covid each month
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to date positive test was on
  group_by(month) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid <- months_total_births %>% # group 2 
  left_join(., months_total_covid, by=c("month")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%
  arrange(month) %>% 
  adorn_totals()

neonates_covid_rates <- neonates_covid %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)


# babies with valid chi and mothers with a valid chi
months_total_births_group_3 <- group3_babies %>% # calculate the total number of live births each month 
  mutate(month = format(as.Date(x_pregnancy_end_date), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_live_births = n())

months_total_covid_group_3 <- group3_covid_babies %>% # calculates total number of neonates in our cohort with covid each month
  mutate(month = format(as.Date(tests_baby_earliest_positive_test), "%Y-%m")) %>% # change to date positive test was on
  group_by(month) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_group_3 <- months_total_births_group_3 %>% 
  left_join(., months_total_covid_group_3, by=c("month")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%
  arrange(month) %>% 
  adorn_totals()

neonates_covid_rates_group_3 <- neonates_covid_group_3 %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)

# looking at children covid rates overall 

# post neonates 
post_neonates_total_births <- data_nrs_live_births %>%# calculate the total number of live births each month in nrs live database and calculate average per month
  filter(nrslb_date_of_birth >= "2020-03-01" & nrslb_date_of_birth <= "2022-01-31") %>%
  mutate(month = format(as.Date(nrslb_date_of_birth), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_babies = n()) %>%
  mutate(average_monthly_babies = floor(mean(total_babies)), 
         nrs_mid_year_est_0_yo = rep(48635), # number taken from Table 1 - https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
         total_post_neonates_denominator = nrs_mid_year_est_0_yo - average_monthly_babies, 
         total_post_neonates_denominator = as.character(total_post_neonates_denominator)) %>%
  select(month, total_post_neonates_denominator) 

post_neonates_covid <- post_neonate_tests %>%
  mutate(month = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_positive_covid = n()) 

post_neonates_total_covid <- post_neonates_total_births %>% # group 2 
  left_join(., post_neonates_covid, by=c("month")) %>%
  mutate(total_positive_covid = replace_na(total_positive_covid, 0)) %>%
  arrange(month) %>%
  adorn_totals(fill = post_neonates_total_births$total_post_neonates_denominator) %>%
  mutate(total_post_neonates_denominator = as.numeric(total_post_neonates_denominator), 
         rate = (total_positive_covid/total_post_neonates_denominator)*100000)


# 1-4 year olds 
one_to_four_yo_covid <- one_to_four_yo_tests %>%
  mutate(month = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_positive_covid = n()) %>%
  adorn_totals() %>%
  mutate(total_no_children_one_to_four = 263806-48635) %>% # number taken from Table 1 - https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
  select(month, total_no_children_one_to_four, total_positive_covid) %>%
  mutate(rate = (total_positive_covid/total_no_children_one_to_four)*100000)


# 5-11 year olds
five_to_eleven_yo_covid <- five_to_11_yo_tests %>%
  mutate(month = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_positive_covid = n()) %>%
  adorn_totals() %>%
  mutate(total_no_children_5_to_11 = 297903+59638+61301) %>% # number taken from Table 1 - https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
  select(month, total_no_children_5_to_11, total_positive_covid) %>%
  mutate(rate = (total_positive_covid/total_no_children_5_to_11)*100000)

# 12-17 year olds
twelve_to_seventeen_yo_covid <- twelve_to_17_yo_tests %>%
  mutate(month = format(as.Date(date_ecoss_specimen), "%Y-%m")) %>% 
  group_by(month) %>%
  summarise(total_positive_covid = n()) %>%
  adorn_totals() %>%
  mutate(total_no_children_12_to_17 = 61018+58637+57487+56993+55890+54249) %>% # numbers taken from Table 1 - https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2020
  select(month, total_no_children_12_to_17, total_positive_covid) %>%
  mutate(rate = (total_positive_covid/total_no_children_12_to_17)*100000)


# import pregnancy rates for context 
rate_in_pregnancy <- readRDS(file.path(paste(infection_output_tables_path, "rate_in_pregnancy.rds", sep ="/"))) 

pregnancy_rates <- rate_in_pregnancy %>%
  select(-c("Feb 2022", "Mar 2022","Apr 2022", "Total Mar 2020 to Apr 2022")) %>%
  pivot_longer(!indicator) %>%
  pivot_wider(id_cols = name, names_from = indicator, values_from=value) %>%
  rename(month = name) %>%
  adorn_totals()

##### maternal demographics #####

# by maternal age group 

total_births_maternal_age <- babies %>% # calculate the total number of live births each month 
  group_by(maternal_age_group) %>%
  summarise(total_live_births = n())

total_covid_maternal_age <- covid_babies %>% # calculates total number of neonates with covid each month
  group_by(maternal_age_group) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_maternal_age <- total_births_maternal_age %>%
  left_join(., total_covid_maternal_age, by=c("maternal_age_group")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>% 
  adorn_totals()

neonates_covid_rates_maternal_age <- neonates_covid_maternal_age %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)


# by maternal deprivation level 

total_births_simd <- babies %>% # calculate the total number of live births each month 
  group_by(simd) %>%
  summarise(total_live_births = n())

total_covid_simd <- covid_babies %>% # calculates total number of neonates with covid each month
  group_by(simd) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_simd <- total_births_simd %>%
  left_join(., total_covid_simd, by=c("simd")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>% 
  adorn_totals()

neonates_covid_rates_simd <- neonates_covid_simd %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)

# by ethnicitiy

total_births_ethnicity <- babies %>% # calculate the total number of live births each month 
  group_by(ethnicity_description) %>%
  summarise(total_live_births = n())

total_covid_ethnicity <- covid_babies %>% # calculates total number of neonates with covid each month
  group_by(ethnicity_description) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_ethnicity <- total_births_ethnicity %>%
  left_join(., total_covid_ethnicity, by=c("ethnicity_description")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>% 
  adorn_totals()

neonates_covid_rates_ethnicity <- neonates_covid_ethnicity %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)

##### baby information ######

# by sex of baby 

total_births_baby_sex<- babies %>% # calculate the total number of live births each month 
  group_by(x_baby_sex) %>%
  summarise(total_live_births = n()) %>%
  arrange(desc(x_baby_sex))

total_covid_baby_sex <- covid_babies %>% # calculates total number of neonates with covid each month
  group_by(x_baby_sex) %>%
  summarise(total_positive_covid_neonates = n()) %>%
  arrange(desc(x_baby_sex))

neonates_covid_baby_sex <- total_births_baby_sex %>%
  left_join(., total_covid_baby_sex, by=c("x_baby_sex")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>% 
  adorn_totals()

neonates_covid_rates_baby_sex <- neonates_covid_baby_sex %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)


# by gestation at birth  
babies <- babies %>%
  mutate(gestation_at_birth = case_when(between(x_gestation_at_outcome, 22, 36) ~ "1 Preterm (22-36w)", 
                                        between(x_gestation_at_outcome, 37, 44) ~ "2 Term+ (37-44w)", 
                                        T ~ "Unknown"), 
         preterm_specific = case_when(between(x_gestation_at_outcome, 22, 33) ~ "1a Earlier Preterm (22-33w)", 
                                       between(x_gestation_at_outcome, 34, 36) ~ "1b Later Preterm (34-36w)"))

covid_babies <- covid_babies %>%
  mutate(gestation_at_birth = case_when(between(x_gestation_at_outcome, 22, 36) ~ "1 Preterm (22-36w)", 
                                        between(x_gestation_at_outcome, 37, 44) ~ "2 Term+ (37-44w)", 
                                        T ~ "Unknown"), 
         preterm_specific = case_when(between(x_gestation_at_outcome, 22, 33) ~ "1a Earlier Preterm (22-33w)", 
                                      between(x_gestation_at_outcome, 34, 36) ~ "1b Later Preterm (34-36w)"))
# by gestation at birth  

total_births_gestation <- babies %>% # calculate the total number of live births each month 
  group_by(gestation_at_birth) %>%
  summarise(total_live_births = n())

total_births_gestation_specific <- babies %>% # calculate the total number of live births each month 
  filter(!is.na(preterm_specific)) %>%
  group_by(preterm_specific) %>%
  summarise(total_live_births = n()) %>%
  rename(gestation_at_birth = preterm_specific)

total_births_gestation <- total_births_gestation %>%
  bind_rows(total_births_gestation_specific) %>%
  arrange(gestation_at_birth)

total_covid_gestation <- covid_babies %>% # calculates total number of neonates with covid each month
  group_by(gestation_at_birth) %>%
  summarise(total_positive_covid_neonates = n())

total_covid_gestation_specific <- covid_babies %>% # calculate the total number of live births each month 
  filter(!is.na(preterm_specific)) %>%
  group_by(preterm_specific) %>%
  summarise(total_positive_covid_neonates = n()) %>%
  rename(gestation_at_birth = preterm_specific)

total_covid_gestation <- total_covid_gestation %>%
  bind_rows(total_covid_gestation_specific) %>%
  arrange(gestation_at_birth)

neonates_covid_gestation <- total_births_gestation %>%
  left_join(., total_covid_gestation, by=c("gestation_at_birth")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) 

total <- neonates_covid_rates_baby_sex %>% 
  filter(x_baby_sex == "Total") %>% 
  rename(gestation_at_birth = x_baby_sex)

neonates_covid_rates_gestation <- neonates_covid_gestation %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000) %>% 
  bind_rows(total)

##### maternal infection status #####
df_testdetails <- readRDS(paste0(folder_temp_data, "tests_details.rds") ) %>% 
  select(upi, index_date, covid_infection) %>% 
  pivot_wider(id_cols = upi, names_from = covid_infection, names_prefix = "test_", values_from = index_date)

maternal_infection_status <- babies %>% # calculate the total number of live births each month 
  left_join(df_testdetails, by = c("mother_upi" = "upi")) %>% 
  select(mother_upi, x_pregnancy_end_date, tests_baby_earliest_positive_test, test_1:test_4) %>% 
  mutate(days_since_test_1 = difftime(test_1, x_pregnancy_end_date, units="days"), 
         days_since_test_2 = difftime(test_2, x_pregnancy_end_date, units="days"), 
         days_since_test_3 = difftime(test_3, x_pregnancy_end_date, units="days"),
         days_since_test_4 = difftime(test_4, x_pregnancy_end_date, units="days"),
         maternal_infection_status_14_days_before_delivery = case_when(days_since_test_1 >= -14 & days_since_test_1 < 2 ~ "infected",  
                                                                       days_since_test_2 >= -14 & days_since_test_2 < 2 ~ "infected",
                                                                       days_since_test_3 >= -14 & days_since_test_3 < 2 ~ "infected",
                                                                       days_since_test_4 >= -14 & days_since_test_4 < 2 ~ "infected",
                                                                       T ~ "not infected")) 

total_births_maternal_infection_status <- maternal_infection_status %>%
  group_by(maternal_infection_status_14_days_before_delivery) %>%
  summarise(total_live_births = n())

covid_babies <- covid_babies %>% 
  left_join(., maternal_infection_status, by = c("mother_upi", "x_pregnancy_end_date", "tests_baby_earliest_positive_test"))

total_covid_maternal_infection_status <- covid_babies %>% # calculates total number of neonates with covid each month
  mutate(days_old_had_covid_test = round(difftime(tests_baby_earliest_positive_test, x_pregnancy_end_date, units="days"))) %>%
  filter(days_old_had_covid_test >=0 & days_old_had_covid_test <=27) %>%
  group_by(maternal_infection_status_14_days_before_delivery) %>%
  summarise(total_positive_covid_neonates = n())

neonates_covid_maternal_infection_status <- total_births_maternal_infection_status %>%
  left_join(., total_covid_maternal_infection_status, by=c("maternal_infection_status_14_days_before_delivery")) %>%
  mutate(total_positive_covid_neonates = replace_na(total_positive_covid_neonates, 0)) %>%  
  adorn_totals()

neonates_covid_rates_maternal_infection_status <- neonates_covid_maternal_infection_status %>%
  mutate(rate = (total_positive_covid_neonates/total_live_births)*100000, 
         lower_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="lower")*100000, 
         upper_ci = conf_int_wilson2(total_positive_covid_neonates, 
                                     total_live_births, 1.96, type="upper")*100000)

covid_babies_infected_mothers <-  covid_babies %>% 
  filter(maternal_infection_status_14_days_before_delivery == "infected") %>% 
  group_by(days_old_had_covid_test) %>% 
  summarise(number = n()) %>% 
  adorn_totals()
