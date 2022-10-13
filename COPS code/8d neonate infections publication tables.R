
template <- loadWorkbook(paste0(folder_templates, "Infection in pregnancy_template.xlsx"))
dates <- readRDS(paste0(folder_temp_data, "all_dates.rds"))
overall_testing_chi_completeness_perc <- readRDS(file.path(paste(folder_temp_data, "overall_testing_chi_completeness_perc.rds", sep ="/")))
chi_completeness_lfd_pos_perc <- readRDS(file.path(paste(folder_temp_data, "lfd_pos_chi_completeness_perc.rds", sep ="/")))
chi_completeness_pcr_pos_perc <- readRDS(file.path(paste(folder_temp_data, "pcr_pos_chi_completeness_perc.rds", sep ="/")))

#truncate infections at 30 april
end_infect_date <- as.Date("2022-04-30")
publication_latest_vacc_date <- as.Date("2022-04-30")#overwrite this variable for this one
#### data updatedness tab ####

fetus_level <- readRDS(paste0(folder_temp_data, "script6_baby_level_record_infection.rds")) %>% 
  filter(x_full_cohort == T)

fetus_level <-fetus_level %>% 
  rename(tests_mother_has_had_pcr_test_at_any_point = tests_mother_has_pos_test_at_any_point) %>%
  rename(tests_mother_positive_test_during_pregnancy_1 =tests_mother_value_positive_test_during_pregnancy_1, 
         tests_mother_positive_test_during_pregnancy_2 = tests_mother_value_positive_test_during_pregnancy_2 )

# Number of pregnancies
n_pregnancies <- fetus_level %>% 
  select(pregnancy_id, mother_upi) %>%
  group_by(pregnancy_id) %>%
  slice(1) %>%
  nrow()
# Number of women
n_mother <- fetus_level %>% 
  select(pregnancy_id, mother_upi) %>%
  group_by(mother_upi) %>%
  slice(1) %>%
  nrow()


temp_preg_valid_chi <- fetus_level %>%
  select(pregnancy_id, chi_validity) %>%
  group_by(pregnancy_id) %>%
  slice(1) %>%
  tabyl(chi_validity) 
#Number pregnancies with valid CHI
n_preg_valid_chi <-  temp_preg_valid_chi[2,2]
#Percent pregnancies with valid CHI
perc_preg_valid_chi <-  format(round(temp_preg_valid_chi[2,3] * 100, digits = 1))

temp_wom_valid_chi <- fetus_level %>%
  select(mother_upi, chi_validity) %>%
  group_by(mother_upi) %>%
  slice(1) %>%
  tabyl(chi_validity) 
#Number women with valid CHI
n_wom_valid_chi <-  temp_wom_valid_chi[2,2]
#Percent women with valid CHI
perc_wom_valid_chi <-  format(round(temp_wom_valid_chi[2,3] * 100, digits = 1))

data_sources <- dates %>% 
  filter(dataset != "Infant deaths" & dataset != "Vaccines" & dataset != "Testing" ) %>% 
  select(read_in_date, latest_record_date)

sigsag_dates <- read_rds(paste0(folder_temp_data, "SICSAG_dates.rds"))
smr01_smr02_dates <- read_rds(paste0(folder_temp_data, "SMR01_and_SMR02_dates.rds"))

data_sources_text_1 <- paste0("This release is based on the mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " update of the COPS study database." )
data_sources_text_2 <- paste0("As this release uses the mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " update of the COPS study database, we can be fairly confident that new pregnancies starting up to the end of ", format(publication_latest_vacc_date %m+% months(-3), "%B %Y"), 
                              " will have been included")
data_sources_text_3 <- paste0("and similarly any end of pregnancy events occuring up to the end of ", format(publication_latest_vacc_date %m+% months(-3), "%B %Y"), " will also have been included.")
data_sources_text_4 <- paste0("For this reason, the information provided (in this release, particularly that relating to infections in pregnancy from ", format(publication_latest_vacc_date %m+% months(-2), "%B %Y"), 
                              " onwards) should be viewed as provisional.")
data_sources_text_5 <- paste0("This release includes records of tests taken on up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), ".")
data_sources_text_6 <- paste0("The extract of testing data was taken from the NHS Scotland Corporate Data Warehouse on ", format(dates$read_in_date[dates$dataset == "Testing"], "%d %B %Y"), "." )
data_sources_text_7 <- paste0("This release includes records of vaccinations delivered on up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), ".")
#data_sources_text_8 <- paste0("The extract of vaccination data was taken from the National Clinical Data Store on ", format(dates$read_in_date[dates$dataset == "Vaccines"], "%d %B %Y"), "." )
data_sources_text_9 <- paste0("Date of data extract for mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " COPS study database update")
data_sources_text_10 <- paste0("Overall, ", chi_completeness_pcr_pos_perc, "% of all positive viral PCR test records for samples taken from the start of the COVID-19 pandemic to ", format(publication_latest_vacc_date, "%d %B %Y"), " have an associated CHI number")
data_sources_text_11 <- paste0("As at mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", the COPS database included a total of ", format(n_pregnancies, big.mark=","), " pregnancies among ", format(n_mother, big.mark=","), 
                               " women: ", format(n_preg_valid_chi, big.mark=","), " (", perc_preg_valid_chi, "%) of these pregnancy records (and ", format(n_wom_valid_chi, big.mark=","), ", ", perc_wom_valid_chi, "% of the women) have an associated CHI number.")
data_sources_text_12 <- paste0("This release includes SMR01 and SMR02 records received by PHS by ", format(smr01_smr02_dates$read_in_date[smr01_smr02_dates$dataset == "SMR01_and_SMR02"], "%d %B %Y"))
data_sources_text_13 <- paste0("This release includes SICSAG records for patients discharged from critical care up to ", format(sigsag_dates$latest_record_date[sigsag_dates$dataset == "SICSAG"], "%d %B %Y"))
data_sources_text_14 <- paste0("and ", chi_completeness_lfd_pos_perc, "% of all positive viral LFD test records for samples taken from 6 Jan 2022 to ", format(publication_latest_vacc_date, "%d %B %Y"), " have an associated CHI number.")


writeData(template, "Data sources", data_sources, startCol = 3, startRow = 20, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_1, startCol = 1, startRow = 17, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_2, startCol = 1, startRow = 35, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_3, startCol = 1, startRow = 36, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_4, startCol = 1, startRow = 39, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_5, startCol = 1, startRow = 52, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_6, startCol = 1, startRow = 53, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_7, startCol = 1, startRow = 75, colNames = FALSE)
#writeData(template, "Data sources", data_sources_text_8, startCol = 1, startRow = 81, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_9, startCol = 3, startRow = 19, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_10, startCol = 1, startRow = 80, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_14, startCol = 1, startRow = 81, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_11, startCol = 1, startRow = 77, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_12, startCol = 1, startRow = 61, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_13, startCol = 1, startRow = 67, colNames = FALSE)

fetus_level_processed <- fetus_level %>%
  filter(chi_validity == "Valid CHI") %>% 
  filter(x_est_conception_date <= publication_latest_vacc_date) %>%   # only include data for the publication period
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
  filter(mother_positive_test_during_pregnancy <= end_infect_date) %>% 
  mutate(month = format(as.Date(mother_positive_test_during_pregnancy), "%Y-%m") ) %>% 
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, mother_positive_test_during_pregnancy))
  

pregnancies <- read_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds")) %>%
  filter(full_cohort == T) %>%
  filter(chi_validity == "Valid CHI") %>% # Only include mothers with a valid CHI
  filter(est_conception_date <= publication_latest_vacc_date) %>%   # only include data for the publication period
  select(-chi_validity) %>%
  mutate(hbres = case_when(is.na(hbres) ~ "z Not Known",
                           hbres=="England/Wales/Northern Ireland" ~ "z Not Known",
                           hbres=="Outside UK" ~ "z Not Known",
                           hbres=="Not Known" ~ "z Not Known",
                           hbres=="No Fixed Abode" ~ "z Not Known",
                           T ~ hbres)) %>%
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>%
  mutate(ethnicity_desc_reporting = cops_reporting_ethnicity(ethnicity_code)) %>%
  mutate(ethnicity_desc_reporting = case_when(is.na(ethnicity_desc_reporting) ~ "5 Unknown/missing", 
                                              TRUE ~ethnicity_desc_reporting) ) %>%
  mutate(simd = as.character(simd)) %>%
  mutate(simd = case_when(is.na(simd) ~ "9 Unknown",
                          simd=="9" ~ "9 Unknown",
                          T ~ simd))

#### Check linkage numbers ####
pregnancies %>%
  select(pregnancy_id, mother_has_had_pcr_test_at_any_point) %>%
  mutate(mother_has_had_pcr_test_at_any_point = case_when(is.na(mother_has_had_pcr_test_at_any_point) ~ 0,
                                                          !is.na(mother_has_had_pcr_test_at_any_point) ~ 1)
  ) %>%
  tabyl(mother_has_had_pcr_test_at_any_point)


#### Cases by Week ####
data_cases_by_week <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == T) %>%
  select(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) %>%
  filter((!is.na(value))) %>%
  filter(value <= publication_latest_vacc_date) %>%
  mutate(week_end = ceiling_date(value, unit = "week", change_on_boundary = F)) %>%  # Set the week start to Monday
  group_by(week_end) %>%
  summarise(number_of_cases = n()) %>%
  ungroup() %>%
  mutate(iso_week = format(week_end, "%Y-W%V"))

pregnancies_weeks_spine <- seq(min_(data_cases_by_week$week_end), max_(data_cases_by_week$week_end), by="weeks") %>%
  as.data.frame() %>%
  rename("week_end" = ".") %>%
  mutate(iso_week = format(week_end, "%Y-W%V")) %>%
  select(week_end, iso_week)

data_cases_by_week_final <- pregnancies_weeks_spine %>%
  left_join(data_cases_by_week, by= c("iso_week" = "iso_week", "week_end" = "week_end")) %>%
  filter(week_end >= as.Date("2020-03-01")) %>%
  mutate(number_of_cases = case_when(is.na(number_of_cases) ~ as.double(0),
                                     T ~ as.double(number_of_cases))) %>%
  adorn_totals(where = "row") %>% 
  select(-iso_week)

# Logic around whether last week in Table1 is a partial week beyond publication_latest_vacc_date
cases_by_week_partial_print <- max_(data_cases_by_week$week_end) > publication_latest_vacc_date

if (cases_by_week_partial_print == TRUE){
  cases_by_week_partial <- paste0("Please note week ending ", format(max_(data_cases_by_week$week_end), "%d %B %Y"), " is shown for comparison against the monthly data. Week ending the ", 
                          format(max_(data_cases_by_week$week_end), "%d %B %Y"), " is a partial week as positive tests up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), " are included in this analysis.") 
} else{
  cases_by_week_partial = ""
}

cases_by_week_moving_notes <- c(cases_by_week_partial, "Grey shading indicates provisional data and may change in future publications", "", "Week indicates the week of onset of COVID-19", 
                                "COVID-19 is defined as occurring in pregnancy if the date of onset occurs at any point from the estimated date of conception (date the woman was 2+0 weeks gestation) up to and including the date the pregnancy ended",
                                "For any individual, the specimen date of their first positive viral PCR result is taken as the date of onset of their first episode of COVID-19.",
                                "Subsequent positive viral PCR results with specimen date within 90 days of their first positive result are discounted.",
                                "If the individual then has a positive viral PCR result with specimen date ≥90 days after their first positive result, this is taken as the date of onset of their second episode of COVID-19.",
                                "Subsequent positive viral PCR results with specimen date within 90 days of this second index date are then discounted as for the first episode of COVID, and so on.")

subtitle <- paste0("Based on COPS pregnancy database updated in mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", and cases of COVID-19 with date of onset up to ", format(publication_latest_vacc_date, "%d %B %Y"))

data_cases_by_week_final %>% write_rds(paste0(folder_temp_data, "infection_output_tables/cases_by_week.rds"))

writeData(template, "N cases by week", subtitle, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "N cases by week", cases_by_week_moving_notes, startCol = 1, startRow = nrow(data_cases_by_week_final) + 7, colNames = FALSE)
writeData(template, "N cases by week", data_cases_by_week_final, startCol=1, startRow=5, colNames = F)

#### Cases by Gestation ####

# create lookup to add months in words to outputs
month_lookup <- pregnancies_weeks_spine %>% 
  filter(week_end <= publication_latest_vacc_date) %>% 
  mutate(month_in_words = format(as.Date(week_end), "%b %Y")) %>% 
  mutate(month = format(as.Date(week_end), "%Y-%m")) %>% 
  select(month_in_words, month) %>% 
  distinct()

data_cases_by_gestation <- pregnancies  %>%
  filter(mother_tested_positive_during_pregnancy == T) %>%
  select(pregnancy_id, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2, est_conception_date) %>%
  pivot_longer(c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) %>%
  filter((!is.na(value))) %>%
  mutate(trimester = trimester(est_conception_date, value)) %>% 
  mutate(month = format(as.Date(value), "%Y-%m") ) %>%
  select(trimester, month) %>%
  group_by(month, trimester) %>%
  summarise(number_of_cases = n()) %>%
  ungroup() %>%
  left_join(month_lookup) %>% 
  select(-month) %>% filter(!is.na(month_in_words)) %>%
  pivot_wider(names_from = "month_in_words", values_from = "number_of_cases") %>%
  adorn_totals(where = c("row", "col")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(admission = "1 - Total") 

## read in hospital and ICU admissions by trimester calculated in 6d
admissions_by_gestation <- readRDS(paste0(folder_temp_data, "infection_output_tables/admissions_by_trimester.rds"))
icu_by_gestation <- readRDS(paste0(folder_temp_data, "infection_output_tables/icu_admissions_by_trimester.rds"))

all_admissions_by_gestation <- admissions_by_gestation %>% 
  bind_rows(icu_by_gestation) %>% 
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = "month_in_words", values_from = "n_covid_adm") %>% 
  adorn_totals(where = "col")

all_admissions_by_gestation %>% write_rds(paste0(folder_temp_data, "infection_output_tables/all_admissions_by_gestation.rds"))

  
cases_admissions_by_gestation <- all_admissions_by_gestation %>% 
  bind_rows(data_cases_by_gestation)

adm_gest_spine <- expand_grid(cases_admissions_by_gestation$trimester, cases_admissions_by_gestation$admission) %>% 
  distinct() %>% 
  rename("trimester" = "cases_admissions_by_gestation$trimester") %>% 
  rename("admission" = "cases_admissions_by_gestation$admission")%>% 
  arrange(trimester, admission)

cases_admissions_by_gestation_on_spine <- adm_gest_spine %>% 
  left_join(cases_admissions_by_gestation, by = c("trimester" = "trimester", "admission" = "admission"))  %>%
  mutate_all(~replace(., is.na(.), 0))
  

#gestation_text_1 <- paste0("Due to the few number of weeks passed since " , format(publication_latest_vacc_date %m+% months(-1), "%B %Y"), " we are unlikely to have a complete count of the number of women in their first trimester during " 
#                           , format(publication_latest_vacc_date %m+% months(-1), "%B %Y"))
#gestation_text_2 <- paste0("Due to the few number of weeks passed since " , format(publication_latest_vacc_date, "%B %Y"), " we are unlikely to have a complete count of the number of women in their first trimester during " 
#                           , format(publication_latest_vacc_date, "%B %Y"))

data_cases_by_gestation %>% write_rds(paste0(folder_temp_data, "infection_output_tables/cases_by_gestation.rds"))

writeData(template, "N cases by gest and adm status", subtitle, startCol = 1, startRow = 2, colNames = FALSE)
#writeData(template, "N cases by gest and adm status", gestation_text_1, startCol = 1, startRow = 21, colNames = FALSE)
#writeData(template, "N cases by gest and adm status", gestation_text_2, startCol = 1, startRow = 22, colNames = FALSE)
writeData(template, "N cases by gest and adm status", select(cases_admissions_by_gestation_on_spine, -c(trimester, admission)), startCol=3, startRow=5)


#### Severe outcomes by Gestation ####

## Overall numbers for comparison: cases
data_cases_by_gestation_severe_outcomes_tab <- pregnancies  %>%
  filter(mother_tested_positive_during_pregnancy == T) %>%
  select(mother_upi, pregnancy_id, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2, est_conception_date) %>%
  pivot_longer(c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) %>%
  filter((!is.na(value))) %>%
  mutate(trimester = trimester(est_conception_date, value)) %>% 
  mutate(month = format(as.Date(value), "%Y-%m") ) %>% filter(value <= publication_latest_vacc_date)

# cases by month
cases_by_gestation_severe_outcomes_tab_monthly <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(trimester, month) %>%
  group_by(month, trimester) %>%
  summarise(n = n()) %>%
  ungroup() 
cases_by_gestation_severe_outcomes_tab_col_total <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(trimester, month) %>%
  mutate(trimester = "Total") %>% 
  group_by(month, trimester) %>%
  summarise(n = n()) %>%
  ungroup() 

cases_by_gestation_severe_outcomes_tab_row_total <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(trimester, month) %>%
  mutate(month = "Total") %>% 
  group_by(month, trimester) %>%
  summarise(n = n()) %>%
  ungroup() 

cases_by_gestation_severe_outcomes_tab <- data_cases_by_gestation_severe_outcomes_tab %>% 
  select(trimester, month) %>%
  mutate(month = "Total") %>% 
  mutate(trimester = "Total") %>% 
  group_by(month, trimester) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  bind_rows(cases_by_gestation_severe_outcomes_tab_monthly) %>% 
  bind_rows(cases_by_gestation_severe_outcomes_tab_col_total) %>% 
  bind_rows(cases_by_gestation_severe_outcomes_tab_row_total) %>%   
  mutate(indicator = "Total confirmed cases of COVID-19 in pregnancy")

# pregnancies by month
preg_severe_outcomes_tab <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(month, pregnancy_id) %>%
  distinct() %>% 
  count(month) %>% 
  right_join(month_lookup)

preg_severe_outcomes_tab_total <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(pregnancy_id) %>%
  distinct() %>% 
  mutate(month = "Total") %>% 
  count(month) %>%
  bind_rows(preg_severe_outcomes_tab) %>% 
  arrange(month) %>% 
  select(-month_in_words) %>% 
  pivot_wider(names_from = month, values_from = n)

# women by month
women_severe_outcomes_tab <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(month, mother_upi) %>%
  distinct() %>% 
  count(month)%>% 
  right_join(month_lookup)

women_severe_outcomes_tab_total <- data_cases_by_gestation_severe_outcomes_tab %>%
  select(mother_upi) %>%
  distinct() %>% 
  mutate(month = "Total") %>% 
  count(month) %>%
  bind_rows(women_severe_outcomes_tab) %>% 
  arrange(month) %>% 
  select(-month_in_words) %>% 
  pivot_wider(names_from = month, values_from = n)


## Overall numbers for comparison: live births ##

data_livebirths <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  select(pregnancy_id, month, x_est_conception_date, mother_positive_test_during_pregnancy, x_pregnancy_end_date, fetus_number) %>% 
  mutate(live_birth_after_infection_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                   end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_live_birth_after_infection = if_else(x_pregnancy_end_date %within% live_birth_after_infection_interval, TRUE, FALSE))

# within 28 days
live_births_after_infection_monthly <- data_livebirths %>% 
  filter(flag_live_birth_after_infection == TRUE) %>% 
  count(month) %>% 
  right_join(month_lookup)

live_births_after_infection <- data_livebirths %>% 
  filter(flag_live_birth_after_infection == TRUE) %>% 
  mutate(month = "Total") %>% 
  select(pregnancy_id, month, fetus_number) %>% 
  distinct() %>% 
  count(month) %>% 
  bind_rows(live_births_after_infection_monthly) %>% 
  mutate(month_in_words = if_else(is.na(month_in_words), "Total", month_in_words)) %>% 
  mutate(indicator = "live birth within 28 days of infection")


# all subsequent live births
subsequent_live_births_monthly <- data_livebirths %>% 
  count(month) %>% 
  right_join(month_lookup)

subsequent_live_births <- data_livebirths %>% 
  mutate(month = "Total") %>% 
  select(pregnancy_id, month, fetus_number) %>% 
  distinct() %>% 
  count(month) %>% 
  bind_rows(subsequent_live_births_monthly) %>% 
  mutate(month_in_words = if_else(is.na(month_in_words), "Total", month_in_words)) %>% 
  mutate(indicator = "subsequent live birth") 


# combine into one df
live_births <- live_births_after_infection %>% 
  bind_rows(subsequent_live_births) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(month, indicator) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = n)


## Maternal deaths

maternal_deaths_by_gest <- readRDS(paste0(folder_temp_data, "infection_output_tables/maternal_deaths_by_gestation.rds"))

## Pregnancies ending in stillbirth ##
data_stillbirths_preg <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(mother_upi, month, pregnancy_id, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, vaccination_status_at_infection) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(covid_associated_stillbirth_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                         end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_stillbirth = if_else(x_pregnancy_end_date %within% covid_associated_stillbirth_interval, TRUE, FALSE)) %>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days
covid_associated_stillbirths_preg_by_gest <- associated_pregnancies(data_stillbirths_preg, pregnancy_id, flag_covid_associated_stillbirth, 
                                                                    month, trimester, "pregnancies ending in covid associated stillbirths")
# subsequent
subsequent_stillbirths_preg_by_gest <- subsequent_pregnancies(data_stillbirths_preg, pregnancy_id, 
                                                              month, trimester, "pregnancies ending in subsequent stillbirths")

## Stillbirths ##
data_stillbirths <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(mother_upi, month, pregnancy_id, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, fetus_number, vaccination_status_at_infection) %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(covid_associated_stillbirth_interval = interval(start = mother_positive_test_during_pregnancy, 
                                               end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_stillbirth = if_else(x_pregnancy_end_date %within% covid_associated_stillbirth_interval, TRUE, FALSE))%>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))
  
# within 28 days 
covid_associated_stillbirths_by_gest <- associated_babies(data_stillbirths, pregnancy_id, flag_covid_associated_stillbirth, 
                                                                    month, trimester, "covid associated stillbirths")
# subsequent
subsequent_stillbirths_by_gest <- subsequent_babies(data_stillbirths, pregnancy_id, 
                                                              month, trimester, "subsequent stillbirths")

## Pregnancies ending in preterm birth ##
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

# within 28 days
covid_associated_preterm_preg_by_gest <- associated_pregnancies(data_preterm_preg, pregnancy_id, flag_covid_associated_preterm, 
                                                                    month, trimester, "pregnancies ending in covid associated preterm births")
# subsequent
subsequent_preterm_preg_by_gest <- subsequent_pregnancies(data_preterm_preg, pregnancy_id, 
                                                              month, trimester, "pregnancies ending in subsequent preterm births")

## Preterm births ##
data_preterm <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  select(mother_upi, month, pregnancy_id, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, vaccination_status_at_infection, x_gestation_at_outcome, fetus_number) %>% 
  mutate(covid_associated_preterm_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                      end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_preterm = if_else(x_pregnancy_end_date %within% covid_associated_preterm_interval, TRUE, FALSE)) %>% 
  mutate(flag_preterm = if_else(x_gestation_at_outcome < 37, TRUE, FALSE)) %>% 
  mutate(flag_very_preterm = if_else(x_gestation_at_outcome < 32, TRUE, FALSE)) %>% 
  select(-x_gestation_at_outcome) %>% 
  filter(flag_preterm == TRUE) %>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days 
covid_associated_preterm_by_gest <- associated_babies(data_preterm, pregnancy_id, flag_covid_associated_preterm, 
                                                          month, trimester, "covid associated preterm births")
# subsequent
subsequent_preterm_by_gest <- subsequent_babies(data_preterm, pregnancy_id, 
                                                    month, trimester, "subsequent preterm births")

## Pregnancies ending in very preterm birth ##
data_very_preterm_preg <- data_preterm_preg %>% 
  filter(flag_very_preterm == TRUE) 

# within 28 days
covid_associated_very_preterm_preg_by_gest <- associated_pregnancies(data_very_preterm_preg, pregnancy_id, flag_covid_associated_preterm, 
                                                                month, trimester, "pregnancies ending in covid associated very preterm births")
# subsequent
subsequent_very_preterm_preg_by_gest <- subsequent_pregnancies(data_very_preterm_preg, pregnancy_id, 
                                                          month, trimester, "pregnancies ending in subsequent very preterm births")

## Very preterm births ##
data_very_preterm <- data_preterm %>% 
  filter(flag_very_preterm == TRUE) 

# within 28 days 
covid_associated_very_preterm_by_gest <- associated_babies(data_very_preterm, pregnancy_id, flag_covid_associated_preterm, 
                                                      month, trimester, "covid associated very preterm births")
# subsequent
subsequent_very_preterm_by_gest <- subsequent_babies(data_very_preterm, pregnancy_id, 
                                                month, trimester, "subsequent very preterm births")



### Apgar score
data_apgar_base <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth" & x_gestation_at_outcome >= 37) %>% 
  select(mother_upi, pregnancy_id, month, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, vaccination_status_at_infection, smr02_apgar_5_minutes) %>% 
  mutate(smr02_apgar_5_minutes = if_else(smr02_apgar_5_minutes == "NR" | smr02_apgar_5_minutes == "RR", NA_character_, smr02_apgar_5_minutes)) %>% 
  mutate(smr02_apgar_5_minutes = str_remove(smr02_apgar_5_minutes, " ")) %>% 
  mutate(smr02_apgar_5_minutes = as.numeric(smr02_apgar_5_minutes)) %>% 
  filter(smr02_apgar_5_minutes < 7) %>% 
  mutate(covid_associated_birth_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                             end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_low_apgar = if_else(x_pregnancy_end_date %within% covid_associated_birth_interval & smr02_apgar_5_minutes < 7, TRUE, FALSE)) %>% 
  mutate(flag_covid_associated_very_low_apgar = if_else(x_pregnancy_end_date %within% covid_associated_birth_interval & smr02_apgar_5_minutes < 4, TRUE, FALSE)) 

## Pregnancies ending in birth with low Apgar score ##
data_low_apgar_preg <- data_apgar_base %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days
covid_associated_low_apgar_preg_by_gest <- associated_pregnancies(data_low_apgar_preg, pregnancy_id, flag_covid_associated_low_apgar, 
                                                                        month, trimester, "pregnancies ending in covid associated low apgar score")
# subsequent
subsequent_low_apgar_preg_by_gest <- subsequent_pregnancies(data_low_apgar_preg, pregnancy_id, 
                                                                  month, trimester, "pregnancies ending in subsequent low apgar score")

## Births wirth with low Apgar score ##
data_low_apgar <- data_apgar_base %>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days
covid_associated_low_apgar_by_gest <- associated_pregnancies(data_low_apgar, pregnancy_id, flag_covid_associated_low_apgar, 
                                                                  month, trimester, "covid associated low apgar score")
# subsequent
subsequent_low_apgar_by_gest <- subsequent_pregnancies(data_low_apgar, pregnancy_id, 
                                                            month, trimester, "subsequent low apgar score")

## Pregnancies ending in birth with very low Apgar score ##
data_very_low_apgar_preg <- data_low_apgar_preg %>% 
  filter(smr02_apgar_5_minutes < 4)

# within 28 days
covid_associated_very_low_apgar_preg_by_gest <- associated_pregnancies(data_very_low_apgar_preg, pregnancy_id, flag_covid_associated_very_low_apgar, 
                                                                  month, trimester, "pregnancies ending in covid associated very low apgar score")
# subsequent
subsequent_very_low_apgar_preg_by_gest <- subsequent_pregnancies(data_very_low_apgar_preg, pregnancy_id, 
                                                            month, trimester, "pregnancies ending in subsequent very low apgar score")

## Births wirth with very low Apgar score ##
data_very_low_apgar <- data_low_apgar %>% 
  filter(smr02_apgar_5_minutes < 4)

# within 28 days
covid_associated_very_low_apgar_by_gest <- associated_pregnancies(data_very_low_apgar, pregnancy_id, flag_covid_associated_very_low_apgar, 
                                                             month, trimester, "covid associated very low apgar score")
# subsequent
subsequent_very_low_apgar_by_gest <- subsequent_pregnancies(data_very_low_apgar, pregnancy_id, 
                                                       month, trimester, "subsequent very low apgar score")


## Pregnancies ending in neonatal deaths
data_neonatal_deaths_preg <- fetus_level_processed_long %>% 
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(mother_upi, pregnancy_id, month, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, vaccination_status_at_infection) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(covid_associated_neonatal_death_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                             end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_neonatal_death = if_else(x_pregnancy_end_date %within% covid_associated_neonatal_death_interval, TRUE, FALSE))%>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days
covid_associated_neonatal_deaths_preg_by_gest <- associated_pregnancies(data_neonatal_deaths_preg, pregnancy_id, flag_covid_associated_neonatal_death, 
                                                                    month, trimester, "pregnancies ending in covid associated neonatal deaths")
# subsequent
subsequent_neonatal_deaths_preg_by_gest <- subsequent_pregnancies(data_neonatal_deaths_preg, pregnancy_id, 
                                                              month, trimester, "pregnancies ending in subsequent neonatal deaths")

## Neonatal deaths
data_neonatal_deaths <- fetus_level_processed_long %>% 
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(mother_upi, pregnancy_id, month, x_est_conception_date, x_pregnancy_end_date, mother_positive_test_during_pregnancy, fetus_number, vaccination_status_at_infection) %>% 
  filter(!is.na(mother_positive_test_during_pregnancy)) %>% 
  mutate(covid_associated_neonatal_death_interval = interval(start = mother_positive_test_during_pregnancy, 
                                                         end =  mother_positive_test_during_pregnancy + days(27))) %>% 
  mutate(flag_covid_associated_neonatal_death = if_else(x_pregnancy_end_date %within% covid_associated_neonatal_death_interval, TRUE, FALSE))%>% 
  mutate(trimester = trimester(x_est_conception_date, mother_positive_test_during_pregnancy))

# within 28 days 
covid_associated_neonatal_deaths_by_gest <- associated_babies(data_neonatal_deaths, pregnancy_id, flag_covid_associated_neonatal_death, 
                                                          month, trimester, "covid associated neonatal deaths")
# subsequent
subsequent_neonatal_deaths_by_gest <- subsequent_babies(data_neonatal_deaths, pregnancy_id, 
                                                    month, trimester, "subsequent neonatal deaths")

## create spine
serious_outcomes_levels <- c("Total confirmed cases of COVID-19 in pregnancy", "covid associated maternal death", "subsequent maternal death", "pregnancies ending in covid associated stillbirths",
                      "pregnancies ending in subsequent stillbirths", "covid associated stillbirths", "subsequent stillbirths", 
                      "pregnancies ending in covid associated preterm births", "pregnancies ending in subsequent preterm births", "covid associated preterm births", "subsequent preterm births",
                      "pregnancies ending in covid associated very preterm births", "pregnancies ending in subsequent very preterm births", "covid associated very preterm births", "subsequent very preterm births",
                      "pregnancies ending in covid associated low apgar score", "pregnancies ending in subsequent low apgar score", "covid associated low apgar score", "subsequent low apgar score",
                      "pregnancies ending in covid associated very low apgar score", "pregnancies ending in subsequent very low apgar score", "covid associated very low apgar score", "subsequent very low apgar score",
                      "pregnancies ending in covid associated neonatal deaths", "pregnancies ending in subsequent neonatal deaths",
                      "covid associated neonatal deaths", "subsequent neonatal deaths")
serious_outcomes_factor <- factor(serious_outcomes_levels, levels = serious_outcomes_levels)

serious_gest_spine_1 <- expand.grid(month_lookup$month, adm_gest_spine$trimester, serious_outcomes_factor) %>% 
  distinct() %>% 
  rename(month = Var1, trimester = Var2, indicator = Var3)

serious_gest_spine <- serious_gest_spine_1 %>% 
  mutate(month = "Total") %>% 
  group_by(month, trimester, indicator) %>% 
  slice(1) %>% 
  ungroup() %>% 
  bind_rows(serious_gest_spine_1) %>% 
  arrange(trimester, month, indicator)

# combine all outcomes into one df
serious_outcomes_combined <- cases_by_gestation_severe_outcomes_tab %>% 
  bind_rows(maternal_deaths_by_gest) %>% 
  bind_rows(covid_associated_stillbirths_preg_by_gest) %>% 
  bind_rows(subsequent_stillbirths_preg_by_gest) %>% 
  bind_rows(covid_associated_stillbirths_by_gest) %>% 
  bind_rows(subsequent_stillbirths_by_gest) %>% 
  bind_rows(covid_associated_preterm_preg_by_gest) %>% 
  bind_rows(subsequent_preterm_preg_by_gest) %>% 
  bind_rows(covid_associated_preterm_by_gest) %>% 
  bind_rows(subsequent_preterm_by_gest) %>% 
  bind_rows(covid_associated_very_preterm_preg_by_gest) %>% 
  bind_rows(subsequent_very_preterm_preg_by_gest) %>% 
  bind_rows(covid_associated_very_preterm_by_gest) %>% 
  bind_rows(subsequent_very_preterm_by_gest) %>% 
  bind_rows(covid_associated_low_apgar_preg_by_gest) %>% 
  bind_rows(subsequent_low_apgar_preg_by_gest) %>% 
  bind_rows(covid_associated_low_apgar_by_gest) %>% 
  bind_rows(subsequent_low_apgar_by_gest) %>% 
  bind_rows(covid_associated_very_low_apgar_preg_by_gest) %>% 
  bind_rows(subsequent_very_low_apgar_preg_by_gest) %>% 
  bind_rows(covid_associated_very_low_apgar_by_gest) %>% 
  bind_rows(subsequent_very_low_apgar_by_gest) %>% 
  bind_rows(covid_associated_neonatal_deaths_preg_by_gest) %>% 
  bind_rows(subsequent_neonatal_deaths_preg_by_gest) %>% 
  bind_rows(covid_associated_neonatal_deaths_by_gest) %>% 
  bind_rows(subsequent_neonatal_deaths_by_gest)

all_serious_outcomes <- serious_gest_spine %>% 
  arrange(month, trimester, indicator) %>% 
  left_join(serious_outcomes_combined, by = c("month", "trimester", "indicator")) %>% 
  mutate_all(~replace(., is.na(.), 0))

all_serious_outcomes_wide <- all_serious_outcomes %>% 
  left_join(month_lookup) %>% 
  select(-month) %>% 
  mutate(month_in_words = if_else(is.na(month_in_words), "Total", month_in_words)) %>% 
  pivot_wider(names_from = month_in_words, values_from = n)

cases_total <- all_serious_outcomes_wide %>% 
  filter(trimester == "Total" & indicator == "Total confirmed cases of COVID-19 in pregnancy")

all_serious_outcomes_by_gest <- all_serious_outcomes_wide %>% 
  filter(trimester != "Total")

all_serious_outcomes_total <- all_serious_outcomes_wide %>% 
  filter(trimester == "Total" & indicator != "Total confirmed cases of COVID-19 in pregnancy")

all_serious_outcomes_total %>% write_rds(paste0(folder_temp_data, "infection_output_tables/infection_severe_outcomes_totals.rds"))
all_serious_outcomes_by_gest %>% write_rds(paste0(folder_temp_data, "infection_output_tables/infection_severe_outcomes_gest.rds"))
live_births %>% write_rds(paste0(folder_temp_data, "infection_output_tables/all_live_births_after_infection.rds"))
  
## read out into template 
writeData(template, "Sev oc by gest NFP", subtitle, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Sev oc by gest NFP", select(all_serious_outcomes_by_gest, -c(trimester, indicator)), startCol=3, startRow=7)

writeData(template, "Severe outcomes tot NFP", subtitle, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(cases_total, -c(trimester, indicator)), startCol=2, startRow=7)
writeData(template, "Severe outcomes tot NFP", preg_severe_outcomes_tab_total, startCol=2, startRow=9, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", women_severe_outcomes_tab_total, startCol=2, startRow=10, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(live_births, -indicator), startCol=2, startRow=11, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(all_serious_outcomes_total, -c(trimester, indicator)), startCol=2, startRow=13, colNames = FALSE)

#### Cases by Vaccination Status ####
data_cases_by_vaccination_status_1 <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(pregnancy_id, mother_upi, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2, dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date) %>%
  pivot_longer(c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) %>%
  filter((!is.na(value))) %>%
  mutate(positive_test_date = value) %>%
  filter(positive_test_date <= end_infect_date) %>%
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, positive_test_date)) %>% 
  mutate(month = format(as.Date(positive_test_date), "%Y-%m") ) %>%
  group_by(month, vaccination_status_at_infection) %>%
  summarise(number_of_cases = n()) %>%
  ungroup()  

data_cases_by_vaccination_status <- data_cases_by_vaccination_status_1 %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = "month_in_words", values_from = "number_of_cases") %>%
  adorn_totals(where = c("row", "col")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(admission = "1 - Total")

cases_by_vaccination_dec_onwards <- data_cases_by_vaccination_status_1 %>% 
  filter(month >= "2020-12") %>%   
  pivot_wider(names_from = "month", values_from = "number_of_cases") %>%
  adorn_totals(where = c("row", "col")) %>%
  mutate_all(~replace(., is.na(.), 0))
  

## read in hospital and ICU admissions by vaccination status calculated in 6d
admissions_by_vaccination <- readRDS(paste0(folder_temp_data, "infection_output_tables/admissions_by_vaccination.rds"))
icu_by_vaccination <- readRDS(paste0(folder_temp_data, "infection_output_tables/icu_admissions_by_vaccination.rds"))

all_admissions_by_vaccination <- admissions_by_vaccination %>% 
  bind_rows(icu_by_vaccination) %>% 
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = "month_in_words", values_from = "n_covid_adm") %>% 
  adorn_totals(where = "col")

cases_admissions_by_vaccination <- all_admissions_by_vaccination %>% 
  bind_rows(data_cases_by_vaccination_status)

adm_vacc_spine <- expand_grid(cases_admissions_by_vaccination$vaccination_status_at_infection, cases_admissions_by_vaccination$admission) %>% 
  distinct() %>% 
  rename("vaccination_status_at_infection" = "cases_admissions_by_vaccination$vaccination_status_at_infection") %>% 
  rename("admission" = "cases_admissions_by_vaccination$admission")%>% 
  arrange(vaccination_status_at_infection, admission)

cases_admissions_by_vaccination_on_spine <- adm_vacc_spine %>% 
  left_join(cases_admissions_by_vaccination, by = c("vaccination_status_at_infection" = "vaccination_status_at_infection", "admission" = "admission"))  %>%
  mutate_all(~replace(., is.na(.), 0))

cases_by_vaccination_dec_onwards %>% write_rds(paste0(folder_temp_data, "infection_output_tables/cases_by_vaccination.rds"))

writeData(template, "N cases by vacc status", subtitle, startCol = 1, startRow = 2, colNames = FALSE)  
writeData(template, "N cases by vacc status", select(cases_admissions_by_vaccination_on_spine, -c(vaccination_status_at_infection, admission)), startCol=3, startRow=5)


#### Severe outcomes by Vaccination status ####

## Overall numbers for comparison: cases
data_cases_by_vacc_severe_outcomes_tab <- pregnancies  %>%
  filter(mother_tested_positive_during_pregnancy == T) %>%
  select(mother_upi, pregnancy_id, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2, dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, est_conception_date) %>%
  pivot_longer(c(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2)) %>%
  filter((!is.na(value))) %>%
  mutate(positive_test_date = value) %>%
  mutate(vaccination_status_at_infection = vaccination_status(dose_1_vacc_occurence_date, dose_2_vacc_occurence_date, dose_3_vacc_occurence_date, positive_test_date)) %>% 
  mutate(month = format(as.Date(value), "%Y-%m") ) 

# cases by month

cases_by_vacc_severe_outcomes_tab_monthly <- data_cases_by_vacc_severe_outcomes_tab %>%
  select(vaccination_status_at_infection, month) %>%
  group_by(month, vaccination_status_at_infection) %>%
  summarise(n = n()) %>%
  ungroup() 
cases_by_vacc_severe_outcomes_tab_col_total <- data_cases_by_vacc_severe_outcomes_tab %>%
  select(vaccination_status_at_infection, month) %>%
  mutate(vaccination_status_at_infection = "Total") %>% 
  group_by(month, vaccination_status_at_infection) %>%
  summarise(n = n()) %>%
  ungroup() 

cases_by_vacc_severe_outcomes_tab_row_total <- data_cases_by_vacc_severe_outcomes_tab %>%
  select(vaccination_status_at_infection, month) %>%
  mutate(month = "Total") %>% 
  group_by(month, vaccination_status_at_infection) %>%
  summarise(n = n()) %>%
  ungroup() 

cases_by_vacc_severe_outcomes_tab <- data_cases_by_vacc_severe_outcomes_tab %>% 
  select(vaccination_status_at_infection, month) %>%
  mutate(month = "Total") %>% 
  mutate(vaccination_status_at_infection = "Total") %>% 
  group_by(month, vaccination_status_at_infection) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  bind_rows(cases_by_vacc_severe_outcomes_tab_monthly) %>% 
  bind_rows(cases_by_vacc_severe_outcomes_tab_col_total) %>% 
  bind_rows(cases_by_vacc_severe_outcomes_tab_row_total) %>%   
  mutate(indicator = "Total confirmed cases of COVID-19 in pregnancy")


## Maternal deaths

maternal_deaths_by_vacc <- readRDS(paste0(folder_temp_data, "infection_output_tables/maternal_deaths_by_vaccination.rds"))

## Pregnancies ending in stillbirth ##
# within 28 days
covid_associated_stillbirths_preg_by_vacc <- associated_pregnancies(data_stillbirths_preg, pregnancy_id, flag_covid_associated_stillbirth, 
                                                                    month, vaccination_status_at_infection, "pregnancies ending in covid associated stillbirths")
# subsequent
subsequent_stillbirths_preg_by_vacc <- subsequent_pregnancies(data_stillbirths_preg, pregnancy_id, 
                                                              month, vaccination_status_at_infection, "pregnancies ending in subsequent stillbirths")

## Stillbirths ##
# within 28 days
covid_associated_stillbirths_by_vacc <- associated_babies(data_stillbirths, pregnancy_id, flag_covid_associated_stillbirth, 
                                                                    month, vaccination_status_at_infection, "covid associated stillbirths")
# subsequent
subsequent_stillbirths_by_vacc <- subsequent_babies(data_stillbirths, pregnancy_id, 
                                                              month, vaccination_status_at_infection, "subsequent stillbirths")

## Pregnancies ending in preterm birth ##
# within 28 days
covid_associated_preterm_preg_by_vacc <- associated_pregnancies(data_preterm_preg, pregnancy_id, flag_covid_associated_preterm, 
                                                                month, vaccination_status_at_infection, "pregnancies ending in covid associated preterm births")
# subsequent
subsequent_preterm_preg_by_vacc <- subsequent_pregnancies(data_preterm_preg, pregnancy_id, 
                                                          month, vaccination_status_at_infection, "pregnancies ending in subsequent preterm births")

## Preterm births ##
# within 28 days 
covid_associated_preterm_by_vacc <- associated_babies(data_preterm, pregnancy_id, flag_covid_associated_preterm, 
                                                      month, vaccination_status_at_infection, "covid associated preterm births")
# subsequent
subsequent_preterm_by_vacc <- subsequent_babies(data_preterm, pregnancy_id, 
                                                month, vaccination_status_at_infection, "subsequent preterm births")

## Pregnancies ending in very preterm birth ##
# within 28 days
covid_associated_very_preterm_preg_by_vacc <- associated_pregnancies(data_very_preterm_preg, pregnancy_id, flag_covid_associated_preterm, 
                                                                month, vaccination_status_at_infection, "pregnancies ending in covid associated very preterm births")
# subsequent
subsequent_very_preterm_preg_by_vacc <- subsequent_pregnancies(data_very_preterm_preg, pregnancy_id, 
                                                          month, vaccination_status_at_infection, "pregnancies ending in subsequent very preterm births")

## Very preterm births ##
# within 28 days 
covid_associated_very_preterm_by_vacc <- associated_babies(data_very_preterm, pregnancy_id, flag_covid_associated_preterm, 
                                                      month, vaccination_status_at_infection, "covid associated very preterm births")
# subsequent
subsequent_very_preterm_by_vacc <- subsequent_babies(data_very_preterm, pregnancy_id, 
                                                month, vaccination_status_at_infection, "subsequent very preterm births")

## Pregnancies ending in birth with low Apgar score ##
# within 28 days
covid_associated_low_apgar_preg_by_vacc <- associated_pregnancies(data_low_apgar_preg, pregnancy_id, flag_covid_associated_low_apgar, 
                                                                  month, vaccination_status_at_infection, "pregnancies ending in covid associated low apgar score")
# subsequent
subsequent_low_apgar_preg_by_vacc <- subsequent_pregnancies(data_low_apgar_preg, pregnancy_id, 
                                                            month, vaccination_status_at_infection, "pregnancies ending in subsequent low apgar score")

## Births wirth with low Apgar score ##
# within 28 days
covid_associated_low_apgar_by_vacc <- associated_pregnancies(data_low_apgar, pregnancy_id, flag_covid_associated_low_apgar, 
                                                             month, vaccination_status_at_infection, "covid associated low apgar score")
# subsequent
subsequent_low_apgar_by_vacc <- subsequent_pregnancies(data_low_apgar, pregnancy_id, 
                                                       month, vaccination_status_at_infection, "subsequent low apgar score")

## Pregnancies ending in birth with very low Apgar score ##
# within 28 days
covid_associated_very_low_apgar_preg_by_vacc <- associated_pregnancies(data_very_low_apgar_preg, pregnancy_id, flag_covid_associated_very_low_apgar, 
                                                                       month, vaccination_status_at_infection, "pregnancies ending in covid associated very low apgar score")
# subsequent
subsequent_very_low_apgar_preg_by_vacc <- subsequent_pregnancies(data_very_low_apgar_preg, pregnancy_id, 
                                                                 month, vaccination_status_at_infection, "pregnancies ending in subsequent very low apgar score")

## Births wirth with very low Apgar score ##
# within 28 days
covid_associated_very_low_apgar_by_vacc <- associated_pregnancies(data_very_low_apgar, pregnancy_id, flag_covid_associated_very_low_apgar, 
                                                                  month, vaccination_status_at_infection, "covid associated very low apgar score")
# subsequent
subsequent_very_low_apgar_by_vacc <- subsequent_pregnancies(data_very_low_apgar, pregnancy_id, 
                                                            month, vaccination_status_at_infection, "subsequent very low apgar score")


## Pregnancies ending in neonatal death ##
# within 28 days
covid_associated_neonatal_deaths_preg_by_vacc <- associated_pregnancies(data_neonatal_deaths_preg, pregnancy_id, flag_covid_associated_neonatal_death, 
                                                                       month, vaccination_status_at_infection, "pregnancies ending in covid associated neonatal deaths")
# subsequent
subsequent_neonatal_deaths_preg_by_vacc <- subsequent_pregnancies(data_neonatal_deaths_preg, pregnancy_id, 
                                                                  month, vaccination_status_at_infection, "pregnancies ending in subsequent neonatal deaths")

## Neonatal deaths
# within 28 days 
covid_associated_neonatal_deaths_by_vacc <- associated_babies(data_neonatal_deaths, pregnancy_id, flag_covid_associated_neonatal_death, 
                                                              month, vaccination_status_at_infection, "covid associated neonatal deaths")
# subsequent
subsequent_neonatal_deaths_by_vacc <- subsequent_babies(data_neonatal_deaths, pregnancy_id, 
                                                        month, vaccination_status_at_infection, "subsequent neonatal deaths")


## create spine
serious_vacc_spine <- expand.grid(serious_gest_spine$month, adm_vacc_spine$vaccination_status_at_infection, serious_outcomes_factor) %>% 
  distinct() %>% 
  rename(month = Var1, vaccination_status_at_infection = Var2, indicator = Var3)

# combine all outcomes into one df
serious_outcomes_combined <- cases_by_vacc_severe_outcomes_tab %>% 
  bind_rows(maternal_deaths_by_vacc) %>% 
  bind_rows(covid_associated_stillbirths_preg_by_vacc) %>% 
  bind_rows(subsequent_stillbirths_preg_by_vacc) %>% 
  bind_rows(covid_associated_stillbirths_by_vacc) %>% 
  bind_rows(subsequent_stillbirths_by_vacc) %>% 
  bind_rows(covid_associated_preterm_preg_by_vacc) %>% 
  bind_rows(subsequent_preterm_preg_by_vacc) %>% 
  bind_rows(covid_associated_preterm_by_vacc) %>% 
  bind_rows(subsequent_preterm_by_vacc) %>% 
  bind_rows(covid_associated_very_preterm_preg_by_vacc) %>% 
  bind_rows(subsequent_very_preterm_preg_by_vacc) %>% 
  bind_rows(covid_associated_very_preterm_by_vacc) %>% 
  bind_rows(subsequent_very_preterm_by_vacc) %>% 
  bind_rows(covid_associated_low_apgar_preg_by_vacc) %>% 
  bind_rows(subsequent_low_apgar_preg_by_vacc) %>% 
  bind_rows(covid_associated_low_apgar_by_vacc) %>% 
  bind_rows(subsequent_low_apgar_by_vacc) %>% 
  bind_rows(covid_associated_very_low_apgar_preg_by_vacc) %>% 
  bind_rows(subsequent_very_low_apgar_preg_by_vacc) %>% 
  bind_rows(covid_associated_very_low_apgar_by_vacc) %>% 
  bind_rows(subsequent_very_low_apgar_by_vacc) %>% 
  bind_rows(covid_associated_neonatal_deaths_preg_by_vacc) %>% 
  bind_rows(subsequent_neonatal_deaths_preg_by_vacc) %>% 
  bind_rows(covid_associated_neonatal_deaths_by_vacc) %>% 
  bind_rows(subsequent_neonatal_deaths_by_vacc)


all_serious_outcomes <- serious_vacc_spine %>% 
  arrange(month, vaccination_status_at_infection, indicator) %>% 
  left_join(serious_outcomes_combined, by = c("month", "vaccination_status_at_infection", "indicator")) %>% 
  mutate_all(~replace(., is.na(.), 0))

all_serious_outcomes_wide <- all_serious_outcomes %>% 
  left_join(month_lookup) %>% 
  select(-month) %>% 
  mutate(month_in_words = if_else(is.na(month_in_words), "Total", month_in_words)) %>% 
  pivot_wider(names_from = month_in_words, values_from = n)

cases_total <- all_serious_outcomes_wide %>% 
  filter(vaccination_status_at_infection == "Total" & indicator == "Total confirmed cases of COVID-19 in pregnancy")

all_serious_outcomes_by_vacc <- all_serious_outcomes_wide %>% 
  filter(vaccination_status_at_infection != "Total")

all_serious_outcomes_total_excel <- all_serious_outcomes_wide %>% 
  filter(vaccination_status_at_infection == "Total" & indicator != "Total confirmed cases of COVID-19 in pregnancy")

## read out into template 
writeData(template, "Sev oc by vacc NFP", subtitle, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Sev oc by vacc NFP", select(all_serious_outcomes_by_vacc, -c(vaccination_status_at_infection, indicator)), startCol=3, startRow=7)
all_serious_outcomes_by_vacc %>% write_rds(paste0(folder_temp_data, "infection_output_tables/infection_severe_outcomes_vacc.rds"))

#### Rate in Pregnancy ####
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= as.Date("2020-03-01")) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start) %>% 
  select(month, month_start)

months_total_pregnant <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing") 
                            & est_conception_date < months$month_start[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}

write_rds(months_total_pregnant, paste0(folder_temp_data, "infection_output_tables/total_preg_by_month.rds"))

# count onset of covid cases in each month
months_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = everything()) %>%
  select(value) %>%
  filter(!is.na(value)) %>% filter(value <= end_infect_date) %>%
  mutate(month = format(as.Date(value), "%Y-%m")) %>%
  select(month) %>%
  group_by(month) %>%
  count() %>%
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy <- months %>%
  left_join(months_total_pregnant, by="month") %>%
  left_join(months_numerators, by="month") %>%
  mutate(covid_infections_during_pregnancy = case_when(is.na(covid_infections_during_pregnancy) ~ 0, # In some months there were no covid infections, so set these NA values to 0
                                                       T ~ as.double(covid_infections_during_pregnancy))) %>%
  mutate("% women with COVID-19 during pregnancy in month" = (covid_infections_during_pregnancy / number_pregnancies) * 100000) %>%
  select(-month_start) %>% 
  pivot_longer(!month, names_to = "indicator", values_to = "value") %>% 
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(id = indicator, names_from = month_in_words, values_from = value)

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

#truncated rate to April 22 (when testing pretty much stopped)
#total_pregnancies_to_Apr22 <-  pregnancies %>%
#  filter(est_conception_date <= as.Date("2022-04-30")) %>%
#  group_by(mother_upi) %>%
#  mutate(mother_total_pregnancies = n()) %>%
#  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
#  slice(1) %>%
#  ungroup() %>%
#  summarise(total_pregnancies_to_Apr22 = n(),
#            total_covid_in_pregnancies_to_Apr22 = sum_(mother_tested_positive_during_pregnancy)) %>%
#  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000) %>%
#  pivot_longer(cols = total_pregnancies:percent, names_to = "indicator", values_to = paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))) %>% 
#  select(-indicator)
##

data_rate_in_pregnancy = data_rate_in_pregnancy %>%
  bind_cols(total_pregnancies) #%>% bind_cols(total_pregnancies_to_Apr22 )

rate_in_pregnancy_text_1 <- paste0("*The total for this row is the total number of women pregnant at any point from 1st March 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")
rate_in_pregnancy_text_2 <- paste0("**The total for this row is the total number of women with COVID-19 in pregnancy with date of onset from 1st March 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")

data_rate_in_pregnancy %>% write_rds(paste0(folder_temp_data, "infection_output_tables/rate_in_pregnancy.rds"))

writeData(template, "Rate in pregnancy", subtitle, startCol = 1, startRow = 2, colNames = FALSE) 
writeData(template, "Rate in pregnancy", rate_in_pregnancy_text_1, startCol = 1, startRow = 12, colNames = FALSE)
writeData(template, "Rate in pregnancy", rate_in_pregnancy_text_2, startCol = 1, startRow = 13, colNames = FALSE)
writeData(template, "Rate in pregnancy", select(data_rate_in_pregnancy, -indicator), startCol=2, startRow=5)

rm(months, months_numerators, months_total_pregnant)


#### Rate in Pregnancy by Age ####

# "months" is deleted above (and didn't contain maternal_age_group) so...
# need to recreate this and add in maternal_age_group for next step to work?
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= as.Date("2020-03-01")) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, maternal_age_group) %>% 
  select(month, month_start, maternal_age_group)

# skeleton using expand grid
months <- expand.grid(months$month_start, months$maternal_age_group) %>% distinct() %>%
  rename(month = Var1, maternal_age_group = Var2) %>%
  mutate(month_start = floor_date(month)) %>%
  select(month, month_start, maternal_age_group) %>%
  mutate(month = format(as.Date(month), "%Y-%m")) %>% 
  mutate(month = as.character(month))

months_total_pregnant <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, maternal_age_group, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing") 
                             & est_conception_date < months$month_start[i] )
                            & maternal_age_group == months$maternal_age_group[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, maternal_age_group) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}


# count onset of covid cases in each month
months_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(maternal_age_group, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = starts_with("mother_positive_test")) %>%
  select(maternal_age_group, value) %>%
  filter(!is.na(value)) %>% filter(value <= end_infect_date) %>%
  mutate(month = format(as.Date(value), "%Y-%m")) %>%
  select(maternal_age_group, month) %>%
  group_by(maternal_age_group, month) %>%
  count() %>%
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy_by_age <- months %>%
  left_join(months_total_pregnant, by=c("month", "maternal_age_group")) %>%
  left_join(months_numerators, by=c("month", "maternal_age_group")) %>%
  select(-month_start) %>%
  mutate(covid_infections_during_pregnancy = case_when(is.na(covid_infections_during_pregnancy) ~ 0, # In some months there were no covid infections, so set these NA values to 0
                                                       T ~ as.double(covid_infections_during_pregnancy))) %>%
  mutate(percent = covid_infections_during_pregnancy/number_pregnancies*100000)

# Maternal Age - N Pregnant Table 
data_rate_in_pregnancy_by_age_pregnancies <- data_rate_in_pregnancy_by_age %>%
  select(month, maternal_age_group, number_pregnancies) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = number_pregnancies) %>%
  adorn_totals(where = "row")

# Maternal Age - N positive covid tests among pregnant women
data_rate_in_pregnancy_by_age_cases <- data_rate_in_pregnancy_by_age %>%
  select(month, maternal_age_group, covid_infections_during_pregnancy) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = covid_infections_during_pregnancy) %>%
  adorn_totals(where = "row")

# Maternal Age - % of women with COVID-19 during pregnancy in month
data_rate_in_pregnancy_by_age_rate <- data_rate_in_pregnancy_by_age %>%
  select(month, maternal_age_group, percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = percent)

data_rate_in_pregnancy_by_age_rate_total <- data_rate_in_pregnancy_by_age %>%
  group_by(month) %>%
  summarise(total_pregnancies = sum_(number_pregnancies), total_covid = sum_(covid_infections_during_pregnancy)) %>%
  mutate(total_percent = total_covid/total_pregnancies * 100000) %>%
  select(month, total_percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = total_percent) %>%
  mutate(maternal_age_group = "9 Total") %>%
  select(maternal_age_group, everything())


data_rate_in_pregnancy_by_age_rate <- data_rate_in_pregnancy_by_age_rate %>%
  bind_rows(data_rate_in_pregnancy_by_age_rate_total)

total_pregnancies_by_age <- pregnancies %>%
  group_by(mother_upi) %>%
  mutate(mother_total_pregnancies = n()) %>%
  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(maternal_age_group) %>%
  summarise(total_pregnancies = n(),
            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
  adorn_totals(where="row") %>%
  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000)

##truncated totals
#total_pregnancies_by_age_to_Apr22 <- pregnancies %>%
#  filter(est_conception_date <= as.Date("2022-04-30")) %>%
#  group_by(mother_upi) %>%
#  mutate(mother_total_pregnancies_to_Apr22 = n()) %>%
#  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
#  slice(1) %>%
#  ungroup() %>%
#  group_by(maternal_age_group) %>%
#  summarise(total_pregnancies_to_Apr22 = n(),
#            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
#  adorn_totals(where="row") %>%
#  mutate(percent = total_covid_in_pregnancies / total_pregnancies_to_Apr22 * 100000)

data_rate_in_pregnancy_by_age_pregnancies <- data_rate_in_pregnancy_by_age_pregnancies %>%
  bind_cols(total_pregnancies_by_age$total_pregnancies) %>%
 # bind_cols(total_pregnancies_by_age_to_Apr22$total_pregnancies_to_Apr22) %>%
  mutate_all(~replace(., is.na(.), 0))
names(data_rate_in_pregnancy_by_age_pregnancies)[length(names(data_rate_in_pregnancy_by_age_pregnancies))]<- paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words)) 
#names(data_rate_in_pregnancy_by_age_pregnancies)[length(names(data_rate_in_pregnancy_by_age_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022") 

data_rate_in_pregnancy_by_age_cases <- data_rate_in_pregnancy_by_age_cases %>%
  bind_cols(total_pregnancies_by_age$total_covid_in_pregnancies) %>%
#  bind_cols(total_pregnancies_by_age_to_Apr22$total_covid_in_pregnancies) %>%
  mutate_all(~replace(., is.na(.), 0))
names(data_rate_in_pregnancy_by_age_cases)[length(names(data_rate_in_pregnancy_by_age_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words)) 
#names(data_rate_in_pregnancy_by_age_cases)[length(names(data_rate_in_pregnancy_by_age_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022") 

data_rate_in_pregnancy_by_age_rate <- data_rate_in_pregnancy_by_age_rate %>%
  bind_cols(total_pregnancies_by_age$percent) %>%
 # bind_cols(total_pregnancies_by_age_to_Apr22$percent) %>%
  mutate_all(~replace(., is.na(.), 0))
names(data_rate_in_pregnancy_by_age_rate)[length(names(data_rate_in_pregnancy_by_age_rate))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words)) 
#names(data_rate_in_pregnancy_by_age_rate)[length(names(data_rate_in_pregnancy_by_age_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022") 


rate_in_pregnancy_text_1_plural <- paste0("*The total for these rows is the total number of women pregnant at any point from 1st March 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")
rate_in_pregnancy_text_2_plural <- paste0("**The total for these rows is the total number of women with COVID-19 in pregnancy with date of onset from 1st March 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")

writeData(template, "Rate by age", subtitle, startCol = 1, startRow = 2, colNames = FALSE) 
writeData(template, "Rate by age", rate_in_pregnancy_text_1_plural, startCol = 1, startRow = 40, colNames = FALSE)
writeData(template, "Rate by age", rate_in_pregnancy_text_2_plural, startCol = 1, startRow = 41, colNames = FALSE)
writeData(template, "Rate by age", select(data_rate_in_pregnancy_by_age_pregnancies, -maternal_age_group),   startCol= 2, startRow= 6)
writeData(template, "Rate by age", select(data_rate_in_pregnancy_by_age_cases, -maternal_age_group), startCol= 2, startRow= 17, colNames = F)
writeData(template, "Rate by age", select(data_rate_in_pregnancy_by_age_rate, -maternal_age_group),        startCol= 2, startRow= 27, colNames = F)

data_rate_in_pregnancy_by_age_rate  %>% write_rds(paste0(folder_temp_data, "infection_output_tables/rate_in_pregnancy_age.rds"))

rm(months, months_numerators, months_total_pregnant)


#### Rate in Pregnancy by SIMD ####
months <- pregnancies %>%
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= as.Date("2020-03-01")) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, simd) %>% 
  select(month, month_start, simd)

months_total_pregnant <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, simd, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & simd == months$simd[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, simd) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}

# count onset of covid cases in each month
months_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(simd, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = starts_with("mother_positive_test")) %>%
  select(simd, value) %>%
  filter(!is.na(value)) %>% filter(value <= end_infect_date) %>%
  mutate(month = format(as.Date(value), "%Y-%m")) %>%
  select(simd, month) %>%
  group_by(simd, month) %>%
  count() %>%
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy_by_simd <- months %>%
  left_join(months_total_pregnant, by=c("month", "simd")) %>%
  left_join(months_numerators, by=c("month", "simd")) %>%
  select(-month_start) %>%
  mutate(covid_infections_during_pregnancy = case_when(is.na(covid_infections_during_pregnancy) ~ 0, # In some months there were no covid infections, so set these NA values to 0
                                                       T ~ as.double(covid_infections_during_pregnancy))) %>%
  mutate(percent = covid_infections_during_pregnancy/number_pregnancies*100000)

# Maternal SIMD - N Pregnant Table 
data_rate_in_pregnancy_by_simd_pregnancies <- data_rate_in_pregnancy_by_simd %>%
  select(month, simd, number_pregnancies) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = number_pregnancies) %>%
  adorn_totals(where = "row")

# Maternal SIMD - N positive covid tests among pregnant women
data_rate_in_pregnancy_by_simd_cases <- data_rate_in_pregnancy_by_simd %>%
  select(month, simd, covid_infections_during_pregnancy) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = covid_infections_during_pregnancy) %>%
  adorn_totals(where = "row")

# Maternal SIMD - % of women with COVID-19 during pregnancy in month
data_rate_in_pregnancy_by_simd_rate <- data_rate_in_pregnancy_by_simd %>%
  select(month, simd, percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = percent)

data_rate_in_pregnancy_by_simd_rate_total <- data_rate_in_pregnancy_by_simd %>%
  group_by(month) %>%
  summarise(total_pregnancies = sum_(number_pregnancies), total_covid = sum_(covid_infections_during_pregnancy)) %>%
  mutate(total_percent = total_covid/total_pregnancies * 100000) %>%
  select(month, total_percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = total_percent) %>%
  mutate(simd = "9 Total") %>%
  select(simd, everything())

total_pregnancies_by_simd <- pregnancies %>%
  group_by(mother_upi) %>%
  mutate(mother_total_pregnancies = n()) %>%
  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(simd) %>%
  summarise(total_pregnancies = n(),
            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
  adorn_totals(where="row") %>%
  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000)

#truncated totals
#total_pregnancies_by_simd_to_Apr22 <- pregnancies %>%
#  filter(est_conception_date <= as.Date("2022-04-30")) %>%
#  group_by(mother_upi) %>%
#  mutate(mother_total_pregnancies = n()) %>%
#  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
#  slice(1) %>%
#  ungroup() %>%
#  group_by(simd) %>%
#  summarise(total_pregnancies = n(),
#            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
#  adorn_totals(where="row") %>%
#  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000)

##bind totals
data_rate_in_pregnancy_by_simd_rate <- data_rate_in_pregnancy_by_simd_rate %>%
  bind_rows(data_rate_in_pregnancy_by_simd_rate_total)


data_rate_in_pregnancy_by_simd_pregnancies <- data_rate_in_pregnancy_by_simd_pregnancies %>%
  bind_cols(total_pregnancies_by_simd$total_pregnancies) #%>%
#  bind_cols(total_pregnancies_by_simd_to_Apr22 $total_pregnancies)  
names(data_rate_in_pregnancy_by_simd_pregnancies)[length(names(data_rate_in_pregnancy_by_simd_pregnancies))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))
#names(data_rate_in_pregnancy_by_simd_pregnancies)[length(names(data_rate_in_pregnancy_by_simd_pregnancies))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022")

data_rate_in_pregnancy_by_simd_cases <- data_rate_in_pregnancy_by_simd_cases %>%
  bind_cols(total_pregnancies_by_simd$total_covid_in_pregnancies)# %>%
 # bind_cols(total_pregnancies_by_simd_to_Apr22$total_covid_in_pregnancies) 
names(data_rate_in_pregnancy_by_simd_cases)[length(names(data_rate_in_pregnancy_by_simd_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))
#names(data_rate_in_pregnancy_by_simd_cases)[length(names(data_rate_in_pregnancy_by_simd_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022")

data_rate_in_pregnancy_by_simd_rate <- data_rate_in_pregnancy_by_simd_rate %>%
  bind_cols(total_pregnancies_by_simd$percent) #%>%
 # bind_cols(total_pregnancies_by_simd_to_Apr22$percent) 
names(data_rate_in_pregnancy_by_simd_rate)[length(names(data_rate_in_pregnancy_by_simd_rate))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words)) 
#names(data_rate_in_pregnancy_by_simd_rate)[length(names(data_rate_in_pregnancy_by_simd_rate))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", "Apr 2022") 


writeData(template, "Rate by SIMD", subtitle, startCol = 1, startRow = 2, colNames = FALSE) 
writeData(template, "Rate by SIMD", rate_in_pregnancy_text_1_plural, startCol = 1, startRow = 37, colNames = FALSE)
writeData(template, "Rate by SIMD", rate_in_pregnancy_text_2_plural, startCol = 1, startRow = 38, colNames = FALSE)
writeData(template, "Rate by SIMD", select(data_rate_in_pregnancy_by_simd_pregnancies, -simd),   startCol= 2, startRow= 6, rowNames = F)
writeData(template, "Rate by SIMD", select(data_rate_in_pregnancy_by_simd_cases, -simd), startCol= 2, startRow= 16, colNames = F)
writeData(template, "Rate by SIMD", select(data_rate_in_pregnancy_by_simd_rate, -simd),        startCol= 2, startRow= 25, colNames = F)

data_rate_in_pregnancy_by_simd_rate  %>% write_rds(paste0(folder_temp_data, "infection_output_tables/rate_in_pregnancy_simd.rds"))

rm(months, months_numerators, months_total_pregnant)


#### Rate in Pregnancy by Ethnicity ####

months <- pregnancies %>%
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= as.Date("2020-03-01")) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, ethnicity_desc_reporting) %>% 
  select(month, month_start, ethnicity_desc_reporting)

months_total_pregnant <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, ethnicity_desc_reporting, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & ethnicity_desc_reporting == months$ethnicity_desc_reporting[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, ethnicity_desc_reporting) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}

# count onset of covid cases in each month
months_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(ethnicity_desc_reporting, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = starts_with("mother_positive_test")) %>%
  select(ethnicity_desc_reporting, value) %>%
  filter(!is.na(value)) %>%  filter(value <= end_infect_date) %>%
  mutate(month = format(as.Date(value), "%Y-%m")) %>%
  select(ethnicity_desc_reporting, month) %>%
  group_by(ethnicity_desc_reporting, month) %>%
  count() %>%
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy_by_ethnicity <- months %>%
  left_join(months_total_pregnant, by=c("month", "ethnicity_desc_reporting")) %>%
  left_join(months_numerators, by=c("month", "ethnicity_desc_reporting")) %>%
  select(-month_start) %>%
  mutate(covid_infections_during_pregnancy = case_when(is.na(covid_infections_during_pregnancy) ~ 0, # In some months there were no covid infections, so set these NA values to 0
                                                       T ~ as.double(covid_infections_during_pregnancy))) %>%
  mutate(percent = covid_infections_during_pregnancy/number_pregnancies*100000) %>%
  left_join(month_lookup) %>% 
  select(-month)

# Maternal ethnicity - N Pregnant Table 
data_rate_in_pregnancy_by_ethnicity_pregnancies <- data_rate_in_pregnancy_by_ethnicity %>%
  select(month_in_words, ethnicity_desc_reporting, number_pregnancies) %>%
  pivot_wider(names_from = month_in_words, values_from = number_pregnancies) %>%
  adorn_totals(where = "row")

# Maternal ethnicity - N positive covid tests among pregnant women
data_rate_in_pregnancy_by_ethnicity_cases <- data_rate_in_pregnancy_by_ethnicity %>%
  select(month_in_words, ethnicity_desc_reporting, covid_infections_during_pregnancy) %>%
  pivot_wider(names_from = month_in_words, values_from = covid_infections_during_pregnancy) %>%
  adorn_totals(where = "row")

# Maternal ethnicity - % of women with COVID-19 during pregnancy in month
data_rate_in_pregnancy_by_ethnicity_rate <- data_rate_in_pregnancy_by_ethnicity %>%
  select(month_in_words, ethnicity_desc_reporting, percent) %>%
  pivot_wider(names_from = month_in_words, values_from = percent)

data_rate_in_pregnancy_by_ethnicity_rate_total <- data_rate_in_pregnancy_by_ethnicity %>%
  group_by(month_in_words) %>%
  summarise(total_pregnancies = sum_(number_pregnancies), total_covid = sum_(covid_infections_during_pregnancy)) %>%
  mutate(total_percent = total_covid/total_pregnancies * 100000) %>%
  select(month_in_words, total_percent) %>%
  pivot_wider(names_from = month_in_words, values_from = total_percent) %>%
  mutate(ethnicity_desc_reporting = "99 Total") %>%
  select(ethnicity_desc_reporting, everything())

data_rate_in_pregnancy_by_ethnicity_rate <- data_rate_in_pregnancy_by_ethnicity_rate %>%
  bind_rows(data_rate_in_pregnancy_by_ethnicity_rate_total)

total_pregnancies_by_ethnicity <- pregnancies %>%
  group_by(mother_upi) %>%
  mutate(mother_total_pregnancies = n()) %>%
  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(ethnicity_desc_reporting) %>%
  summarise(total_pregnancies = n(),
            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
  adorn_totals(where="row") %>%
  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000)

#add total and truncated totals
data_rate_in_pregnancy_by_ethnicity_pregnancies <- data_rate_in_pregnancy_by_ethnicity_pregnancies %>%
  bind_cols(total_pregnancies_by_ethnicity$total_pregnancies) 
 
names(data_rate_in_pregnancy_by_ethnicity_pregnancies)[length(names(data_rate_in_pregnancy_by_ethnicity_pregnancies))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))

data_rate_in_pregnancy_by_ethnicity_cases <- data_rate_in_pregnancy_by_ethnicity_cases %>%
  bind_cols(total_pregnancies_by_ethnicity$total_covid_in_pregnancies) 
names(data_rate_in_pregnancy_by_ethnicity_cases)[length(names(data_rate_in_pregnancy_by_ethnicity_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))

data_rate_in_pregnancy_by_ethnicity_rate <- data_rate_in_pregnancy_by_ethnicity_rate %>%
  bind_cols(total_pregnancies_by_ethnicity$percent) 
names(data_rate_in_pregnancy_by_ethnicity_rate)[length(names(data_rate_in_pregnancy_by_ethnicity_rate))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))


writeData(template, "Rate by Ethnicity", subtitle, startCol = 1, startRow = 2, colNames = FALSE) 
writeData(template, "Rate by Ethnicity", rate_in_pregnancy_text_1_plural, startCol = 1, startRow = 34, colNames = FALSE)
writeData(template, "Rate by Ethnicity", rate_in_pregnancy_text_2_plural, startCol = 1, startRow = 35, colNames = FALSE)
writeData(template, "Rate by Ethnicity", select(data_rate_in_pregnancy_by_ethnicity_pregnancies, -ethnicity_desc_reporting),   startCol= 2, startRow= 6)
writeData(template, "Rate by Ethnicity", select(data_rate_in_pregnancy_by_ethnicity_cases, -ethnicity_desc_reporting), startCol= 2, startRow= 15, colNames = F)
writeData(template, "Rate by Ethnicity", select(data_rate_in_pregnancy_by_ethnicity_rate, -ethnicity_desc_reporting),  startCol= 2, startRow= 23, colNames = F)

data_rate_in_pregnancy_by_ethnicity_rate  %>% write_rds(paste0(folder_temp_data, "infection_output_tables/rate_in_pregnancy_ethnicity.rds"))

rm(months, months_numerators, months_total_pregnant)


#### Rate in Pregnancy by NHS Board ####
# We use an alternative method for generating our months spine for this section. This ensures we don't miss out any combinations where there are low numbers.
# We may want to use this method for the other sections, too, just in case. 
months <- pregnancies_weeks_spine <- seq(as.Date("2020-03-01"), publication_latest_vacc_date, by="months") %>%
  as.data.frame() %>%
  rename("month_start" = ".")
  
healthboards <- pregnancies %>%
  select(hbres) %>%
  distinct()

months <- expand_grid(months, healthboards) %>% 
  mutate(month = format(as.Date(month_start), "%Y-%m")) %>% 
  select(month, month_start, hbres)

months_total_pregnant <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, hbres, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & hbres == months$hbres[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, hbres) %>% 
    filter(!is.na(flag))
  
  months_total_pregnant[i,]$number_pregnancies <- number[1,]$n
}

# count onset of covid cases in each month
months_numerators <- pregnancies %>%
  filter(mother_tested_positive_during_pregnancy == 1) %>%
  select(hbres, mother_positive_test_during_pregnancy_1, mother_positive_test_during_pregnancy_2) %>%
  pivot_longer(cols = starts_with("mother_positive_test")) %>%
  select(hbres, value) %>%
  filter(!is.na(value)) %>% filter(value <= end_infect_date) %>%
  mutate(month = format(as.Date(value), "%Y-%m")) %>%
  select(hbres, month) %>%
  group_by(hbres, month) %>%
  count() %>%
  rename(covid_infections_during_pregnancy = n)

data_rate_in_pregnancy_by_hbres <- months %>%
  left_join(months_total_pregnant, by=c("month", "hbres")) %>%
  left_join(months_numerators, by=c("month", "hbres")) %>%
  select(-month_start) %>%
  mutate(covid_infections_during_pregnancy = case_when(is.na(covid_infections_during_pregnancy) ~ 0, # In some months there were no covid infections, so set these NA values to 0
                                                       T ~ as.double(covid_infections_during_pregnancy))) %>%
  mutate(percent = covid_infections_during_pregnancy/number_pregnancies*100000) %>%
  arrange(hbres)

# Maternal hbres - N Pregnant Table 
data_rate_in_pregnancy_by_hbres_pregnancies <- data_rate_in_pregnancy_by_hbres %>%
  select(month, hbres, number_pregnancies) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = number_pregnancies) %>%
  adorn_totals(where = "row")

# Maternal hbres - N positive covid tests among pregnant women
data_rate_in_pregnancy_by_hbres_cases <- data_rate_in_pregnancy_by_hbres %>%
  select(month, hbres, covid_infections_during_pregnancy) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = covid_infections_during_pregnancy) %>%
  adorn_totals(where = "row")

# Maternal hbres - % of women with COVID-19 during pregnancy in month
data_rate_in_pregnancy_by_hbres_rate <- data_rate_in_pregnancy_by_hbres %>%
  select(month, hbres, percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>% 
  pivot_wider(names_from = month_in_words, values_from = percent)

data_rate_in_pregnancy_by_hbres_rate_total <- data_rate_in_pregnancy_by_hbres %>%
  group_by(month) %>%
  summarise(total_pregnancies = sum_(number_pregnancies), total_covid = sum_(covid_infections_during_pregnancy)) %>%
  mutate(total_percent = total_covid/total_pregnancies * 100000) %>%
  select(month, total_percent) %>%
  left_join(month_lookup) %>% 
  select(-month) %>%
  pivot_wider(names_from = month_in_words, values_from = total_percent) %>%
  mutate(hbres = "Total") %>%
  select(hbres, everything())

data_rate_in_pregnancy_by_hbres_rate <- data_rate_in_pregnancy_by_hbres_rate %>%
  bind_rows(data_rate_in_pregnancy_by_hbres_rate_total)

total_pregnancies_by_hbres <- pregnancies %>%
  group_by(mother_upi) %>%
  mutate(mother_total_pregnancies = n()) %>%
  mutate(mother_tested_positive_during_pregnancy = max_(mother_tested_positive_during_pregnancy)) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(hbres) %>%
  summarise(total_pregnancies = n(),
            total_covid_in_pregnancies = sum_(mother_tested_positive_during_pregnancy)) %>%
  adorn_totals(where="row") %>%
  mutate(percent = total_covid_in_pregnancies / total_pregnancies * 100000)


data_rate_in_pregnancy_by_hbres_pregnancies <- data_rate_in_pregnancy_by_hbres_pregnancies %>%
  bind_cols(total_pregnancies_by_hbres$total_pregnancies) 
names(data_rate_in_pregnancy_by_hbres_pregnancies)[length(names(data_rate_in_pregnancy_by_hbres_pregnancies))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))

data_rate_in_pregnancy_by_hbres_cases <- data_rate_in_pregnancy_by_hbres_cases %>%
  bind_cols(total_pregnancies_by_hbres$total_covid_in_pregnancies)
 
names(data_rate_in_pregnancy_by_hbres_cases)[length(names(data_rate_in_pregnancy_by_hbres_cases))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words)) 

data_rate_in_pregnancy_by_hbres_rate <- data_rate_in_pregnancy_by_hbres_rate %>%
  bind_cols(total_pregnancies_by_hbres$percent)
names(data_rate_in_pregnancy_by_hbres_rate)[length(names(data_rate_in_pregnancy_by_hbres_rate))]<-paste0("Total ", first(month_lookup$month_in_words), " to ", last(month_lookup$month_in_words))

writeData(template, "Rate by NHS Board", subtitle, startCol = 1, startRow = 2, colNames = FALSE) 
writeData(template, "Rate by NHS Board", rate_in_pregnancy_text_1_plural, startCol = 1, startRow = 64, colNames = FALSE)
writeData(template, "Rate by NHS Board", rate_in_pregnancy_text_2_plural, startCol = 1, startRow = 65, colNames = FALSE)
writeData(template, "Rate by NHS Board", select(data_rate_in_pregnancy_by_hbres_pregnancies, -hbres),   startCol= 2, startRow= 6)
writeData(template, "Rate by NHS Board", select(data_rate_in_pregnancy_by_hbres_cases, -hbres), startCol= 2, startRow= 25, colNames = F)
writeData(template, "Rate by NHS Board", select(data_rate_in_pregnancy_by_hbres_rate, -hbres),  startCol= 2, startRow= 43, colNames = F)

data_rate_in_pregnancy_by_hbres_rate  %>% write_rds(paste0(folder_temp_data, "infection_output_tables/rate_in_pregnancy_hbres.rds"))

rm(months, months_numerators, months_total_pregnant)



#### Severe outcomes rates ####

# READ IN ####
all_numerators <- readRDS(paste0(folder_temp_data, "infection_output_tables/infection_severe_outcomes_totals.rds")) %>% 
  select(indicator, Total)

live_births <- readRDS(paste0(folder_temp_data, "infection_output_tables/all_live_births_after_infection.rds")) %>% 
  select(indicator, Total)

live_births_28_days <- live_births$Total[live_births$indicator == "live birth within 28 days of infection"]

live_births_after_covid <- live_births$Total[live_births$indicator == "subsequent live birth"]

all_births_within_28_days <- live_births$Total[live_births$indicator == "live birth within 28 days of infection"] +
  all_numerators$Total[all_numerators$indicator == "covid associated stillbirths"]

all_births_after_covid <- live_births$Total[live_births$indicator == "subsequent live birth"] +
  all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"]


## Stillbirth rate (per 1,000 total births) for babies born within 28 days of maternal infection
stillbirth_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated stillbirths"], 
                                    denominator = all_births_within_28_days,
                                    rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## Stillbirth rate (per 1,000 total births) for all babies born following maternal infection
stillbirth_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"], 
                                 denominator = all_births_after_covid,
                                 rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))


## Neonatal mortality rate (per 1,000 total births) for babies born within 28 days of maternal infection
neonatal_mortality_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated neonatal deaths"], 
                                            denominator = live_births_28_days,
                                            rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## Neonatal mortality rate (per 1,000 total births) for all babies born following maternal infection
neonatal_mortality_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent neonatal deaths"], 
                                         denominator = live_births_after_covid,
                                         rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## extended perinatal mortality rate (per 1,000 total births) for babies born within 28 days of maternal infection
extended_perinatal_mortality_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated neonatal deaths"] +
                                                        all_numerators$Total[all_numerators$indicator == "covid associated stillbirths"], 
                                                      denominator = all_births_within_28_days,
                                                      rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## extended perinatal mortality rate (per 1,000 total births) for all babies born following maternal infection
extended_perinatal_mortality_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent neonatal deaths"] +
                                                     all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"], 
                                                   denominator = all_births_after_covid,
                                                   rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## preterm rate (per 100 total births) for babies born within 28 days of maternal infection
preterm_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated preterm births"], 
                                 denominator = live_births_28_days,
                                 rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## preterm rate (per 100 total births) for all babies born following maternal infection
preterm_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent preterm births"], 
                              denominator = live_births_after_covid,
                              rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very preterm rate (per 100 total births) for babies born within 28 days of maternal infection
very_preterm_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated very preterm births"], 
                                      denominator = live_births_28_days,
                                      rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very preterm rate (per 100 total births) for all babies born following maternal infection
very_preterm_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent very preterm births"], 
                                   denominator = live_births_after_covid,
                                   rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## low apgar rate (per 100 total births) for babies born within 28 days of maternal infection
low_apgar_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated low apgar score"], 
                                   denominator = live_births_28_days - all_numerators$Total[all_numerators$indicator == "covid associated preterm births"],
                                   rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## low apgar rate (per 100 total births) for all babies born following maternal infection
low_apgar_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent low apgar score"], 
                                denominator = live_births_after_covid  - all_numerators$Total[all_numerators$indicator == "subsequent preterm births"],
                                rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very low apgar rate (per 100 total births) for babies born within 28 days of maternal infection
very_low_apgar_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "covid associated very low apgar score"], 
                                        denominator = live_births_28_days - all_numerators$Total[all_numerators$indicator == "covid associated preterm births"],
                                        rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very low apgar rate (per 100 total births) for all babies born following maternal infection
very_low_apgar_after_covid <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent very low apgar score"], 
                                     denominator = live_births_after_covid  - all_numerators$Total[all_numerators$indicator == "subsequent preterm births"],
                                     rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

### Read out rates into Excel

writeData(template, "Severe outcomes tot NFP", all_births_within_28_days, startCol = 2, startRow = 41, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", all_births_after_covid, startCol = 2, startRow = 42, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", stillbirth_within_28_days, startCol = 5, startRow = 44, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", stillbirth_after_covid, startCol = 5, startRow = 45, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", neonatal_mortality_within_28_days, startCol = 5, startRow = 47, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", neonatal_mortality_after_covid, startCol = 5, startRow = 48, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", extended_perinatal_mortality_within_28_days, startCol = 5, startRow = 50, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", extended_perinatal_mortality_after_covid, startCol = 5, startRow = 51, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", preterm_within_28_days, startCol = 5, startRow = 53, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", preterm_after_covid, startCol = 5, startRow = 54, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", very_preterm_within_28_days, startCol = 5, startRow = 56, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", very_preterm_after_covid, startCol = 5, startRow = 57, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", low_apgar_within_28_days, startCol = 5, startRow = 59, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", low_apgar_after_covid, startCol = 5, startRow = 60, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", very_low_apgar_within_28_days, startCol = 5, startRow = 62, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", very_low_apgar_after_covid, startCol = 5, startRow = 63, colNames = FALSE)

saveWorkbook(template, (paste0(folder_outputs, "Infections/Infection_output_", Sys.Date(), ".xlsx")), overwrite = TRUE)
