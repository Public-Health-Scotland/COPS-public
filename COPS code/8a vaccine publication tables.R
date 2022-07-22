########################## Publication script to create COPS vaccination information ############################

theme_set(theme_classic())

# Read in Excel template
template <- openxlsx::loadWorkbook(file.path(paste(folder_templates, "Vaccination in pregnancy_template.xlsx", sep = "/")))

#### data updatedness tab ####

dates <- readRDS(paste0(folder_temp_data, "all_dates.rds"))
overall_vacc_chi_completeness_perc <- readRDS(file.path(paste(folder_temp_data, "overall_vacc_chi_completeness_perc.rds", sep ="/")))
fetus_level <- readRDS(paste0(folder_temp_data, "script6_baby_level_record.rds")) %>% filter(x_full_cohort == T)


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

data_sources_text_1 <- paste0("This release is based on the mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " update of the COPS study database." )
data_sources_text_2 <- paste0("As this release uses the mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " update of the COPS study database, we can be fairly confident that new pregnancies starting up to the end of ", format(publication_latest_vacc_date %m+% months(-3), "%B %Y"), 
                              " will have been included")
data_sources_text_3 <- paste0("and similarly any end of pregnancy events occuring up to the end of ", format(publication_latest_vacc_date %m+% months(-3), "%B %Y"), " will also have been included.")
data_sources_text_4 <- paste0("For this reason, the information provided (in this release, particularly that relating to vaccinations in pregnancy delivered in ", format(publication_latest_vacc_date %m+% months(-2), "%B %Y"), 
                              " onwards) should be viewed as provisional.")
data_sources_text_5 <- paste0("This release includes records of vaccinations delivered on up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), ".")
data_sources_text_6 <- paste0("The extract of vaccination data was taken from the National Clinical Data Store on ", format(dates$read_in_date[dates$dataset == "Vaccines"], "%d %B %Y"), "." )
data_sources_text_7 <- paste0("Date of data extract for mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), " COPS study database update")
data_sources_text_8 <- paste0("Overall, ", overall_vacc_chi_completeness_perc, "% of all vaccination records for vaccinations given from the start of the programme to ", format(publication_latest_vacc_date, "%d %B %Y"), " have an associated CHI number.")
data_sources_text_9 <- paste0("As at mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", the COPS database included a total of ", format(n_pregnancies, big.mark=","), " pregnancies among ", format(n_mother, big.mark=","), 
                              " women: ", format(n_preg_valid_chi, big.mark=","), " (", perc_preg_valid_chi, "%) of these pregnancy records (and ", format(n_wom_valid_chi, big.mark=","), ", ", perc_wom_valid_chi, "% of the women) have an associated CHI number.")


writeData(template, "Data sources", data_sources, startCol = 3, startRow = 20, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_1, startCol = 1, startRow = 17, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_2, startCol = 1, startRow = 35, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_3, startCol = 1, startRow = 36, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_4, startCol = 1, startRow = 39, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_5, startCol = 1, startRow = 52, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_6, startCol = 1, startRow = 53, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_7, startCol = 3, startRow = 19, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_8, startCol = 1, startRow = 57, colNames = FALSE)
writeData(template, "Data sources", data_sources_text_9, startCol = 1, startRow = 59, colNames = FALSE)

################################## read in data 
#pregnancy cohort 
pregnancies_1 <- read_rds(paste0(folder_temp_data, "script6b_pregnancy_level_record.rds")) %>% 
  filter(chi_validity == "Valid CHI")

fetus_level_processed <- fetus_level %>%
  filter(chi_validity == "Valid CHI") %>% 
  filter(x_est_conception_date <= publication_latest_vacc_date) %>%   # only include data for the publication period
  select(-chi_validity) %>% 
  mutate(month_pregnancy_end = format(x_pregnancy_end_date, "%Y-%m")) %>% 
  group_by(pregnancy_id) %>% 
  mutate(fetus_number = row_number()) %>% 
  ungroup()

fetus_level_processed_long <- fetus_level_processed %>% 
  select(pregnancy_id, mother_upi,fetus_number, x_est_conception_date, x_pregnancy_end_date, outcome, starts_with("dose"), x_neonatal_death, x_gestation_at_outcome, smr02_apgar_5_minutes) %>% 
  pivot_longer(!c(pregnancy_id, fetus_number, mother_upi, x_est_conception_date, x_pregnancy_end_date, outcome, x_neonatal_death, x_gestation_at_outcome, smr02_apgar_5_minutes),
               names_to = c("vacc_dose_number", ".value"), 
               names_pattern = "dose_(.)_(.*)", 
               values_drop_na = TRUE) %>% 
  mutate(follow_up_date = if_else(is.na(x_pregnancy_end_date), publication_latest_vacc_date, x_pregnancy_end_date)) %>% 
  mutate(vaccine_in_preg_interval = interval(start = x_est_conception_date, 
                                             end =  follow_up_date)) %>%
  mutate(vaccine_in_preg = if_else(vacc_occurence_date %within% vaccine_in_preg_interval, TRUE, FALSE)) %>% 
  mutate(month_vaccine = format(as.Date(vacc_occurence_date), "%Y-%m") ) %>% 
  mutate(smr02_apgar_5_minutes = if_else(smr02_apgar_5_minutes == "NR" | smr02_apgar_5_minutes == "RR", NA_character_, smr02_apgar_5_minutes)) %>% 
  mutate(smr02_apgar_5_minutes = str_remove(smr02_apgar_5_minutes, " ")) %>% 
  mutate(smr02_apgar_5_minutes = as.numeric(smr02_apgar_5_minutes)) %>% 
  mutate(vacc_dose_number = if_else(vacc_dose_number == "4", "3", vacc_dose_number))


################ process pregnancy level file for output
pregnancies <- pregnancies_1 %>% 
  filter(est_conception_date <= publication_latest_vacc_date) %>%   
  mutate(pregnancy_end_date = case_when(is.na(pregnancy_end_date) ~ as.Date("1970-01-01"),
                                       T ~ pregnancy_end_date)) %>% 
  mutate(mother_age_at_conception = case_when(is.na(mother_age_at_conception) ~ 0,
                                              T ~ mother_age_at_conception))%>% 
  mutate(maternal_age_group = cops_age_group(mother_age_at_conception)) %>% 
  mutate(simd = case_when(is.na(simd) ~ "Unknown",
                          simd==9 ~ "Unknown",
                          T ~ as.character(simd))) %>% 
  mutate(ethnicity_desc_reporting = cops_reporting_ethnicity(ethnicity_code)) %>%
  mutate(ethnicity_desc_reporting = case_when(is.na(ethnicity_desc_reporting) ~ "5 Unknown/missing", 
                                              TRUE ~ethnicity_desc_reporting) )%>% 
  mutate(hbres = case_when(is.na(hbres) ~ "X - Unknown",
                           hbres == "NA" ~ "X - Unknown",
                           hbres == "No Fixed Abode" ~ "X - Unknown",
                           hbres == "Outside UK" ~ "X - Unknown",
                           hbres == "Not Known" ~ "X - Unknown",
                           hbres == "England/Wales/Northern Ireland" ~ "X - Unknown",
                           T ~ as.character(hbres)))


###### The eligible for vaccination sub-cohort would include women pregnant on 8 Dec 2020
##(the date the first vaccine was delivered outwith a clinical trial in Scotland) plus those subsequently becoming pregnant
#12.5.21discussed with Rachael she wants the full cohort. 


#Pivot data to one line per dose and calculate when the dose happened in relation to the pregnancy
preg_vacc_data_long <- pregnancies %>% 
  select(pregnancy_id, mother_upi, est_conception_date, pregnancy_end_date, maternal_age_group, simd, hbres, overall_outcome, starts_with("dose"), ethnicity_desc_reporting) %>% 
  pivot_longer(!c(pregnancy_id, mother_upi, est_conception_date, pregnancy_end_date, maternal_age_group, simd, hbres, overall_outcome, ethnicity_desc_reporting),
               names_to = c("vacc_dose_number", ".value"), 
               names_pattern = "dose_(.)_(.*)", 
               values_drop_na = TRUE) %>% 
  mutate(postcondays = as.numeric(difftime(vacc_occurence_date, est_conception_date, units = "days"))) %>%
  mutate(vacc_dose_number = as.numeric(vacc_dose_number)) %>% 
  mutate(vaccine_preg = case_when(vacc_occurence_date >= est_conception_date 
                                  & vacc_occurence_date <= pregnancy_end_date  ~ 1,
                                  vacc_occurence_date >= est_conception_date 
                                  & pregnancy_end_date == "1970-01-01" ~ 1,
                                  T ~ NA_real_)) %>% 
  mutate(vaccineb4_preg = case_when(vacc_occurence_date < est_conception_date ~ 1, T ~ NA_real_)) %>%
  mutate(vaccineafter_preg=case_when(vacc_occurence_date > pregnancy_end_date 
                                     & pregnancy_end_date != "1970-01-01" ~ 1, T ~ NA_real_)) %>% 
  mutate(week_ending = ceiling_date(vacc_occurence_date, unit = "week", change_on_boundary = F)) %>% 
  mutate(month_vaccine = format(as.Date(vacc_occurence_date), "%Y-%m"))

month_lookup <- preg_vacc_data_long %>% 
  mutate(month_vaccine_words = format(as.Date(vacc_occurence_date), "%b %Y")) %>% 
  count(month_vaccine, month_vaccine_words) %>% 
  select(-n)

preg_vacc_data_main <- preg_vacc_data_long %>% 
  filter(vaccine_preg == 1) %>% 
  select(-c(vaccineb4_preg, vaccineafter_preg)) %>% 
  mutate(Stage_of_Preg = trimester(est_conception_date, vacc_occurence_date))


preg_vacc_data_main_check <- preg_vacc_data_main %>% 
  count(mother_upi)
preg_vacc_data_main_check_type <- preg_vacc_data_main %>% 
  count(vacc_product_name)

######################## table 1: N vacs by dose week #########################

# Note objects in environment to retain
objects_to_retain <- c(ls(), "objects_to_retain")

preg_vacc_table_1_1 <- preg_vacc_data_main %>% 
  count(week_ending,  vacc_dose_number)

preg_vacc_table_1_2 <- preg_vacc_data_main %>% 
  mutate(vacc_dose_number = 99) %>% 
  count(week_ending, vacc_dose_number)

Table1_weeks <-  preg_vacc_table_1_1 %>% 
  bind_rows(preg_vacc_table_1_2) %>% 
  pivot_wider(names_from = "vacc_dose_number", values_from = "n", names_prefix = "dose_") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(week_ending = as.character(week_ending))

Table1_total <- preg_vacc_table_1_1 %>% 
  bind_rows(preg_vacc_table_1_2) %>% 
  group_by(vacc_dose_number) %>%
  summarise(n = sum(n)) %>% 
  pivot_wider(names_from = "vacc_dose_number", values_from = "n", names_prefix = "dose_") %>% 
  mutate(week_ending = "Total")

Table1 <- Table1_weeks %>% 
  bind_rows(Table1_total)
  
Table1 %>% write_rds(paste0(folder_temp_data, "network_folder/table_1_n_by_week.rds"))

preg_vacc_table_1_1 %>% 
  mutate(vacc_dose_number = as.factor(vacc_dose_number)) %>% 
  ggplot(aes(x=week_ending, y=n, group=vacc_dose_number)) +
  geom_line(aes(colour = vacc_dose_number)) +
  xlab("Week ending") +
  ylab("Number of vaccinations") +
  labs(colour = "Dose number")

n_vacs_text_1 <- paste0("Based on COPS database of pregnant women updated in mid-", format(dates$read_in_date[dates$dataset == "Vaccines"], "%B %Y"), ", and records of vaccinations delivered up to ", format(publication_latest_vacc_date, "%d %B %Y"))

# Logic around whether last week in Table1 is a partial week beyond publication_latest_vacc_date
n_vacs_text_2_print <- (lubridate::ymd(Table1$week_ending[[nrow(Table1) - 1]]) > publication_latest_vacc_date)

if (n_vacs_text_2_print == TRUE){
  n_vacs_text_2 <- paste0("Please note week ending ", format(max(preg_vacc_table_1_1$week_ending), "%d %B %Y"), " is shown for comparison against the monthly data. Week ending the ", 
                          format(max(preg_vacc_table_1_1$week_ending), "%d %B %Y"), " is a partial week as vaccines up to and including ", format(publication_latest_vacc_date, "%d %B %Y"), " are included in this analysis.") 
} else{
  n_vacs_text_2 = ""
}

n_vacs_text_3 <- paste0("Grey shading indicates provisional data and may change in future publications")
n_vacs_text_4 <- paste0("The vaccination roll-out in Scotland started on 8th December 2020")
n_vacs_text_5 <- paste0("Vaccination is defined as occurring in pregnancy if the date of vaccination occurs at any point from the estimated date of conception (date the woman was 2+0 weeks gestation) up to and including the date the pregnancy ended.")
n_vacs_text_6 <- paste0("Provision of third primary doses and booster doses started in September 2021")
n_vacs_text_7 <- paste0("Week is based on the date of vaccination")
n_vacs_text_8 <- paste0("Provision of second booster doses to eligible individuals started in February 2022")

openxlsx::writeData(
  wb = template, sheet = "N vacs by dose week", x = Table1,
  startCol = 1, startRow = 5,
  colNames = FALSE,
  borders = "all", borderColour = "#000000", borderStyle = "thin")
#insertPlot(template, "N vacs by dose week", width = 6, startCol = 6, startRow = 9,
           #height = 3.5, fileType = "png", units = "in")
writeData(template, "N vacs by dose week", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_2, startCol = 1, startRow = nrow(Table1) + 13, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_3, startCol = 1, startRow = nrow(Table1) + 6, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_4, startCol = 1, startRow = nrow(Table1) + 8, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_5, startCol = 1, startRow = nrow(Table1) + 9, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_6, startCol = 1, startRow = nrow(Table1) + 10, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_8, startCol = 1, startRow = nrow(Table1) + 11, colNames = FALSE)
writeData(template, "N vacs by dose week", n_vacs_text_7, startCol = 1, startRow = nrow(Table1) + 12, colNames = FALSE)

# Remove all objects not in list to retain
# rm(list = setdiff(ls(), objects_to_retain))


######################## table 2 gestation #########################

# from this point onwards combine 3rd and 4th doses
preg_vacc_data_main <- preg_vacc_data_main %>% 
  mutate(vacc_dose_number = if_else(vacc_dose_number == 4, 3, vacc_dose_number))

preg_vacc_data2 <- preg_vacc_data_main %>% 
  count(month_vaccine, Stage_of_Preg, vacc_dose_number)

preg_vacc_data3 <- preg_vacc_data_main %>%  
  mutate(Stage_of_Preg= "Total during pregnancy") %>% 
  count(month_vaccine, Stage_of_Preg, vacc_dose_number)

Table2_total <- preg_vacc_data2 %>% 
  bind_rows(preg_vacc_data3) %>% 
  group_by(month_vaccine, Stage_of_Preg) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = "month_vaccine_words", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total = rowSums(.[-(1)]))

Table2 <-  preg_vacc_data2 %>% 
  bind_rows(preg_vacc_data3) %>%   
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = "month_vaccine_words", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total = rowSums(.[-(1:2)])) %>%
  bind_rows(Table2_total) %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number)) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Total")) %>%
  arrange(Stage_of_Preg, vacc_dose_number)

# read out table to rds
Table2 %>% write_rds(paste0(folder_temp_data, "network_folder/table_2_n_by_gestation.rds"))

#read out into Excel template
Table2_excel <- Table2 %>% select(-c(Stage_of_Preg, vacc_dose_number))

#gestation_text_1 <- paste0("Due to the few number of weeks passed since " , format(publication_latest_vacc_date %m+% months(-1), "%B %Y"), " we are unlikely to have a complete count of the number of women in their first trimester during " 
#                           , format(publication_latest_vacc_date %m+% months(-1), "%B %Y"))
#gestation_text_2 <- paste0("Due to the few number of weeks passed since " , format(publication_latest_vacc_date, "%B %Y"), " we are unlikely to have a complete count of the number of women in their first trimester during " 
#                           , format(publication_latest_vacc_date, "%B %Y"))

writeData(template, "N vacs by gestation", Table2_excel, startCol = 3, startRow = 5, colNames = TRUE)
writeData(template, "N vacs by gestation", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
#writeData(template, "N vacs by gestation", gestation_text_1, startCol = 1, startRow = 21, colNames = FALSE)
#writeData(template, "N vacs by gestation", gestation_text_2, startCol = 1, startRow = 22, colNames = FALSE)

preg_vacc_data2 %>% 
  group_by(month_vaccine, Stage_of_Preg) %>%
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=month_vaccine, y=n, group=Stage_of_Preg)) +
  geom_line(aes(colour=Stage_of_Preg)) + 
  xlab("Month") +
  ylab("Number of vaccinations") +
  labs(colour = "Stage of pregnancy")

#insertPlot(template, "N vacs by gestation", width = 6, startCol = 1, startRow = 24,
 #          height = 3.5, fileType = "png", units = "in")


preg_vacc_data_main %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number)) %>% 
  ggplot(aes(x=Stage_of_Preg, group=vacc_dose_number)) +
  geom_bar(position ='dodge', aes(fill=vacc_dose_number)) +  
  xlab("Month") +
  ylab("Number of vaccinations")

#insertPlot(template, "N vacs by gestation", width = 6, startCol = 6, startRow = 24,
           #height = 3.5, fileType = "png", units = "in")

#rm(preg_vacc_data2, preg_vacc_data3, Table2, Table2_excel, Table2_total)


######################## Admissions by gestation - NOT FOR PUBLICATION #########################

overall_vaccines_by_month <- Table2 %>% 
  filter(vacc_dose_number == "Total") %>% 
  select(-vacc_dose_number) %>% 
  mutate(indicator = "1 - Total vaccinations given") %>% 
  mutate(Stage_of_Preg = if_else(Stage_of_Preg == "Total during pregnancy", "Total", Stage_of_Preg)) 

admissions_by_gest <- readRDS(paste0(folder_temp_data, "network_folder/admission_by_trimester.rds"))

icu_by_gest <- readRDS(paste0(folder_temp_data, "network_folder/icu_by_trimester.rds"))

all_admissions <- admissions_by_gest %>% 
  bind_rows(icu_by_gest) %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n) %>% 
  adorn_totals(where = "col")

admissions_by_gestation_table <- all_admissions %>% 
  bind_rows(overall_vaccines_by_month)  %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(Stage_of_Preg, indicator) %>% 
  select(-c(Stage_of_Preg, indicator))


writeData(template, "N admission by gest NFP", admissions_by_gestation_table, startCol = 3, startRow = 6, colNames = TRUE)
writeData(template, "N admission by gest NFP", n_vacs_text_1, startCol = 1, startRow = 4, colNames = FALSE)



######################## Severe outcomes by gestation - NOT FOR PUBLICATION #########################

## Overall numbers for comparison: vaccinations

overall_vaccines_by_month <- Table2 %>% 
  filter(vacc_dose_number == "Total") %>% 
  select(-vacc_dose_number) %>% 
  mutate(indicator = "Total vaccinations given") %>% 
  mutate(Stage_of_Preg = if_else(Stage_of_Preg == "Total during pregnancy", "Total", Stage_of_Preg)) %>% 
  pivot_longer(cols = !c(Stage_of_Preg, indicator), names_to = "month_vaccine_words", values_to = "n") %>%
  left_join(month_lookup) %>% 
  mutate(month_vaccine = if_else(is.na(month_vaccine), "Total", month_vaccine)) %>% 
  select(-month_vaccine_words)

## Overall numbers for comparison: pregnancies with vaccination total

overall_vaccines_preg_monthly <- preg_vacc_data_main %>% 
  select(pregnancy_id, month_vaccine) %>% 
  distinct() %>% 
  count(month_vaccine) %>% 
  right_join(month_lookup)

overall_vaccines_preg <- preg_vacc_data_main %>% 
  select(pregnancy_id) %>% 
  distinct() %>% 
  mutate(month_vaccine = "Total") %>%
  count(month_vaccine) %>% 
  bind_rows(overall_vaccines_preg_monthly) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  arrange(month_vaccine) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n)

# write total out for use in markdown
write_rds(overall_vaccines_preg, (paste0(folder_temp_data, "network_folder/vaccinated_pregnancies.rds")))

## Overall numbers for comparison: women with vaccination total

overall_vaccines_women_monthly <- preg_vacc_data_main %>% 
  select(mother_upi, month_vaccine) %>% 
  distinct() %>% 
  count(month_vaccine) %>% 
  right_join(month_lookup)

overall_vaccines_women <- preg_vacc_data_main %>% 
  select(mother_upi) %>% 
  distinct() %>% 
  mutate(month_vaccine = "Total") %>%
  count(month_vaccine) %>% 
  bind_rows(overall_vaccines_preg_monthly) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  arrange(month_vaccine) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n)


## Overall numbers for comparison: live births ##

data_livebirths <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(live_birth_after_vacc_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_live_birth_after_vacc = if_else(x_pregnancy_end_date %within% live_birth_after_vacc_interval, TRUE, FALSE))

# within 28 days
live_births_after_vac_monthly <- data_livebirths %>% 
  filter(flag_live_birth_after_vacc == TRUE) %>% 
  count(month_vaccine) %>% 
  right_join(month_lookup)

live_births_after_vac <- data_livebirths %>% 
  filter(flag_live_birth_after_vacc == TRUE) %>% 
  mutate(month_vaccine = "Total") %>% 
  select(pregnancy_id, month_vaccine, fetus_number) %>% 
  distinct() %>% 
  count(month_vaccine) %>% 
  bind_rows(live_births_after_vac_monthly) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  mutate(indicator = "live birth within 28 days of vaccine")


# all subsequent live births
subsequent_live_births_monthly <- data_livebirths %>% 
  count(month_vaccine) %>% 
  right_join(month_lookup)

subsequent_live_births <- data_livebirths %>% 
  mutate(month_vaccine = "Total") %>% 
  select(pregnancy_id, month_vaccine, fetus_number) %>% 
  distinct() %>% 
  count(month_vaccine) %>% 
  bind_rows(subsequent_live_births_monthly) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  mutate(indicator = "subsequent live birth") 


# combine into one df
live_births <- live_births_after_vac %>% 
  bind_rows(subsequent_live_births) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  arrange(month_vaccine, indicator) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n)

  

## critical care admissions ##

#icu_by_gest <- readRDS(paste0(folder_temp_data, "network_folder/icu_by_trimester.rds"))


## Maternal deaths ##

maternal_deaths_by_gest <- readRDS(paste0(folder_temp_data, "network_folder/maternal_deaths_by_trimester.rds"))

## Pregnancies with Stillbirths ##
data_stillbirths_preg <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_stillbirth_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_stillbirth = if_else(x_pregnancy_end_date %within% vacc_associated_stillbirth_interval, TRUE, FALSE)) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# associated stillbirths
vacc_associated_stillbirths_preg_by_gest <- associated_pregnancies(data_stillbirths_preg, pregnancy_id, flag_vacc_associated_stillbirth, 
                                                                   month_vaccine, Stage_of_Preg, "pregnancies with associated stillbirths")
# subsequent stillbirths
vacc_subsequent_stillbirths_preg_by_gest <- subsequent_pregnancies(data_stillbirths_preg, pregnancy_id, 
                                                                   month_vaccine, Stage_of_Preg, "pregnancies with subsequent stillbirths")


## Stillbirths ##
data_stillbirths <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_stillbirth_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_stillbirth = if_else(x_pregnancy_end_date %within% vacc_associated_stillbirth_interval, TRUE, FALSE)) %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# associated stillbirths
vacc_associated_stillbirths_by_gest <- associated_babies(data_stillbirths, pregnancy_id, flag_vacc_associated_stillbirth, 
                                                                   month_vaccine, Stage_of_Preg, "associated stillbirths")
# subsequent stillbirths
vacc_subsequent_stillbirths_by_gest <- subsequent_babies(data_stillbirths, pregnancy_id, 
                                                                   month_vaccine, Stage_of_Preg, "subsequent stillbirths")


## Pregnancies with pre-term births ##
data_preterm_preg <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, x_gestation_at_outcome, vacc_product_name) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_preterm_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_preterm = if_else(x_pregnancy_end_date %within% vacc_associated_preterm_interval, TRUE, FALSE)) %>% 
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
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# associated preterm births
vacc_associated_preterm_preg_by_gest <- associated_pregnancies(data_preterm_preg, pregnancy_id, flag_vacc_associated_preterm, 
                                                                   month_vaccine, Stage_of_Preg, "pregnancies with associated preterm births")
# subsequent preterm births
vacc_subsequent_preterm_preg_by_gest <- subsequent_pregnancies(data_preterm_preg, pregnancy_id, 
                                                                   month_vaccine, Stage_of_Preg, "pregnancies with subsequent preterm births")


## Preterm births ##
data_preterm <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, x_gestation_at_outcome, vacc_product_name, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_preterm_interval = interval(start = vacc_occurence_date, 
                                                     end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_preterm = if_else(x_pregnancy_end_date %within% vacc_associated_preterm_interval, TRUE, FALSE)) %>% 
  mutate(flag_preterm = if_else(x_gestation_at_outcome < 37, TRUE, FALSE)) %>% 
  mutate(flag_very_preterm = if_else(x_gestation_at_outcome < 32, TRUE, FALSE)) %>% 
  select(-x_gestation_at_outcome) %>% 
  filter(flag_preterm == TRUE) %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date)) %>% 
  mutate(month_vaccine = format(as.Date(vacc_occurence_date), "%Y-%m") )


# associated preterm births
vacc_associated_preterm_by_gest <- associated_babies(data_preterm, pregnancy_id, flag_vacc_associated_preterm, 
                                                         month_vaccine, Stage_of_Preg, "associated preterm births")
# subsequent preterm births
vacc_subsequent_preterm_by_gest <- subsequent_babies(data_preterm, pregnancy_id, 
                                                         month_vaccine, Stage_of_Preg, "subsequent preterm births")


## Pregnancies with very pre-term births ##
data_very_preterm_preg <- data_preterm_preg %>% 
  filter(flag_very_preterm == TRUE)

# associated very preterm births
vacc_associated_very_preterm_preg_by_gest <- associated_pregnancies(data_very_preterm_preg, pregnancy_id, flag_vacc_associated_preterm, 
                                                                 month_vaccine, Stage_of_Preg, "pregnancies with associated very preterm births")
# subsequent very preterm births
vacc_subsequent_very_preterm_preg_by_gest <- subsequent_pregnancies(data_very_preterm_preg, pregnancy_id, 
                                                                month_vaccine, Stage_of_Preg, "pregnancies with subsequent very preterm births")


## Very preterm births ##
data_very_preterm <- data_preterm %>% 
  filter(flag_very_preterm == TRUE)

# associated very preterm births
vacc_associated_very_preterm_by_gest <- associated_babies(data_very_preterm, pregnancy_id, flag_vacc_associated_preterm, 
                                                                    month_vaccine, Stage_of_Preg, "associated very preterm births")
# subsequent very preterm births
vacc_subsequent_very_preterm_by_gest <- subsequent_babies(data_very_preterm, pregnancy_id, 
                                                                     month_vaccine, Stage_of_Preg, "subsequent very preterm births")


### Apgar score
data_apgar_base <- fetus_level_processed_long %>% 
  filter(outcome == "Live birth" & x_gestation_at_outcome >= 37 & vaccine_in_preg == TRUE) %>% 
  select(mother_upi, pregnancy_id, month_vaccine, x_est_conception_date, x_pregnancy_end_date, vacc_occurence_date, smr02_apgar_5_minutes, vacc_product_name) %>% 
  filter(smr02_apgar_5_minutes < 7) %>% 
  mutate(associated_birth_interval = interval(start = vacc_occurence_date, 
                                                     end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_associated_low_apgar = if_else(x_pregnancy_end_date %within% associated_birth_interval & smr02_apgar_5_minutes < 7, TRUE, FALSE)) %>% 
  mutate(flag_associated_very_low_apgar = if_else(x_pregnancy_end_date %within% associated_birth_interval & smr02_apgar_5_minutes < 4, TRUE, FALSE)) 

## Pregnancies ending in birth with low Apgar score ##
data_low_apgar_preg <- data_apgar_base %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# within 28 days
vacc_associated_low_apgar_preg_by_gest <- associated_pregnancies(data_low_apgar_preg, pregnancy_id, flag_associated_low_apgar, 
                                                                  month_vaccine, Stage_of_Preg, "pregnancies ending in associated low apgar score")
# subsequent
vacc_subsequent_low_apgar_preg_by_gest <- subsequent_pregnancies(data_low_apgar_preg, pregnancy_id, 
                                                                 month_vaccine, Stage_of_Preg, "pregnancies ending in subsequent low apgar score")

## Births wirth with low Apgar score ##
data_low_apgar <- data_apgar_base %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# within 28 days
vacc_associated_low_apgar_by_gest <- associated_pregnancies(data_low_apgar, pregnancy_id, flag_associated_low_apgar, 
                                                             month_vaccine, Stage_of_Preg, "associated low apgar score")
# subsequent
vacc_subsequent_low_apgar_by_gest <- subsequent_pregnancies(data_low_apgar, pregnancy_id, 
                                                            month_vaccine, Stage_of_Preg, "subsequent low apgar score")

## Pregnancies ending in birth with very low Apgar score ##
data_very_low_apgar_preg <- data_low_apgar_preg %>% 
  filter(smr02_apgar_5_minutes < 4)

# within 28 days
vacc_associated_very_low_apgar_preg_by_gest <- associated_pregnancies(data_very_low_apgar_preg, pregnancy_id, flag_associated_very_low_apgar, 
                                                                      month_vaccine, Stage_of_Preg, "pregnancies ending in associated very low apgar score")
# subsequent
vacc_subsequent_very_low_apgar_preg_by_gest <- subsequent_pregnancies(data_very_low_apgar_preg, pregnancy_id, 
                                                                      month_vaccine, Stage_of_Preg, "pregnancies ending in subsequent very low apgar score")

## Births with with very low Apgar score ##
data_very_low_apgar <- data_low_apgar %>% 
  filter(smr02_apgar_5_minutes < 4)

# within 28 days
vacc_associated_very_low_apgar_by_gest <- associated_pregnancies(data_very_low_apgar, pregnancy_id, flag_associated_very_low_apgar, 
                                                                 month_vaccine, Stage_of_Preg, "associated very low apgar score")
# subsequent
vacc_subsequent_very_low_apgar_by_gest <- subsequent_pregnancies(data_very_low_apgar, pregnancy_id, 
                                                                 month_vaccine, Stage_of_Preg, "subsequent very low apgar score")


## Pregnancies with neonatal deaths ##
data_neonatal_preg <- fetus_level_processed_long %>% 
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_neonatal_death_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_neonatal_death = if_else(x_pregnancy_end_date %within% vacc_associated_neonatal_death_interval, TRUE, FALSE)) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# associated neonatal deaths
vacc_associated_neonatal_death_preg_by_gest <- associated_pregnancies(data_neonatal_preg, pregnancy_id, flag_vacc_associated_neonatal_death, 
                                                                    month_vaccine, Stage_of_Preg, "pregnancies with associated neonatal death")
# subsequent neonatal deaths
vacc_subsequent_neonatal_death_preg_by_gest <- subsequent_pregnancies(data_neonatal_preg, pregnancy_id, 
                                                                     month_vaccine, Stage_of_Preg, "pregnancies with subsequent neonatal death")


## Neonatal deaths ##
data_neonatal_deaths <- fetus_level_processed_long %>%
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_neonatal_death_interval = interval(start = vacc_occurence_date, 
                                                            end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_neonatal_death = if_else(x_pregnancy_end_date %within% vacc_associated_neonatal_death_interval, TRUE, FALSE)) %>% 
  mutate(Stage_of_Preg = trimester(x_est_conception_date, vacc_occurence_date))

# associated neonatal deaths
vacc_associated_neonatal_death_by_gest <- associated_babies(data_neonatal_deaths, pregnancy_id, flag_vacc_associated_neonatal_death, 
                                                          month_vaccine, Stage_of_Preg, "associated neonatal death")
# subsequent neonatal deaths
vacc_subsequent_neonatal_death_by_gest <- subsequent_babies(data_neonatal_deaths, pregnancy_id, 
                                                           month_vaccine, Stage_of_Preg, "subsequent neonatal death")

## create spine
serious_outcomes <- c("Total vaccinations given", "associated maternal death", "subsequent maternal death",
                      "pregnancies with associated stillbirths", "pregnancies with subsequent stillbirths",
                      "associated stillbirths", "subsequent stillbirths", "pregnancies with associated preterm births", "pregnancies with subsequent preterm births",
                      "associated preterm births", "subsequent preterm births", "pregnancies with associated very preterm births", "pregnancies with subsequent very preterm births",
                      "associated very preterm births", "subsequent very preterm births", 
                      "pregnancies ending in associated low apgar score", "pregnancies ending in subsequent low apgar score", "associated low apgar score", "subsequent low apgar score",
                      "pregnancies ending in associated very low apgar score", "pregnancies ending in subsequent very low apgar score", "associated very low apgar score", "subsequent very low apgar score","pregnancies with associated neonatal death",
                      "pregnancies with subsequent neonatal death", "associated neonatal death",
                      "subsequent neonatal death")
serious_outcomes_factor <- factor(serious_outcomes, levels = serious_outcomes)

months_incl_total <- overall_vaccines_by_month %>% 
  select(month_vaccine) %>% 
  distinct()

serious_gest_spine <- expand.grid(months_incl_total$month_vaccine, Table2$Stage_of_Preg, serious_outcomes_factor) %>% 
  distinct() %>% 
  rename(month_vaccine = Var1, Stage_of_Preg = Var2, indicator = Var3) %>% 
  mutate(Stage_of_Preg = as.character(Stage_of_Preg)) %>% 
  mutate(Stage_of_Preg = case_when(Stage_of_Preg == "Total during pregnancy" ~ "Total", 
                                   T ~ Stage_of_Preg))

# combine all outcomes into one df
serious_outcomes_combined <- maternal_deaths_by_gest %>% 
  bind_rows(vacc_associated_stillbirths_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_stillbirths_preg_by_gest) %>% 
  bind_rows(vacc_associated_stillbirths_by_gest) %>% 
  bind_rows(vacc_subsequent_stillbirths_by_gest) %>% 
  bind_rows(vacc_associated_preterm_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_preterm_preg_by_gest) %>% 
  bind_rows(vacc_associated_preterm_by_gest) %>% 
  bind_rows(vacc_subsequent_preterm_by_gest) %>% 
  bind_rows(vacc_associated_very_preterm_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_very_preterm_preg_by_gest) %>% 
  bind_rows(vacc_associated_very_preterm_by_gest) %>% 
  bind_rows(vacc_subsequent_very_preterm_by_gest) %>% 
  bind_rows(vacc_associated_low_apgar_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_low_apgar_preg_by_gest) %>% 
  bind_rows(vacc_associated_low_apgar_by_gest) %>% 
  bind_rows(vacc_subsequent_low_apgar_by_gest) %>% 
  bind_rows(vacc_associated_very_low_apgar_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_very_low_apgar_preg_by_gest) %>% 
  bind_rows(vacc_associated_very_low_apgar_by_gest) %>% 
  bind_rows(vacc_subsequent_very_low_apgar_by_gest) %>% 
  bind_rows(vacc_associated_neonatal_death_preg_by_gest) %>% 
  bind_rows(vacc_subsequent_neonatal_death_preg_by_gest) %>% 
  bind_rows(vacc_associated_neonatal_death_by_gest) %>% 
  bind_rows(vacc_subsequent_neonatal_death_by_gest) %>% 
  bind_rows(overall_vaccines_by_month)

all_serious_outcomes <- serious_gest_spine %>% 
  arrange(Stage_of_Preg, indicator)  %>% 
  left_join(serious_outcomes_combined, by = c("month_vaccine", "Stage_of_Preg", "indicator")) %>% 
  mutate_all(~replace(., is.na(.), 0))

all_serious_outcomes_wide <- all_serious_outcomes %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n)
cases_total <- all_serious_outcomes_wide %>% 
  filter(Stage_of_Preg == "Total" & indicator == "Total vaccinations given")

all_serious_outcomes_total_excel <- all_serious_outcomes_wide %>% 
  filter(Stage_of_Preg == "Total" & indicator != "Total vaccinations given")

all_serious_outcomes_wide %>% write_rds(paste0(folder_temp_data, "network_folder/vaccine_severe_outcomes_stage.rds"))
all_serious_outcomes_total_excel %>% write_rds(paste0(folder_temp_data, "network_folder/vaccine_severe_outcomes_totals.rds"))
live_births %>% write_rds(paste0(folder_temp_data, "network_folder/all_live_births_after_vaccination.rds"))

## read out into template
writeData(template, "Sev oc by gest NFP", n_vacs_text_1, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Sev oc by gest NFP", select(all_serious_outcomes_wide, -c(Stage_of_Preg, indicator)), startCol=3, startRow=6)

writeData(template, "Severe outcomes tot NFP", n_vacs_text_1, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(cases_total, -c(Stage_of_Preg, indicator)), startCol=2, startRow=6)
writeData(template, "Severe outcomes tot NFP", overall_vaccines_preg, startCol=2, startRow=8, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", overall_vaccines_women, startCol=2, startRow=9, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(live_births, -indicator), startCol=2, startRow=10, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", select(all_serious_outcomes_total_excel, -c(Stage_of_Preg, indicator)), startCol=2, startRow=12, colNames = FALSE)


######################## table 3 - vaccine type #########################

preg_vacc_data4 <- preg_vacc_data_main %>%  
  count(month_vaccine, vacc_product_name, vacc_dose_number)


preg_vacc_data5 <- preg_vacc_data_main %>% 
  mutate(vacc_product_name= "Total - all types") %>% 
  count(month_vaccine, vacc_product_name, vacc_dose_number)

Table3_total <- preg_vacc_data4 %>% 
  bind_rows(preg_vacc_data5) %>% 
  group_by(month_vaccine, vacc_product_name) %>%
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = "month_vaccine_words", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total = rowSums(.[-(1)]))

Table3_temp <-  preg_vacc_data4 %>% 
  bind_rows(preg_vacc_data5) %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = "month_vaccine_words", values_from = "n") %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Total = rowSums(.[-(1:2)])) %>% 
  bind_rows(Table3_total) %>% 
  mutate(vacc_dose_number = as.character(vacc_dose_number)) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Total")) %>%
  arrange(vacc_product_name, vacc_dose_number) 

# build skeleton in case there aren't cases for any combination
dose_skeleton <- Table3_temp %>% 
  count(vacc_dose_number) %>% select(-n)
product_skeleton <- Table3_temp %>% 
  count(vacc_product_name) %>% 
  pivot_wider(names_from = vacc_product_name, values_from = n)
Table3_skeleton <- bind_cols(dose_skeleton, product_skeleton) %>% 
  pivot_longer(!vacc_dose_number, names_to = "vacc_product_name", values_to = "count") %>% 
  select(vacc_product_name, vacc_dose_number)

# add table to skeleton
Table3 <- Table3_skeleton %>% 
  full_join(Table3_temp) %>%  
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# read out table to rds
Table3 %>% write_rds(paste0(folder_temp_data, "network_folder/table_3_n_by_type.rds"))

#read out into Excel template
Table3_excel <- Table3 %>% 
  mutate(vacc_product_number = case_when(str_detect(vacc_product_name, "Pfizer") ~ 1,
                                         str_detect(vacc_product_name, "AstraZeneca") ~ 2,
                                         str_detect(vacc_product_name, "Moderna") ~ 3,
                                         str_detect(vacc_product_name, "Total") ~ 4)) %>% 
  arrange(vacc_product_number, vacc_dose_number) %>% 
  select(-c(vacc_product_name, vacc_dose_number, vacc_product_number))

writeData(template, "N vacs by type", Table3_excel, startCol = 3, startRow = 4, colNames = TRUE)
writeData(template, "N vacs by type", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)

ggplot(data=preg_vacc_data4, aes(x=month_vaccine, y=n, group=vacc_product_name)) +
  geom_line(aes(colour=vacc_product_name))

ggplot(data=preg_vacc_data_main, aes(x=vacc_product_name)) +
  geom_bar()


######################## Admissions by vaccine type - NOT FOR PUBLICATION #########################

overall_vaccines_by_month <- Table3 %>% 
  filter(vacc_dose_number == "Total") %>% 
  select(-vacc_dose_number) %>% 
  mutate(indicator = "1 - Total vaccinations given")

admissions_by_type <- readRDS(paste0(folder_temp_data, "network_folder/admission_by_product.rds"))

icu_by_type <- readRDS(paste0(folder_temp_data, "network_folder/icu_by_product.rds"))

all_admissions <- admissions_by_type %>% 
  bind_rows(icu_by_type) %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  pivot_wider(names_from = month_vaccine_words, values_from = n) %>% 
  adorn_totals(where = "col")

admissions_by_type_table <- all_admissions %>% 
  bind_rows(overall_vaccines_by_month)  %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(vacc_product_number = case_when(str_detect(vacc_product_name, "Pfizer") ~ 1,
                                         str_detect(vacc_product_name, "AstraZeneca") ~ 2,
                                         str_detect(vacc_product_name, "Moderna") ~ 3,
                                         str_detect(vacc_product_name, "Total") ~ 4)) %>% 
  arrange(vacc_product_number, indicator) %>% 
  select(-c(vacc_product_name, indicator, vacc_product_number))


writeData(template, "N admission by type NFP", admissions_by_type_table, startCol = 3, startRow = 6, colNames = TRUE)
writeData(template, "N admission by type NFP", n_vacs_text_1, startCol = 1, startRow = 4, colNames = FALSE)


######################## Severe outcomes by vaccine type - NOT FOR PUBLICATION #########################

## Overall numbers for comparison: vaccinations

overall_vaccines_by_month <- Table3 %>% 
  filter(vacc_dose_number == "Total") %>% 
  select(-vacc_dose_number) %>% 
  mutate(indicator = "Total vaccinations given") %>% 
  pivot_longer(cols = !c(vacc_product_name, indicator), names_to = "month_vaccine_words", values_to = "n") %>%
  left_join(month_lookup) %>% 
  mutate(month_vaccine = if_else(is.na(month_vaccine), "Total", month_vaccine)) %>% 
  select(-month_vaccine_words) %>% 
  mutate(vacc_product_name = if_else(vacc_product_name == "Total - all types", "Total", vacc_product_name))

## critical care admissions ##

#icu_by_product <- readRDS(paste0(folder_temp_data, "network_folder/icu_by_product.rds"))


## Maternal deaths ##

maternal_deaths_by_product <- readRDS(paste0(folder_temp_data, "network_folder/maternal_deaths_by_product.rds"))

## Pregnancies with Stillbirths ##
data_stillbirths_preg <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, vacc_product_name) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_stillbirth_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_stillbirth = if_else(x_pregnancy_end_date %within% vacc_associated_stillbirth_interval, TRUE, FALSE)) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup()

# associated stillbirths
vacc_associated_stillbirths_preg_by_type <- associated_pregnancies(data_stillbirths_preg, pregnancy_id, flag_vacc_associated_stillbirth, 
                                                                   month_vaccine, vacc_product_name, "pregnancies with associated stillbirths")
# subsequent stillbirths
vacc_subsequent_stillbirths_preg_by_type <- subsequent_pregnancies(data_stillbirths_preg, pregnancy_id, 
                                                                   month_vaccine, vacc_product_name, "pregnancies with subsequent stillbirths")

## Stillbirths ##
data_stillbirths <- fetus_level_processed_long %>% 
  filter(outcome == "Stillbirth") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, vacc_product_name, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_stillbirth_interval = interval(start = vacc_occurence_date, 
                                                        end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_stillbirth = if_else(x_pregnancy_end_date %within% vacc_associated_stillbirth_interval, TRUE, FALSE))

# associated stillbirths
vacc_associated_stillbirths_by_type <- associated_babies(data_stillbirths, pregnancy_id, flag_vacc_associated_stillbirth, 
                                                         month_vaccine, vacc_product_name, "associated stillbirths")
# subsequent stillbirths
vacc_subsequent_stillbirths_by_type <- subsequent_babies(data_stillbirths, pregnancy_id, 
                                                         month_vaccine, vacc_product_name, "subsequent stillbirths")

## Pregnancies with pre-term births ##
# associated preterm births
vacc_associated_preterm_preg_by_type <- associated_pregnancies(data_preterm_preg, pregnancy_id, flag_vacc_associated_preterm, 
                                                                    month_vaccine, vacc_product_name, "pregnancies with associated preterm births")
# subsequent preterm births
vacc_subsequent_preterm_preg_by_type <- subsequent_pregnancies(data_preterm_preg, pregnancy_id, 
                                                                month_vaccine, vacc_product_name, "pregnancies with subsequent preterm births")

## Preterm births ##
# associated preterm births
vacc_associated_preterm_by_type <- associated_babies(data_preterm, pregnancy_id, flag_vacc_associated_preterm, 
                                                     month_vaccine, vacc_product_name, "associated preterm births")
# subsequent preterm births
vacc_subsequent_preterm_by_type <- subsequent_babies(data_preterm, pregnancy_id, 
                                                     month_vaccine, vacc_product_name, "subsequent preterm births")


## Pregnancies with very pre-term births ##
# associated preterm births
vacc_associated_very_preterm_preg_by_type <- associated_pregnancies(data_very_preterm_preg, pregnancy_id, flag_vacc_associated_preterm, 
                                                               month_vaccine, vacc_product_name, "pregnancies with associated very preterm births")
# subsequent preterm births
vacc_subsequent_very_preterm_preg_by_type <- subsequent_pregnancies(data_very_preterm_preg, pregnancy_id, 
                                                               month_vaccine, vacc_product_name, "pregnancies with subsequent very preterm births")

## Very pre-term births ##
# associated preterm births
vacc_associated_very_preterm_by_type <- associated_babies(data_very_preterm, pregnancy_id, flag_vacc_associated_preterm, 
                                                     month_vaccine, vacc_product_name, "associated very preterm births")
# subsequent preterm births
vacc_subsequent_very_preterm_by_type <- subsequent_babies(data_very_preterm, pregnancy_id, 
                                                     month_vaccine, vacc_product_name, "subsequent very preterm births")

## Pregnancies ending in birth with low Apgar score ##
# within 28 days
vacc_associated_low_apgar_preg_by_type <- associated_pregnancies(data_low_apgar_preg, pregnancy_id, flag_associated_low_apgar, 
                                                                 month_vaccine, vacc_product_name, "pregnancies ending in associated low apgar score")
# subsequent
vacc_subsequent_low_apgar_preg_by_type <- subsequent_pregnancies(data_low_apgar_preg, pregnancy_id, 
                                                                 month_vaccine, vacc_product_name, "pregnancies ending in subsequent low apgar score")

## Births wirth with low Apgar score ##
# within 28 days
vacc_associated_low_apgar_by_type <- associated_pregnancies(data_low_apgar, pregnancy_id, flag_associated_low_apgar, 
                                                            month_vaccine, vacc_product_name, "associated low apgar score")
# subsequent
vacc_subsequent_low_apgar_by_type <- subsequent_pregnancies(data_low_apgar, pregnancy_id, 
                                                            month_vaccine, vacc_product_name, "subsequent low apgar score")

## Pregnancies ending in birth with very low Apgar score ##
# within 28 days
vacc_associated_very_low_apgar_preg_by_type <- associated_pregnancies(data_very_low_apgar_preg, pregnancy_id, flag_associated_very_low_apgar, 
                                                                      month_vaccine, vacc_product_name, "pregnancies ending in associated very low apgar score")
# subsequent
vacc_subsequent_very_low_apgar_preg_by_type <- subsequent_pregnancies(data_very_low_apgar_preg, pregnancy_id, 
                                                                      month_vaccine, vacc_product_name, "pregnancies ending in subsequent very low apgar score")

## Births with with very low Apgar score ##
# within 28 days
vacc_associated_very_low_apgar_by_type <- associated_pregnancies(data_very_low_apgar, pregnancy_id, flag_associated_very_low_apgar, 
                                                                 month_vaccine, vacc_product_name, "associated very low apgar score")
# subsequent
vacc_subsequent_very_low_apgar_by_type <- subsequent_pregnancies(data_very_low_apgar, pregnancy_id, 
                                                                 month_vaccine, vacc_product_name, "subsequent very low apgar score")


## Pregnancies with neonatal deaths ##
data_neonatal_preg <- fetus_level_processed_long %>% 
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, vacc_product_name) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_neonatal_death_interval = interval(start = vacc_occurence_date, 
                                                            end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_neonatal_death = if_else(x_pregnancy_end_date %within% vacc_associated_neonatal_death_interval, TRUE, FALSE)) %>% 
  group_by(pregnancy_id) %>% 
  mutate(x_est_conception_date = min_(x_est_conception_date),
         x_pregnancy_end_date = max_(x_pregnancy_end_date)) %>% 
  distinct() %>% 
  ungroup()

# associated neonatal deaths
vacc_associated_neonatal_death_preg_by_type <- associated_pregnancies(data_neonatal_preg, pregnancy_id, flag_vacc_associated_neonatal_death, 
                                                                      month_vaccine, vacc_product_name, "pregnancies with associated neonatal death")
# subsequent neonatal deaths
vacc_subsequent_neonatal_death_preg_by_type <- subsequent_pregnancies(data_neonatal_preg, pregnancy_id, 
                                                                      month_vaccine, vacc_product_name, "pregnancies with subsequent neonatal death")

## Neonatal deaths ##
data_neonatal_deaths <- fetus_level_processed_long %>%
  filter(x_neonatal_death =="Early neonatal death (d0-6)" | x_neonatal_death =="Late neonatal death (d7-27)") %>% 
  select(pregnancy_id, x_est_conception_date, vacc_occurence_date, x_pregnancy_end_date, vaccine_in_preg, month_vaccine, vacc_product_name, fetus_number) %>% 
  filter(vaccine_in_preg == TRUE) %>% 
  mutate(vacc_associated_neonatal_death_interval = interval(start = vacc_occurence_date, 
                                                            end =  vacc_occurence_date + days(27))) %>% 
  mutate(flag_vacc_associated_neonatal_death = if_else(x_pregnancy_end_date %within% vacc_associated_neonatal_death_interval, TRUE, FALSE))

# associated neonatal deaths
vacc_associated_neonatal_death_by_type <- associated_babies(data_neonatal_deaths, pregnancy_id, flag_vacc_associated_neonatal_death, 
                                                            month_vaccine, vacc_product_name, "associated neonatal death")
# subsequent neonatal deaths
vacc_subsequent_neonatal_death_by_type <- subsequent_babies(data_neonatal_deaths, pregnancy_id, 
                                                            month_vaccine, vacc_product_name, "subsequent neonatal death")

## create spine

serious_product_spine <- expand.grid(months_incl_total$month_vaccine, Table3_skeleton$vacc_product_name, serious_outcomes_factor) %>% 
  distinct() %>% 
  rename(month_vaccine = Var1, vacc_product_name = Var2, indicator = Var3) %>% 
  mutate(vacc_product_name = as.character(vacc_product_name)) %>% 
  mutate(vacc_product_name = if_else(vacc_product_name == "Total - all types", "Total", vacc_product_name))
  
# combine all outcomes into one df
serious_outcomes_combined <- maternal_deaths_by_product %>% 
  bind_rows(vacc_associated_stillbirths_preg_by_type) %>% 
  bind_rows(vacc_subsequent_stillbirths_preg_by_type) %>% 
  bind_rows(vacc_associated_stillbirths_by_type) %>% 
  bind_rows(vacc_subsequent_stillbirths_by_type) %>% 
  bind_rows(vacc_associated_preterm_preg_by_type) %>% 
  bind_rows(vacc_subsequent_preterm_preg_by_type) %>% 
  bind_rows(vacc_associated_preterm_by_type) %>% 
  bind_rows(vacc_subsequent_preterm_by_type) %>% 
  bind_rows(vacc_associated_very_preterm_preg_by_type) %>% 
  bind_rows(vacc_subsequent_very_preterm_preg_by_type) %>% 
  bind_rows(vacc_associated_very_preterm_by_type) %>% 
  bind_rows(vacc_subsequent_very_preterm_by_type) %>% 
  bind_rows(vacc_associated_low_apgar_preg_by_type) %>% 
  bind_rows(vacc_subsequent_low_apgar_preg_by_type) %>% 
  bind_rows(vacc_associated_low_apgar_by_type) %>% 
  bind_rows(vacc_subsequent_low_apgar_by_type) %>% 
  bind_rows(vacc_associated_very_low_apgar_preg_by_type) %>% 
  bind_rows(vacc_subsequent_very_low_apgar_preg_by_type) %>% 
  bind_rows(vacc_associated_very_low_apgar_by_type) %>% 
  bind_rows(vacc_subsequent_very_low_apgar_by_type) %>% 
  bind_rows(vacc_associated_neonatal_death_preg_by_type) %>% 
  bind_rows(vacc_subsequent_neonatal_death_preg_by_type) %>% 
  bind_rows(vacc_associated_neonatal_death_by_type) %>% 
  bind_rows(vacc_subsequent_neonatal_death_by_type) %>% 
  bind_rows(overall_vaccines_by_month)


all_serious_outcomes <- serious_product_spine %>% 
  mutate(vacc_product_number = case_when(str_detect(vacc_product_name, "Pfizer") ~ 1,
                                         str_detect(vacc_product_name, "AstraZeneca") ~ 2,
                                         str_detect(vacc_product_name, "Moderna") ~ 3,
                                         str_detect(vacc_product_name, "Total") ~ 4)) %>% 
  arrange(vacc_product_number, indicator) %>% 
  select(-vacc_product_number) %>% 
  left_join(serious_outcomes_combined, by = c("month_vaccine", "vacc_product_name", "indicator")) %>% 
  mutate_all(~replace(., is.na(.), 0))

all_serious_outcomes_wide <- all_serious_outcomes %>% 
  left_join(month_lookup) %>% 
  select(-month_vaccine) %>% 
  mutate(month_vaccine_words = if_else(is.na(month_vaccine_words), "Total", month_vaccine_words)) %>% 
  pivot_wider(id_cols = c(vacc_product_name, indicator), names_from = month_vaccine_words, values_from = n)

cases_total <- all_serious_outcomes_wide %>% 
  filter(vacc_product_name == "Total" & indicator == "Total vaccinations given")

all_serious_outcomes_total_excel <- all_serious_outcomes_wide %>% 
  filter(vacc_product_name == "Total" & indicator != "Total vaccinations given")

saveRDS(all_serious_outcomes_wide,paste0(folder_temp_data,"network_folder/vaccine_severe_outcomes_type.rds" ))
## read out into template 
writeData(template, "Sev oc by type NFP", n_vacs_text_1, startCol = 1, startRow = 4, colNames = FALSE)
writeData(template, "Sev oc by type NFP", select(all_serious_outcomes_wide, -c(vacc_product_name, indicator)), startCol=3, startRow=6)

######################## table 4 Uptake in pregnancy  #########################
# create list of all relevant months to date
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= vacc_start_date) %>% 
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start) %>% 
  select(month, month_start)

months_denominators <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, month_start, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                            & est_conception_date < months$month_start[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag) %>% 
    filter(!is.na(flag))
    
    months_denominators[i,]$number_pregnancies <- number[1,]$n
}

months_denominators <- months_denominators %>% 
  left_join(month_lookup, by = c("month" = "month_vaccine"))

# tidy and add months in words
preg_vacc_data_uptake_1 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%Y-%m")) %>% 
  count(month_vacc, mother_upi) %>% 
  count(month_vacc) %>% 
  rename(number_vaccinated=n) %>% 
  right_join(months_denominators, by = c("month_vacc" = "month")) %>% 
  filter(!is.na(month_vaccine_words))

#calculate total numbers
#Table4_total_1 <- pregnancies %>% 
#  mutate(month_vaccine_words = case_when((pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" )
#                           & est_conception_date < max(months$month_start) 
#                           ~ paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))) %>% 
#  count(month_vaccine_words) %>% 
#  filter(!is.na(month_vaccine_words)) %>% 
#  rename(number_pregnancies = n)

###new section revised. 
Table4_total_1 <- pregnancies %>% 
  filter(pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" ) %>% 
  arrange(mother_upi, est_conception_date) %>%
  group_by(mother_upi) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  rename(number_pregnancies = n)


Table4_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words) %>% 
  rename(number_vaccinated = n)



Table4_total <- Table4_total_1 %>% 
  left_join(Table4_total_2) %>% 
  bind_rows(preg_vacc_data_uptake_1) %>% 
  mutate(percen_vacc=(number_vaccinated/number_pregnancies)) %>% 
  arrange(month_vacc)

# read out table to rds
Table4_total %>% write_rds(paste0(folder_temp_data, "network_folder/table_4_overall_uptake.rds"))

options(scipen = 999)
Table4 <- Table4_total %>% 
  select(month_vaccine_words, number_pregnancies, number_vaccinated, percen_vacc) %>% 
  pivot_longer(!month_vaccine_words, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(id = indicator, names_from = month_vaccine_words, values_from = value) %>% 
  select(-indicator)

uptake_pregnancy_text_1 <- paste0("*The total for this row is the total number of women pregnant at any point from 1st December 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")
uptake_pregnancy_text_2 <- paste0("**The total for this row is the total number of women receiving any COVID-19 vaccination during pregnancy from 1st December 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")

writeData(template, "Uptake in pregnancy", Table4, startCol = 2, startRow = 5, colNames = TRUE)
writeData(template, "Uptake in pregnancy", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Uptake in pregnancy", uptake_pregnancy_text_1, startCol = 1, startRow = 12, colNames = FALSE)
writeData(template, "Uptake in pregnancy", uptake_pregnancy_text_2, startCol = 1, startRow = 14, colNames = FALSE)


######################## table 5 uptake age   #########################

# create list of all relevant months to date
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= vacc_start_date) %>% 
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, maternal_age_group) %>% 
  select(month, month_start, maternal_age_group)

# TEMP FIX
months <- expand.grid(months$month_start, months$maternal_age_group) %>% distinct() %>%
  rename(month = Var1, maternal_age_group = Var2) %>%
  mutate(month_start = floor_date(month)) %>%
  select(month, month_start, maternal_age_group) %>%
  mutate(month = format(as.Date(month), "%Y-%m")) %>% 
  mutate(month = as.character(month))

months_denominators <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, month_start, maternal_age_group, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                            & est_conception_date < months$month_start[i] )
                            & maternal_age_group == months$maternal_age_group[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, maternal_age_group) %>% 
    filter(!is.na(flag))
  
  months_denominators[i,]$number_pregnancies <- number[1,]$n
}

months_denominators <- months_denominators %>% 
  left_join(month_lookup, by = c("month" = "month_vaccine"))

# tidy and add months in words
preg_vacc_data_uptake_2 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%Y-%m")) %>% 
  count(month_vacc, mother_upi, maternal_age_group) %>% 
  count(month_vacc, maternal_age_group) %>% 
  rename(number_vaccinated=n) %>% 
  right_join(months_denominators, by = c("month_vacc" = "month", "maternal_age_group" = "maternal_age_group")) %>% 
  filter(!is.na(month_vaccine_words))

#calculate total numbers
Table5_total_1 <- pregnancies %>% 
  filter(pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" ) %>% 
  arrange(mother_upi, est_conception_date) %>%
  group_by(mother_upi) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>%  
  count(month_vaccine_words, maternal_age_group) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  rename(number_pregnancies = n)

Table5_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi, maternal_age_group) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words, maternal_age_group) %>% 
  rename(number_vaccinated = n)

# build skeleton in case there aren't cases for any combination
age_skeleton <- Table5_total_1 %>% 
  count(maternal_age_group) %>% 
  pivot_wider(names_from = maternal_age_group, values_from = n)
Table5_skeleton <- bind_cols(month_lookup, age_skeleton) %>% 
  pivot_longer(!c(month_vaccine, month_vaccine_words), names_to = "maternal_age_group", values_to = "count") %>% 
  select(month_vaccine_words, month_vaccine, maternal_age_group)

# build final table
Table5_total <- Table5_total_1 %>% 
  left_join(Table5_total_2) %>% 
  bind_rows(preg_vacc_data_uptake_2) %>% 
  full_join(Table5_skeleton, by = c("month_vaccine_words" = "month_vaccine_words", "maternal_age_group" = "maternal_age_group",
                                     "month_vacc" = "month_vaccine")) %>% 
  mutate(percen_vacc=(number_vaccinated/number_pregnancies)) %>% 
  bind_rows(Table4_total) %>% 
  arrange(month_vacc, maternal_age_group) %>% 
  select(month_vaccine_words, maternal_age_group, number_pregnancies, number_vaccinated, percen_vacc) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "9 Total")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
# read out table to rds
Table5_total %>% write_rds(paste0(folder_temp_data, "network_folder/table_5_uptake_by_age.rds"))

# fill out excel template
Table5_pregnant <- Table5_total %>% 
  select(month_vaccine_words, maternal_age_group, number_pregnancies) %>% 
  pivot_wider(id = maternal_age_group, names_from = month_vaccine_words, values_from = number_pregnancies) %>% 
  select(-maternal_age_group)

writeData(template, "Uptake by age", Table5_pregnant, startCol = 2, startRow = 6, colNames = TRUE)

Table5_vaccinated <- Table5_total %>% 
  select(month_vaccine_words, maternal_age_group, number_vaccinated) %>% 
  pivot_wider(id = maternal_age_group, names_from = month_vaccine_words, values_from = number_vaccinated) %>% 
  select(-maternal_age_group)

writeData(template, "Uptake by age", Table5_vaccinated, startCol = 2, startRow = 17, colNames = FALSE)

Table5_percentage <- Table5_total %>% 
  select(month_vaccine_words, maternal_age_group, percen_vacc) %>% 
  pivot_wider(id = maternal_age_group, names_from = month_vaccine_words, values_from = percen_vacc) %>% 
  select(-maternal_age_group)

uptake_pregnancy_text_1_plural <- paste0("*The total for these rows is the total number of women pregnant at any point from 1st December 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")
uptake_pregnancy_text_2_plural <- paste0("**The total for these rows is the total number of women receiving any COVID-19 vaccination during pregnancy from 1st December 2020 to ", format(publication_latest_vacc_date, "%d %B %Y"), " inclusive.")

writeData(template, "Uptake by age", Table5_percentage, startCol = 2, startRow = 27, colNames = FALSE)
writeData(template, "Uptake by age", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Uptake by age", uptake_pregnancy_text_1_plural, startCol = 1, startRow = 36, colNames = FALSE)
writeData(template, "Uptake by age", uptake_pregnancy_text_2_plural, startCol = 1, startRow = 38, colNames = FALSE)


######################## table 6 uptake SIMD #########################

# create list of all relevant months to date
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= vacc_start_date) %>% 
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, simd) %>% 
  select(month, month_start, simd) 

months_denominators <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, month_start, simd, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>%   mutate(simd = case_when(is.na(simd) ~ "Unknown",
                                                       T ~ as.character(simd))) %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & simd == months$simd[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, simd) %>% 
    filter(!is.na(flag))
  
  months_denominators[i,]$number_pregnancies <- number[1,]$n
}

months_denominators <- months_denominators %>% 
  left_join(month_lookup, by = c("month" = "month_vaccine"))

# tidy and add months in words
preg_vacc_data_uptake_3 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%Y-%m")) %>%  
  count(month_vacc, mother_upi, simd) %>% 
  count(month_vacc, simd) %>% 
  rename(number_vaccinated=n) %>% 
  right_join(months_denominators, by = c("month_vacc" = "month", "simd" = "simd")) %>% 
  filter(!is.na(month_vaccine_words))

#calculate total numbers
Table6_total_1 <- pregnancies %>% 
  filter(pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" ) %>% 
  arrange(mother_upi, est_conception_date) %>%
  group_by(mother_upi) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>%
  count(month_vaccine_words, simd) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  rename(number_pregnancies = n)

Table6_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi, simd) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words, simd) %>% 
  rename(number_vaccinated = n)

# build skeleton in case there aren't cases for any combination
simd_skeleton <- Table6_total_1 %>% 
  count(simd) %>% 
  pivot_wider(names_from = simd, values_from = n)
Table6_skeleton <- bind_cols(month_lookup, simd_skeleton) %>% 
  pivot_longer(!c(month_vaccine, month_vaccine_words), names_to = "simd", values_to = "count") %>% 
  select(month_vaccine_words, month_vaccine, simd)

# build final table
Table6_total <- Table6_total_1 %>% 
  left_join(Table6_total_2) %>% 
  bind_rows(preg_vacc_data_uptake_3) %>% 
  mutate(simd = as.character(simd)) %>% 
  full_join(Table6_skeleton, by = c("month_vaccine_words" = "month_vaccine_words", "simd" = "simd",
                                    "month_vacc" = "month_vaccine")) %>% 
  mutate(percen_vacc=(number_vaccinated/number_pregnancies)) %>% 
  bind_rows(Table4_total) %>% 
  arrange(month_vacc, simd) %>% 
  select(month_vaccine_words, simd, number_pregnancies, number_vaccinated, percen_vacc) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Total")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
         
# read out table to rds
Table6_total %>% write_rds(paste0(folder_temp_data, "network_folder/table_6_uptake_by_simd.rds"))
         
# fill out excel template
Table6_pregnant <- Table6_total %>% 
    select(month_vaccine_words, simd, number_pregnancies) %>% 
    pivot_wider(id = simd, names_from = month_vaccine_words, values_from = number_pregnancies) %>% 
    select(-simd)
         
writeData(template, "Uptake by SIMD", Table6_pregnant, startCol = 2, startRow = 6, colNames = TRUE)
         
Table6_vaccinated <- Table6_total %>% 
    select(month_vaccine_words, simd, number_vaccinated) %>% 
    pivot_wider(id = simd, names_from = month_vaccine_words, values_from = number_vaccinated) %>% 
    select(-simd)
         
writeData(template, "Uptake by SIMD", Table6_vaccinated, startCol = 2, startRow = 16, colNames = FALSE)
         
Table6_percentage <- Table6_total %>% 
  select(month_vaccine_words, simd, percen_vacc) %>% 
  pivot_wider(id = simd, names_from = month_vaccine_words, values_from = percen_vacc) %>% 
  select(-simd)
         
writeData(template, "Uptake by SIMD", Table6_percentage, startCol = 2, startRow = 25, colNames = FALSE)

writeData(template, "Uptake by SIMD", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Uptake by SIMD", uptake_pregnancy_text_1_plural, startCol = 1, startRow = 33, colNames = FALSE)
writeData(template, "Uptake by SIMD", uptake_pregnancy_text_2_plural, startCol = 1, startRow = 35, colNames = FALSE)


######################## uptake ethnicity #########################

# create list of all relevant months to date
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= vacc_start_date) %>% 
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, ethnicity_desc_reporting) %>% 
  select(month, month_start, ethnicity_desc_reporting) 

months_denominators <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, month_start, ethnicity_desc_reporting, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>%   mutate(ethnicity_desc_reporting = case_when(is.na(ethnicity_desc_reporting) ~ "Unknown",
                                                       T ~ as.character(ethnicity_desc_reporting))) %>% 
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & ethnicity_desc_reporting == months$ethnicity_desc_reporting[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, ethnicity_desc_reporting) %>% 
    filter(!is.na(flag))
  
  months_denominators[i,]$number_pregnancies <- number[1,]$n
}

months_denominators <- months_denominators %>% 
  left_join(month_lookup, by = c("month" = "month_vaccine"))

# tidy and add months in words
preg_vacc_data_uptake_3 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%Y-%m")) %>%  
  count(month_vacc, mother_upi, ethnicity_desc_reporting) %>% 
  count(month_vacc, ethnicity_desc_reporting) %>% 
  rename(number_vaccinated=n) %>% 
  right_join(months_denominators, by = c("month_vacc" = "month", "ethnicity_desc_reporting" = "ethnicity_desc_reporting")) %>% 
  filter(!is.na(month_vaccine_words))

#calculate total numbers
Table6a_total_1 <- pregnancies %>% 
  filter(pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" ) %>% 
  arrange(mother_upi, est_conception_date) %>%
  group_by(mother_upi) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>%
  count(month_vaccine_words, ethnicity_desc_reporting) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  rename(number_pregnancies = n)

Table6a_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi, ethnicity_desc_reporting) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words, ethnicity_desc_reporting) %>% 
  rename(number_vaccinated = n)

# build skeleton in case there aren't cases for any combination
ethnicity_desc_reporting_skeleton <- Table6a_total_1 %>% 
  count(ethnicity_desc_reporting) %>% 
  pivot_wider(names_from = ethnicity_desc_reporting, values_from = n)
ethnicityskeleton <- bind_cols(month_lookup, ethnicity_desc_reporting_skeleton) %>% 
  pivot_longer(!c(month_vaccine, month_vaccine_words), names_to = "ethnicity_desc_reporting", values_to = "count") %>% 
  select(month_vaccine_words, month_vaccine, ethnicity_desc_reporting)

# build final table
Table6a_total <- Table6a_total_1 %>% 
  left_join(Table6a_total_2) %>% 
  bind_rows(preg_vacc_data_uptake_3) %>% 
  mutate(ethnicity_desc_reporting = as.character(ethnicity_desc_reporting)) %>% 
  full_join(ethnicityskeleton, by = c("month_vaccine_words" = "month_vaccine_words", "ethnicity_desc_reporting" = "ethnicity_desc_reporting",
                                    "month_vacc" = "month_vaccine")) %>% 
  mutate(percen_vacc=(number_vaccinated/number_pregnancies)) %>% 
  bind_rows(Table4_total) %>% 
  arrange(month_vacc, ethnicity_desc_reporting) %>% 
  select(month_vaccine_words, ethnicity_desc_reporting, number_pregnancies, number_vaccinated, percen_vacc) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Total")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# read out table to rds
Table6a_total %>% write_rds(paste0(folder_temp_data, "network_folder/uptake_by_ethnicity_desc_reporting.rds"))

# fill out excel template
Table6a_pregnant <- Table6a_total %>% 
  select(month_vaccine_words, ethnicity_desc_reporting, number_pregnancies) %>% 
  pivot_wider(id = ethnicity_desc_reporting, names_from = month_vaccine_words, values_from = number_pregnancies) %>% 
  select(-ethnicity_desc_reporting)

writeData(template, "Uptake by ethnicity", Table6a_pregnant, startCol = 2, startRow = 6, colNames = TRUE)

Table6a_vaccinated <- Table6a_total %>% 
  select(month_vaccine_words, ethnicity_desc_reporting, number_vaccinated) %>% 
  pivot_wider(id = ethnicity_desc_reporting, names_from = month_vaccine_words, values_from = number_vaccinated) %>% 
  select(-ethnicity_desc_reporting)

writeData(template, "Uptake by ethnicity", Table6a_vaccinated, startCol = 2, startRow = 15, colNames = FALSE)

Table6a_percentage <- Table6a_total %>% 
  select(month_vaccine_words, ethnicity_desc_reporting, percen_vacc) %>% 
  pivot_wider(id = ethnicity_desc_reporting, names_from = month_vaccine_words, values_from = percen_vacc) %>% 
  select(-ethnicity_desc_reporting)

writeData(template, "Uptake by ethnicity", Table6a_percentage, startCol = 2, startRow = 23, colNames = FALSE)

writeData(template, "Uptake by ethnicity", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Uptake by ethnicity", uptake_pregnancy_text_1_plural, startCol = 1, startRow = 30, colNames = FALSE)
writeData(template, "Uptake by ethnicity", uptake_pregnancy_text_2_plural, startCol = 1, startRow = 32, colNames = FALSE)



######################## table 7 uptake HB #########################

# create list of all relevant months to date
months <- pregnancies %>% 
  filter(overall_outcome != "Ongoing" & pregnancy_end_date >= vacc_start_date) %>% 
  mutate(month = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  mutate(month_start = floor_date(pregnancy_end_date, unit = "month")) %>% 
  count(month, month_start, hbres) %>% 
  select(month, month_start, hbres) 

months_denominators <- months %>% 
  mutate(number_pregnancies = 0) %>% 
  select(month, month_start, hbres, number_pregnancies)

# count number pregnant in each month
for(i in 1:nrow(months)){
  number <-  pregnancies %>%  
    mutate(flag = case_when(((pregnancy_end_date >= months$month_start[i] | overall_outcome == "Ongoing")
                             & est_conception_date < months$month_start[i] )
                            & hbres == months$hbres[i] ~ 1,
                            T ~ NA_real_)) %>% 
    count(flag, hbres) %>% 
    filter(!is.na(flag))
  
  months_denominators[i,]$number_pregnancies <- number[1,]$n
}

months_denominators <- months_denominators %>% 
  left_join(month_lookup, by = c("month" = "month_vaccine"))

# tidy and add months in words
preg_vacc_data_uptake_4 <- preg_vacc_data_main %>% 
  mutate(month_vacc = format(vacc_occurence_date, format = "%Y-%m")) %>%  
  count(month_vacc, mother_upi, hbres) %>% 
  count(month_vacc, hbres) %>% 
  rename(number_vaccinated=n) %>% 
  right_join(months_denominators, by = c("month_vacc" = "month", "hbres" = "hbres")) %>% 
  filter(!is.na(month_vaccine_words))

#calculate total numbers
Table7_total_1 <- pregnancies %>% 
  filter(pregnancy_end_date >= vacc_start_date | overall_outcome == "Ongoing" ) %>% 
  arrange(mother_upi, est_conception_date) %>%
  group_by(mother_upi) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>%
  count(month_vaccine_words, hbres) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  rename(number_pregnancies = n)

Table7_total_2 <- preg_vacc_data_main %>% 
  count(mother_upi, hbres) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words))) %>% 
  count(month_vaccine_words, hbres) %>% 
  rename(number_vaccinated = n)

# build skeleton in case there aren't cases for any combination
hbres_skeleton <- Table7_total_1 %>% 
  count(hbres) %>% 
  pivot_wider(names_from = hbres, values_from = n)
Table7_skeleton <- bind_cols(month_lookup, hbres_skeleton) %>% 
  pivot_longer(!c(month_vaccine, month_vaccine_words), names_to = "hbres", values_to = "count") %>% 
  select(month_vaccine_words, month_vaccine, hbres)

# build final table
Table7_total <- Table7_total_1 %>% 
  left_join(Table7_total_2) %>% 
  bind_rows(preg_vacc_data_uptake_4) %>% 
  mutate(hbres = as.character(hbres)) %>% 
  full_join(Table7_skeleton, by = c("month_vaccine_words" = "month_vaccine_words", "hbres" = "hbres",
                                    "month_vacc" = "month_vaccine")) %>% 
  mutate(percen_vacc=(number_vaccinated/number_pregnancies)) %>% 
  bind_rows(Table4_total) %>% 
  arrange(month_vacc, hbres) %>% 
  select(month_vaccine_words, hbres, number_pregnancies, number_vaccinated, percen_vacc) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Y - Total")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# read out table to rds
Table7_total %>% write_rds(paste0(folder_temp_data, "network_folder/table_7_uptake_by_hbres.rds"))

# fill out excel template
Table7_pregnant <- Table7_total %>% 
  select(month_vaccine_words, hbres, number_pregnancies) %>% 
  pivot_wider(id = hbres, names_from = month_vaccine_words, values_from = number_pregnancies) %>% 
  select(-hbres)

writeData(template, "Uptake by NHS Board", Table7_pregnant, startCol = 2, startRow = 6, colNames = TRUE)

Table7_vaccinated <- Table7_total %>% 
  select(month_vaccine_words, hbres, number_vaccinated) %>% 
  pivot_wider(id = hbres, names_from = month_vaccine_words, values_from = number_vaccinated) %>% 
  select(-hbres)

writeData(template, "Uptake by NHS Board", Table7_vaccinated, startCol = 2, startRow = 25, colNames = FALSE)

Table7_percentage <- Table7_total %>% 
  select(month_vaccine_words, hbres, percen_vacc) %>% 
  pivot_wider(id = hbres, names_from = month_vaccine_words, values_from = percen_vacc) %>% 
  select(-hbres)

writeData(template, "Uptake by NHS Board", Table7_percentage, startCol = 2, startRow = 43, colNames = FALSE)
writeData(template, "Uptake by NHS Board", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Uptake by NHS Board", uptake_pregnancy_text_1_plural, startCol = 1, startRow = 60, colNames = FALSE)
writeData(template, "Uptake by NHS Board", uptake_pregnancy_text_2_plural, startCol = 1, startRow = 62, colNames = FALSE)

######################## table 8 overall coverage #########################

vacc_coverage_data_initial <- preg_vacc_data_long %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date >= vacc_occurence_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>% 
  group_by(pregnancy_id) %>% 
  mutate(num_vacc = n()) %>% 
  select(pregnancy_id, month_delivered, num_vacc, maternal_age_group, simd, hbres, ethnicity_desc_reporting) %>% 
  distinct() %>% 
  ungroup()
  
number_at_least_one_vacc <- vacc_coverage_data_initial %>% 
  count(month_delivered) %>% 
  rename(onevacc = n)

onevacc_total <- number_at_least_one_vacc %>% 
  summarise(onevacc = sum(onevacc))

number_two_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 2) %>% 
  count(month_delivered) %>% 
  rename(twovacc = n)

twovacc_total <- number_two_vacc %>% 
  summarise(twovacc = sum(twovacc))

number_three_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 3) %>% 
  count(month_delivered) %>% 
  rename(threevacc = n)

threevacc_total <- number_three_vacc %>% 
  summarise(threevacc = sum(threevacc))

number_deliveries <- pregnancies %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%
  count(month_delivered) %>% 
  rename(deliveries = n)

deliveries_total <- number_deliveries %>% 
  summarise(deliveries = sum(deliveries))

Table8_total <- deliveries_total %>% 
  bind_cols(onevacc_total, twovacc_total, threevacc_total) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))

Table8 <- number_deliveries %>% 
  left_join(number_at_least_one_vacc) %>% 
  left_join(number_two_vacc) %>% 
  left_join(number_three_vacc) %>% 
  left_join(month_lookup, by = c("month_delivered" = "month_vaccine")) %>% 
  bind_rows(Table8_total) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(onevacc_percent = onevacc / deliveries) %>% 
  mutate(twovacc_percent = twovacc / deliveries) %>% 
  mutate(threevacc_percent = threevacc / deliveries) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  select(month_vaccine_words, month_delivered, deliveries, onevacc, onevacc_percent, twovacc, twovacc_percent, threevacc, threevacc_percent)

# read out table to rds
Table8 %>% write_rds(paste0(folder_temp_data, "network_folder/table_8_total_coverage.rds"))

Table8_excel <- Table8 %>% 
  select(-month_delivered) %>% 
  pivot_longer(!month_vaccine_words, names_to = "indicator", values_to = "value") %>% 
  pivot_wider(id = indicator, names_from = month_vaccine_words, values_from = value) %>% 
  select(-indicator)

writeData(template, "Coverage by delivery", Table8_excel, startCol = 2, startRow = 5, colNames = TRUE)
writeData(template, "Coverage by delivery", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)


########################table 9 coverage - Age Breakdown #########################

number_at_least_one_vacc <- vacc_coverage_data_initial %>% 
  count(month_delivered, maternal_age_group) %>% 
  rename(onevacc = n)

onevacc_total_by_age <- number_at_least_one_vacc %>% 
  group_by(maternal_age_group) %>% 
  summarise(onevacc = sum(onevacc))

number_two_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 2) %>% 
  count(month_delivered, maternal_age_group) %>% 
  rename(twovacc = n)

twovacc_total_by_age <- number_two_vacc %>% 
  group_by(maternal_age_group) %>% 
  summarise(twovacc = sum(twovacc))

number_three_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 3) %>% 
  count(month_delivered, maternal_age_group) %>% 
  rename(threevacc = n)

threevacc_total_by_age <- number_three_vacc %>% 
  group_by(maternal_age_group) %>% 
  summarise(threevacc = sum(threevacc))

number_deliveries <- pregnancies %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%
  count(month_delivered, maternal_age_group) %>% 
  rename(deliveries = n)

deliveries_total_by_age <- number_deliveries %>% 
  group_by(maternal_age_group) %>% 
  summarise(deliveries = sum(deliveries))

Table9_total_by_age <- deliveries_total_by_age %>% 
  left_join(onevacc_total_by_age) %>% 
  left_join(twovacc_total_by_age) %>% 
  left_join(threevacc_total_by_age) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))

Table9 <- Table5_skeleton %>% 
  left_join(number_deliveries, by = c("month_vaccine" = "month_delivered", "maternal_age_group" = "maternal_age_group")) %>% 
  left_join(number_at_least_one_vacc, by = c("month_vaccine" = "month_delivered", "maternal_age_group" = "maternal_age_group")) %>% 
  left_join(number_two_vacc, by = c("month_vaccine" = "month_delivered", "maternal_age_group" = "maternal_age_group")) %>% 
  left_join(number_three_vacc, by = c("month_vaccine" = "month_delivered", "maternal_age_group" = "maternal_age_group")) %>% 
  bind_rows(Table8) %>% 
  mutate(maternal_age_group = if_else(is.na(maternal_age_group), "9 Total", maternal_age_group)) %>% 
  bind_rows(Table9_total_by_age) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(onevacc_percent = onevacc / deliveries) %>% 
  mutate(twovacc_percent = twovacc / deliveries) %>% 
  mutate(threevacc_percent = threevacc / deliveries) %>% 
  mutate(month_delivered = if_else(is.na(month_delivered), month_vaccine, month_delivered)) %>% 
  arrange(month_delivered, maternal_age_group) %>% 
  select(month_vaccine_words, month_delivered, maternal_age_group, deliveries, onevacc, onevacc_percent, twovacc, twovacc_percent, threevacc, threevacc_percent) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# read out table to rds
Table9 %>% write_rds(paste0(folder_temp_data, "network_folder/table_9_coverage_by_age.rds"))

# read out to Excel: split into separate tables

Table9_deliveries <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, deliveries) 
writeData(template, "Coverage by age", Table9_deliveries, startCol = 6, startRow = 6, colNames = TRUE)

Table9_onevacc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, onevacc) 
writeData(template, "Coverage by age", Table9_onevacc, startCol = 6, startRow = 17, colNames = FALSE)

Table9_onevacc_perc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, onevacc_percent)
writeData(template, "Coverage by age", Table9_onevacc_perc, startCol = 6, startRow = 27, colNames = FALSE)

Table9_twovacc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, twovacc)
writeData(template, "Coverage by age", Table9_twovacc, startCol = 6, startRow = 37, colNames = FALSE)

Table9_twovacc_perc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, twovacc_percent)
writeData(template, "Coverage by age", Table9_twovacc_perc, startCol = 6, startRow = 47, colNames = FALSE)

Table9_threevacc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, threevacc)
writeData(template, "Coverage by age", Table9_threevacc, startCol = 6, startRow = 57, colNames = FALSE)

Table9_threevacc_perc <- coverage_split_table(Table9, month_vaccine_words, maternal_age_group, threevacc_percent)
writeData(template, "Coverage by age", Table9_threevacc_perc, startCol = 6, startRow = 67, colNames = FALSE)

writeData(template, "Coverage by age", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)

coverage_age_text_1 <- paste0("*The totals for these rows include all deliveries occurring in Dec 2020 to ", format(publication_latest_vacc_date, "%B %Y"), " inclusive")
writeData(template, "Coverage by age", coverage_age_text_1, startCol = 1, startRow = 81, colNames = FALSE)

########################table 10 coverage - SIMD Breakdown #########################

number_at_least_one_vacc <- vacc_coverage_data_initial %>% 
  count(month_delivered, simd) %>% 
  rename(onevacc = n)

onevacc_total_by_SIMD <- number_at_least_one_vacc %>% 
  group_by(simd) %>% 
  summarise(onevacc = sum(onevacc))

number_two_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 2) %>% 
  count(month_delivered, simd) %>% 
  rename(twovacc = n)

twovacc_total_by_SIMD <- number_two_vacc %>% 
  group_by(simd) %>% 
  summarise(twovacc = sum(twovacc))

number_three_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 3) %>% 
  count(month_delivered, simd) %>% 
  rename(threevacc = n)

threevacc_total_by_SIMD <- number_three_vacc %>% 
  group_by(simd) %>% 
  summarise(threevacc = sum(threevacc))

number_deliveries <- pregnancies %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  count(month_delivered, simd) %>% 
  rename(deliveries = n)

deliveries_total_by_SIMD <- number_deliveries %>% 
  group_by(simd) %>% 
  summarise(deliveries = sum(deliveries))

Table10_total_by_SIMD <- deliveries_total_by_SIMD %>% 
  left_join(onevacc_total_by_SIMD) %>% 
  left_join(twovacc_total_by_SIMD) %>% 
  left_join(threevacc_total_by_SIMD) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))

Table10 <- number_deliveries %>% 
  left_join(number_at_least_one_vacc, by = c("month_delivered" = "month_delivered", "simd" = "simd")) %>% 
  left_join(number_two_vacc, by = c("month_delivered" = "month_delivered", "simd" = "simd")) %>% 
  left_join(number_three_vacc, by = c("month_delivered" = "month_delivered", "simd" = "simd")) %>% 
  left_join(month_lookup, by = c("month_delivered" = "month_vaccine")) %>% 
  bind_rows(Table8) %>% 
  mutate(simd = if_else(is.na(simd), "X Total", simd)) %>% 
  bind_rows(Table10_total_by_SIMD) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(onevacc_percent = onevacc / deliveries) %>% 
  mutate(twovacc_percent = twovacc / deliveries) %>% 
  mutate(threevacc_percent = threevacc / deliveries) %>% 
  arrange(month_delivered, simd) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  select(month_vaccine_words, month_delivered, simd, deliveries, onevacc, onevacc_percent, twovacc, twovacc_percent, threevacc, threevacc_percent)

# read out table to rds
Table10 %>% write_rds(paste0(folder_temp_data, "network_folder/table_10_coverage_by_SIMD.rds"))

# read out to Excel: split into separate tables
Table10_deliveries <- coverage_split_table(Table10, month_vaccine_words, simd, deliveries) 
writeData(template, "Coverage by SIMD", Table10_deliveries, startCol = 6, startRow = 6, colNames = TRUE)

Table10_onevacc <- coverage_split_table(Table10, month_vaccine_words, simd, onevacc) 
writeData(template, "Coverage by SIMD", Table10_onevacc, startCol = 6, startRow = 16, colNames = FALSE)

Table10_onevacc_perc <- coverage_split_table(Table10, month_vaccine_words, simd, onevacc_percent)
writeData(template, "Coverage by SIMD", Table10_onevacc_perc, startCol = 6, startRow = 25, colNames = FALSE)

Table10_twovacc <- coverage_split_table(Table10, month_vaccine_words, simd, twovacc)
writeData(template, "Coverage by SIMD", Table10_twovacc, startCol = 6, startRow = 34, colNames = FALSE)

Table10_twovacc_perc <- coverage_split_table(Table10, month_vaccine_words, simd, twovacc_percent)
writeData(template, "Coverage by SIMD", Table10_twovacc_perc, startCol = 6, startRow = 43, colNames = FALSE)

Table10_threevacc <- coverage_split_table(Table10, month_vaccine_words, simd, threevacc)
writeData(template, "Coverage by SIMD", Table10_threevacc, startCol = 6, startRow = 52, colNames = FALSE)

Table10_threevacc_perc <- coverage_split_table(Table10, month_vaccine_words, simd, threevacc_percent)
writeData(template, "Coverage by SIMD", Table10_threevacc_perc, startCol = 6, startRow = 61, colNames = FALSE)

writeData(template, "Coverage by SIMD", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Coverage by SIMD", coverage_age_text_1, startCol = 1, startRow = 75, colNames = FALSE)


######################## coverage - Ethnicity Breakdown #########################

number_at_least_one_vacc <- vacc_coverage_data_initial %>% 
  count(month_delivered, ethnicity_desc_reporting) %>% 
  rename(onevacc = n)

onevacc_total_by_ethnicity_desc_reporting <- number_at_least_one_vacc %>% 
  group_by(ethnicity_desc_reporting) %>% 
  summarise(onevacc = sum(onevacc))

number_two_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 2) %>% 
  count(month_delivered, ethnicity_desc_reporting) %>% 
  rename(twovacc = n)

twovacc_total_by_ethnicity_desc_reporting <- number_two_vacc %>% 
  group_by(ethnicity_desc_reporting) %>% 
  summarise(twovacc = sum(twovacc))

number_three_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 3) %>% 
  count(month_delivered, ethnicity_desc_reporting) %>% 
  rename(threevacc = n)

threevacc_total_by_ethnicity_desc_reporting <- number_three_vacc %>% 
  group_by(ethnicity_desc_reporting) %>% 
  summarise(threevacc = sum(threevacc))

number_deliveries <- pregnancies %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  count(month_delivered, ethnicity_desc_reporting) %>% 
  rename(deliveries = n)

deliveries_total_by_ethnicity_desc_reporting <- number_deliveries %>% 
  group_by(ethnicity_desc_reporting) %>% 
  summarise(deliveries = sum(deliveries))

Table10_total_by_ethnicity_desc_reporting <- deliveries_total_by_ethnicity_desc_reporting %>% 
  left_join(onevacc_total_by_ethnicity_desc_reporting) %>% 
  left_join(twovacc_total_by_ethnicity_desc_reporting) %>% 
  left_join(threevacc_total_by_ethnicity_desc_reporting) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))

Table10 <- number_deliveries %>% 
  left_join(number_at_least_one_vacc, by = c("month_delivered" = "month_delivered", "ethnicity_desc_reporting" = "ethnicity_desc_reporting")) %>% 
  left_join(number_two_vacc, by = c("month_delivered" = "month_delivered", "ethnicity_desc_reporting" = "ethnicity_desc_reporting")) %>% 
  left_join(number_three_vacc, by = c("month_delivered" = "month_delivered", "ethnicity_desc_reporting" = "ethnicity_desc_reporting")) %>% 
  left_join(month_lookup, by = c("month_delivered" = "month_vaccine")) %>% 
  bind_rows(Table8) %>% 
  mutate(ethnicity_desc_reporting = if_else(is.na(ethnicity_desc_reporting), "X Total", ethnicity_desc_reporting)) %>% 
  bind_rows(Table10_total_by_ethnicity_desc_reporting) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(onevacc_percent = onevacc / deliveries) %>% 
  mutate(twovacc_percent = twovacc / deliveries) %>% 
  mutate(threevacc_percent = threevacc / deliveries) %>% 
  arrange(month_delivered, ethnicity_desc_reporting) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  select(month_vaccine_words, month_delivered, ethnicity_desc_reporting, deliveries, onevacc, onevacc_percent, twovacc, twovacc_percent, threevacc, threevacc_percent)

# read out table to rds
Table10 %>% write_rds(paste0(folder_temp_data, "network_folder/coverage_by_ethnicity.rds"))

# read out to Excel: split into separate tables
Table10_deliveries <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, deliveries) 
writeData(template, "Coverage by ethnicity", Table10_deliveries, startCol = 6, startRow = 6, colNames = TRUE)

Table10_onevacc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, onevacc) 
writeData(template, "Coverage by ethnicity", Table10_onevacc, startCol = 6, startRow = 15, colNames = FALSE)

Table10_onevacc_perc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, onevacc_percent)
writeData(template, "Coverage by ethnicity", Table10_onevacc_perc, startCol = 6, startRow = 23, colNames = FALSE)

Table10_twovacc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, twovacc)
writeData(template, "Coverage by ethnicity", Table10_twovacc, startCol = 6, startRow = 31, colNames = FALSE)

Table10_twovacc_perc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, twovacc_percent)
writeData(template, "Coverage by ethnicity", Table10_twovacc_perc, startCol = 6, startRow = 39, colNames = FALSE)

Table10_threevacc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, threevacc)
writeData(template, "Coverage by ethnicity", Table10_threevacc, startCol = 6, startRow = 47, colNames = FALSE)

Table10_threevacc_perc <- coverage_split_table(Table10, month_vaccine_words, ethnicity_desc_reporting, threevacc_percent)
writeData(template, "Coverage by ethnicity", Table10_threevacc_perc, startCol = 6, startRow = 55, colNames = FALSE)

writeData(template, "Coverage by ethnicity", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Coverage by ethnicity", coverage_age_text_1, startCol = 1, startRow = 67, colNames = FALSE)


########################table 11 coverage - HB Breakdown #########################

number_at_least_one_vacc <- vacc_coverage_data_initial %>% 
  count(month_delivered, hbres) %>% 
  rename(onevacc = n)

onevacc_total_by_hbres <- number_at_least_one_vacc %>% 
  group_by(hbres) %>% 
  summarise(onevacc = sum(onevacc))

number_two_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 2) %>% 
  count(month_delivered, hbres) %>% 
  rename(twovacc = n)

twovacc_total_by_hbres <- number_two_vacc %>% 
  group_by(hbres) %>% 
  summarise(twovacc = sum(twovacc))

number_three_vacc <- vacc_coverage_data_initial %>% 
  filter(num_vacc >= 3) %>% 
  count(month_delivered, hbres) %>% 
  rename(threevacc = n)

threevacc_total_by_hbres <- number_three_vacc %>% 
  group_by(hbres) %>% 
  summarise(threevacc = sum(threevacc))

number_deliveries <- pregnancies %>% 
  filter(overall_outcome == "Stillbirth" | overall_outcome == "Live birth") %>% 
  mutate(month_delivered = format(as.Date(pregnancy_end_date), "%Y-%m")) %>% 
  filter(pregnancy_end_date >= vacc_start_date) %>% 
  filter(pregnancy_end_date <= publication_latest_vacc_date ) %>%  
  count(month_delivered, hbres) %>% 
  rename(deliveries = n)

deliveries_total_by_hbres <- number_deliveries %>% 
  group_by(hbres) %>% 
  summarise(deliveries = sum(deliveries))

Table11_total_by_hbres <- deliveries_total_by_hbres %>% 
  left_join(onevacc_total_by_hbres) %>% 
  left_join(twovacc_total_by_hbres) %>% 
  left_join(threevacc_total_by_hbres) %>% 
  mutate(month_vaccine_words = paste0("Total - ", first(month_lookup$month_vaccine_words), " - ", last(month_lookup$month_vaccine_words)))

Table11 <- number_deliveries %>% 
  left_join(number_at_least_one_vacc, by = c("month_delivered" = "month_delivered", "hbres" = "hbres")) %>% 
  left_join(number_two_vacc, by = c("month_delivered" = "month_delivered", "hbres" = "hbres")) %>% 
  left_join(number_three_vacc, by = c("month_delivered" = "month_delivered", "hbres" = "hbres")) %>% 
  left_join(month_lookup, by = c("month_delivered" = "month_vaccine")) %>% 
  bind_rows(Table8) %>% 
  mutate(hbres = if_else(is.na(hbres), "Y - Total", hbres)) %>% 
  bind_rows(Table11_total_by_hbres) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(onevacc_percent = onevacc / deliveries) %>% 
  mutate(twovacc_percent = twovacc / deliveries) %>% 
  mutate(threevacc_percent = threevacc / deliveries) %>% 
  arrange(month_delivered, hbres) %>% 
  filter(!is.na(month_vaccine_words)) %>% 
  select(month_vaccine_words, month_delivered, hbres, deliveries, onevacc, onevacc_percent, twovacc, twovacc_percent, threevacc, threevacc_percent)

# read out table to rds
Table11 %>% write_rds(paste0(folder_temp_data, "network_folder/table_11_coverage_by_hbres.rds"))

# read out to Excel: split into 5 tables
Table11_deliveries <- coverage_split_table(Table11, month_vaccine_words, hbres, deliveries) 
writeData(template, "Coverage by NHS Board", Table11_deliveries, startCol = 6, startRow = 6, colNames = TRUE)

Table11_onevacc <- coverage_split_table(Table11, month_vaccine_words, hbres, onevacc)
writeData(template, "Coverage by NHS Board", Table11_onevacc, startCol = 6, startRow = 25, colNames = FALSE)

Table11_onevacc_perc <- coverage_split_table(Table11, month_vaccine_words, hbres, onevacc_percent)
writeData(template, "Coverage by NHS Board", Table11_onevacc_perc, startCol = 6, startRow = 43, colNames = FALSE)

Table11_twovacc <- coverage_split_table(Table11, month_vaccine_words, hbres, twovacc)
writeData(template, "Coverage by NHS Board", Table11_twovacc, startCol = 6, startRow = 61, colNames = FALSE)

Table11_twovacc_perc <- coverage_split_table(Table11, month_vaccine_words, hbres, twovacc_percent)
writeData(template, "Coverage by NHS Board", Table11_twovacc_perc, startCol = 6, startRow = 79, colNames = FALSE)

Table11_threevacc <- coverage_split_table(Table11, month_vaccine_words, hbres, threevacc)
writeData(template, "Coverage by NHS Board", Table11_threevacc, startCol = 6, startRow = 97, colNames = FALSE)

Table11_threevacc_perc <- coverage_split_table(Table11, month_vaccine_words, hbres, threevacc_percent)
writeData(template, "Coverage by NHS Board", Table11_threevacc_perc, startCol = 6, startRow = 115, colNames = FALSE)

writeData(template, "Coverage by NHS Board", n_vacs_text_1, startCol = 1, startRow = 2, colNames = FALSE)
writeData(template, "Coverage by NHS Board", coverage_age_text_1, startCol = 1, startRow = 137, colNames = FALSE)


#### Severe outcomes rates ####

# READ IN ####
all_numerators <- readRDS(paste0(folder_temp_data, "network_folder/vaccine_severe_outcomes_totals.rds")) %>% 
  select(indicator, Total)

live_births <- readRDS(paste0(folder_temp_data, "network_folder/all_live_births_after_vaccination.rds")) %>% 
  select(indicator, Total)

live_births_28_days <- live_births$Total[live_births$indicator == "live birth within 28 days of vaccine"]

live_births_after_vaccine <- live_births$Total[live_births$indicator == "subsequent live birth"]

all_births_within_28_days <- live_births$Total[live_births$indicator == "live birth within 28 days of vaccine"] +
  all_numerators$Total[all_numerators$indicator == "associated stillbirths"]

all_births_after_vaccine <- live_births$Total[live_births$indicator == "subsequent live birth"] +
  all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"]


## Stillbirth rate (per 1,000 total births) for babies born within 28 days of maternal vaccination
stillbirth_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated stillbirths"], 
                                    denominator = all_births_within_28_days,
                                    rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## Stillbirth rate (per 1,000 total births) for all babies born following maternal vaccination
stillbirth_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"], 
                                 denominator = all_births_after_vaccine,
                                 rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))


## Neonatal mortality rate (per 1,000 live births) for babies born within 28 days of maternal vaccination
neonatal_mortality_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated neonatal death"], 
                                            denominator = live_births_28_days,
                                            rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## Neonatal mortality rate (per 1,000 live births) for all babies born following maternal vaccination
neonatal_mortality_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent neonatal death"], 
                                         denominator = live_births_after_vaccine,
                                         rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## extended perinatal mortality rate (per 1,000 total births) for babies born within 28 days of maternal vaccination
extended_perinatal_mortality_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated neonatal death"] +
                                                        all_numerators$Total[all_numerators$indicator == "associated stillbirths"], 
                                                      denominator = all_births_within_28_days,
                                                      rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## extended perinatal mortality rate (per 1,000 total births) for all babies born following maternal vaccination
extended_perinatal_mortality_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent neonatal death"] +
                                                     all_numerators$Total[all_numerators$indicator == "subsequent stillbirths"], 
                                                   denominator = all_births_after_vaccine,
                                                   rate = 1e3*numerator/denominator) %>% 
  mutate(ci_lower = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e3*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## preterm rate (per 100 live births) for babies born within 28 days of maternal vaccination
preterm_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated preterm births"], 
                                 denominator = live_births_28_days,
                                 rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## preterm rate (per 100 live births) for all babies born following maternal vaccination
preterm_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent preterm births"], 
                              denominator = live_births_after_vaccine,
                              rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very preterm rate (per 100 live births) for babies born within 28 days of maternal vaccination
very_preterm_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated very preterm births"], 
                                      denominator = live_births_28_days,
                                      rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very preterm rate (per 100 live births) for all babies born following maternal vaccination
very_preterm_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent very preterm births"], 
                                   denominator = live_births_after_vaccine,
                                   rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## low apgar rate (per 100 live births- preterm births) for babies born within 28 days of maternal vaccination
low_apgar_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated low apgar score"], 
                                   denominator = live_births_28_days - all_numerators$Total[all_numerators$indicator == "associated preterm births"],
                                   rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## low apgar rate (per 100 live births - preterm births) for all babies born following maternal vaccination
low_apgar_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent low apgar score"], 
                                denominator = live_births_after_vaccine  - all_numerators$Total[all_numerators$indicator == "subsequent preterm births"],
                                rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very low apgar rate (per 100 live births - preterm births) for babies born within 28 days of maternal vaccination
very_low_apgar_within_28_days <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "associated very low apgar score"], 
                                        denominator = live_births_28_days - all_numerators$Total[all_numerators$indicator == "associated preterm births"],
                                        rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

## very low apgar rate (per 100 live births - preterm births) for all babies born following maternal vaccination
very_low_apgar_after_vaccine <- tibble(numerator = all_numerators$Total[all_numerators$indicator == "subsequent very low apgar score"], 
                                     denominator = live_births_after_vaccine  - all_numerators$Total[all_numerators$indicator == "subsequent preterm births"],
                                     rate = 1e2*numerator/denominator) %>% 
  mutate(ci_lower = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "lower"),
         ci_upper = 1e2*conf_int_wilson2(numerator, denominator, z = Z_95, type = "upper"))

### Read out rates into Excel

writeData(template, "Severe outcomes tot NFP", all_births_within_28_days, startCol = 2, startRow = 41, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", all_births_after_vaccine, startCol = 2, startRow = 42, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", stillbirth_within_28_days, startCol = 4, startRow = 44, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", stillbirth_after_vaccine, startCol = 4, startRow = 45, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", neonatal_mortality_within_28_days, startCol = 4, startRow = 47, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", neonatal_mortality_after_vaccine, startCol = 4, startRow = 48, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", extended_perinatal_mortality_within_28_days, startCol = 4, startRow = 50, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", extended_perinatal_mortality_after_vaccine, startCol = 4, startRow = 51, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", preterm_within_28_days, startCol = 4, startRow = 53, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", preterm_after_vaccine, startCol = 4, startRow = 54, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", very_preterm_within_28_days, startCol = 4, startRow = 56, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", very_preterm_after_vaccine, startCol = 4, startRow = 57, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", low_apgar_within_28_days, startCol = 4, startRow = 59, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", low_apgar_after_vaccine, startCol = 4, startRow = 60, colNames = FALSE)

writeData(template, "Severe outcomes tot NFP", very_low_apgar_within_28_days, startCol = 4, startRow = 62, colNames = FALSE)
writeData(template, "Severe outcomes tot NFP", very_low_apgar_after_vaccine, startCol = 4, startRow = 63, colNames = FALSE)


#### Save out workbook ####
saveWorkbook(template, (paste0(folder_outputs, "network_folder/Vaccine_output_", Sys.Date(), ".xlsx")), overwrite =TRUE)
