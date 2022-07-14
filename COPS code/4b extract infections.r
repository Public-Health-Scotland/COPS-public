
########## read in infections data. 

#### Connect to DVPROD ####
con <- dbConnect(odbc()### DATABASE CONNECTION DETAILS REMOVED FOR PUBLIC RELEASE

tic("extracting covid tests")
tests_pcr <- dbGetQuery(con,
                        "select distinct
                        subject_upi,
                        specimen_id,
                        test_result,
                        subject_date_of_birth,
                        date_ecoss_specimen,
                        flag_covid_symptomatic,
                        test_reason,
                        test_type,
                        test_ch1_result_ct_value, 
                        test_ch2_result_ct_value, 
                        test_ch3_result_ct_value, 
                        test_ch4_result_ct_value, 
                        test_ch1_target_gene, 
                        test_ch2_target_gene, 
                        test_ch3_target_gene, 
                        test_ch4_target_gene, 
                        test_ch1_target_gene_result, 
                        test_ch2_target_gene_result, 
                        test_ch3_target_gene_result, 
                        test_ch4_target_gene_result
                        from
                        covid_testing.lighthouse_and_ecoss_covid_testing
                        where
                        (test_type = 'PCR') AND
                        (test_result = 'POSITIVE' OR
                        (test_result = 'NEGATIVE' AND date_ecoss_specimen >= '2022-01-06')) AND
                        flag_test_result_transcribed = 0 AND
                        flag_test_result_denotified = 0") %>%
  distinct() 

tests_pcr <-tests_pcr %>%
  mutate(date_ecoss_specimen = as.Date(date_ecoss_specimen)) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi))) %>%
  mutate(test_type = "PCR")

tests_lfd <- dbGetQuery(
  con,
  "select
  subject_upi,
  specimen_id,
  date_specimen,
  test_result,
  subject_date_of_birth,
  test_reason
  from 
  covid_testing.covid_testing_antigen_results
  where 
  test_result = 'POSITIVE'
  and date_specimen >= '2022-01-06' ") %>%
  distinct()
tests_lfd <- tests_lfd %>% 
  mutate(date_ecoss_specimen = as.Date(date_specimen)) %>%
  select(-date_specimen) %>%
  rename(upi = subject_upi) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(upi = chi_pad(as.character(upi))) %>% 
  mutate(upi_check = chi_check(as.character(upi)))%>%
  mutate(test_type = "LFD")

tests_wgs <- dbGetQuery(
  con, 
  "select
  specimen_id , wgs_sequence_id, variant_of_interest, variant_calling, 
  moi_list , genomic_lineage, date_test_received_in_seer
  from 
  covid_testing.covid_testing_wgs_results")  %>%
  distinct()
toc()


tests <- tests_pcr %>%
  bind_rows(tests_lfd)


#### chi completeness check ####
chi_completeness_lfd_pos <- tests_lfd %>% 
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>% 
  count(upi_check) %>% 
  pivot_wider(names_from = upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate(percentage = round(valid_chi / (valid_chi + missing) * 100, digits = 1))

chi_completeness_pcr_pos <- tests_pcr %>% 
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>% 
  filter(test_result == "POSITIVE") %>% 
  count(upi_check) %>% 
  pivot_wider(names_from = upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate(percentage = round(valid_chi / (valid_chi + missing) * 100, digits = 1))

chi_completeness_check <- tests %>% 
  filter(date_ecoss_specimen <= Sys.Date()) %>% 
  mutate(week_ending = ceiling_date(date_ecoss_specimen, unit = "week", change_on_boundary = F)) %>% 
  count(week_ending, upi_check) %>% 
  pivot_wider(names_from = upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(percentage = round(missing / (valid_chi + missing) * 100, digits = 2))

overall_chi_completeness <- tests %>% 
  count(upi_check) %>% 
  pivot_wider(names_from = upi_check, values_from = n) %>% 
  clean_names() %>% 
  mutate(percentage = round(valid_chi / (valid_chi + missing) * 100, digits = 1))

overall_chi_completeness_perc <- overall_chi_completeness$percentage[1]
chi_completeness_lfd_pos_perc <- chi_completeness_lfd_pos$percentage[1]
chi_completeness_pcr_pos_perc <- chi_completeness_pcr_pos$percentage[1]
saveRDS(overall_chi_completeness_perc, paste0(folder_temp_data, "overall_testing_chi_completeness_perc.rds"))
saveRDS(chi_completeness_lfd_pos_perc, paste0(folder_temp_data, "lfd_pos_chi_completeness_perc.rds"))
saveRDS(chi_completeness_pcr_pos_perc, paste0(folder_temp_data, "pcr_pos_chi_completeness_perc.rds"))

theme_set(theme_classic())
chi_completeness_check %>% 
  ggplot(aes(x=week_ending, y=percentage)) +
  geom_line(aes()) +
  xlab("Week ending") +
  ylab("Percentage testing records missing CHI")

template <- loadWorkbook(paste0(folder_templates, "Infection CHI check.xlsx"))
writeData(template, "Infection CHI completeness", chi_completeness_check, colNames = FALSE, startCol = 1, startRow = 2)
insertPlot(template, "Infection CHI completeness", width = 6, startCol = 6, startRow = 2,
           height = 3.5, fileType = "png", units = "in")
saveWorkbook(template, (paste0(folder_outputs, "network_folder/Infections_CHI_check_", Sys.Date(), ".xlsx")), overwrite =TRUE)


#### Number of neonates testing positive using only testing data ####
neonate_tests <- tests %>%
  mutate(age_in_days = difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")) %>% 
  filter(age_in_days <= 29) %>% 
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>%
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything()) %>%
  filter(test_result == "POSITIVE")%>% 
  mutate(age_in_days = difftime(date_ecoss_specimen, subject_date_of_birth, units = "days")) %>% 
  filter(age_in_days <= 27) %>% 
  group_by(upi) %>%
  slice(1)

number_positive_neonates <- nrow(neonate_tests)
saveRDS(number_positive_neonates, paste0(folder_temp_data, "number_neonatal_infections_testing_only.rds"))

#### select only tests for members of our cohort ####

print(paste0("There were ", nrow(tests), " tests extracted from the database"))

all_upis <- read_csv(paste0(folder_cohorts, "all_upis.csv"))

tests <- tests %>%
  inner_join(all_upis, by = "upi")

print(paste0(nrow(tests), " of these tests match a UPI in our COPS cohort"))

#### select only wgs records that match these tests ####

tests_wgs <- tests_wgs %>% left_join(tests) %>% filter(!is.na(upi)) %>%
  select(upi, specimen_id, wgs_sequence_id, variant_of_interest, variant_calling, genomic_lineage)
print(paste0(nrow(tests_wgs), " of tests matched to cohort have a WGS record"))

write_rds(tests_wgs, paste0(folder_temp_data, "wgs_tests.rds"), compress = "gz")
tests_wgs <- readRDS(paste0(folder_temp_data, "wgs_tests.rds"))
#rm(tests_wgs)

#### Cut the data down to one test per day and apply the rule to flip positive LFDs to negative if followed by a negative PCR within 2 days ####
# Favour PCR tests over LFDs, and favour positive tests over negative or indeterminate
tic("applying Negative-PCR-after-Positive-LFD rule")

tests <- tests %>%
  select(-subject_date_of_birth) %>% 
  mutate(original_test_class = paste0(test_result, "_", test_type)) %>%
  select(upi, date_ecoss_specimen, original_test_class, test_type, test_result, everything()) %>%
  arrange(upi, date_ecoss_specimen) %>%
  mutate(test_preference = case_when(original_test_class == "POSITIVE_PCR" ~ 1,
                                     original_test_class == "NEGATIVE_PCR" ~ 2,
                                     original_test_class == "POSITIVE_LFD" ~ 3,
                                     original_test_class == "NEGATIVE_LFD" ~ 4,
                                     T ~ 5)) %>%
  group_by(upi, date_ecoss_specimen) %>%
  arrange(test_preference) %>%
  slice(1) %>%
  select(-test_preference) %>%
  ungroup() %>%
  group_by(upi) %>%
  mutate(one_test_later = paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later")) %>%
  mutate(two_tests_later = paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later")) %>% 
  ungroup() %>%
  mutate(flip_positive_lfd_to_negative = case_when(original_test_class == "POSITIVE_LFD" & one_test_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   original_test_class == "POSITIVE_LFD" & two_tests_later %in% c("NEGATIVE_PCR_1_days_later", "NEGATIVE_PCR_2_days_later") ~ T,
                                                   T ~ F) ) %>%
  mutate(test_result = case_when(flip_positive_lfd_to_negative == T ~ "NEGATIVE",
                                 T ~ test_result)) %>%
  select(upi, date_ecoss_specimen, test_type, test_result, original_test_class, everything())

toc()


#### make sure we only pick positive tests that are attached to a valid chi ####
tests %<>%
  filter(upi_check == "Valid CHI") %>% 
  select(-upi_check) %>%
  filter(date_ecoss_specimen <= publication_latest_vacc_date) %>%
  filter(test_result == "POSITIVE")


#### Apply PHS Positive reporting method ###
# This requires that any positive tests within 90 days of a person's first positive test will be discarded.
# After 90 days a person may have another positive test, but further positive tests within 90 days of that test will be discarded.
# This ensures that we pick up one positive result per infection, rather than multiple positive tests referring to the same COVID infection.
# The method used here is identical to that used to group SMR01, SMR02 etc. records for COPS.
tests <- tests %>%
  moving_index_deduplication(., upi, date_ecoss_specimen, 90) %>%
  rename(covid_infection = cops_event)
tests_grouped <- tests %>%  
  arrange(upi, covid_infection, date_ecoss_specimen) %>%
  group_by(upi, covid_infection) %>%
  slice(1) %>%
  ungroup() %>%
  select(upi, covid_infection, everything())


#### Determins symptomatic status ####
#augment sympton flag with test_reason
#further development - include details from PCR following an LFD
#do for all tests to allow linking of non-index cases to an index case up to 2 days before
test_symptoms <- tests %>% 
  mutate(x_symptomatic = ifelse(!is.na(flag_covid_symptomatic) & flag_covid_symptomatic=="true", "true", 
                                ifelse(!is.na(test_reason) & (str_detect(test_reason, "symptomatic")|str_detect(test_reason, "symptom")), "true", flag_covid_symptomatic)))

#table(test_symptoms$x_symptomatic, test_symptoms$flag_covid_symptomatic, useNA="always")

#### Determine S-gene Positivity  ####
#further development - include details from PCR following an LFD
tests_sgene <- tests %>%
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
  pivot_wider(c(upi,specimen_id,  gene_test_id), names_from = type, values_from = value) %>% 
  filter(is.na(gene_type) == FALSE) %>% 
  pivot_wider(c(upi, specimen_id), names_from = c(gene_type), values_from = c(ct_value, gene_result)) %>%
  mutate(sgene_classification = case_when(gene_result_SGENE == "NEGATIVE" & (ct_value_ORF1AB <= 30 | ct_value_NGENE <= 30) ~ "True S Gene Dropout",
                                          gene_result_SGENE == "NEGATIVE" & (is.na(ct_value_ORF1AB) & is.na(ct_value_NGENE)) ~ "No Ct Values",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_ORF1AB > 30 & ct_value_NGENE > 30 ~ "Weak Positive",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_ORF1AB > 30 & is.na(ct_value_NGENE) ~ "Weak Positive",
                                          gene_result_SGENE == "NEGATIVE" & ct_value_NGENE > 30 & is.na(ct_value_ORF1AB) ~ "Weak Positive",
                                          gene_result_SGENE == "POSITIVE" ~ "Positive S Gene",
                                          TRUE ~ "other")) %>%
  select(upi, specimen_id, sgene_classification)

tests_long <- tests %>%
  select(upi, specimen_id,  test_result, date_ecoss_specimen, test_type, flag_covid_symptomatic, covid_infection) %>%
  left_join(tests_sgene, by=c("upi", "specimen_id"))
#create tests detail file.
tests_detail <- left_join(tests_long, test_symptoms)
tests_detail <- left_join(tests_detail , tests_wgs) %>%
                      select(upi, specimen_id, date_ecoss_specimen, test_type, sgene_classification,
                            original_test_class ,x_symptomatic,wgs_sequence_id,variant_of_interest,variant_calling,genomic_lineage)



#Identify all tests within 2 days of index tests ####
#list of index IDs
index_cases <- tests_grouped %>% select(upi, specimen_id, covid_infection, date_ecoss_specimen) %>%
  rename(index_specimen_id = specimen_id, index_date = date_ecoss_specimen)
##join to all tests with same UPI
linked_test_IDs <- left_join(index_cases, tests_detail) %>%
  mutate(index_case = ifelse(specimen_id == index_specimen_id,1,0)) %>% # flag index cases
  arrange(upi, date_ecoss_specimen) %>% 
  group_by(upi, covid_infection) %>% 
  #compute one_ and two_ tests_later ONLY for index cases
  mutate(one_test_later = ifelse(index_case==1, 
                                 paste0(lead(original_test_class), "_", difftime(lead(date_ecoss_specimen), date_ecoss_specimen), "_days_later"),
                                 NA)) %>%
  mutate(two_tests_later = ifelse(index_case==1, 
                                  paste0(lead(original_test_class, 2), "_", difftime(lead(date_ecoss_specimen, 2), date_ecoss_specimen), "_days_later"),
                                  NA)) %>% 
  ungroup() %>%
  #if one_ and two_ tests later are in range, then link the specimen IDs of those tests
  mutate(linked_ID_1 = ifelse(one_test_later %in% c("POSITIVE_PCR_1_days_later", "POSITIVE_PCR_2_days_later", 
                                                         "POSITIVE_LFD_1_days_later", "POSITIVE_LFD_2_days_later"),lead(specimen_id,1),NA), 
         linked_ID_2 = ifelse(two_tests_later %in% c("POSITIVE_PCR_1_days_later", "POSITIVE_PCR_2_days_later", 
                                                     "POSITIVE_LFD_1_days_later", "POSITIVE_LFD_2_days_later"), lead(specimen_id,2), NA)) #%>%
 linked_IDS_only <- linked_test_IDs %>% 
   select(upi,covid_infection,index_case, index_date, index_specimen_id, specimen_id, linked_ID_1, linked_ID_2) %>%
  filter(index_case==1) 

linked_IDs_long <-  linked_IDS_only %>%
  #pivot longer to get all the specimen_ids.
  rename(linked_ID_0 = specimen_id) %>%
  pivot_longer(cols = c(linked_ID_0, linked_ID_1, linked_ID_2), names_to = "test_number", values_to = "specimen_id") %>%
  filter(!is.na(specimen_id))


#join detials for all linked tests
details <- left_join(linked_IDs_long, tests_detail)
details <- details %>% pivot_wider(id_cols = c(upi, covid_infection, index_date, index_specimen_id),
                        values_from = c( "specimen_id" ,"date_ecoss_specimen" , 
                                        "test_type","sgene_classification", "original_test_class" ,
                                        "x_symptomatic"  ,"wgs_sequence_id" ,  "variant_of_interest"  ,
                                        "variant_calling"  ,  "genomic_lineage" ), names_from = c(test_number) )

#definitive test values
details <- details %>%   rowwise() %>%
  mutate(final_sgene_classification = 
         first_(
           c(sgene_classification_linked_ID_0, sgene_classification_linked_ID_1, sgene_classification_linked_ID_2)), 
         final_symptomatic = first_(
           c(x_symptomatic_linked_ID_0, x_symptomatic_linked_ID_1, x_symptomatic_linked_ID_2)), 
         final_voi = first(c(variant_of_interest_linked_ID_0,
                           variant_of_interest_linked_ID_1, 
                           variant_of_interest_linked_ID_2)), 
  final_lineage = first_(c(genomic_lineage_linked_ID_0,
                        genomic_lineage_linked_ID_1, 
                        genomic_lineage_linked_ID_2))) %>%
  select(upi, covid_infection, index_date, index_specimen_id,test_type_linked_ID_0,  wgs_sequence_id_linked_ID_0, original_test_class_linked_ID_0,
         specimen_id_linked_ID_1, specimen_id_linked_ID_2, 
       date_ecoss_specimen_linked_ID_1, date_ecoss_specimen_linked_ID_2, 
       wgs_sequence_id_linked_ID_1  ,wgs_sequence_id_linked_ID_2, 
       test_type_linked_ID_1, test_type_linked_ID_2, 
         final_sgene_classification, final_symptomatic, final_voi , final_lineage ) %>%
  rename(index_wgs_sequence_id = wgs_sequence_id_linked_ID_0, index_test_type = test_type_linked_ID_0,  index_original_test_class = original_test_class_linked_ID_0)
saveRDS(details,paste0(folder_temp_data, "tests_details.rds") )
#details <-readRDS(paste0(folder_temp_data, "tests_details.rds"))
#names(details)
####

tests_final <- tests_grouped %>%
  lapply(., as.character) %>%
  as_tibble() %>%
  pivot_longer(cols=c(specimen_id, date_ecoss_specimen, test_result, test_type), names_to = "var", values_to = "data") %>%
  mutate(positive_test = case_when(var == "date_ecoss_specimen" ~ paste0("positive_test_", covid_infection),
                                   T ~ paste0("positive_test_", covid_infection, "_", var))) %>%
  select(upi, positive_test, data) %>%
  pivot_wider(names_from = positive_test, values_from = data)

names(tests_final) # consider removing all the sgene & sympton flags from this file as they now exist in the test_details file.
write_rds(tests_final, paste0(folder_temp_data, "infections_all_test_dates.rds"), compress = "gz")
tests_final<- readRDS(paste0(folder_temp_data, "infections_all_test_dates.rds"))

#### Summarise S-Gene and Symptomatic Data ####
summary_s_gene <- tests_long %>%
  select(date_ecoss_specimen, sgene_classification) %>%
  mutate(month = format(date_ecoss_specimen, "%Y %m")) %>%
  group_by(month, sgene_classification) %>%
  count() %>%
  arrange(month) %>%
  pivot_wider(names_from = month, values_from = n) %>%
  adorn_totals()

summary_symptomatic <- tests_long %>%
  select(date_ecoss_specimen, flag_covid_symptomatic) %>%
  mutate(month = format(date_ecoss_specimen, "%Y %m")) %>%
  group_by(month, flag_covid_symptomatic) %>%
  count() %>%
  arrange(month) %>%
  pivot_wider(names_from = month, values_from = n) %>%
  adorn_totals()

summary_test_type <- tests_long %>%
  select(date_ecoss_specimen, test_type) %>%
  mutate(month = format(date_ecoss_specimen, "%Y %m")) %>%
  group_by(month, test_type) %>%
  count() %>%
  arrange(month) %>%
  pivot_wider(names_from = month, values_from = n) %>%
  adorn_totals()

summary_test_reason <- tests %>%
  select(test_reason) %>%
  group_by(test_reason) %>%
  count() %>%
  adorn_totals()

write_csv(summary_s_gene, paste0(folder_temp_data, "summary_s_gene.csv"))
write_csv(summary_symptomatic, paste0(folder_temp_data, "summary_symptomatic.csv"))
write_csv(summary_test_type, paste0(folder_temp_data, "summary_test_type.csv"))
write_csv(summary_test_reason, paste0(folder_temp_data, "summary_test_reason.csv"))


#dates
dataset_dates("Testing", tests_long$date_ecoss_specimen)

rm(tests, tests_long, tests_final, chi_completeness_check, tests_grouped, tests_sgene,
   template, all_upis, tests_valid_chi,
   summary_s_gene, summary_symptomatic, summary_test_type, summary_test_reason,
   con, neonate_tests, number_positive_neonates)
