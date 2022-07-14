MASTER_id_file <- readRDS("/network_folder/MASTER_id_file.rds")

## Add on EAVE ID to our cohort

pregnancies4 <-  
  left_join(pregnancies3, MASTER_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-c(date_last_checked, cops, source, date, UPI_NUMBER, chili_checked)) %>% 
  rename("mother_eave_linkno" = "EAVE_LINKNO")


#### Add on QCovid data using EAVE ID
#which date to use depends if before/after 2021
pregnancies4a <- pregnancies4 %>% filter(as.Date(pregnancy_start_date) < as.Date("2021-01-01"))
pregnancies4b <- pregnancies4 %>% filter(as.Date(pregnancy_start_date) >= as.Date("2021-01-01"))

#march qcovid (unless only in later file)
data_q_covida <- data_q_covid %>% filter(qversion == "March2020")
data_q_covidb <- data_q_covid %>% filter(!(eave_linkno %in% data_q_covida$eave_linkno) )
data_q_covida <- rbind(data_q_covida, data_q_covidb)
rm(data_q_covidb)

data_q_covidc <- data_q_covid %>% filter(qversion != "March2020")
data_q_covidd <- data_q_covid %>% filter(!(eave_linkno %in% data_q_covidc$eave_linkno) )
data_q_covidc <- rbind(data_q_covidc, data_q_covidd)
rm(data_q_covidd)

pregnancies5a <-
  left_join(pregnancies4a, data_q_covida, by=c("mother_eave_linkno" = "eave_linkno"))

pregnancies5b <-
  left_join(pregnancies4b, data_q_covidc, by=c("mother_eave_linkno" = "eave_linkno"))

pregnancies5 <- rbind(pregnancies5a, pregnancies5b)
rm(pregnancies5a, pregnancies5b)

tabyl(pregnancies5$q_covid)


## Add on Shielding

pregnancies6 <- left_join(pregnancies5, data_shielding, by=c("mother_upi" = "shielding_chi"))

tabyl(pregnancies6$shielding_shield)

## Add on Vaccine data

pregnancies7 <- left_join(pregnancies6, data_vaccine, by=c("mother_upi" = "mother_upi"))

tabyl(pregnancies7$dose_1_vacc_product_name)

write_rds(pregnancies7, paste0(folder_temp_data, "script5_pregnancy_record_matched.rds"), compress = "gz")

rm(pregnancies1, pregnancies2, pregnancies3, pregnancies_live_birth, pregnancies_other, data_aas, data_an_booking, data_cops_births,
   data_gp_losses, data_nhs_live_births, data_nrs_live_births, data_nrs_stillbirths, data_smr01, data_smr02, data_q_covid,
   df, MASTER_id_file, no_eave_id, preexisting_eave_id, pregnancies3_upi, pregnancies4, pregnancies5, pregnancies6, pregnancies7,
   data_shielding, data_vaccine, data_q_covida,data_q_covidc, MASTER_id_file)
