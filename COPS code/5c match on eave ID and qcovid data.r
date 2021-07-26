MASTER_id_file <- ### EXTRACT/DATABASE CONNECTION DETAILS
                  ### REMOVED FOR PUBLIC RELEASE

## Add on EAVE ID to our cohort

pregnancies4 <-  
  left_join(pregnancies3, MASTER_id_file, by = c("mother_upi" = "CHINumber")) %>% 
  select(-c(date_last_checked, cops, source, date, UPI_NUMBER, chili_checked)) %>% 
  rename("mother_eave_linkno" = "EAVE_LINKNO")


#### Add on QCovid data using EAVE ID

pregnancies5 <-
  left_join(pregnancies4, data_q_covid, by=c("mother_eave_linkno" = "eave_linkno"))

tabyl(pregnancies5$q_covid)


## Add on Shielding

pregnancies6 <- left_join(pregnancies5, data_shielding, by=c("mother_upi" = "shielding_chi"))

tabyl(pregnancies6$shielding_shield)


write_rds(pregnancies6, paste0(folder_temp_data, "script5_pregnancy_record_matched.rds"))

rm(pregnancies1, pregnancies2, pregnancies3, pregnancies_live_birth, pregnancies_other, data_aas, data_an_booking, data_cops_births,
   data_gp_losses, data_nhs_live_births, data_nrs_live_births, data_nrs_stillbirths, data_smr01, data_smr02, data_q_covid,
   df, MASTER_id_file, no_eave_id, preexisting_eave_id, pregnancies3_upi, pregnancies4, pregnancies5)
