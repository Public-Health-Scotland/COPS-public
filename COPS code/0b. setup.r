library(odbc)
library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(naniar)
library(hablar)
library(uuid)
library(phsmethods)
library(readxl)

###
### FOLDER LOCATIONS REMOVED FOR PUBLIC RELEASE
###

dedupe_period <- 83 # This variable is used to deduplicate outcomes within datasets

###
### DATABASE CONNECTION DETAILS REMOVED FOR PUBLIC RELEASE
###

icd10_miscarriage <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Miscarriage ICD10"))
icd10_molar <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Molar ICD10"))
icd10_ectopic <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Ectopic ICD10"))
read_miscarriage <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Miscarriage Read") %>% clean_names()
read_molar <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Molar Read") %>% clean_names()
read_ectopic <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Ectopic Read") %>% clean_names()

hb_lookup <- read_excel(paste0(folder_documentation, "HealthboardLookup.xlsx")) %>% clean_names()

assumed_gestation_booking <- 10
assumed_gestation_ectopic <- 8
assumed_gestation_miscarriage <- 10
assumed_gestation_smr02_termination <- 16
assumed_gestation_aas_termination <- 10
assumed_gestation_stillbirth <- 32
assumed_gestation_live_birth <- 40

feasible_gestation_lb <- 16:44
feasible_gestation_sb <- 24:44
feasible_gestation_miscarriage <- 4:23
feasible_gestation_termination <- 4:44
feasible_gestation_booking <- 4:44

feasible_age <- 11:55
feasible_bmi <- 10:100


#### functions ####
dataset_dates <- function(string_name, date_variable) {
    x <- data.frame(dataset = string_name,
               read_in_date = Sys.Date(),
               earliest_record_date = min(date_variable),
               latest_record_date = max(date_variable))
  write_rds(x, paste0(folder_temp_data, string_name, "_dates.rds"))
}