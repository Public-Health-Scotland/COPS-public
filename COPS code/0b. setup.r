### Install packages ----

#install.packages("here")
#install.packages("remotes")

library(remotes)
#remotes::install_github("Public-Health-Scotland/phstemplates", ref = "main")

#install.packages("cowplot")

#renv::snapshot()

### Load packages ----

library(renv)
library(here)
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
library(kableExtra)
library(phstemplates)
library(ggplot2)
library(scales)
library(captioner)
library(flextable)
library(officer)
library(magrittr)
library(mschart)
library(openxlsx)
library(haven)
library(tictoc)
library(ggthemes)
library(cowplot)

## FOLDER LOCATIONS REMOVED FOR PUBLIC RELEASE ##


dedupe_period <- 83 # This variable is used to deduplicate outcomes within datasets

cohort_start_date <- lubridate::ymd("2015-01-01")

publication_latest_vacc_date <- lubridate::ymd("2022-07-31") # This date may need amended
publication_date <- lubridate::ymd("2022-09-07") # This date may need amended

vacc_start_date <- as.Date("2020-12-01")


SMRAConnection <- dbConnect(odbc(),
                            dsn = "SMRA",
                            uid = Sys.info()[["user"]], #Assumes the user's SMR01 username is the same as their R server username
                            pwd = read_lines("~/pass.txt")) #Assumes the user has their password saved in a pass.txt file in their home directory on the R server

icd10_miscarriage                             <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Miscarriage ICD10"))
icd10_molar                                   <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Molar ICD10"))
icd10_ectopic                                 <- as_vector(read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Ectopic ICD10"))
icd10_acute_covid19                           <- read_csv(paste0(folder_documentation, "icd10_covid.csv")) %>% filter(condition == "Acute COVID-19") %>% select(icd10) %>% as_vector()
icd10_history_of_covid19                      <- read_csv(paste0(folder_documentation, "icd10_covid.csv")) %>% filter(condition == "History of COVID-19") %>% select(icd10) %>% as_vector()
icd10_adverse_event_after_covid19_vaccine     <- read_csv(paste0(folder_documentation, "icd10_covid.csv")) %>% filter(condition == "Adverse event after COVID-19 vaccine") %>% select(icd10) %>% as_vector()
read_miscarriage                              <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Miscarriage Read") %>% clean_names()
read_molar                                    <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Molar Read") %>% clean_names()
read_ectopic                                  <- read_excel(paste0(folder_documentation, "Early spontaneous loss codes.xlsx"), "Ectopic Read") %>% clean_names()

smr02_ethnicity_lookup       <- read_csv(paste0(folder_documentation, "smr02_ethnicity_lookup.csv"))

hb_lookup <- read_excel(paste0(folder_documentation, "HealthboardLookup.xlsx")) %>% clean_names()

assumed_gestation_booking            <- 10
assumed_gestation_ectopic            <- 8
assumed_gestation_miscarriage        <- 10
assumed_gestation_smr02_termination  <- 16
assumed_gestation_aas_termination    <- 10
assumed_gestation_stillbirth         <- 32
assumed_gestation_live_birth         <- 40

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

cops_age_group <- function(age_variable) {case_when(age_variable >= 11 & age_variable <= 19 ~ " 1 <= 19", 
                                                    age_variable >= 20 & age_variable <= 24 ~ "2 20-24", 
                                                    age_variable >= 25 & age_variable <= 29 ~ "3 25-29",
                                                    age_variable >= 30 & age_variable <= 34 ~ "4 30-34",
                                                    age_variable >= 35 & age_variable <= 39 ~ "5 35-39",
                                                    age_variable >= 40 & age_variable <= 55 ~ "7 >=40", TRUE ~ "8 Unknown")}

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

is_code_in_codevector <- function(code, codevector){
  # returns TRUE if the first 3 or 4 characters of a string (ICD10 code) are contained in a supplied vector of 3 and/or 4 digit codes
  str_sub(code, 1, 3) %in% codevector[str_length(codevector) == 3] |
    str_sub(code, 1, 4) %in% codevector[str_length(codevector) == 4]
}

coacross <- function(...) {
  # due to dplyr quirk, need to define this function in order to do coalsece() operations over multiple columns selected via tidy select helpers
  # e.g.   mutate(test = coacross(starts_with("whatever")))
  coalesce(!!!across(...))
}

add_flag_columns <- function(df, all_codes_list, col_contains = "condition", output = "logical"){
  # for a tibble with column names containing the string "condition" (optionally can specify using col_contains argument), and a list containing named vectors of ICD10 codes,
  # this will append "flag" columns -- one for each condition in "all_codes_list". 
  # These will either be TRUE/FALSE (TRUE if an appropriate code is detected in any position) or "name_of_condition/NA_character", depending on whether output = "logical" or "names"
  for(condition in names(all_codes_list)){
    colname <- paste("flag", str_remove(condition, "_codes"), sep = "_")
    if(output == "logical"){
      df <- mutate(df, {{colname}} := if_any(contains(col_contains), ~is_code_in_codevector(., all_codes_list[[condition]])))
    }
    if(output == "names"){
      df <- mutate(df, {{colname}} := if_else(if_any(contains(col_contains), ~is_code_in_codevector(., all_codes_list[[condition]])) == TRUE, condition, NA_character_ ))
    }
  }
  return(df)
}

generate_recodes <- function(df_labelled, colname, output = "vector"){
  # Takes a column from a labelled dataframe (e.g from have SPSS import) and returns a vector or tibble that can be used to recode e.g. the values to the labels or vice-versa.
  # Useful if data values are numeric and the labels are more informative strings.
  # By default returns a named vector (easier for use in dplyr::recode) but can optionally return a tibble instead (set e.g. output = "tibble")
  labels <- attr(unique(df_labelled[[{{colname}}]]), which = "labels")
  
  out <- tibble(codes = labels,
                values = labels %>% names())
  
  if(output == "vector"){
    out <- deframe(out)
  }
  
  return(out)
  
}

clear_temp_tables <- function(conn){
  # clears old dbplyr tables off the SQL database where possible
  tables <- odbc::dbListTables(conn)
  for(table in tables[str_sub(tables, 1, 7) == "dbplyr_"]){try(odbc::dbRemoveTable(conn, table), silent = TRUE)}
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



## baby-level split for measures within 28 days (with a flag to identify this)
associated_babies <- function(data, parent_id, flag, month, split, measure){
  monthly <- data %>% 
    filter({{flag}} == TRUE) %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_total <- data %>% 
    filter({{flag}} == TRUE) %>% 
    mutate({{split}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_col_total <- data %>% 
    filter({{flag}} == TRUE) %>% 
    mutate({{split}} := "Total") %>% 
    mutate({{month}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  final <- data %>%
    filter({{flag}} == TRUE) %>% 
    select({{parent_id}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{split}}) %>% 
    mutate({{month}} := "Total") %>% 
    bind_rows(monthly) %>% 
    bind_rows(monthly_row_total) %>% 
    bind_rows(monthly_row_col_total) %>% 
    mutate(indicator = {{measure}})
}

## baby-level split for all subsequent measures (with no flag: assumes all records in dataset need to be counted)
subsequent_babies <- function(data, parent_id, month, split, measure){
  monthly <- data %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_total <- data %>% 
    mutate({{split}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_col_total <- data %>% 
    mutate({{split}} := "Total") %>% 
    mutate({{month}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  final <- data %>%
    select({{parent_id}}, {{split}}, fetus_number) %>% 
    distinct() %>% 
    count({{split}}) %>% 
    mutate({{month}} := "Total") %>% 
    bind_rows(monthly) %>% 
    bind_rows(monthly_row_total) %>% 
    bind_rows(monthly_row_col_total) %>% 
    mutate(indicator = {{measure}})
}

## pregnancy-level split for measures within 28 days (with a flag to identify this)
associated_pregnancies <- function(data, parent_id, flag, month, split, measure){
  monthly <- data %>% 
    filter({{flag}} == TRUE) %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_total <- data %>% 
    filter({{flag}} == TRUE) %>% 
    mutate({{split}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_col_total <- data %>% 
    filter({{flag}} == TRUE) %>% 
    mutate({{split}} := "Total") %>% 
    mutate({{month}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  final <- data %>%
    filter({{flag}} == TRUE) %>% 
    select({{parent_id}}, {{split}}) %>% 
    distinct() %>% 
    count({{split}}) %>% 
    mutate({{month}} := "Total") %>% 
    bind_rows(monthly) %>% 
    bind_rows(monthly_row_total) %>% 
    bind_rows(monthly_row_col_total) %>% 
    mutate(indicator = {{measure}})
}
## pregnancy-level split for all subsequent measures (with no flag: assumes all records in dataset need to be counted)
subsequent_pregnancies <- function(data, parent_id, month, split, measure){
  monthly <- data %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_total <- data %>% 
    mutate({{split}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  monthly_row_col_total <- data %>% 
    mutate({{split}} := "Total") %>% 
    mutate({{month}} := "Total") %>% 
    select({{parent_id}}, {{month}}, {{split}}) %>% 
    distinct() %>% 
    count({{month}}, {{split}}) 
  
  final <- data %>%
    select({{parent_id}}, {{split}}) %>% 
    distinct() %>% 
    count({{split}}) %>% 
    mutate({{month}} := "Total") %>% 
    bind_rows(monthly) %>% 
    bind_rows(monthly_row_total) %>% 
    bind_rows(monthly_row_col_total) %>% 
    mutate(indicator = {{measure}})
}

# takes coverage data and splits it into separate tables for output
coverage_split_table <- function(data, month, identifier, value){
  final <- data %>% 
    select({{month}}, {{identifier}}, {{value}}) %>% 
    filter({{month}} != "Dec 2020" & {{month}} != "Jan 2021" & {{month}} != "Feb 2021" 
           & {{month}} != "Mar 2021") %>% 
    pivot_wider(id = {{identifier}}, names_from = {{month}}, values_from = {{value}}) %>% 
    select(-{{identifier}})
  return(final)
}


#### Wilson CI score function ####
Z_95 <- qnorm(1- 0.05/2)

conf_int_wilson2 <- function(O, n, z, type = "upper", correction = "continuity", N = NA_integer_, clamp = TRUE){
  # Returns Wilson score estimates for confidence intervals on binomial proportions.
  # 
  # Optional corrections can be applied for continuity and sampling from a finite population.
  # 
  # O = observed samples with the required characteristic, 
  # n = total sample size, 
  # z = number of standard deviations required to define desired confidence
  #     interval under a Gaussian e.g. 1.96 for 95% conf int,
  # type = "upper" or "lower" to return the upper or lower confidence interval,
  # correction = "continuity", "fpc", "both" will apply continuity and/or finite population
  #     corrections to the Wilson score. Any other value for "correction" will return
  #     an uncorrected value,
  # N = total population size; when sample tested (of size n) is a sub-sample of total population N,
  #     a finite population correction can be calculated.
  # clamp = TRUE limits confidence intervals to be >=0 and <=1
  # 
  # Methodology: will use the formulation of the Wilson score from 
  #     https://corplingstats.wordpress.com/2019/04/27/correcting-for-continuity/
  #     as it allows easy separation of the effects due to continuity correction and finite pop correction.
  #     Continuity correction can be expressed as an addition/subtraction to p, and
  #     finite pop correction can be applied by dividing n by the square of the usual
  #     finite pop correction coefficient sqrt((N-n)/(N-1))  [i.e. equiv to multiply n by (N - 1)/(N - n)]
  # 
  #     Using no corrections (and clamp = FALSE) yields the same results as equations 2a/2b in PHE methods bumf:
  #     https://fingertips.phe.org.uk/documents/APHO%20Tech%20Briefing%203%20Common%20PH%20Stats%20and%20CIs.pdf
  # 
  #     Using continuity correction (and clamp = TRUE) yields the same results as 
  #     base R function prop.test()
  # 
  # Examples:
  #     
  
  # coerce inputs to positive integers (or positive real for "z") if needed
  O <- abs(as.integer(O))
  n <- abs(as.integer(n))
  N <- abs(as.integer(N))
  z <- abs(as.double(z))
  
  if(!(type %in% c("upper", "lower"))){
    warning("Please specify type as either \"upper\" or \"lower\". Defaulting to upper...")
    type = "upper"
  }
  
  # corrections to apply if no corrections are required ...
  # (i.e. add 0 to p and multiply n by 1)
  p_correction <- 0
  n_correction <- 1 
  
  # continuity correction
  if(correction == "continuity" | correction == "both"){
    if(type == "upper"){
      p_correction <- (1/(2*n))
    }
    if(type == "lower"){
      p_correction <- -(1/(2*n))
    }
  }
  
  # finite pop correction
  if(correction == "fpc" | correction == "both"){
    if(is.na(N) == TRUE | N < 1 | N <= n){
      warning("Finite population correction requires you to specify a total population N (greater than n).
              As this is not specified, finite population correction will not be applied...")
    } else{
      n_correction <- (N - 1)/(N - n)
    }
  }
  
  # apply any corrections to p due to continuity, or n -- due to finite population
  p <- (O/n) + p_correction
  n <- n*n_correction
  
  # finally, calculate the Wilson score for either upper or lower
  if(type == "upper"){conf <- (p + (z^2/(2*n)) + ( z*sqrt( ((p/n)*(1 - p)) + (z^2/(4*(n^2) ))  )  ))  /  (1 + (z^2/n))  }
  if(type == "lower"){conf <- (p + (z^2/(2*n)) - ( z*sqrt( ((p/n)*(1 - p)) + (z^2/(4*(n^2) ))  )  ))  /  (1 + (z^2/n))  }
  
  # apply some logic for special cases
  conf <- ifelse(O == 0 & type == "lower", 0, conf)
  conf <- ifelse(O == n & type == "upper", 1, conf)
  
  if(clamp == TRUE){
    conf <- ifelse(conf < 0, 0, conf)
    conf <- ifelse(conf > 1, 1, conf)
  }
  
  return(conf)
  }


moving_index_deduplication <- function(dataframe, identifier, date, deduplication_period) {
  
  tic() 
  
  dataframe <- dataframe %>%
    arrange({{ identifier }}, {{ date }}) %>%
    mutate(cops_event = 1)
  
  print(paste0(nrow(dataframe), " rows in dataframe"))
  
  # If a UPI number only has one observation then we can just assign that single observation to group 1 without putting it through the loop
  dataframe_single_observations <- dataframe %>%
    group_by({{ identifier }}) %>%
    filter(n() == 1) %>%
    ungroup()
  
  print(paste0(nrow(dataframe_single_observations), " single observations in dataframe"))
  
  # If a UPI number only has two observations then we don't need to put it through the loop - we can just compare the dates of the two observations
  # and quickly assign them to the correct groups.
  dataframe_double_observations <- dataframe %>%
    group_by({{ identifier }}) %>%
    filter(n() == 2) %>%
    mutate(cops_event = if_else(row_number() == 2 & difftime({{ date }}, lag({{ date }}), units = "days") > deduplication_period, 2, 1)) %>%
    ungroup()
  
  print(paste0(nrow(dataframe_double_observations), " double observations in dataframe"))
  
  dataframe_completed_groupings <-
    dataframe_single_observations %>%
    bind_rows(dataframe_double_observations)
  
  dataframe %<>% group_by({{ identifier }}) %>%
    filter(n() > 2) %>%
    ungroup()
  
  loop <- 1
  
  repeat {
    
    tic()
    print(loop)
    loop <- loop + 1
    
    print(paste0(nrow(dataframe), " rows in loop"))
    
    # This code groups records into COPS Events.
    print("Determining index date")
    # dataframe %<>% mutate(index_date = first({{ date }})) # The first observed admission date for a woman becomes our initial index date, and then changes on every iteration to the first admission date which occurs >83 days after the previous index date.
    dataframe <- dataframe %>% 
      dtplyr::lazy_dt() %>% 
      group_by({{ identifier }}, cops_event) %>% 
      mutate(index_date = first({{ date }})) %>% 
      collect() %>% ungroup()
    
    print("Calculating time differences")
    dataframe %<>% mutate(days_since_index_event = difftime({{ date }}, index_date, units = "days"))
    
    print("Setting COPS event counter")
    dataframe %<>% mutate(cops_event = if_else(days_since_index_event > deduplication_period, cops_event + 1, cops_event))
    
    print("Determining completed groupings")
    # dataframe %<>% group_by({{ identifier }})
    # dataframe %<>% mutate(max_days = max(days_since_index_event)) %>% ungroup()
    dataframe <- dataframe %>% 
      dtplyr::lazy_dt() %>% 
      group_by({{ identifier }}) %>% 
      mutate(max_days = max(days_since_index_event)) %>% 
      collect() %>% 
      ungroup()
    
    dataframe %<>% mutate(grouping_complete = if_else(max_days <= deduplication_period, 1, 0))
    temp_complete_grouping <- dataframe %>% filter(grouping_complete == T) %>% select(-grouping_complete)
    
    print("Removing completed groupings from loop")
    print(paste0(nrow(temp_complete_grouping), " rows removed from next loop"))
    dataframe_completed_groupings %<>% bind_rows(temp_complete_grouping)
    dataframe %<>% filter(grouping_complete == F) %>% select(-grouping_complete)
    
    rm(temp_complete_grouping)
    
    toc()
    
    if (nrow(dataframe) == 0) {
      print("Rows succesfully grouped into COPS events")
      break # If no records take place more than 83 days after that person's latest index event, then we've successfully allocated every row to its proper COPS event group.
    }
    print("Running another loop...")
  }
  
  dataframe_completed_groupings %<>%
    select(-c(index_date, days_since_index_event, max_days)) %>%
    arrange({{ identifier }}, {{ date }})
  
  toc()
  return(dataframe_completed_groupings)
  
}

