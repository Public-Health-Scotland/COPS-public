# This is the functions file for creating cohorts.
# Provides helper functions for long select / mutate options that
#  otherwise will be repeated many times

library(tidyverse)
library(glue)

# adding columns for joining datasets -------------------------------------

vaccinated_mutate_function <- function(df, variable) {
  df %>%
    select(
      mother_eave_linkno,
      pregnancy_id,
      mother_age_at_conception,
      conception_quarter,
      glue({variable}, "_gestation_at_reference_date")
    ) %>%
    rename(
      gestation_at_reference_date := !!glue({variable}, "_gestation_at_reference_date")) %>%
    mutate(
      gestation_at_reference_date = floor(gestation_at_reference_date),
      mother_age_at_conception_1 = mother_age_at_conception,
      mother_age_at_conception_2 = mother_age_at_conception,
      gestation_at_reference_date_1 = floor(gestation_at_reference_date),
      gestation_at_reference_date_2 = floor(gestation_at_reference_date)
    ) %>%
    rename_with(.cols = everything(), ~ glue("{.}_vacc"))
}

unvaccinated_mutate_function <- function(df) {
  df %>%
    select(
      mother_eave_linkno,
      pregnancy_id,
      mother_age_at_conception,
      conception_quarter,
      start_match_wk,
      end_match_wk
    ) %>%
    mutate(
      mother_age_at_conception_plus1 = mother_age_at_conception + 1,
      mother_age_at_conception_minus1 = mother_age_at_conception - 1
    ) %>%
    rename_with(.cols = everything(), ~ glue::glue("{.}_unvacc"))
}

infected_mutate_function <- function(df, variable) {
  df %>%
    select(
      mother_eave_linkno,
      pregnancy_id,
      mother_age_at_conception,
      conception_quarter,
      glue({variable}, "_gestation_at_index_date")
    ) %>%
    rename(
      gestation_at_index_date := !!glue({variable}, "_gestation_at_index_date")) %>%
    mutate(
      gestation_at_index_date = floor(gestation_at_index_date),
      mother_age_at_conception_1 = mother_age_at_conception,
      mother_age_at_conception_2 = mother_age_at_conception,
      gestation_at_index_date_1 = floor(gestation_at_index_date),
      gestation_at_index_date_2 = floor(gestation_at_index_date)
    ) %>%
    rename_with(.cols = everything(), ~ glue("{.}_inf"))
}

infected_mutate_function_reference <- function(df, variable) {
  df %>%
    select(
      mother_eave_linkno,
      pregnancy_id,
      mother_age_at_conception,
      conception_quarter,
      glue({variable}, "_gestation_at_reference_date")
    ) %>%
    rename(
      gestation_at_reference_date := !!glue({variable}, "_gestation_at_reference_date")) %>%
    mutate(
      gestation_at_reference_date = floor(gestation_at_reference_date),
      mother_age_at_conception_1 = mother_age_at_conception,
      mother_age_at_conception_2 = mother_age_at_conception,
      gestation_at_reference_date_1 = floor(gestation_at_reference_date),
      gestation_at_reference_date_2 = floor(gestation_at_reference_date)
    ) %>%
    rename_with(.cols = everything(), ~ glue("{.}_inf"))
}

uninfected_mutate_function <- function(df) {
  df %>%
    select(
      mother_eave_linkno,
      pregnancy_id,
      mother_age_at_conception,
      conception_quarter,
      start_match_wk,
      end_match_wk
    ) %>%
    mutate(
      mother_age_at_conception_plus1 = mother_age_at_conception + 1,
      mother_age_at_conception_minus1 = mother_age_at_conception - 1
    ) %>%
    rename_with(.cols = everything(), ~ glue::glue("{.}_uninf"))
}

# cohort matching function ------------------------------------------------

cohort_matching_function_with_conception_quarter <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    conception_quarter_vacc == conception_quarter_unvacc,
    mother_age_at_conception_1_vacc >= mother_age_at_conception_minus1_unvacc,
    mother_age_at_conception_2_vacc <= mother_age_at_conception_plus1_unvacc,
    gestation_at_reference_date_1_vacc >= start_match_wk_unvacc,
    gestation_at_reference_date_2_vacc <= end_match_wk_unvacc
  )]
}

cohort_matching_function_with_conception_quarter_no_vacc <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    conception_quarter_vacc == conception_quarter_unvacc,
    mother_age_at_conception_1_vacc >= mother_age_at_conception_minus1_unvacc,
    mother_age_at_conception_2_vacc <= mother_age_at_conception_plus1_unvacc
    #gestation_at_reference_date_1_vacc >= start_match_wk_unvacc,
    #gestation_at_reference_date_2_vacc <= end_match_wk_unvacc
  )]
}

cohort_matching_function_no_conception_quarter <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    # conception_quarter_vacc == conception_quarter_unvacc,
    mother_age_at_conception_1_vacc >= mother_age_at_conception_minus1_unvacc,
    mother_age_at_conception_2_vacc <= mother_age_at_conception_plus1_unvacc,
    gestation_at_reference_date_1_vacc >= start_match_wk_unvacc,
    gestation_at_reference_date_2_vacc <= end_match_wk_unvacc
  )]
}

cohort_matching_function_infection_with_conception_quarter <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    conception_quarter_inf == conception_quarter_uninf,
    mother_age_at_conception_1_inf >= mother_age_at_conception_minus1_uninf,
    mother_age_at_conception_2_inf <= mother_age_at_conception_plus1_uninf,
    gestation_at_index_date_1_inf >= start_match_wk_uninf,
    gestation_at_index_date_2_inf <= end_match_wk_uninf
  )]
}

cohort_matching_function_infection_no_conception_quarter <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    # conception_quarter_vacc == conception_quarter_unvacc,
    mother_age_at_conception_1_inf >= mother_age_at_conception_minus1_uninf,
    mother_age_at_conception_2_inf <= mother_age_at_conception_plus1_uninf,
    gestation_at_index_date_1_inf >= start_match_wk_uninf,
    gestation_at_index_date_2_inf <= end_match_wk_uninf
  )]
}

cohort_matching_function_infection_with_conception_quarter_reference <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    conception_quarter_inf == conception_quarter_uninf,
    mother_age_at_conception_1_inf >= mother_age_at_conception_minus1_uninf,
    mother_age_at_conception_2_inf <= mother_age_at_conception_plus1_uninf,
    gestation_at_reference_date_1_inf >= start_match_wk_uninf,
    gestation_at_reference_date_2_inf <= end_match_wk_uninf
  )]
}

cohort_matching_function_infection_no_conception_quarter_reference <- function(dt_first, dt_to_join) {
  # this joins the z2_dt to z1_dt, by the variables in on() below
  dt_first[dt_to_join, on = .(
    # conception_quarter_vacc == conception_quarter_unvacc,
    mother_age_at_conception_1_inf >= mother_age_at_conception_minus1_uninf,
    mother_age_at_conception_2_inf <= mother_age_at_conception_plus1_uninf,
    gestation_at_reference_date_1_inf >= start_match_wk_uninf,
    gestation_at_reference_date_2_inf <= end_match_wk_uninf
  )]
}

# cohort ordering function -----------------------------------------------

cohort_ordering_function <- function(dt) {
  dt[, .(pregnancy_id_vacc, pregnancy_id_unvacc)][, `:=`(number_of_matches = .N),
    by = .(pregnancy_id_vacc)
  ][order(number_of_matches)][, `:=`(c("index"), {
    index <- .GRP
    index <- str_pad(index, 5, "left", pad = "0")
    index <- paste("index", index, sep = "_")
    .(index)
  }), by = .(number_of_matches, pregnancy_id_vacc)][order(index)]
}

cohort_ordering_function_infection <- function(dt) {
  dt[, .(pregnancy_id_inf, pregnancy_id_uninf)][, `:=`(number_of_matches = .N),
                                                  by = .(pregnancy_id_inf)
                                                  ][order(number_of_matches)][, `:=`(c("index"), {
                                                    index <- .GRP
                                                    index <- str_pad(index, 5, "left", pad = "0")
                                                    index <- paste("index", index, sep = "_")
                                                    .(index)
                                                  }), by = .(number_of_matches, pregnancy_id_inf)][order(index)]
}

# cohort filtering function -----------------------------------------------

# these functions take a LONG TIME to run - they're not used anymore as the 
# second loop takes too long with the cohorts
#cohort_filtering_function <- function(cohort_list, results_list, n) {
#for (i in 1:length(cohort_list)) {
#  results_list[[i]] <-
#    cohort_list[[i]][cohort_list[[i]][, .I[sample.int(.N, min(min(n, .N), .N))],
#                                          by = .(index)
#                                          ]$V1]
#  
#  for (j in i:length(cohort_list)) {
#    cohort_list[[j]] <- cohort_list[[j]][!(pregnancy_id_unvacc %in% results_list[[i]]$pregnancy_id_unvacc)]
#  }
#}
#  
#}
#
#cohort_filtering_function_reversed <- function(cohort_list, results_list, n) {
#  for (i in 1:length(cohort_list)) {
#    results_list[[i]] <-
#      cohort_list[[i]][cohort_list[[i]][, .I[sample.int(.N, min(min(n, .N), .N))],
#                                        by = .(index)
#                                        ]$V1]
#    
#    for (j in i:length(cohort_list)) {
#      cohort_list[[j]] <- cohort_list[[j]][!(pregnancy_id_vacc %in% results_list[[i]]$pregnancy_id_vacc)]
#    }
#  }
#  
#}


# merging functions -------------------------------------------------------

cohort_matching_function_vacc_as_case <- function(cohort, full_pregnancy_data) {
  cohort %>%
    select(-number_of_matches, -index) %>%
    left_join(preg_data, by = c(
      "pregnancy_id_vacc" = "pregnancy_id"
    )) %>% 
    rename(
      pregnancy_id_case = pregnancy_id_vacc
    ) %>%
    rename_with(~ paste0(., "_case"), mother_eave_linkno:congenital_anomaly_ending_pandemic) %>% 
    left_join(preg_data, by = c(
      "pregnancy_id_unvacc" = "pregnancy_id"
    )) %>%
    rename(
      pregnancy_id_control = pregnancy_id_unvacc
    ) %>%
    rename_with(~ paste0(., "_control"), mother_eave_linkno:congenital_anomaly_ending_pandemic) 
}

