# Compile report

# Note objects in environment created before running this script ---------------
objects_to_keep <- c(ls(), "objects_to_keep")

# Obtain date the COPS cohort was updated
publication_cops_db_update_date <- readRDS(paste0(folder_temp_data, "all_dates.rds")) %>%
  dplyr::filter(dataset == "SMR01") %>%
  dplyr::select(read_in_date) %>%
  .[[1, 1]]

# Parameters -------------------------------------------------------------------

tmp_path_subproject_root <- file.path(
  paste(here::here(),
        "network_folder",
        "network_folder",
        "8c infections monthly publication Word",
        sep = "/")
)

params <- list(
  # Path to the root directory of this subproject within the wider COPS project
  path_subproject_root = tmp_path_subproject_root,
  
  # Path to the R Markdown file to render
  path_rmd = file.path(paste(tmp_path_subproject_root, "8_c_r_markdown.Rmd", sep = "/")),
  
  # Path to a temporary Microsoft Word document to render out to
  path_tmp_word = file.path(
    paste0(folder_temp_data,
           paste0(format(publication_date, "%y-%m-%d"), "-covid19-publication_infection-in-pregnancy_text.docx"))
  ), 
  
  # Path to the Microsoft Word document to render out to
  path_word = file.path(
    paste(paste0(folder_outputs, "network_folder/"),
          paste0(format(publication_date, "%y-%m-%d"), "-covid19-publication_infection-in-pregnancy_text.docx"))
  ), 
  
  # Path to 'compile' script
  path_compile = file.path(paste(tmp_path_subproject_root, "8_c_compile.R", sep = "/")),
  path_chart = file.path(paste0(folder_temp_data, "8_c_chart.rds")), # Path to chart
  # Path to rds files containing data for Excel part of publication
  path_infections_output_tables = file.path(paste0(folder_outputs, "Infections"))
)

rm(tmp_path_subproject_root)

# Compile Report ---------------------------------------------------------------

source(params$path_compile)

# Remove objects in environment created by running this script -----------------

rm(list = setdiff(ls(), objects_to_keep))
rm(objects_to_keep)
