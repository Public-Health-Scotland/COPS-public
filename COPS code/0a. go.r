### Run this to update COPS cohort ###

# Define path to scripts
path_to_scripts <- file.path(paste(here::here(), "network_folder", "network_folder/", sep = "/"))
 
#Setup, read in packages and get folder directories
source(file.path(paste0(path_to_scripts, "0b. setup.r")))

#Read-in data
source(file.path(paste0(path_to_scripts, "1a. read in smr01.r")))
source(file.path(paste0(path_to_scripts, "1b. read in smr02.r")))
source(file.path(paste0(path_to_scripts, "1c. read in nrs livebirths.r")))
source(file.path(paste0(path_to_scripts, "1d. read in nhs livebirths.r")))
source(file.path(paste0(path_to_scripts, "1e. read in aas.r")))
source(file.path(paste0(path_to_scripts, "1f. read in nrs stillbirths.r")))
source(file.path(paste0(path_to_scripts, "1g. read in gp early losses.r")))
source(file.path(paste0(path_to_scripts, "1h. read in an booking.r")))
# source(file.path(paste0(path_to_scripts, "1i. read in ecoss testing data.r")))
source(file.path(paste0(path_to_scripts, "1j. read in infant deaths.r")))
source(file.path(paste0(path_to_scripts, "1k. read in qcovid.r")))
source(file.path(paste0(path_to_scripts, "1m read in vaccine.r")))
source(file.path(paste0(path_to_scripts, "1n. read in shielding.r"))) 

#Calculate numbers filtered out and recoded in read-in stages
source(file.path(paste0(path_to_scripts, "1l. numbers filtered and recoded.r")))

#Create live birth record
source(file.path(paste0(path_to_scripts, "2 create live birth record.R")))

#Create record of all outcomes
source(file.path(paste0(path_to_scripts, "3 create pregnancy record.r")))
rm(pregnancies_all_outcomes_and_ongoing)

#Create list of all upis to send to other teams for extracts
source(file.path(paste0(path_to_scripts, "4a create df of all upi numbers.r")))

#Read in infections data for relevant UPIs
source(file.path(paste0(path_to_scripts, "4b extract infections.r")))

#Combine first and last recorded dates for each dataset into table
source(file.path(paste0(path_to_scripts, "4z. dates for each data source.r")))

#Add on variables from all sources. Only run 5a and 5c if not doing a full cohort refresh.
source(file.path(paste0(path_to_scripts, "5a match pregnancy data to records.r")))
source(file.path(paste0(path_to_scripts, "5b create new eave IDs.r"))) #Only run script 5b on a full run through when you expect new CHIs in the cohort
source(file.path(paste0(path_to_scripts, "5c match on eave ID and qcovid data.r")))

#Determine authoritative final versions of variables for use in outputs
source(file.path(paste0(path_to_scripts, "6a determine authoritative values.r")))

#Add in infection data
source(file.path(paste0(path_to_scripts, "6aa add on infection data.r")))

#Create multipurpose pregnancy level file for outputs
source(file.path(paste0(path_to_scripts, "6b create pregnancy level file.R")))

#Create maternal outcomes datasets
source(file.path(paste0(path_to_scripts, "6c extract-maternal-outcomes-data.R")))
#Analyse maternal outcomes data
source(file.path(paste0(path_to_scripts, "6d maternal-outcomes-tables.R")))

#Read out anonymised data to non-confi area
source(file.path(paste0(path_to_scripts, "6e write anonymised files.r")))

# Produce infections comparator data
source(file.path(paste0(path_to_scripts, "7a infections comparator data.r")))
# Produce infections comparator data
source(file.path(paste0(path_to_scripts, "7b vaccinations comparator data.r")))

#Produce descriptive tables and figures in RMarkdown
# source(file.path(paste0(path_to_scripts, "7 produce descriptive tables.rmd"))) Script 7 has been replaced by the Output 1 bookdown output

#Publication script to create COPS vaccination information
source(file.path(paste0(path_to_scripts, "8a vaccine publication tables.R")))

# Produce "COVID-19 vaccination for pregnant women" publication report in
# Microsoft Word format
source(file.path(paste0(path_to_scripts, "network_folder/", "8_b_create_report.R")))

#Publication script to create COPS infection information
source(file.path(paste0(path_to_scripts, "8c infections monthly publication.R")))

# Produce "COVID-19 infections in pregnant women" publication report in
# Microsoft Word format
source(file.path(paste0(path_to_scripts, "network_folder/", "8_c_create_report.R")))


