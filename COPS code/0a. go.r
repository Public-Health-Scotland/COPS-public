### Run this to update COPS cohort ###
library(hablar)
set_wd_to_script_path()
getwd()

#Setup, read in packages and get folder directories
source("0b. setup.r")

#Read-in data
source("1a. read in smr01.r")
source("1b. read in smr02.r")
source("1c. read in nrs livebirths.r")
source("1d. read in nhs livebirths.r")
source("1e. read in aas.r")
source("1f. read in nrs stillbirths.r")
source("1g. read in gp early losses.r")
source("1h. read in an booking.r")
source("1i. read in ecoss testing data.r")
source("1j. read in infant deaths.r")
source("1k. read in qcovid.r")
source("1m read in vaccine.r")
source("1n. read in shielding.r")

#Calculate numbers filtered out and recoded in read-in stages
 source("1l. numbers filtered and recoded.r")

#Combine first and last recorded dates for each dataset into table
 source("1o. dates for each data source.r")

#Create live birth record
source("2 create live birth record.R")

#Create record of all outcomes
source("3 create pregnancy record.r")
rm(pregnancies_all_outcomes_and_ongoing)

#Create list of all upis to send to other teams for extracts
source("4 create df of all upi numbers.r")

#Add on variables from all sources. Only run 5a and 5c if not doing a full cohort refresh.
source("5a match pregnancy data to records.r")
source("5b create new eave IDs.r") #Only run script 5b on a full run through when you expect new CHIs in the cohort
source("5c match on eave ID and qcovid data.r")

#Determine authoritative final versions of variables for use in outputs
source("6a determine authoritative values.r")

#Create multipurpose pregnancy level file for outputs
source("6b create pregnancy level file.R")

#Produce descriptive tables and figures in RMarkdown
source("7 produce descriptive tables.rmd")