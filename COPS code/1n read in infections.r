
##########read in infections data. 

### packages ------------------------------------------------------------

if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, odbc, janitor, lubridate, glue,
               phsmethods, tidylog)

###
### DATABASE CONNECTION DETAILS REMOVED FOR PUBLIC RELEASE
###

# ### Extract data --------------------------------------------------------

### extract all pcr tests 
## restrict columns and time period as required
## exclude void, transcribed and de-notified results

#limiting data just to try can change this once further in the process. 

pcr_tests <- ### EXTRACT/DATABASE CONNECTION DETAILS
             ### REMOVED FOR PUBLIC RELEASE

all_upis <- ### EXTRACT/DATABASE CONNECTION DETAILS
            ### REMOVED FOR PUBLIC RELEASE

test <- pcr_tests %>% inner_join(all_upis, by= c("subject_upi"="upi"))

write_rds(test, paste0("tests_cops.rds"))


