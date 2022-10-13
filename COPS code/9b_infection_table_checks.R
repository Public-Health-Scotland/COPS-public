---
title: "Checks on Infections tables"
author: "Emily Moore"
date: "2/1/2022"
output:
  html_document:
    theme: simplex
    toc: yes
    toc_float: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)

path_to_scripts <- file.path(paste(here::here(), "folder", sep = "/"))

#Set values for publication
pub_month <- "June 2022"
#Setup, read in packages and get folder directories

library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(naniar)
library(hablar)
library(janitor)
library(knitr)
library(ggplot2)
library(kableExtra)
library(phsmethods)
library(scales)
folder_data      <- "folder"
folder_cohorts   <- "folder"
folder_temp_data <- "folder"

```

##Infections tables checks

This markdown runs a series of checks on the tables that are written to the Infection_output_yyyymmdd.xlsx tables.

Checks are titled by the name of the tab they refer to in the excel sheet.

This document was last recreated on `r Sys.Date()`. 

The checks were run on the results dated `r as.Date(file.info(paste0(folder_temp_data, "infection_output_tables"))$mtime)`, for the `r pub_month` publication.


```{r load tables, echo=FALSE, include=FALSE}
name_list <- list.files(paste0(folder_temp_data, "infection_output_tables/"), pattern = ".rds")
file.info(list.files(paste0(folder_temp_data, "infection_output_tables")))
#path <- paste0(folder_temp_data, "infection_output_tables/")
filepath_list <- paste0(folder_temp_data, "infection_output_tables/", name_list)
temp <- lapply(filepath_list, readRDS)
names(temp) <- str_remove(name_list, ".rds")
```

##Cases per week

Number of COVID-19 cases in pregnant women, by week, Scotland

```{r vaccs_by_week, fig.width=9, fig.height=4, echo=FALSE}
table1 <- temp$cases_by_week
table1 <- table1 %>% filter(week_end!="Total")
table1$week_end <-as.Date(table1$week_end)
p <- ggplot(data=table1, aes(x=week_end)) +
  geom_line(aes( y=number_of_cases, group=1, colour = "number_of_cases")) +
  labs(title = "N of cases by week ", colour = "Cases") +
  ylab("cases") + xlab("Week" )
p
```

##Rate in pregnancy
Rate of COVID-19  in pregnant women, by month, Scotland
```{r rate, echo=FALSE, warning=FALSE, fig.width=9, fig.height=4}
table2 <- temp$rate_in_pregnancy %>% 
  filter(indicator=="% women with COVID-19 during pregnancy in month") %>%#its mis labelled, actually N per 100k
  select(-contains("Total"), -indicator) 
# table2$`Jan 2022` <- 0 
comparison_pop <- temp$female_pop_simd_age %>% select(females_only, Total) 
comparison <- temp$female_count_age   #bring in N for all women same age range
comparison$females_only <- comparison_pop$females_only
comparison <- comparison %>% filter(females_only=="Total") %>% select(-Total, -females_only)
comparison_pop <- comparison_pop$Total[comparison_pop$females_only=="Total"]

comp_rate<- comparison/comparison_pop*100000
names(table2) <- names(comp_rate)
x <- as.Date(paste0(names(comp_rate),"-01"), "%Y-%m-%d")

df <- cbind(x, as.numeric(comp_rate[1,]), as.numeric(table2[1,]))
df <- as.data.frame(df)
names(df) <- c("Date", "comp_rate", "preg_rate")
df$Date <- as.Date(df$Date, origin="1970-01-01")
p <- ggplot(data=df,aes(x=Date)) +
  geom_line(aes( y=preg_rate,  colour = "rate_in_pregnancy")) +
  geom_line(aes( y=comp_rate, colour = "rate in females 18-44")) +
  labs(title = "Rate of cases by week per 100,000", colour = "Cases") +
  ylab("cases per 100k") + xlab("Month" )
p

```
##Rate by age
Rate of COVID-19  in pregnant women, by maternal age and month, Scotland
```{r rate_age, echo=FALSE, warning=FALSE, fig.width=9, fig.height=4}
table2 <- temp$rate_in_pregnancy_age#=
name_tabs <- table2$maternal_age_group
table2 <- table2 %>% select(-maternal_age_group)
table2 <- as.data.frame(t(table2))
names(table2) <- name_tabs
table2$month <-  as.Date(paste0( "01 ",rownames(table2)), "%d %b %Y" )
table2 <- table2 %>% filter(!is.na(month))

p <- ggplot(data=table2,aes(x=month)) +
  geom_line(aes( y=` 1 <= 19`,  colour = "rate 18-19 yrs")) +
  geom_line(aes( y=`2 20-24`, colour = "rate 20-24 yrs")) +
  geom_line(aes( y=`3 25-29`, colour = "rate 25-29 yrs")) +
  geom_line(aes( y=`4 30-34`, colour = "rate 30-34 yrs")) +
    geom_line(aes( y=`5 35-39`, colour = "rate 35-39 yrs")) +
    geom_line(aes( y=`7 >=40`, colour = "rate 40+ yrs")) +
   labs(title = "Case rate per 100,000 in pregnant women, by age group", colour = "Age group") +
  ylab("cases per 100k") + xlab("Month" )
p

```
##Rate by SIMD
Rate of COVID-19  in pregnant women, by maternal deprivation level and month, Scotland

```{r rate_simd, echo=FALSE, warning=FALSE, fig.width=9, fig.height=4}
table3 <- temp$rate_in_pregnancy_simd
name_tabs <- table3$simd
table3 <- table3 %>% select(-simd)
table3 <- as.data.frame(t(table3))
names(table3) <- name_tabs
table3$month <-  as.Date(paste0( "01 ",rownames(table3)), "%d %b %Y" )
table3 <- table3 %>% filter(!is.na(month))

p <- ggplot(data=table3,aes(x=month)) +
  geom_line(aes( y=`1=most deprived`,  colour = "1 = most deprived")) +
  geom_line(aes( y=`2`, colour = "2")) +
  geom_line(aes( y=`3`, colour = "3")) +
  geom_line(aes( y=`4`, colour = "4")) +
    geom_line(aes( y=`5=least deprived`, colour = "5 = least deprived")) +
   labs(title = "Case rate per 100,000 in pregnant women, by deprivation", colour = "SIMD") +
  ylab("cases per 100k") + xlab("Month" )
p
rm(table3)
```
##Rate by board
Rate of COVID-19  in pregnant women, by maternal NHS Board of residence and month, Scotland


```{r rate_hb, echo=FALSE,fig.width=9, fig.height=4, warning=FALSE}
tab4 <- temp$rate_in_pregnancy_hbres#??? need to resave files?
name_tabs <- tab4 $hbres
tab4  <- tab4  %>% select(-hbres)
tab4  <- as.data.frame(t(tab4 ))
names(tab4 ) <- name_tabs
tab4 $month <-  as.Date(paste0( "01 ",rownames(tab4 )), "%d %b %Y" )
tab4  <- tab4 %>% filter(!is.na(month))

p <- ggplot(data=tab4,aes(x=month)) +
  geom_line(aes( y=`Ayrshire and Arran`,  colour = "Ayrshire & Arran")) +
  geom_line(aes( y=`Borders`, colour = "Borders")) +
  geom_line(aes( y=`Dumfries and Galloway`, colour = "D &G")) +
  geom_line(aes( y=`Fife`, colour = "Fife")) +
  geom_line(aes( y=`Forth Valley`, colour = "Forth Valley")) +
   geom_line(aes( y=`Greater Glasgow and Clyde`, colour = "GG&C")) +
   geom_line(aes( y=`Lothian`, colour = "Lothian")) +
   geom_line(aes( y=`Tayside`, colour = "Tayside")) +
   labs(title = "Case rate per 100,000 in pregnant women, by HB residence", colour = "NHS board of residence") +
  ylab("cases per 100k") + xlab("Date" )
p
rm(tab4)
```

##Rate by ethnicity
Rate of COVID-19  in pregnant women, by maternal ethnicity and month, Scotland

```{r rate_simd2, echo=FALSE, warning=FALSE, fig.width=9, fig.height=4}
table3 <- temp$rate_in_pregnancy_ethnicity
name_tabs <- table3$ethnicity_desc_reporting
table3 <- table3 %>% select(-ethnicity_desc_reporting)
table3 <- as.data.frame(t(table3))
names(table3) <- name_tabs
table3$month <-  as.Date(paste0( "01 ",rownames(table3)), "%d %b %Y" )
table3 <- table3 %>% filter(!is.na(month))

p <- ggplot(data=table3,aes(x=month)) +
  geom_line(aes( y=`1 White`,  colour = "White")) +
  geom_line(aes( y=`2 South Asian`, colour = "South Asian")) +
  geom_line(aes( y=`3 Black/Caribbean/African`, colour = "Black/Caribbean/African")) +
  geom_line(aes( y=`4 Other or mixed ethnicity`, colour = "Other or mixed")) +
    geom_line(aes( y=`5 Unknown/missing`, colour = "unknown/missing")) +
   labs(title = "Case rate per 100,000 in pregnant women, by ethnicity", colour = "Ethnicity") +
  ylab("cases per 100k") + xlab("Month" )
p
#rm(table3)
```


##Number of cases by gestation

Number of COVID-19 cases in pregnant women, by stage of pregnancy and month, Scotland

```{r n_by_gestation, echo=FALSE, warning=FALSE}
tab5 <- temp$cases_by_gestation

tab5 <-  tab5 %>% filter(trimester!="Total") %>% select(-Total, -admission)
name_tabs <- tab5$trimester
tab5 <- tab5 %>% select(-trimester)
tab5 <- as.data.frame(t(tab5))
names(tab5) <- name_tabs
tab5$month <-  as.Date(paste0( "01 ",rownames(tab5)), "%d %b %Y" )
tab5 <- tab5 %>% filter(!is.na(month))

p <- ggplot(data=tab5,aes(x=month)) +
  geom_line(aes( y=`1 - first-trimester`,  colour = "1st")) +
  geom_line(aes( y=`2 - second-trimester`, colour = "2nd")) +
  geom_line(aes( y=`3 - third-trimester`, colour = "3rd")) +
   labs(title = "Cases in pregnant women, by trimester", colour = "trimester") +
  ylab("cases ") + xlab("Month" )
p
rm(tab5)
```

## Admissions by gestation

An episode of COVID-19 is defined as associated with hospital admission if the woman is admitted to hospital ≤14 days after the date of onset of COVID-19, or if the date of onset occurs at any point during a hospital admission (date of admission to date of discharge inclusive).Admissions to maternity units (SMR02) and general acute units (SMR01) are both included.

An episode of COVID-19 is defined as associated with critical care admission if the woman is admitted to critical care ≤21 days after the date of onset of COVID-19, or if the date of onset occurs at any point during a critical care admission (date of admission to date of discharge inclusive).Admissions to all intensive care units, all general high dependency units, and the 7 obstetric high dependency units that submit data to SICSAG are included.


```{r admit_by_gestation, echo=FALSE, fig.width=9, fig.height=4,warning=FALSE}
tab5 <- temp$cases_by_gestation %>% select(-Total, -admission) %>% pivot_longer(cols =-trimester, names_to = "month", values_to = "n_cases") %>% mutate(month = as.Date(paste0( "01 ",month), "%d %b %Y")) %>% mutate(month = substr(as.character(month),1,7))
                                                                                                                                             
tab6a <- temp$admissions_by_trimester %>% filter(trimester !="Total")
tab6b <- temp$icu_admissions_by_trimester %>% filter(trimester !="Total") %>% rename(n_covid_icu = n_covid_adm)

p <- ggplot(data=tab6a,aes(x=month, y=n_covid_adm, group=trimester, colour= trimester)) +
  geom_line()+
   labs(title = "N of hospital admissions in pregnant women with covid19, by trimester", colour = "Trimester") +
  ylab("Admissions") + xlab("Date" )
p
p <- ggplot(data=tab6b,aes(x=month, y=n_covid_icu, group=trimester, colour= trimester)) +
  geom_line()+
   labs(title = "N of ICU admissions in pregnant women with covid19, by trimester", colour = "Trimester") +
  ylab("Admissions") + xlab("Date" )
p


tab6 <- left_join(tab5, select(tab6a,-admission)) 
tab6 <- left_join(tab6, select(tab6b,-admission)) 
tab6 <- tab6 %>% mutate(n_covid_adm = replace_na(n_covid_adm ,0), n_covid_icu = replace_na(n_covid_icu,0) ) %>%
  mutate(adm_prop = n_covid_adm/n_cases, icu_prop = n_covid_icu/n_cases) %>% filter(trimester!="Total")

p <- ggplot(data=tab6,aes(x=month, y= adm_prop, group=trimester, colour= trimester)) +
  geom_line()+
   labs(title = "Proportion of cases with associated admissions in pregnant women with covid19, by trimester", colour = "Trimester") +
  ylab("proportion of cases with hospital admission") + xlab("Date" )
p

p <- ggplot(data=tab6,aes(x=month, y= icu_prop, group=trimester, colour= trimester)) +
  geom_line()+
   labs(title = "Proportion of cases with associated ICU admissions in pregnant women with covid19, by trimester", colour = "Trimester") +
  ylab("proportion of cases with icu admission") + xlab("Date" )
p

##% of cases leading to admission, by trimester

rm(tab6, tab6a)
```

##N cases by vacc status

Number of COVID-19 cases in pregnant women, by vaccination status and month, Scotland

```{r cases_by_vacc, echo=FALSE, warning=FALSE}
tab7<- temp$cases_by_vaccination

name_tabs <- tab7$vaccination_status_at_infection
tab7 <- tab7 %>% select(-vaccination_status_at_infection)
tab7 <- as.data.frame(t(tab7))
names(tab7) <- name_tabs
tab7$month <-  as.Date(paste0(rownames(tab7), "-01"), "%Y-%m-%d" )
tab7 <- tab7 %>% filter(!is.na(month))

p <- ggplot(data=tab7,aes(x=month)) +
  geom_line(aes( y=`0 - Unvaccinated`,  colour = "0 - Unvaccinated")) +
  geom_line(aes( y=`1 - One dose`, colour = "1 - One dose")) +
  geom_line(aes( y=`2 - Two doses`, colour = "2 - Two doses")) +
  geom_line(aes( y=`3 - Three doses`, colour = "3 - Three doses")) +
   labs(title = "Cases in pregnant women, by vaccination status", colour = "Vacc status") +
  ylab("cases ") + xlab("Month" )
p

rm(tab7)
```
###Admissions by vaccination status

An episode of COVID-19 is defined as associated with hospital admission if the woman is admitted to hospital ≤14 days after the date of onset of COVID-19, 
or if the date of onset occurs at any point during a hosptial admission (date of admission to date of discharge inclusive).
Admissions to maternity units (SMR02) and general acute units (SMR01) are both included.

An episode of COVID-19 is defined as associated with critical care admission if the woman is admitted to critical care ≤21 days after the date of onset of COVID-19, 
or if the date of onset occurs at any point during a critical care admission (date of admission to date of discharge inclusive).
Admissions to ICU and HDU (SICSAG) are both included.


```{r adm_by_vacc, echo=FALSE, warning=FALSE}
tab7<- temp$cases_by_vaccination %>% select(-Total) %>% pivot_longer(cols = -vaccination_status_at_infection, names_to = "month", values_to = "n_cases")
tab7b <- temp$admissions_by_vaccination 

tab7b <-tab7b %>% filter(vaccination_status_at_infection !="Total")%>% mutate(monthdate  = paste0(month, "-01")) %>%
  mutate(monthdate = as.Date(monthdate, "%Y-%m-%d"))%>%
  group_by(month ) %>%
  mutate(n = sum(n_covid_adm)) %>% ungroup() %>%
  mutate(percentage = n_covid_adm /n) %>% ungroup() %>%
  mutate(vaccination_status_at_infection = fct_relevel(vaccination_status_at_infection, 
           "3 - Three doses",  "2 - Two doses","1 - One dose",  "0 - Unvaccinated" ))

tab7c <- temp$icu_admissions_by_vaccination %>% filter(vaccination_status_at_infection !="Total")%>% mutate(monthdate  = paste0(month, "-01")) %>%
  mutate(monthdate = as.Date(monthdate, "%Y-%m-%d"))%>%
  rename(n_covid_icu = n_covid_adm) %>% 
  group_by(month ) %>%
    mutate(n = sum(n_covid_icu)) %>% ungroup() %>%
  mutate(percentage = n_covid_icu /n) %>% ungroup()%>%
  mutate(vaccination_status_at_infection = fct_relevel(vaccination_status_at_infection, 
           "3 - Three doses",  "2 - Two doses","1 - One dose",  "0 - Unvaccinated" ))
 

p <- ggplot(data=tab7b) +
   geom_line(aes(x=monthdate,y=n_covid_adm, colour=vaccination_status_at_infection)) +
   labs(title = "N of hospital admissions in pregnant women with covid19, by vaccination status", colour = "vaccination status at infection") +
  ylab("Admissions") + xlab("Date" )
p
p <- ggplot(data=tab7c) +
   geom_line(aes(x=monthdate,y=n_covid_icu, colour=vaccination_status_at_infection)) +
   labs(title = "N of ICU admissions in pregnant women with covid19, by vaccination status", colour = "vaccination status at infection") +
  ylab("Admissions") + xlab("Date" )
p


tab7 <- left_join(tab7, select(tab7b, c(month, vaccination_status_at_infection, n_covid_adm)))
tab7 <- left_join(tab7, select(tab7c, c(month, vaccination_status_at_infection, n_covid_icu)))
 
tab7 <- tab7 %>% mutate(n_covid_adm = replace_na(n_covid_adm ,0), n_covid_icu = replace_na(n_covid_icu,0) ) %>%
  mutate(adm_prop = n_covid_adm/n_cases, icu_prop = n_covid_icu/n_cases) %>% filter(vaccination_status_at_infection!="Total")

p <- ggplot(data=tab7,aes(x=month, y= adm_prop, group=vaccination_status_at_infection, colour= vaccination_status_at_infection)) +
  geom_line()+
   labs(title = "Proportion of C19 cases with associated admissions in pregnant women, by vaccination_status_at_infection", colour = "Vaccination status") +
  ylab("proportion of cases with hospital admission") + xlab("Date" )
p

p <- ggplot(data=tab7,aes(x=month, y= icu_prop, group=vaccination_status_at_infection, colour= vaccination_status_at_infection)) +
  geom_line()+
   labs(title = "Proportion of covid19 cases with associated ICU admissions in pregnant women, by vaccination status", colour = "Vaccination status") +
  ylab("proportion of cases with icu admission") + xlab("Date" )
p
rm(tab7, tab7b, tab7c)
```


##Severe outcomes cases by gest NFP

Number of confirmed cases of COVID-19 in pregnant women with severe outcomes, by stage of pregnancy and month, Scotland

```{r severe_oc_gest, echo=FALSE, warning=FALSE}
tab8 <- temp$infection_severe_outcomes_totals
tab8a <- temp$infection_severe_outcomes_gest


severe <- tab8a %>% select(trimester, indicator, Total)
pregnancy_rows <- severe$indicator[grep("pregnancies", severe$indicator)]
severe_total <- tab8
severe <- severe %>% filter(!(indicator %in% pregnancy_rows)) %>%
  group_by(indicator) %>% mutate(total_outcomes_in_cohort = sum(Total)) %>%
  ungroup() %>% mutate(proportion_of_outcome_by_trimester = paste0(round(Total/total_outcomes_in_cohort*100,1), " %")) %>% arrange(indicator)

knitr::kable(severe, caption= "Proportion  of outcomes in cohort that occurred by trimester", col.names = c("Trimester", "Outcome", "N", "N of outcome accross all stages", "Proportion of outcome /nin this trimester")) %>%
column_spec (c(1), border_right = T) %>%
kable_styling()

```
##Severe outcomes cases by vacc NFP

Number of confirmed cases of COVID-19 in pregnant women with severe outcomes, by vaccination status and month, Scotland

```{r severe_oc_vacc, echo=FALSE, warning=FALSE}
tab9 <- temp$infection_severe_outcomes_totals
tab9a <- temp$infection_severe_outcomes_vacc
severe <- tab9a %>% select(vaccination_status_at_infection, indicator, Total)
pregnancy_rows <- severe$indicator[grep("pregnancies", severe$indicator)]
severe_total <- tab9
severe <- severe %>% filter(!(indicator %in% pregnancy_rows)) %>%
  group_by(indicator) %>% mutate(total_outcomes_in_cohort = sum(Total)) %>%
  ungroup() %>% mutate(proportion_of_outcome_by_vacc_status = paste0(round(Total/total_outcomes_in_cohort*100,1), " %")) %>% arrange(indicator)

knitr::kable(severe, caption= "Proportion  of outcomes in cohort that occurred by vaccination status", col.names = c("Vaccine status", "Outcome", "N", "N of outcome across all statuses", "Proportion of outcome for this status")) %>%
column_spec (c(1), border_right = T) %>%
kable_styling()


```


## Infections in neonates

```{r, echo=FALSE}
 neonates_rates<- readRDS(paste0(folder_temp_data, "neonate_output_tables/neonates_covid_rates.rds")) %>% select(-contains("Total"))
 neonates_rates <- neonates_rates %>% pivot_longer(-indicator)
neonates_n <- neonates_rates %>% filter(indicator=="total_positive_covid_neonates")

p <- ggplot(data=neonates_n) +
  geom_line(aes(x=name, y= value, group=1))+
   labs(title = "N infections in neonates") +
  ylab("N cases") + xlab("Date" )
p

neonates_r <- neonates_rates %>% filter(indicator=="Rate of COVID-19 in neonates (per 100,000 babies)")

p <- ggplot(data=neonates_r) +
  geom_line(aes(x=name, y= value, group=1))+
   labs(title = "Rate of infections in neonates") +
  ylab("N cases per 100,000") + xlab("Date" )
p

```

```{r, echo=FALSE}
neonate_mat_vacc <-readRDS(paste0(folder_temp_data, "folder/maternal_vaccination_status_long.rds"))
neonate_mat_vacc <- neonate_mat_vacc %>% pivot_longer(-month)

p <- ggplot(data=neonate_mat_vacc,aes(x=month, y= value, group=name, colour= name)) +
  geom_line() +
   labs(title = "Infections in neonates, by mother vaccination status", colour = "Trimester") +
  ylab("N neonatal infections") + xlab("Date" )
p
```
