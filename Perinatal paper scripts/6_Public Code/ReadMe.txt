Here is the code used to analyse paper: 
Baby and maternal outcomes following SARS-CoV-2 infection and COVID-19 vaccination during pregnancy: a national population-based matched cohort study, The Lancet RH Europe 

---------------------------------------------------------------------------------------------------

 - maternal-outcomes-linkage.R - uses the data from COPS refresh that uses mother upi to link up to hospital records to flag of interest maternal outcomes (vte, pregnancy-related bleeding, hypertension, icu/death) 

Analysis Code folder
--------------------

- 00-setup- loads in the cohort data and applies fixes to the data /removes data where infections occured before study start date 
- 01-Descriptive-Analysis-baby-outcomes.Rmd - calculates characteristics for each baby outcomes cohort (infected and vaccinated)
- 02a-statistical analyses-baby-outcomes-infection.R - Main statistical analyses for baby outcomes infected cohorts
- 02b-statistical-analyses-baby-outcomes-vaccination.R - Main statistical analyses for baby outcomes vaccinated cohorts
- 02c-supplementary1-statistical-analyses-baby-outcomes-infection.R - statistical analyses for infection < 20 weeks gestation
- 02d-supplementary2-statistical-analyses-baby-outcomes-infection.R - statistical analyses for infection > 20 weeks gestation
- 03-Descriptive-Analysis-maternal-outcomes.Rmd - calcualtes characteristics for each maternal outcomes cohort (infected and vaccinated)
- 04a-statistical analyses-maternal-outcomes-infection.R - Main statistical analyses for maternal outcomes infected cohorts 
- 04b-statistical-analyses-maternal-outcomes-vaccination.R - Main statistical analyses for maternal outcomes vaccinated cohort
- 04c-supplementary1-statistical-analyses-baby-outcomes-infection.R - statistical analyses for maternal outcomes infected cohorts < 20 weeks gestation 
- 04d-supplementary2-statistical-analyses-baby-outcomes-infection.R - statistical analyses for maternal outcomes infected cohorts > 20 weeks gestation 
- calculated-controls-pre-matching.R - calculates characteristics of the unexposed controls pre-matching 
- preterm_lag.R - calculates the lag between infection and delivery onset in preterm and very preterm cohorts 
- subgroup_analysis_forest_plot.R - creates the forest plot for the subgroup analyses  
-----------------------------------------------------------------------------------------------------------------
