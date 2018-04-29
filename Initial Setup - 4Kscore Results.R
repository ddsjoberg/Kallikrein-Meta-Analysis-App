# this program sets up the data for the Shiny app
library(dplyr)
here::here()

#  importing the data
data = list()
data[["meta.auc"]] = readxl::read_xlsx("HTML Full Data Final.xlsx", sheet = 4) %>%
  mutate(
    cohort = case_when(
      cohort == "Overall (fixed effects estimat" ~ "Overall (fixed effects estimate)",
      cohort == "Overall (random effects estima" ~ "Overall (random effects estimate)",
      TRUE ~ cohort
    ),
    cohort.est = paste(cohort, es, sep = ": ")
  )
data[["cohort.n"]] = readxl::read_xlsx("HTML Full Data Final.xlsx", sheet = 1) 
data[["table.one"]] = readxl::read_xlsx("HTML Full Data Final.xlsx", sheet = 2)
data[["table.one.cohort"]] = readxl::read_xlsx("HTML Full Data Final.xlsx", sheet = 3) 


psa.ranges = 
  data$cohort.n %>%
  select(psalo, psahi) %>%
  distinct %>%
  mutate(psa.range = paste(psalo, "to", psahi))
age.ranges = 
  data$cohort.n %>%
  select(agelo, agehi) %>%
  distinct %>%
  mutate(age.range = paste(agelo, "to", agehi))  
dre.set = 
  data$cohort.n %>%
  select(negdre) %>% 
  distinct %>%
  mutate(dre.set = case_when(
    negdre == 0 ~ "Positive and Negative DRE",
    negdre == 1 ~ "Negative DRE"
  ))

contemp.set = 
  data$cohort.n %>%
  select(contemporary) %>%
  distinct %>%
  mutate(contemp.set = case_when(
    contemporary == 0 ~ "All Cohorts",
    contemporary == 1 ~ "Contemporary Cohorts Only"
  ))  

cohort.full = 
  data$cohort.n %>%
  select(cohort) %>%
  distinct %>%
  mutate(cohort.full = case_when(
    cohort == "Finnish ERSPC 1" ~ "ERSPC Finland: Screening Round 1",
    cohort == "Finnish ERSPC 2-3" ~ "ERSPC Finland: Screening Rounds 2 & 3",
    cohort == "Goteborg 2" ~ "ERSPC Goteborg, Sweden: Screening Round 2",
    cohort == "Opko" ~ "4Kscore Prospective Validation Study",
    cohort == "Rotterdam 2-3" ~ "ERSPC Rotterdam, The Netherlands: Screening Rounds 2 & 3",
    cohort == "Tarn" ~ "Tarn, France Clinical Biopsy Cohort",
    cohort == "UPCA" ~ "Malmo, Sweden Clinical Biopsy Cohort",
    cohort == "Veterans Affairs" ~ "Southern US Veterans Affairs Cohort"
  ))  


# adding in the formatted filters to each set
data[["cohort.n"]] = data[["cohort.n"]] %>% 
  left_join(psa.ranges) %>% left_join(age.ranges) %>% left_join(dre.set) %>% left_join(contemp.set) %>% left_join(cohort.full)  

data[["table.one"]] = data[["table.one"]] %>%
  left_join(psa.ranges) %>% left_join(age.ranges) %>% left_join(dre.set) %>% left_join(contemp.set)  
  
data[["table.one.cohort"]] = data[["table.one.cohort"]] %>%
  left_join(psa.ranges) %>% left_join(age.ranges) %>% left_join(dre.set) %>% left_join(contemp.set) %>% left_join(cohort.full)  %>%
  group_by(cohort, psa.range, age.range, dre.set, contemp.set) %>%
  mutate(cohort.disp = ifelse(1:n() == 1, cohort, NA)) %>%
  ungroup

data[["meta.auc"]] = data[["meta.auc"]] %>%
  left_join(psa.ranges) %>% left_join(age.ranges) %>% left_join(dre.set) %>% left_join(contemp.set) %>% left_join(cohort.full) 



psa.range.list = psa.ranges$psa.range %>%
  purrr::map(., ~ as.character(list(.x)))
names(psa.range.list) = psa.ranges$psa.range

age.range.list = age.ranges$age.range %>%
  purrr::map(., ~ as.character(list(.x)))
names(age.range.list) = age.ranges$age.range

dre.set.list = dre.set$dre.set %>%
  purrr::map(., ~ as.character(list(.x)))
names(dre.set.list) = dre.set$dre.set

contemp.set.list = list()
contemp.set.list$'All Cohorts' = 0
contemp.set.list$'Contemporary Cohorts Only' = 1



save.image("data.RData")  

