library(haven)
library(tidyverse)
library(labelled)


df_total <- tibble()
for (year in 2003:2014) {
  df_year <- read_sav(paste0("data/outcomes/osf/Race IAT.public.", year,".sav"), encoding="latin1") %>% 
            filter(session_status=="C") %>% 
    select_if(names(.) %in% c('session_id', 'age', 'birthyear','sex',
                              'Mn_RT_all_3467', 'country', 'countryres',
                              'edu', 'STATE',  'politicalid_6', 'politicalid_7', "year", "month", "day", "q4", "major", "occupation", "num")) %>% 
    mutate(file=year)
  df_total <- df_total %>% bind_rows(df_year)
  print(year)
  rm(df_year)
}

  df_year <- read_sav(
    paste0("data/outcomes/osf/Race IAT.public.2015.sav"),
    col_select = c('session_id', 'age', 'birthyear',  'sex', 'sex_5',
                   'Mn_RT_all_3467',  'countryres',
                   'edu', 'STATE', 'session_status', 'politicalid_7', "year", "month", "day", "q4", "major", "occupation", "occuself",
                   "occuselfdetail", "wealthself", "incomeself", "employment", "occupation_self", "num")) %>% 
    filter(session_status=="C") %>% 
    mutate(file=2015)
  df_total <- df_total %>% bind_rows(df_year)
  print(2015)
  rm(df_year)
  

  for (year in c(2016)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'sex_5', 'birthsex',
                     'Mn_RT_all_3467',  'countryres',
                     'edu', 'STATE', 'session_status', 'politicalid_7',"year", "month", "day", "q4", "broughtwebsite", "occuself",
                     "occuselfdetail", "wealthself",  "incomeself",  "employment",
                     "occupation_self", "occupation_self_002", "occupation_selfdetail_001", "num_002", "num")) %>% 
      filter(session_status=="C") %>% 
      mutate(file=year)
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}

  for (year in c(2017:2018)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'birthsex',
                     'Mn_RT_all_3467',  'countryres_num',
                     'edu', 'STATE', 'session_status', 'politicalid_7', "year", "month", "day","broughtwebsite",
                      "occupation_self_002", "occupation_selfdetail_001", "num_002")) %>% 
      filter(session_status=="C") %>% 
      mutate(file=year)
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}
  
  for (year in c(2019)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'birthSex',
                     'Mn_RT_all_3467',  'countryres_num',
                     'edu', 'STATE', 'session_status',  'politicalid_7', "year", "month", "day","broughtwebsite",
                     "occuSelf" ,"occupation_self_002", "occuSelfDetail", "num_002", "num002" ))  %>% 
      filter(session_status=="C") %>% 
      mutate(file=year)
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}
  
  for (year in c(2020:2021)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'birthSex',
                     'Mn_RT_all_3467',  'countryres_num',
                     'edu', 'STATE', 'session_status',  'politicalid_7', "year", "month", "day","broughtwebsite",
                     "occuSelf" ,"occupation_self_002", "occuSelfDetail", "num_002", "num002" )) %>% 
      filter(session_status=="C") %>% 
      mutate(file=year)
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}
  
  
  for (year in c(2022)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'birthSex', 
                     'Mn_RT_all_3467',  'countryres_num',
                     'edu', 'STATE', 'session_status',  'politicalid_7',"year", "month", "day","broughtwebsite",
                     "occuSelf" ,"occupation_self_002", "occuSelfDetail", "num_002", "num002" )) %>% 
      filter(session_status %in% c("C", "L", "R")) %>% 
      mutate(file=year) %>% mutate(birthyear = as.numeric(birthyear)) %>% mutate(birthSex = as.numeric(birthSex))
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}

  
  for (year in c(2023)) {
    df_year <- read_sav(
      paste0("data/outcomes/osf/Race IAT.public.",year,".sav"),
      col_select = c('session_id', 'birthyear',  'birthSex', "genderidentity" , "genderIdentity_0002"     ,  "transIdentity"   ,
                     'Mn_RT_all_3467',  'countryres_num',
                     'edu', 'STATE', 'session_status',  'politicalid_7',"year", "month", "day","broughtwebsite",
                     "occuSelf" ,"occupation_self_002", "occuSelfDetail", "num_002", "num002" )) %>% 
      filter(session_status %in% c("C", "L", "R")) %>% 
      mutate(file=year) %>% mutate(birthyear = as.numeric(birthyear)) %>% mutate(birthSex = as.numeric(birthSex))
    df_total <- df_total %>% bind_rows(df_year)
    print(year)
    rm(df_year)}

df_total_save <- df_total    
  
df_total <- df_total_save %>% 
  mutate(birthyear= if_else(!is.na(age) & age>1900, age, birthyear)) %>% 
  mutate(birthyear= if_else(!is.na(age)& age<1900, file-age, birthyear)) %>%
  mutate(birthsex= if_else(!is.na(birthSex), birthSex, birthsex)) %>% 
  mutate(birthsex= if_else(!is.na(sex_5), sex_5, birthsex)) %>% 
  mutate(birthsex= if_else((!is.na(sex)&sex=="m"), 1, birthsex)) %>% 
  mutate(birthsex= if_else((!is.na(sex)&sex=="f"), 2, birthsex)) %>%
  mutate(is_us=countryres) %>% 
  mutate(is_us = if_else((!is.na(country)&country=="US"), "1", is_us)) %>% 
  mutate(is_us = if_else((!is.na(country)&country!="US"), "other country", is_us)) %>% 
  mutate(countryres= if_else(!is.na(country), as.character(country), countryres)) %>% 
  mutate(countryres= if_else(!is.na(countryres_num), as.character(countryres_num), countryres)) %>% 
  filter(birthyear > 1900) %>% 
  mutate(edu = if_else((file==2003 & edu == 9), NA, edu)) %>% # for year 2003 edu==9 means ?????
  mutate(age=file-birthyear) %>% 
  mutate(monthyear = make_date(year, month),
                       monthyearday = make_date(year, month, day),
                      weekday = wday(monthyearday)) %>%
  mutate(q4= to_character(q4)) %>% 
  mutate(broughtwebsite = tolower(broughtwebsite)) %>% 
    mutate(broughtwebsite= if_else(is.na(broughtwebsite), q4, broughtwebsite)) %>% 
   mutate(complete_num=if_else(is.na(num),
                                  case_match(num_002, 1 ~ "0", 2 ~ "1",
                                             3 ~ "2", 4 ~ "3-5", 5 ~ "6+", .default = NA),
                                  num))

write_csv(df_total, "data/outcomes/outcomes20032023.csv")  


# write age and id only


df_age <- df_total %>% 
  mutate(id2=session_id) %>% select(session_id, id2, age)

df_age <- df_age[!duplicated(df_age$session_id),]
write_csv(df_age,"data/outcomes/outcomes20032023_id_age_only.csv" )



                                                