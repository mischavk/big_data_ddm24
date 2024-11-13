library(tidyverse)
counting_df <- read_csv("C:/Users/misch/Desktop/final_analysis_code/counting_df.csv") %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(across(where(is.numeric), ~(./6127948)*100))

            