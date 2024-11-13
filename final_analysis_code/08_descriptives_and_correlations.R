library(tidyverse)
library(xtable)
library(psych)

options(scipen=5)

critical_mahalanobis_gsr = 250.627
critical_mahalanobis_ddm = 225.145
critical_mahalanobis_levy = 257.137

outcomes20032023 <- read_csv("C:/Users/misch/Downloads/outcomes20032023.csv")

df_gsr <- read_csv("C:/Users/misch/Downloads/df_gsr.csv") %>% 
  filter(mahalanobis_distance < critical_mahalanobis_gsr  & age>29 & age < 71)

df_sv <- read_csv("C:/Users/misch/Downloads/df_sv_st0_ddm.csv") %>% 
  filter(mahalanobis_distance < critical_mahalanobis_ddm  & age>29 & age < 71)

df_levy <- read_csv("C:/Users/misch/Downloads/df_levy.csv") %>% 
  filter(mahalanobis_distance < critical_mahalanobis_levy  & age>29 & age < 71)

# Create latex descriptives tables

print(xtable(describe(df_sv %>% select( `$\\nu_{c}$` = v_congruent,
                                        `$\\nu_{i}$` = v_incongruent,
                                        `$a_{c}$` = a_congruent,
                                        `$a_{i}$` = a_incongruent,
                                        `$\\tau_{+}$` = tplus,
                                        `$\\tau_{-}$` = tminus,
                                        `$s_\\tau$` = st0,
                                        `$s_\\nu$` = sv) %>%
                        drop_na(), quant = c(.05,.95)) %>%
               select(Median = median, Q0.05, Q0.95) %>% 
               mutate(Median = round(Median,2),
                      CI1 = paste0("[", round(Q0.05,2), "-"),
                      CI2 = paste0(round(Q0.95,2), "]")) %>% 
                        select(Median, CI1, CI2) %>% t()),
             
               
      sanitize.colnames.function = identity,
      sanitize.text.function = identity)

print(xtable(describe(df_levy %>% select( `$\\nu_{congruent}$` = v_congruent,
                                          `$\\nu_{incongruent}$` = v_incongruent,
                                          `$a_{congruent}$` = a_congruent,
                                          `$a_{incongruent}$` = a_incongruent,
                                          `$\\tau_{correct}$` = tplus,
                                          `$\\tau_{error}$` = tminus,
                                          `$\\alpha$` = alpha) %>%
                        drop_na(), quant = c(.05,.95)) %>%
               select(Median = median, Q0.05, Q0.95) %>% 
               mutate(Median = round(Median,2),
                      CI1 = paste0("[", round(Q0.05,2), "-"),
                      CI2 = paste0(round(Q0.95,2), "]")) %>% 
               select(Median, CI1, CI2) %>% t()),
      
      
      sanitize.colnames.function = identity,
      sanitize.text.function = identity)

print(xtable(describe(df_gsr %>% select(`$\\gamma$`=similarity,
                                        `$\\nu_{picture}$` = v_picture,
                                        `$\\nu_{word}$` = v_word,
                                        `$a_{congruent}$` = a_congruent,
                                        `$a_{incongruent}$` = a_incongruent,
                                        `$\\tau_{correct}$` = tplus,
                                        `$\\tau_{error}$` = tminus,
                                        `$s_\\tau$` = st0,
                                        `$s_\\nu$` = sv) %>%
                        drop_na(), quant = c(.05,.95)) %>%
               select(Median = median, Q0.05, Q0.95) %>% 
               mutate(Median = round(Median,2),
                      CI1 = paste0("[", round(Q0.05,2), "-"),
                      CI2 = paste0(round(Q0.95,2), "]")) %>% 
               select(Median, CI1, CI2) %>% t()),
      
      
      sanitize.colnames.function = identity,
      sanitize.text.function = identity)


# Correlation tables

print(xtable(summary((correlation::correlation(df_sv %>% select( `$\\nu_{congruent}$` = v_congruent,
                                                                 `$\\nu_{incongruent}$` = v_incongruent,
                                                                 `$a_{congruent}$` = a_congruent,
                                                                 `$a_{incongruent}$` = a_incongruent,
                                                                 `$\\tau_{correct}$` = tplus,
                                                                 `$\\tau_{error}$` = tminus,
                                                                 `$s_\\tau$` = st0,
                                                                 `$s_\\nu$` = sv) %>%
                                                 drop_na(),
                                               bayesian=T, redundant=T))) %>% 
               remove_rownames %>% column_to_rownames(var="Parameter") %>% sjmisc::rotate_df()
             ), sanitize.colnames.function = identity, sanitize.text.function = identity)


print(xtable(summary((correlation::correlation(df_levy %>% select( `$\\nu_{congruent}$` = v_congruent,
                                                                   `$\\nu_{incongruent}$` = v_incongruent,
                                                                   `$a_{congruent}$` = a_congruent,
                                                                   `$a_{incongruent}$` = a_incongruent,
                                                                   `$\\tau_{correct}$` = tplus,
                                                                   `$\\tau_{error}$` = tminus,
                                                                   `$\\alpha$` = alpha) %>%
                                                 drop_na(),
                                               bayesian=T, redundant=T))) %>% 
               remove_rownames %>% column_to_rownames(var="Parameter") %>% sjmisc::rotate_df()
             ), sanitize.colnames.function = identity, sanitize.text.function = identity)

print(xtable(summary((correlation::correlation(df_gsr %>% select(`$\\gamma$`=similarity,
                                                                 `$\\nu_{picture}$` = v_picture,
                                                                 `$\\nu_{word}$` = v_word,
                                                                 `$a_{congruent}$` = a_congruent,
                                                                 `$a_{incongruent}$` = a_incongruent,
                                                                 `$\\tau_{correct}$` = tplus,
                                                                 `$\\tau_{error}$` = tminus,
                                                                 `$s_\\tau$` = st0,
                                                                 `$s_\\nu$` = sv) %>%
                                                 drop_na(),
                                               bayesian=T, redundant=T))) %>% 
               remove_rownames %>% column_to_rownames(var="Parameter") %>% sjmisc::rotate_df()
             ), sanitize.colnames.function = identity, sanitize.text.function = identity)
