# DATA PREPARATION ---- 
library(readr)
library(tidyverse)
library(marginaleffects)
library(cowplot)
library(readxl)
library(scales)
library(haven)
library(dvmisc)
library(ggsci)
library(correlation)
library(brms)


# get model parameters, filter mahalanobis outliers (p <.001) and age <20 and >70

critical_mahalanobis_gsr = 250.627
critical_mahalanobis_ddm = 225.145
critical_mahalanobis_levy = 257.137

df_gsr <- read_csv("C:/Users/misch/Downloads/df_gsr.csv") %>%
  select(similarity:st0, age, session_id, mahalanobis_distance) %>% 
  filter(mahalanobis_distance < critical_mahalanobis_gsr) %>% 
  filter(age>29 & age < 71) %>% 
  mutate(gsr_quantile_v_word = ntile(v_word, 10)) %>% 
  mutate(gsr_quantile_v_picture = ntile(v_picture, 10)) %>% 
  mutate(gsr_quantile_a_congruent = ntile(a_congruent, 10)) %>% 
  mutate(gsr_quantile_a_incongruent = ntile(a_incongruent, 10)) %>% 
  mutate(gsr_quantile_tplus = ntile(tplus, 10)) %>% 
  mutate(gsr_quantile_tminus = ntile(tminus, 10))%>%
  mutate(gsr_quantile_sv = ntile(sv, 10)) %>% 
  mutate(gsr_quantile_st0 = ntile(st0, 10)) %>% 
  mutate(gsr_quantile_similarity = ntile(similarity, 10)) %>%
  mutate(gsr_scale_v_word = scale(v_word)) %>% 
  mutate(gsr_scale_v_picture = scale(v_picture)) %>% 
  mutate(gsr_scale_a_congruent = scale(a_congruent)) %>% 
  mutate(gsr_scale_a_incongruent = scale(a_incongruent)) %>% 
  mutate(gsr_scale_tplus = scale(tplus))  %>% 
  mutate(gsr_scale_tminus = scale(tminus))  %>% 
  mutate(gsr_scale_sv = scale(sv)) %>% 
  mutate(gsr_scale_st0 = scale(st0))%>% 
  mutate(gsr_scale_similarity = scale(similarity)) %>% 
  select(age, session_id, starts_with("gsr_"))%>% 
  distinct(session_id, .keep_all = TRUE) 


df_sv <- read_csv("C:/Users/misch/Downloads/df_sv_st0_ddm.csv") %>%
  select(v_congruent:sv, age, session_id, mahalanobis_distance) %>% 
  filter(mahalanobis_distance < critical_mahalanobis_ddm) %>% 
  filter(age>29 & age < 71) %>% 
  mutate(sv_quantile_v_congruent = ntile(v_congruent, 10)) %>% 
  mutate(sv_quantile_v_incongruent = ntile(v_incongruent, 10)) %>% 
  mutate(sv_quantile_a_congruent = ntile(a_congruent, 10)) %>% 
  mutate(sv_quantile_a_incongruent = ntile(a_incongruent, 10)) %>% 
  mutate(sv_quantile_tplus = ntile(tplus, 10)) %>% 
  mutate(sv_quantile_tminus = ntile(tminus, 10))%>% 
  mutate(sv_quantile_sv = ntile(sv, 10)) %>% 
  mutate(sv_quantile_st0 = ntile(st0, 10))%>% 
  mutate(sv_scale_v_congruent = scale(v_congruent)) %>% 
  mutate(sv_scale_v_incongruent = scale(v_incongruent)) %>% 
  mutate(sv_scale_a_congruent = scale(a_congruent)) %>% 
  mutate(sv_scale_a_incongruent = scale(a_incongruent)) %>% 
  mutate(sv_scale_tplus = scale(tplus)) %>% 
  mutate(sv_scale_tminus = scale(tminus)) %>% 
  mutate(sv_scale_sv = scale(sv))%>% 
  mutate(sv_scale_st0 = scale(st0)) %>% 
  select(age, session_id, starts_with("sv_"))%>% 
  distinct(session_id, .keep_all = TRUE) 

df_levy <- read_csv("C:/Users/misch/Downloads/df_levy.csv") %>%
  select(v_congruent:alpha, age, session_id, mahalanobis_distance) %>% 
  filter(mahalanobis_distance < critical_mahalanobis_levy  & age>29 & age < 71) %>%  
  mutate(levy_quantile_v_congruent = ntile(v_congruent, 10)) %>% 
  mutate(levy_quantile_v_incongruent = ntile(v_incongruent, 10)) %>% 
  mutate(levy_quantile_a_congruent = ntile(a_congruent, 10)) %>% 
  mutate(levy_quantile_a_incongruent = ntile(a_incongruent, 10)) %>% 
  mutate(levy_quantile_tplus = ntile(tplus, 10)) %>% 
  mutate(levy_quantile_tminus = ntile(tminus, 10))%>% 
  mutate(levy_quantile_alpha = ntile(alpha, 10)) %>% 
  mutate(levy_scale_v_congruent = scale(v_congruent)) %>% 
  mutate(levy_scale_v_incongruent = scale(v_incongruent)) %>% 
  mutate(levy_scale_a_congruent = scale(a_congruent)) %>% 
  mutate(levy_scale_a_incongruent = scale(a_incongruent)) %>% 
  mutate(levy_scale_tplus = scale(tplus)) %>% 
  mutate(levy_scale_tminus = scale(tminus)) %>% 
  mutate(levy_scale_alpha = scale(alpha)) %>% 
  select(age, session_id, starts_with("levy_"))%>% 
  distinct(session_id, .keep_all = TRUE) 

# get occupational data

big_data_demographic <- read_csv("C:/Users/misch/Downloads/outcomes20032023.csv",
                                          col_select = c("session_id", "age", "birthsex", "genderIdentity_0002", "transIdentity")) %>%
  unique(.) %>% 
  mutate(birthsex = ifelse((is.na(birthsex) & transIdentity == 2), genderIdentity_0002, birthsex)) %>%
  select(-genderIdentity_0002, -transIdentity) %>% 
  filter(birthsex %in% c(1,2)) %>% 
  mutate(Gender=factor(birthsex, labels=c("male", "female"))) %>% 
  mutate(age_categories = cut(age, breaks = c(29,40,50,60,71))) %>% drop_na()

job <- read_csv("C:/Users/misch/Downloads/job.csv") %>% unique(.) # get job data data

big_data_demographic <- big_data_demographic %>% left_join(job) 

#"OPR Job Rating" -> add as additional variable

OccupationalPrestigeRatings <- read_csv("C:/Users/misch/Downloads/OccupationalPrestigeRatings.csv") 
OccupationalPrestigeRatings <-OccupationalPrestigeRatings %>% select(values=`OPR Job Rating`, labels=`ONET SOC 2018 Code`) 
OccupationalPrestigeRatings <-OccupationalPrestigeRatings[!str_detect(OccupationalPrestigeRatings$labels, "Nov"),]
str_sub(OccupationalPrestigeRatings$labels, 6,7) <- "00"
OccupationalPrestigeRatings <- OccupationalPrestigeRatings %>% group_by(labels) %>% summarize_all(mean, na.rm=T) %>% drop_na()

big_data_prestige <- big_data_demographic %>%
  mutate(prestige= recode(occupation_full,!!!(deframe(OccupationalPrestigeRatings)), .default = 0)) %>% 
  filter(prestige>0)

# join model and demographic data

df_prestige_for_plot <- left_join(big_data_prestige, df_gsr) %>% left_join(df_sv) %>% left_join(df_levy)%>%
  select(prestige, Gender, age, age_categories, contains("quantile")) %>%  drop_na()

df_prestige_grouped <- df_prestige_for_plot %>% pivot_longer(cols=contains("quantile"), names_to="parameter", values_to="quantile") %>%
  group_by(quantile, age_categories, Gender, parameter) %>% 
  summarize(mean = mean(prestige, na.rm=TRUE), se= sd(prestige, na.rm=TRUE)/sqrt(n())) %>% drop_na() %>% 
  mutate(Model = if_else(
    str_detect(parameter, "gsr"), "GSR", if_else(
      str_detect(parameter, "sv"), "DDM", if_else(
        str_detect(parameter, "levy"), "Levy", NA))))  %>% 
  mutate(parameter = str_remove_all(parameter, "gsr_quantile_")) %>% 
  mutate(parameter = str_remove_all(parameter, "sv_quantile_")) %>%
  mutate(parameter = str_remove_all(parameter, "levy_quantile_")) 

df_prestige_grouped$parameter <- factor(df_prestige_grouped$parameter,
                                      levels=c(
                                        "v_congruent","a_congruent", "sv",
                                        "v_incongruent","a_incongruent","st0", 
                                        "v_word","tplus", "alpha", 
                                        "v_picture", "tminus","similarity" ))

variable_names <- c(
  `v_congruent` = "Drift Congruent",
  `v_incongruent` = "Drift Incongruent",
  `v_word` = "Drift Word",
  `v_picture` = "Drift Picture",
  `a_congruent` = "Boundary Congruent",
  `a_incongruent` = "Boundary Incongruent",
  `tplus` = "NDT Correct",
  `tminus` = "NDT Error",
  `alpha` = "Alpha",
  `sv` = "Drift Variability",
  `st0` = "NDT Variability",
  `similarity` = "Similarity")


ggplot(df_prestige_grouped, aes(quantile, mean, color=age_categories, shape=Model, linetype=Model,alpha=Gender)) +
  geom_point(size=1.1) +  geom_errorbar(aes(quantile,ymin=mean-se,ymax=mean+se),width=0.0, alpha=.3) +
  geom_line(aes(group=interaction(age_categories,Gender, Model)))  +  theme_minimal(base_size = 7) +
  facet_wrap(~parameter, scales="fixed", nrow=3, dir="v",
             labeller = as_labeller(variable_names))+
  scale_colour_npg(labels = c("30-40", "40-50", "50-60", "60-70")) +
  ylab("Job Prestige") + xlab("Quantile")+
  guides(color=guide_legend("Age groups"))+   scale_x_continuous(breaks =c(1,3,5,7,9), label =paste0("Q", c(1,3,5,7,9)))+ scale_alpha_discrete(range = c(0.4, 1))+
   coord_cartesian(ylim=c(54,59.5))

ggsave("full_prestige_plot_v4.png", width=6, height=4.5, dpi=600, bg="white")

# MODELS ----

df_prestige_for_modeling_scaled <- left_join(big_data_prestige, df_gsr) %>% left_join(df_sv) %>% left_join(df_levy)%>%
  select(prestige, Gender, age, age_categories, contains("scale")) %>%  drop_na() %>% 
  mutate(across(where(is.numeric), scale)) %>%
  mutate(across(where(is.matrix), as.numeric))

gsr_prestige <-
  brm(data = df_prestige_for_modeling_scaled, 
      prestige ~ (gsr_scale_sv+gsr_scale_v_picture+gsr_scale_v_word+gsr_scale_a_congruent+gsr_scale_a_incongruent+gsr_scale_tplus+gsr_scale_similarity+gsr_scale_st0+gsr_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(gsr_prestige, file="gsr_prestige_v3")

sv_prestige <-
  brm(data = df_prestige_for_modeling_scaled, 
      prestige ~ (sv_scale_sv+sv_scale_v_congruent+sv_scale_v_incongruent+sv_scale_a_congruent+sv_scale_a_incongruent+sv_scale_tplus+sv_scale_st0+sv_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(sv_prestige, file="sv_prestige_v3")


levy_prestige <-
  brm(data = df_prestige_for_modeling_scaled, 
      prestige ~ (levy_scale_alpha+levy_scale_v_congruent+levy_scale_v_incongruent+levy_scale_a_congruent+levy_scale_a_incongruent+levy_scale_tplus+levy_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(levy_prestige, file="levy_prestige_v3")

# create tables

tbl_regression(sv_prestige) %>% as_kable(format = "latex")
tbl_regression(levy_prestige) %>% as_kable(format = "latex")
tbl_regression(gsr_prestige) %>% as_kable(format = "latex")

