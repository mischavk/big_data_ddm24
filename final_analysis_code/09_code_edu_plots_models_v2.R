library(readr)
library(tidyverse)
library(marginaleffects)
library(cowplot)
library(readxl)
library(scales)
library(haven)
library(dvmisc)
library(ggsci)
library(brms)
library(ggh4x)

# get model parameters, filter improper estimates (NaNs), mahalanobis outliers (p <.001) and age <30 and >70

critical_mahalanobis_gsr = 250.627
critical_mahalanobis_ddm = 225.145
critical_mahalanobis_levy = 257.137

df_gsr <- read_csv("C:/Users/misch/Downloads/df_gsr.csv") %>%
  select(similarity:st0, age, session_id, mahalanobis_distance) %>% drop_na() %>% 
  filter(mahalanobis_distance < critical_mahalanobis_gsr  & age>29 & age < 71)  %>% 
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
  select(v_congruent:sv, age, session_id, mahalanobis_distance) %>% drop_na() %>% 
  filter(mahalanobis_distance < critical_mahalanobis_ddm  & age>29 & age < 71)  %>% 
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
  select(v_congruent:alpha, age, session_id, mahalanobis_distance) %>% drop_na() %>% 
  filter(mahalanobis_distance < critical_mahalanobis_levy  & age>29 & age < 71)  %>% 
  mutate(levy_scale_v_congruent = scale(v_congruent)) %>% 
  mutate(levy_scale_v_incongruent = scale(v_incongruent)) %>% 
  mutate(levy_scale_a_congruent = scale(a_congruent)) %>% 
  mutate(levy_scale_a_incongruent = scale(a_incongruent)) %>% 
  mutate(levy_scale_tplus = scale(tplus)) %>% 
  mutate(levy_scale_tminus = scale(tminus)) %>% 
  mutate(levy_scale_alpha = scale(alpha)) %>% 
  select(age, session_id, starts_with("levy_"))%>% 
  distinct(session_id, .keep_all = TRUE) 

# get educational data -------

codes_edu <- data.frame(values=1:14,
                        labels = c("e. school", "j. high", "high school", "h.s. grad", "college", "associate's",
                     "bachelor's", "grad school","master's", "Ph.D. etc.", "etc", "etc", "etc", "etc"))

big_data_edu <- read_csv("C:/Users/misch/Downloads/outcomes20032023.csv",
                         col_select = c("session_id", "age", "edu","birthsex", "genderIdentity_0002", "transIdentity" )) %>%  unique(.) %>% 
  mutate(edu=as.factor(if_else(edu>9, 10, edu))) %>% # recode all forms of doctorate and M.B.A. as one category
  mutate(birthsex = ifelse((is.na(birthsex) & transIdentity == 2), genderIdentity_0002, birthsex)) %>% # recode gender for missing birthsex
  select(-genderIdentity_0002, -transIdentity) %>% 
  filter(birthsex %in% c(1,2)) %>% 
  mutate(Gender = factor(birthsex, labels = c("male", "female")))%>% 
  mutate(age_categories = cut(age, breaks = c(29,40,50,60,71))) %>% 
  distinct(session_id, .keep_all = TRUE)

# join model and demographic data

df_edu <- left_join(big_data_edu, df_gsr) %>% left_join(df_sv) %>% left_join(df_levy)%>%
  select(edu, Gender, age, age_categories, contains("scale")) %>%  drop_na()

df_edu_grouped <- df_edu %>% pivot_longer(cols=contains("scale"), names_to="parameter") %>%
  group_by(edu, age_categories, Gender, parameter) %>% 
  summarize(mean = mean(value, na.rm=TRUE), se= sd(value, na.rm=TRUE)/sqrt(n())) %>% drop_na() %>% 
  mutate(Model = if_else(
    str_detect(parameter, "gsr"), "GSR", if_else(
    str_detect(parameter, "sv"), "DDM", if_else(
    str_detect(parameter, "levy"), "Levy", NA)))) %>% 
  mutate(parameter = str_remove_all(parameter, "gsr_scale_")) %>% 
  mutate(parameter = str_remove_all(parameter, "sv_scale_")) %>%
  mutate(parameter = str_remove_all(parameter, "levy_scale_"))

df_edu_grouped$parameter <- factor(df_edu_grouped$parameter,
                       levels=c(
                         "v_congruent", "v_incongruent", "v_word", "v_picture",
                         "a_congruent", "a_incongruent", "tplus", "tminus",
                         "sv", "st0", "alpha", "similarity"))

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


# PLOTS ----

ggplot(df_edu_grouped, aes(edu, mean, color=age_categories, shape=Model, linetype=Model,alpha=Gender)) +
  geom_point(size=1.1) +  geom_errorbar(aes(edu,ymin=mean-se,ymax=mean+se),width=0.0, alpha=.3) +
  geom_line(aes(group=interaction(age_categories,Gender, Model)))  +  theme_minimal(base_size = 7) +
  facet_wrap(~parameter, scales="fixed",
             labeller = as_labeller(variable_names))+
  scale_colour_npg(labels = c("30-40", "40-50", "50-60", "60-70")) +
  scale_x_discrete(breaks =1:10, labels =c(codes_edu$labels[1:9], "Ph.D. etc."),guide = guide_axis(angle = 35))+
  ylab("Mean parameter value") + xlab("Education")+
  guides(color=guide_legend("Age groups"))+ scale_alpha_discrete(range = c(0.4, 1))

ggsave("full_edu_plot_v4.png", width=6, height=4.5, dpi=600, bg="white")



# MODELS ------

df_edu_scaled = df_edu %>%  
  mutate(across(where(is.matrix), as.numeric))

gsr_edu <-
  brm(data = df_edu_scaled, 
      family = cumulative(link = logit),
      ordered(edu) ~ (gsr_scale_sv+gsr_scale_v_picture+gsr_scale_v_word+gsr_scale_a_congruent+gsr_scale_a_incongruent+gsr_scale_tplus+gsr_scale_similarity+gsr_scale_st0+gsr_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(gsr_edu, file="gsr_edu_v3")

sv_edu <-
  brm(data = df_edu_scaled, 
      family = cumulative(link = logit),
      ordered(edu) ~ (sv_scale_sv+sv_scale_v_congruent+sv_scale_v_incongruent+sv_scale_a_congruent+sv_scale_a_incongruent+sv_scale_tplus+sv_scale_st0+sv_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(sv_edu, file="sv_edu_v3")


levy_edu <-
  brm(data = df_edu_scaled, 
      family = cumulative(link = logit),
      ordered(edu) ~ (levy_scale_alpha+levy_scale_v_congruent+levy_scale_v_incongruent+levy_scale_a_congruent+levy_scale_a_incongruent+levy_scale_tplus+levy_scale_tminus)*Gender*age_categories,
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 22)

save(levy_edu, file="levy_edu_v3")

#create tables

tbl_regression(sv_edu) %>% as_kable(format = "latex")
tbl_regression(levy_edu) %>% as_kable(format = "latex")
tbl_regression(gsr_edu) %>% as_kable(format = "latex")