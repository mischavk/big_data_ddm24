library(tidyverse)
library(brms)
library(marginaleffects)
library(ggdist)
library(ggsci)

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


# Education regression plot ----


load("C:/Users/misch/Desktop/final_analysis_code/sv_edu_v3")
load("C:/Users/misch/Desktop/final_analysis_code/levy_edu_v3")
load("C:/Users/misch/Desktop/final_analysis_code/gsr_edu_v3")


edu_sv_draws <- slopes(sv_edu, type="link",
                       variables = c( "sv_scale_sv", "sv_scale_v_congruent", "sv_scale_v_incongruent",
                                      "sv_scale_tplus", "sv_scale_tminus","sv_scale_st0", "sv_scale_a_congruent", "sv_scale_a_incongruent"),
                       newdata = datagrid("Gender" =c("male", "female"),
                                          "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>%select(draw, term, Gender, age_categories) %>%  mutate(Outcome="Education", Model ="DDM")

edu_levy_draws <- slopes(levy_edu, type="link",
                         variables = c( "levy_scale_alpha", "levy_scale_v_congruent", "levy_scale_v_incongruent",
                                        "levy_scale_tplus","levy_scale_tminus" , "levy_scale_a_congruent", "levy_scale_a_incongruent"),
                         newdata = datagrid("Gender"=c("male", "female"),
                                            "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>%  mutate(Outcome="Education", Model ="Levy")

edu_gsr_draws <- slopes(gsr_edu, type="link",
                        variables = c( "gsr_scale_sv", "gsr_scale_v_word", "gsr_scale_v_picture",
                                       "gsr_scale_tplus", "gsr_scale_st0", "gsr_scale_tminus", "gsr_scale_similarity", "gsr_scale_a_congruent", "gsr_scale_a_incongruent"),
                        newdata = datagrid("Gender"=c("male", "female"),
                                           "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>%  
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>% mutate(Outcome="Education", Model ="GSR")

samples_edu <- rbind(edu_sv_draws, edu_levy_draws) %>% rbind (edu_gsr_draws) %>% 
  mutate(term = str_remove_all(term, "gsr_scale_")) %>% 
  mutate(term = str_remove_all(term, "sv_scale_")) %>%
  mutate(term = str_remove_all(term, "levy_scale_")) 


samples_edu$term <- factor(samples_edu$term,
                           levels=c(
                             "v_congruent", "v_incongruent", "v_word", "v_picture",
                             "a_congruent", "a_incongruent", "tplus", "tminus",
                             "sv", "st0", "alpha", "similarity"))

# creating actual plot

ggplot(samples_edu,
       aes(y = draw,
           side=Gender,
           x= age_categories,
           fill=age_categories,
           linetype = Model,
           alpha=Gender, slab_linetype = Model)) + 
  stat_slab(scale = 0.6,slab_linewidth = 0.4, point_size=0.8, color="black", normalize="panels") +
  geom_hline(yintercept = 0, col = "black")+
  labs(y = "Marginal Effect of Parameters on Education",
       x = NULL,
       fill="Age group"
  )  +scale_side_mirrored()+ylim(-0.26, 0.20)+ theme_minimal()+
  
  annotate("text", x = 1.25, y = 0.18, label = "♂", size = 4)+
  annotate("text", x = 0.75, y = 0.18, label = "♀", size = 4) +
  theme(axis.text.x = element_text(size =7))+
  
  facet_wrap (~term, nrow=6, ncol=4,scales="free_y",
              labeller = as_labeller(variable_names))+
  theme(legend.position="bottom",legend.text=element_text(size=8))+guides(x =  guide_axis(angle = 0))+
  scale_fill_npg(labels = c("30-40 years", "40-50 years", "50-60 years", "60-70 years"))+
  scale_x_discrete(labels = c("30-40\nyears", "40-50\nyears", "50-60\nyears", "60-70\nyears"))+
  scale_alpha_discrete(range = c(0.3, 0.8))+
  guides(fill=guide_legend(nrow = 2))+
  guides(linetype=guide_legend(nrow = 2))+
  guides(side=guide_legend(nrow = 2))

ggsave("plot_regression_edu_v4.png", dpi=600, height=7, width=7, bg="white")


# Income regression plot

load("C:/Users/misch/Desktop/final_analysis_code/sv_income_v3")
load("C:/Users/misch/Desktop/final_analysis_code/levy_income_v3")
load("C:/Users/misch/Desktop/final_analysis_code/gsr_income_v3")

income_sv_draws <- slopes(sv_income,
              variables = c( "sv_scale_sv", "sv_scale_v_congruent", "sv_scale_v_incongruent",
                              "sv_scale_tplus", "sv_scale_tminus","sv_scale_st0", "sv_scale_a_congruent", "sv_scale_a_incongruent"),
              newdata = datagrid("Gender" =c("male", "female"),
                                 "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>%select(draw, term, Gender, age_categories) %>%  mutate(Outcome="Income", Model ="DDM")

income_levy_draws <- slopes(levy_income,
                          variables = c( "levy_scale_alpha", "levy_scale_v_congruent", "levy_scale_v_incongruent",
                                         "levy_scale_tplus","levy_scale_tminus" , "levy_scale_a_congruent", "levy_scale_a_incongruent"),
                          newdata = datagrid("Gender"=c("male", "female"),
                                             "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>%  mutate(Outcome="Income", Model ="Levy")

income_gsr_draws <- slopes(gsr_income,
                            variables = c( "gsr_scale_sv", "gsr_scale_v_word", "gsr_scale_v_picture",
                                           "gsr_scale_tplus", "gsr_scale_st0", "gsr_scale_tminus", "gsr_scale_similarity", "gsr_scale_a_congruent", "gsr_scale_a_incongruent"),
                            newdata = datagrid("Gender" = c("male", "female"),
                                               "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>% mutate(Outcome="Income", Model ="GSR")

samples_income <- rbind(income_sv_draws, income_levy_draws) %>% rbind (income_gsr_draws)%>% 
  mutate(term = str_remove_all(term, "gsr_scale_")) %>% 
  mutate(term = str_remove_all(term, "sv_scale_")) %>%
  mutate(term = str_remove_all(term, "levy_scale_")) 


samples_income$term <- factor(samples_income$term,
                       levels=c(
                         "v_congruent", "v_incongruent", "v_word", "v_picture",
                         "a_congruent", "a_incongruent", "tplus", "tminus",
                         "sv", "st0", "alpha", "similarity"))

# create actual plot 

ggplot(samples_income,
       aes(y = draw,
           side=Gender,
           x= age_categories,
           fill=age_categories,
           linetype = Model,
           alpha=Gender, slab_linetype = Model)) + 
  stat_slab(scale = 0.6,slab_linewidth = 0.4, point_size=0.8, color="black", normalize="panels") +
  geom_hline(yintercept = 0, col = "black")+
  labs(y = "Marginal Effect of Parameters on Yearly $ Income",
       x = NULL,
       fill="Age group"
  )  +scale_side_mirrored()+ylim(-0.20, 0.24)+ theme_minimal()+
  
  annotate("text", x = 1.25, y = 0.18, label = "♂", size = 4)+
  annotate("text", x = 0.75, y = 0.18, label = "♀", size = 4) +
  theme(axis.text.x = element_text(size =7))+
  
  facet_wrap (~term, nrow=6, ncol=4,scales="free_y",
              labeller = as_labeller(variable_names))+
  theme(legend.position="bottom",legend.text=element_text(size=8))+guides(x =  guide_axis(angle = 0))+
  scale_fill_npg(labels = c("30-40 years", "40-50 years", "50-60 years", "60-70 years"))+
  scale_x_discrete(labels = c("30-40\nyears", "40-50\nyears", "50-60\nyears", "60-70\nyears"))+
  scale_alpha_discrete(range = c(0.3, 0.8))+
  guides(fill=guide_legend(nrow = 2))+
  guides(linetype=guide_legend(nrow = 2))+
  guides(side=guide_legend(nrow = 2))


ggsave("plot_regression_income_v4.png", dpi=600, height=7, width=7, bg="white")


# Prestige regression plot ----
load("C:/Users/misch/Desktop/final_analysis_code/sv_prestige_v3")
load("C:/Users/misch/Desktop/final_analysis_code/levy_prestige_v3")
load("C:/Users/misch/Desktop/final_analysis_code/gsr_prestige_v3")

prestige_sv_draws <- slopes(sv_prestige,
                          variables = c( "sv_scale_sv", "sv_scale_v_congruent", "sv_scale_v_incongruent",
                                         "sv_scale_tplus", "sv_scale_tminus","sv_scale_st0", "sv_scale_a_congruent", "sv_scale_a_incongruent"),
                          newdata = datagrid("Gender" =c("male", "female"),
                                             "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>%select(draw, term, Gender, age_categories) %>%  mutate(Outcome="Prestige", Model ="DDM")

prestige_levy_draws <- slopes(levy_prestige,
                            variables = c( "levy_scale_alpha", "levy_scale_v_congruent", "levy_scale_v_incongruent",
                                           "levy_scale_tplus","levy_scale_tminus" , "levy_scale_a_congruent", "levy_scale_a_incongruent"),
                            newdata = datagrid("Gender"=c("male", "female"),
                                               "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>%  mutate(Outcome="Prestige", Model ="Levy")

prestige_gsr_draws <- slopes(gsr_prestige,
                           variables = c( "gsr_scale_sv", "gsr_scale_v_word", "gsr_scale_v_picture",
                                          "gsr_scale_tplus", "gsr_scale_st0", "gsr_scale_tminus", "gsr_scale_similarity", "gsr_scale_a_congruent", "gsr_scale_a_incongruent"),
                           newdata = datagrid("Gender" = c("male", "female"),
                                              "age_categories" = c("(29,40]","(40,50]", "(50,60]", "(60,71]"))) %>% 
  posterior_draws() %>% select(draw, term, Gender,age_categories) %>% mutate(Outcome="Prestige", Model ="GSR")

samples_prestige <- rbind(prestige_sv_draws, prestige_levy_draws) %>% rbind (prestige_gsr_draws)%>% 
  mutate(term = str_remove_all(term, "gsr_scale_")) %>% 
  mutate(term = str_remove_all(term, "sv_scale_")) %>%
  mutate(term = str_remove_all(term, "levy_scale_")) 


samples_prestige$term <- factor(samples_prestige$term,
                       levels=c(
                         "v_congruent", "v_incongruent", "v_word", "v_picture",
                         "a_congruent", "a_incongruent", "tplus", "tminus",
                         "sv", "st0", "alpha", "similarity"))

# create actual plot

ggplot(samples_prestige,
       aes(y = draw,
           side=Gender,
           x= age_categories,
           fill=age_categories,
           linetype = Model,
           alpha=Gender, slab_linetype = Model)) + 
  stat_slab(scale = 0.6,slab_linewidth = 0.4, point_size=0.8, color="black", normalize="panels") +
  geom_hline(yintercept = 0, col = "black")+
  labs(y = "Marginal Effect of Parameters on Job Prestige",
       x = NULL,
       fill="Age group"
  )  +scale_side_mirrored()+ylim(-0.15, 0.15)+ theme_minimal()+
  
  annotate("text", x = 1.25, y = 0.125, label = "♂", size = 4)+
  annotate("text", x = 0.75, y = 0.125, label = "♀", size = 4) +
  
  facet_wrap (~term, nrow=6, ncol=4,scales="free_y",
              labeller = as_labeller(variable_names))+
  theme(legend.position="bottom",legend.text=element_text(size=8))+
  guides(x =  guide_axis(angle = 0))+
  theme(axis.text.x = element_text(size =7))+
  scale_fill_npg(labels = c("30-40 years", "40-50 years", "50-60 years", "60-70 years"))+
  scale_x_discrete(labels = c("30-40\nyears", "40-50\nyears", "50-60\nyears", "60-70\nyears"))+
  scale_alpha_discrete(range = c(0.3, 0.8))+
  guides(fill=guide_legend(nrow = 2))+
  guides(linetype=guide_legend(nrow = 2))+
  guides(side=guide_legend(nrow = 2))

  ggsave("plot_regression_prestige_v4.png", dpi=600, height=7, width=7, bg="white")

