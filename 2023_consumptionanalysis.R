####LOAD PACKAGES AND DATA#################################################################################################
library(readxl)
library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)

setwd("C:/Users/Ally/OneDrive/Analysis for R")

rm(list = ls())
#CHOW
chow <- read_xlsx("Chow_2023.xlsx",sheet="Combined")
chow<- chow %>%
  mutate(chow_consumption = `Raw Chow Consumption (g)`,
         injection = `Designer Drug`,
         sex = Sex,
         dreadd_type = `DREADD Type`,
         exclude=`Exclude`,
         rat = parse_number(`Rat ID`)) %>% 
  select(rat, sex, dreadd_type,chow_consumption, injection,exclude)
  # take out habituation and excluded animals
chow <- filter(chow, is.na(exclude),injection != "Habituation" & injection != "CNO")

#SUCROSE
sucrose <- read_xlsx("Sucrose_2023.xlsx",sheet="Combined")
sucrose<- sucrose %>%
  mutate(sucrose_consumption = `Sucrose Consumption (g)`,
         water_consumption = `Water Consumption (g)`,
         preference_consumption = `test preference (%)`,
         injection = `Designer Drug`,
         sex = Sex,
         dreadd_type = `DREADD Type`,
         exclude=`Exclude`,
         rat = parse_number(`Rat ID`)) %>% 
  select(rat, sex, dreadd_type,sucrose_consumption,water_consumption,preference_consumption, injection,exclude,day)
  # take out habituation and excluded animals
sucrose <- filter(sucrose, is.na(exclude), injection != "Habituation" & injection != "CNO")



#######RUN MIXED EFFECT MODELS#############################################################################################

###CHOWMODELS
### Gi Chow
gi_chow_model <- lmer(chow_consumption ~ as.factor(injection) * sex + (1 | rat), data = chow%>% filter(dreadd_type == "Gi"))
summary(gi_chow_model)
gi_chow_model_anova <-anova(gi_chow_model)
capture.output(gi_chow_model, file = "gi_chow_model_lme_results.doc")
capture.output(gi_chow_model_anova, file = "gi_chow_model_anova_results.doc")

Gichowmeansbyrat<-chow%>% filter(dreadd_type == "Gi")%>%
  group_by(rat,injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)


Gichowmeans<-chow%>% filter(dreadd_type == "Gi")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)


Gichowmeanssex<-chow%>% filter(dreadd_type == "Gi")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)

### Gq Chow
gq_chow_model <- lmer(chow_consumption ~ as.factor(injection) * sex + (1 | rat), data = chow%>% filter(dreadd_type == "Gq"))
summary(gq_chow_model)
gq_chow_model_anova <-anova(gq_chow_model)
capture.output(gq_chow_model, file = "gq_chow_model_lme_results.doc")
capture.output(gq_chow_model_anova, file = "gq_chow_model_anova_results.doc")


Gqchowmeansbyrat<-chow%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)


Gqchowmeans<-chow%>% filter(dreadd_type == "Gq")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)


Gqchowmeanssex<-chow%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)

### mCherry Chow
mcherry_chow_model <- lmer(chow_consumption ~ as.factor(injection) * sex + (1 | rat), data = chow%>% filter(dreadd_type == "mCherry"))
summary(mcherry_chow_model)
mcherry_chow_model_anova <-anova(mcherry_chow_model)
capture.output(mcherry_chow_model, file = "mcherry_chow_model_lme_results.doc")
capture.output(mcherry_chow_model_anova, file = "mcherry_chow_model_anova_results.doc")

mcherrychowmeansbyrat<-chow%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)

mcherrychowmeans<-chow%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)


mcherrychowmeanssex<-chow%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), chow_consumption)
## Follow up comparisons on significant models (Gq Chow)

emGqchow <- emmeans(gq_chow_model,specs = ~ as.factor(injection) + sex )

#because sex and injection interaction found, will do comparisons withing sex and between sex/ injection
# from grid in emmeans of Gq_chow, 1=JHU 0.05 mg/kg   F, 2=JHU 0.50 mg/kg   F, 3=  Saline   F ,4=JHU 0.05 mg/kg   M,5=JHU 0.50 mg/kg   M,6= Saline   M 

f00<- c(0,0,1,0,0,0)
f05<- c(1,0,0,0,0,0)
f50<- c(0,1,0,0,0,0)
m00<- c(0,0,0,0,0,1)
m05<- c(0,0,0,1,0,0)
m50<- c(0,0,0,0,1,0)

gqchowcon<-contrast(emGqchow ,method = list("Female saline - Female 0.05 mg/mL" = f00 - f05,
                           "Female saline - Female 0.50 mg/mL" = f00 - f50,
                           "FemaleDS 0.05 mg/mL - Female 0.50 mg/mL" = f05 - f50,
                           "Male saline - Male 0.05 mg/mL" = m00 - m05,
                           "Male saline - Male 0.50 mg/mL" = m00 - m50,
                           "Male 0.05 mg/mL - Male 0.50 mg/mL" = m05- m50,
                           "Female saline - Male saline" = f00 - m00,
                           "Female 0.05mg/mL - Male 0.05 mg/mL" = f05 - m05,
                           "Female 0.50mg/mL - Male 0.50mg/mL" = f50 - m50))
capture.output(gqchowcon, file = "gqchow_pairwise_results.doc")



###SUCROSE MODELS
### Gi Sucrose
gi_suc_model <- lmer(sucrose_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gi"))
summary(gi_suc_model)
gi_suc_model_anova <-anova(gi_suc_model)
capture.output(gi_suc_model, file = "gi_suc_model_lme_results.doc")
capture.output(gi_suc_model, file = "gi_suc_model_anova_results.doc")


Gisucmeansbyrat<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

Gisucmeans<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

Gisucmeanssex<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

### Gq Sucrose
gq_suc_model <- lmer(sucrose_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gq"))
summary(gq_suc_model)
gq_suc_model_anova <-anova(gq_suc_model)
capture.output(gq_suc_model, file = "gq_suc_model_lme_results.doc")
capture.output(gq_suc_model_anova, file = "gq_suc_model_anova_results.doc")

Gqsucmeansbyrat<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

Gqsucmeans<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

Gqsucmeanssex<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

### mCherry Sucrose
mcherry_suc_model <- lmer(sucrose_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "mCherry"))
summary(mcherry_suc_model)
mcherry_suc_model_anova <-anova(mcherry_suc_model)
capture.output(mcherry_suc_model, file = "mcherry_suc_model_lme_results.doc")
capture.output(mcherry_suc_model_anova, file = "mcherry_suc_model_anova_results.doc")

mcherrysucmeansbyrat<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

mcherrysucmeans<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)

mcherrysucmeanssex<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), sucrose_consumption)
## pairwise Sucrose models

## Follow up comparisons on significant models (Gq Suc)

emGqsuc<- emmeans(gq_suc_model,specs = ~ as.factor(injection) + sex )

#because sex and injection interaction found, will do comparisons withing sex and between sex/ injection
# from grid in emmeans of Gq_chow, 1=JHU 0.05 mg/kg   F, 2=JHU 0.50 mg/kg   F, 3=  Saline   F ,4=JHU 0.05 mg/kg   M,5=JHU 0.50 mg/kg   M,6= Saline   M 

f00<- c(0,0,1,0,0,0)
f05<- c(1,0,0,0,0,0)
f50<- c(0,1,0,0,0,0)
m00<- c(0,0,0,0,0,1)
m05<- c(0,0,0,1,0,0)
m50<- c(0,0,0,0,1,0)

gqsuccon<-contrast(emGqsuc ,method = list("Female saline - Female 0.05 mg/mL" = f00 - f05,
                                            "Female saline - Female 0.50 mg/mL" = f00 - f50,
                                            "FemaleDS 0.05 mg/mL - Female 0.50 mg/mL" = f05 - f50,
                                            "Male saline - Male 0.05 mg/mL" = m00 - m05,
                                            "Male saline - Male 0.50 mg/mL" = m00 - m50,
                                            "Male 0.05 mg/mL - Male 0.50 mg/mL" = m05- m50,
                                            "Female saline - Male saline" = f00 - m00,
                                            "Female 0.05mg/mL - Male 0.05 mg/mL" = f05 - m05,
                                            "Female 0.50mg/mL - Male 0.50mg/mL" = f50 - m50))
capture.output(gqsuccon, file = "gqsuc_pairwise_results.doc")


##PREFERENCE MODLES
### Gi Sucrose PREf
gi_pref_model <- lmer(preference_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gi"))
summary(gi_pref_model)
gi_pref_model_anova <-anova(gi_pref_model)
capture.output(gi_pref_model, file = "gi_pref_model_lme_results.doc")
capture.output(gi_pref_model, file = "gi_pref_model_anova_results.doc")

Giprefmeans<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)

Giprefmeanssex<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)


### Gq Sucrose
gq_pref_model <- lmer(preference_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gq"))
summary(gq_pref_model)
gq_pref_model_anova <-anova(gq_pref_model)
capture.output(gq_pref_model, file = "gq_pref_model_lme_results.doc")
capture.output(gq_pref_model_anova, file = "gq_pref_model_anova_results.doc")

Gqprefmeans<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)

Gqprefmeanssex<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)

### mCherry Sucrose
mcherry_pref_model <- lmer(preference_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "mCherry"))
summary(mcherry_pref_model)
mcherry_pref_model_anova <-anova(mcherry_pref_model)
capture.output(mcherry_pref_model, file = "mcherry_pref_model_lme_results.doc")
capture.output(mcherry_pref_model_anova, file = "mcherry_pref_model_anova_results.doc")

mcherryprefmeans<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)

mcherryprefmeanssex<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), preference_consumption)


## pairwise pref models

## Follow up comparisons on significant models (gq preference)

emgqpref<- emmeans(gq_pref_model,specs = ~ as.factor(injection) + sex )

#because sex and injection interaction found, will do comparisons withing sex and between sex/ injection
# from grid in emmeans of Gq_chow, 1=JHU 0.05 mg/kg   F, 2=JHU 0.50 mg/kg   F, 3=  Saline   F ,4=JHU 0.05 mg/kg   M,5=JHU 0.50 mg/kg   M,6= Saline   M 

f00<- c(0,0,1,0,0,0)
f05<- c(1,0,0,0,0,0)
f50<- c(0,1,0,0,0,0)
m00<- c(0,0,0,0,0,1)
m05<- c(0,0,0,1,0,0)
m50<- c(0,0,0,0,1,0)

gqprefcon<-contrast(emgqpref ,method = list("Female saline - Female 0.05 mg/mL" = f00 - f05,
                                                    "Female saline - Female 0.50 mg/mL" = f00 - f50,
                                                    "Female 0.05 mg/mL - Female 0.50 mg/mL" = f05 - f50,
                                                    "Male saline - Male 0.05 mg/mL" = m00 - m05,
                                                    "Male saline - Male 0.50 mg/mL" = m00 - m50,
                                                    "Male 0.05 mg/mL - Male 0.50 mg/mL" = m05- m50,
                                                    "Female saline - Male saline" = f00 - m00,
                                                    "Female 0.05mg/mL - Male 0.05 mg/mL" = f05 - m05,
                                                    "Female 0.50mg/mL - Male 0.50mg/mL" = f50 - m50))
capture.output(gqprefcon, file = "gqpref_pairwise_results.doc")


emmcherrypref<- emmeans(mcherry_pref_model,specs = ~ as.factor(injection) + sex )

#because sex and injection interaction found, will do comparisons withing sex and between sex/ injection
# from grid in emmeans of Gq_chow, 1=JHU 0.05 mg/kg   F, 2=JHU 0.50 mg/kg   F, 3=  Saline   F ,4=JHU 0.05 mg/kg   M,5=JHU 0.50 mg/kg   M,6= Saline   M 

f00<- c(0,0,1,0,0,0)
f05<- c(1,0,0,0,0,0)
f50<- c(0,1,0,0,0,0)
m00<- c(0,0,0,0,0,1)
m05<- c(0,0,0,1,0,0)
m50<- c(0,0,0,0,1,0)

mcherryprefcon<-contrast(emmcherrypref ,method = list("Female saline - Female 0.05 mg/mL" = f00 - f05,
                                            "Female saline - Female 0.50 mg/mL" = f00 - f50,
                                            "Female 0.05 mg/mL - Female 0.50 mg/mL" = f05 - f50,
                                            "Male saline - Male 0.05 mg/mL" = m00 - m05,
                                            "Male saline - Male 0.50 mg/mL" = m00 - m50,
                                            "Male 0.05 mg/mL - Male 0.50 mg/mL" = m05- m50,
                                            "Female saline - Male saline" = f00 - m00,
                                            "Female 0.05mg/mL - Male 0.05 mg/mL" = f05 - m05,
                                            "Female 0.50mg/mL - Male 0.50mg/mL" = f50 - m50))
capture.output(mcherryprefcon, file = "mcherrypref_pairwise_results.doc")




##WATER MODLES
### Gi 
gi_h2o_model <- lmer(water_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gi"))
summary(gi_h2o_model)
gi_h2o_model_anova <-anova(gi_h2o_model)
capture.output(gi_h2o_model, file = "gi_h2o_model_lme_results.doc")
capture.output(gi_h2o_model, file = "gi_h2o_model_anova_results.doc")


Gih2omeans<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)

Gih2omeanssex<-sucrose%>% filter(dreadd_type == "Gi")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)


### Gq 
gq_h2o_model <- lmer(water_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "Gq"))
summary(gq_h2o_model)
gq_h2o_model_anova <-anova(gq_h2o_model)
capture.output(gq_h2o_model, file = "gq_h2o_model_lme_results.doc")
capture.output(gq_h2o_model_anova, file = "gq_h2o_model_anova_results.doc")


Gqh2omeans<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)

Gqh2omeanssex<-sucrose%>% filter(dreadd_type == "Gq")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)

### mCherry 
mcherry_h2o_model <- lmer(water_consumption ~ as.factor(injection) * sex + (1 | rat), data = sucrose %>% filter(dreadd_type == "mCherry"))
mcherry_h2o_model_anova <-anova(mcherry_h2o_model)
capture.output(mcherry_h2o_model, file = "mcherry_h2o_model_lme_results.doc")
capture.output(mcherry_h2o_model_anova, file = "mcherry_h2o_model_anova_results.doc")


mcherryh2omeans<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)

mcherryh2omeanssex<-sucrose%>% filter(dreadd_type == "mCherry")%>%
  group_by(injection,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), water_consumption)

## pairwise h20 models

## Follow up comparisons on significant models (mcherry h2o)

emmcherryh2o<- emmeans(mcherry_h2o_model,specs = ~ as.factor(injection) + sex )

#because sex and injection interaction found, will do comparisons withing sex and between sex/ injection
# from grid in emmeans of Gq_chow, 1=JHU 0.05 mg/kg   F, 2=JHU 0.50 mg/kg   F, 3=  Saline   F ,4=JHU 0.05 mg/kg   M,5=JHU 0.50 mg/kg   M,6= Saline   M 

f00<- c(0,0,1,0,0,0)
f05<- c(1,0,0,0,0,0)
f50<- c(0,1,0,0,0,0)
m00<- c(0,0,0,0,0,1)
m05<- c(0,0,0,1,0,0)
m50<- c(0,0,0,0,1,0)

mcherryh2ocon<-contrast(emmcherryh2o ,method = list("Female saline - Female 0.05 mg/mL" = f00 - f05,
                                          "Female saline - Female 0.50 mg/mL" = f00 - f50,
                                          "Female 0.05 mg/mL - Female 0.50 mg/mL" = f05 - f50,
                                          "Male saline - Male 0.05 mg/mL" = m00 - m05,
                                          "Male saline - Male 0.50 mg/mL" = m00 - m50,
                                          "Male 0.05 mg/mL - Male 0.50 mg/mL" = m05- m50,
                                          "Female saline - Male saline" = f00 - m00,
                                          "Female 0.05mg/mL - Male 0.05 mg/mL" = f05 - m05,
                                          "Female 0.50mg/mL - Male 0.50mg/mL" = f50 - m50))
capture.output(mcherryh2ocon, file = "gqsuc_pairwise_results.doc")




############################### PLOTS##########################################################################################
## Gi chow plots

library(ggplot2)
gi_chow2 <- chow%>% filter(dreadd_type == "Gi")
gi_chow2$injection <- factor(gi_chow2$injection)
gi_chow2$rat <- factor(gi_chow2$rat)

ggplot(gi_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))

ggsave("Gi Chow Consumption_all.pdf")


ggplot(gi_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gi Chow Consumption facet.pdf")


## Gq chow plots
gq_chow2 <- chow%>% filter(dreadd_type == "Gq")
gq_chow2$injection <- factor(gq_chow2$injection)
gq_chow2$rat <- factor(gq_chow2$rat)


ggplot(gq_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gq Chow Consumption_all.pdf")


ggplot(gq_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gq Chow Consumption_facet.pdf")

##mCherry chow plots
mCherry_chow2 <-chow%>% filter(dreadd_type == "mCherry")
mCherry_chow2$injection <- factor(mCherry_chow2$injection)
mCherry_chow2$rat <- factor(mCherry_chow2$rat)


ggplot(mCherry_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mcherry Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("mCherry Chow Consumption.pdf")

ggplot(mCherry_chow2, aes(x=factor(injection,level=c('Saline','JHU 0.05 mg/kg','JHU 0.50 mg/kg')), y=chow_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mcherry Chow Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("mCherry Chow Consumption_facet.pdf")



## sucrose plots#################################

##Gi
gi_suc2 <- sucrose%>% filter(dreadd_type == "Gi")
gi_suc2$injection <- factor(gi_suc2$injection)
gi_suc2$rat <- factor(gi_suc2$rat)


ggplot(gi_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gi Sucrose Consumption_all.pdf")


ggplot(gi_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gi Sucrose Consumption_facet_mvf.pdf")

##Gq
gq_suc2 <- sucrose%>% filter(dreadd_type == "Gq")
gq_suc2$injection <- factor(gq_suc2$injection)
gq_suc2$rat <- factor(gq_suc2$rat)


ggplot(gq_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gq Sucrose Consumption_all.pdf")

ggplot(gq_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gq Sucrose Consumption_facet_mvf.pdf")

##mCherry
mCherry_suc2 <- sucrose%>% filter(dreadd_type == "mCherry")
mCherry_suc2$injection <- factor(mCherry_suc2$injection)
mCherry_suc2$rat <- factor(mCherry_suc2$rat)

ggplot(mCherry_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mCherry Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("mCherry Sucrose Consumption_all.pdf")



ggplot(mCherry_suc2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=sucrose_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mCherry Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("mCherry Sucrose Consumption_facet_mvf.pdf")




#### sucrose preference plots########################
##Gi
gi_sucpref2 <- sucrose%>% filter(dreadd_type == "Gi")
gi_sucpref2$injection <- factor(gi_sucpref2$injection)
gi_sucpref2$rat <- factor(gi_sucpref2$rat)


ggplot(gi_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gi Sucrose Pref Consumption_all.pdf")


ggplot(gi_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%)")  + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gi Sucrose Pref Consumption_facet_mvf.pdf")

##Gq
gq_sucpref2 <- sucrose%>% filter(dreadd_type == "Gq")
gq_sucpref2$injection <- factor(gq_sucpref2$injection)
gq_sucpref2$rat <- factor(gq_sucpref2$rat)


ggplot(gq_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid")) 
ggsave("Gq Sucrose Pref Consumption_all.pdf")

ggplot(gq_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%) ")  + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
  
ggsave("Gq Sucrose Pref Consumption_facet_mvf.pdf")

##mCherry
mCherry_sucpref2 <- sucrose%>% filter(dreadd_type == "mCherry")
mCherry_sucpref2$injection <- factor(mCherry_sucpref2$injection)
mCherry_sucpref2$rat <- factor(mCherry_sucpref2$rat)

ggplot(mCherry_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mCherry Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid")) 
ggsave("mCherry Sucrose Pref Consumption_all.pdf")



ggplot(mCherry_sucpref2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=preference_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mCherry Sucrose Pref Consumption",x ="DREADD Ligand (mg/mL)", y = "Sucrose Preference (%) ")  + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex))  
ggsave("mCherry Sucrose Pref Consumption_facet_mvf.pdf")




##water plots################################
##Gi
gi_h2o2 <- sucrose%>% filter(dreadd_type == "Gi")
gi_h2o2$injection <- factor(gi_h2o2$injection)
gi_h2o2$rat <- factor(gi_h2o2$rat)

ggplot(gi_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gi water Sucrose Consumption.pdf")

ggplot(gi_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gi water Sucrose Consumption_facet.pdf")

##Gq
gq_h2o2 <- sucrose%>% filter(dreadd_type == "Gq")
gq_h2o2$injection <- factor(gq_h2o2$injection)
gq_h2o2$rat <- factor(gq_h2o2$rat)

ggplot(gq_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("Gq water Sucrose Consumption.pdf")

ggplot(gq_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("Gq water Sucrose Consumption_facet.pdf")


##mcherry

mcherry_h2o2 <- sucrose%>% filter(dreadd_type == "mCherry")
mcherry_h2o2$injection <- factor(mcherry_h2o2$injection)
mcherry_h2o2$rat <- factor(mcherry_h2o2$rat)

ggplot(mcherry_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("mcherry water Sucrose Consumption.pdf")

ggplot(mcherry_h2o2, aes(x=factor(injection,level=c('Saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=water_consumption,fill=injection)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="water Sucrose Consumption",x ="DREADD Ligand (mg/mL)", y = "Raw Consumption (g) ")+ 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))+
  facet_grid(cols = vars(sex)) 
ggsave("mcherry water Sucrose Consumption_facet.pdf")
