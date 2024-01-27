library(readxl)
library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)


setwd("G:/Shared drives/Richard Lab/Data/Ally/GADDREADD DS Task/Raw Data")


rm(list = ls())

#total PEs
totalPE <- read_xlsx("totalPEtab.xlsx")
totalPE<- totalPE %>%
  mutate(totalPE = `totalPEs`,
         stage =`stage`,
         day=`day`,
         ligand = `ligand`,
         sex = `sex`,
         dreadd= `dreadd`,
         rat = `RatID`) %>% 
  select(rat, sex, dreadd,totalPE,ligand,exclude,day,stage)
# take out habituation and excluded animals
totalPE <- filter(totalPE, is.na(exclude), ligand !='nan')

setwd("C:/Users/Ally/OneDrive/Analysis for R")


##plots######
##Gi
gi_PE <- totalPE%>% filter(dreadd == "Gi")
gi_PE$injection <- factor(gi_PE$ligand)
gi_PE$rat <- factor(gi_PE$rat)

ggplot(gi_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gi TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
   
ggsave("giPE.pdf")


ggplot(gi_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary( position="dodge",fun = "mean", geom = "bar")+
  #geom_line( aes(group=rat,linetype=type))+
  facet_grid(cols=vars(sex)) +
  stat_summary(fun = "mean", geom = "bar")+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  labs(title="TotalPEs",x ="ligand", y = "Total PEs ")+
  labs(title="Gi TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("giPE_sex.pdf")


##Gq
gq_PE <- totalPE%>% filter(dreadd == "Gq")
gq_PE$injection <- factor(gq_PE$ligand)
gq_PE$rat <- factor(gq_PE$rat)

ggplot(gq_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="Gq TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("gqPE.pdf")


ggplot(gq_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary( position="dodge",fun = "mean", geom = "bar")+
  #geom_line( aes(group=rat,linetype=type))+
  facet_grid(cols=vars(sex)) +
  stat_summary(fun = "mean", geom = "bar")+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  labs(title="TotalPEs",x ="ligand", y = "Total PEs ")+
  labs(title="Gq TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("gqPE_sex.pdf")

##mcherry

mcherry_PE <- totalPE%>% filter(dreadd == "mCherry")
mcherry_PE$injection <- factor(mcherry_PE$ligand)
mcherry_PE$rat <- factor(mcherry_PE$rat)

ggplot(mcherry_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="mcherry TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("mcherryPE.pdf")


ggplot(mcherry_PE, aes(x=factor(ligand,level=c('saline','JHU 0.05mg/kg','JHU 0.50mg/kg')), y=totalPE,fill=ligand)) + 
  stat_summary( position="dodge",fun = "mean", geom = "bar")+
  #geom_line( aes(group=rat,linetype=type))+
  facet_grid(cols=vars(sex)) +
  stat_summary(fun = "mean", geom = "bar")+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2)+
  labs(title="TotalPEs",x ="ligand", y = "Total PEs ")+
  labs(title="mcherry TotalPEs",x ="ligand", y = "Total PEs ") + 
  scale_fill_manual(values=c("#9ecae1","#3182bd","grey"))+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))
ggsave("mcherryPE_sex.pdf")


##lme
gi_totalPE_model <- lmer(totalPE ~ as.factor(ligand) * sex + (1 | rat), data = totalPE%>% filter(dreadd == "Gi"))
summary(gi_totalPE_model)
gi_totalPE_model_anova <-anova(gi_totalPE_model)
capture.output(gi_totalPE_model, file = "gi_totalPE_model_lme_results.doc")
capture.output(gi_totalPE_model_anova, file = "gi_totalPE_model_anova_results.doc")

## means
GiPEmeansbysex<-totalPE%>% filter(dreadd == "Gi")%>%
  group_by(ligand,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)

GiPEmeans<-totalPE%>% filter(dreadd == "Gi")%>%
  group_by(ligand) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)


gq_totalPE_model <- lmer(totalPE ~ as.factor(ligand) * sex + (1 | rat), data = totalPE%>% filter(dreadd == "Gq"))
summary(gq_totalPE_model)
gq_totalPE_model_anova <-anova(gq_totalPE_model)
capture.output(gq_totalPE_model, file = "gq_totalPE_model_lme_results.doc")
capture.output(gq_totalPE_model_anova, file = "gq_totalPE_model_anova_results.doc")

## means
GqPEmeansbyrat<-totalPE%>% filter(dreadd == "Gq")%>%
  group_by(ligand,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)

GqPEmeans<-totalPE%>% filter(dreadd == "Gq")%>%
  group_by(ligand) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)

GqPEmeansbysex<-totalPE%>% filter(dreadd == "Gq")%>%
  group_by(ligand,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)


mcherry_totalPE_model <- lmer(totalPE ~ as.factor(ligand) * sex + (1 | rat), data = totalPE%>% filter(dreadd == "mCherry"))
summary(mcherry_totalPE_model)
mcherry_totalPE_model_anova <-anova(mcherry_totalPE_model)
capture.output(mcherry_totalPE_model, file = "mcherry_totalPE_model_lme_results.doc")
capture.output

## means
mcherryPEmeansbyrat<-totalPE%>% filter(dreadd == "mCherry")%>%
  group_by(ligand,rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)

mcherryPEmeans<-totalPE%>% filter(dreadd == "mCherry")%>%
  group_by(ligand) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)

mcherryPEmeansbysex<-totalPE%>% filter(dreadd == "mCherry")%>%
  group_by(ligand,sex) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), totalPE)
