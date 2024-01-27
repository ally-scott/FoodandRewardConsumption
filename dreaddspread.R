####LOAD PACKAGES AND DATA#################################################################################################
library(readxl)
library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(rio)
insrm(list = ls())

# specifying the path name
path <- "C:/Users/Ally/OneDrive/Thesis/"

setwd(path)

# accessing all the sheets 
sheet = excel_sheets("dreaddspread.xlsx")

# applying sheet names to dataframe names
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel("dreaddspread.xlsx", sheet=x))

# attaching all dataframes together
data_frame = bind_rows(data_frame, .id="Sheet")

data_frame<- data_frame %>%
  mutate(rat = parse_number(`Sheet`),
         volume = `volume`,
         sex = sex,
         dreadd_type = `DREADD Type`,
         brainregion= `Region name`,
         pixelcount= `Object pixels`,
         objectcount=`Object count`) %>% 
  select(rat, volume, sex, dreadd_type,brainregion, objectcount)
# take out habituation and excluded animals
spreadd<- filter(data_frame, objectcount >5,brainregion!='Clear Label')


spreadmenasbyregion<-spreadd%>% 
  group_by(brainregion) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), objectcount)

spreadmenasbyrat<-spreadd%>% 
  group_by(rat) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), objectcount)



spreadsumnbyrat<-spreadd%>% 
  group_by(rat)%>%
  summarise(totalobjectcount = sum(objectcount)) %>%
  right_join(spreadd, by = "rat")
  
spreadsumnbyrat <- spreadsumnbyrat %>% mutate(cellperc = (objectcount/totalobjectcount) *100)

#spreadmenasbyregion<-spreadd%>% 
 # group_by(brainregion) %>%
  #summarise(across(c(totalobjectcount),sum))
  
spreadcellperct<-spreadsumnbyrat%>% 
  group_by(brainregion) %>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())), cellperc)  

#MODLES
### want to add in volume and sex eventually
spread_model <- lmer(cellperc ~ as.factor(brainregion) + (1 | rat), data = spreadsumnbyrat)
summary(spread_model)
spread_anova <-anova(spread_model)



## Follow up comparisons on significant models 

brainregioncontrast<- emmeans(spread_model,specs = ~ as.factor(brainregion) )

VP<- c(0,0,0,1)
corticofugaltractandcoronaradiata<- c(0,0,1,0)
caudateputamen<- c(0,1,0,0)
bednucstriaterm<- c(1,0,0,0)

spreaddcon<-contrast(brainregioncontrast ,method = list("VP - corticofugal tract and corona radiata" = VP - corticofugaltractandcoronaradiata,
                                                    "VP - Caudate putamen" = VP - caudateputamen,
                                                    "VP - Bed nucleus of the stria terminalis" = VP - bednucstriaterm,
                                                    "corticofugal tract and corona radiata-Bed nucleus of the stria terminalis" = corticofugaltractandcoronaradiata - bednucstriaterm,
                                                    "corticofugal tract and corona radiata-causate putamen" = corticofugaltractandcoronaradiata - caudateputamen))
capture.output(spreaddcon, file = "spread_pairwise_results.doc")



## brain region plots

library(ggplot2)

ggplot(spreadd, aes(x = reorder(brainregion, -objectcount), y=objectcount,fill=brainregion)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2,aes(shape=as.factor(volume)))+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="DREADD spread",x ="Brain Region", y = "Object count (#) ")+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))

ggsave("spreadd_all.pdf")

ggplot(spreadsumnbyrat, aes(x = reorder(brainregion, -cellperc), y=cellperc,fill=brainregion)) + 
  stat_summary(fun = "mean", geom = "bar")+
  geom_line(aes(group=rat),size = 1,alpha=0.2)+
  geom_point(size = 3,alpha=0.2,aes(shape=as.factor(volume)))+
  stat_summary(fun.data = mean_se,geom = "errorbar", size = 1)+
  labs(title="DREADD spread",x ="Brain Region", y = "Object perc (%) ")+ 
  theme_classic()+ 
  theme( axis.line = element_line(colour = "black", size = 2, linetype = "solid"))+
  theme( axis.ticks = element_line(colour = "black", size = 2, linetype = "solid"))

ggsave("spreaddperc_all.pdf")


