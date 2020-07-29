library(readxl)
library(magrittr)
library(lme4)
library(car)
library(tidyverse)
library(afex)
#setwd('M:/Lab_Shared/HaoLu/mGLMM_Anoo')

CI_data <- read_excel("M:/Lab_Shared/HaoLu/GLMM_Anoo/GLMM_stats_dataset.xlsx")

CI_data$ID %<>% as.factor()
CI_data$Slope %<>% as.factor()
CI_data$Position %<>% as.factor()
CI_data$Channel %<>% as.factor()

CI_data$Accuracy = CI_data$Accuracy/100

CI_data_raw = CI_data
CI_data$Response = cbind(CI_data$Accuracy * 80,(1-CI_data$Accuracy)*80)

CI_data_exp1 = CI_data[CI_data$Slope==360,]
CI_data_exp2 = CI_data[CI_data$Slope!=360,]


options(contrasts = c('contr.sum','contr.poly')) #default contrast setting does not satisfy type-III anova assumption

CI_data_exp2$Slope = droplevels(CI_data_exp2$Slope) # proven to be crucial so afex package don't freak out



#m.full = glmer(Response ~ Slope*Position*Channel+(1|ID), data=CI_data_exp2, family='binomial')
m.alt = glmer(Response ~ Slope+Position+Channel+Slope:Position + Position:Channel+(1|ID), data=CI_data_exp2, family='binomial')
m.full = glmer(cbind(CI_data_exp2$Accuracy * 80,(1-CI_data_exp2$Accuracy)*80) ~ Slope*Position*Channel+(1|ID), data=CI_data_exp2, family=binomial,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
m.alt = glmer(Response ~ Position*Channel+(1|ID), data=CI_data_exp1, family='binomial')

#ggplot(CI_data,aes(x=Channel,y=Accuracy,group=ID,color=ID))+geom_line()+facet_grid(Position~Slope)

CI_data_norm <- CI_data %>% group_by(ID) %>% mutate(NormAccuracy=Accuracy-mean(Accuracy))

#m.alt = glmer(Response ~ Slope*Channel+(1|ID), data=CI_data_exp2, family='binomial')
#print(summary(m.full))
anova(m.full)

library(parallel)
(nc <- detectCores()) # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster

mix = mixed(Response ~ Slope*Position*Channel+(1|ID), data=CI_data_exp2, family=binomial,method="PB",cl=cl,all_fit=FALSE,args_test=list(cl=cl, nsim=1000))
nice(mix)
