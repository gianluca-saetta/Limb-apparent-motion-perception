#Set working directory. The file "Study2_Dataset.csv" should be in the same folder of this script.  
setwd(getwd())

#Load csv file in R
Data <- read.csv("Study2_Dataset.csv", header = TRUE, stringsAsFactors = FALSE)

#### Demographic info ####
attach(Data)
Data_demo_AMP <- aggregate(data = Data[which(group == "AMP"),], age ~ subject_id +  gender + amputation_side,  FUN = unique) 
Data_demo_BID <- aggregate(data = Data[which(group == "BID"),], age ~ subject_id +  gender + amputation_side,  FUN = unique)
Data_demo_HC <- aggregate(data = Data[which(group == "HC"),], age ~ subject_id ,  FUN = unique) #only males 

#Report demographic info
library(report)
report(Data_demo_AMP)
report(Data_demo_BID)
report(Data_demo_HC)

#### Descriptive statistics on phantom limb sensations and prosthesis use in individuals with an amputation ####
#Create a df only for amputee containing unique values for parameters defining the phantom limb sensations
attach(Data)
Data_phantomlimb <- aggregate(data = Data[which(group == "AMP"),], phantom_control ~  subject_id + age  + prostheis_wear_yn + prosthesis_wear_how_often
                              + prosthesis_partofbody + prosthesis_control + phantom_presence_lastthreemonths +
                                 + phantom_sensationstrength_lastfourweeks + phantom_presence_howoften,  FUN = unique) 

report(Data_phantomlimb)

#Assess the presence of phantom limb sensations over the past three months
#0 = No, I have never experienced a phantom limb
#1 = No, but I used to experience a phantom in the past
#2 = Yes, I currently am experiencing a phantom limb
table(Data_phantomlimb$phantom_presence_lastthreemonths) 

#### Descriptive statistics on the desire for amputation in individuals affected by body integrity dysphoria ####
Data_BID <- aggregate(data = Data[which(group == "BID"),], pure_desire ~  subject_id + age  + 
                        + erotic_attraction + pretending_behav,  FUN = unique) 

report(Data_BID)

#### Data analysis preparation ####
#Define factors
Data$soa <- as.factor(Data$soa)
Data$sensorimotor_state <- as.factor(Data$sensorimotor_state)
Data$subject_id <- as.factor(Data$subject_id)

#Recode responses
Data$key_press[Data$key_press == 0] = -1

#### Data analysis in individuals with an amputation ####
#Test the need for a multilevel approach by modeling a random intercept for each participants. This follows the methods described in 
#Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. Sage publications.
library(lmerTest)
library(multilevel)
interceptonly_amp <- gls(key_press ~ 1,  data = Data[which(group == "AMP"),], method = "ML")
summary(interceptonly)
randominterceptonly_amp <-  lme(key_press ~ 1, data = Data[which(group == "AMP"),], random = ~1|subject_id, method = "ML" )
anova(interceptonly_amp, randominterceptonly_amp)

#Linear mixed model 
mlm_amp <- lmer(formula = key_press ~  sensorimotor_state*soa*prosthesis_partofbody* +
               (1|subject_id), data = Data[which(group == "AMP"),])
aov_mlm_amp <- anova(mlm_amp)

#Create a table formatted according to the APA style
library(knitr)
table_mlm_amp <- kable(aov_mlm_amp, digits = 3, format = "pandoc")

#Post-hoc tests for the SOA effect
library(emmeans)
emm_amp_soa <- emmeans(mlm_amp, ~ soa, lmerTest.limit = 7424, pbkrtest.limit = 7424)
contrast(emm_amp_soa, method = "pairwise", adjust = "bonferroni")

#Plot 3-way interaction (sensorimotor state by soa by prosthesis part of body (i.e., prosthesis ownership))
library(ggplot2)
library(tidyverse)
theme_set(
  theme_bw() 
    ) 
ggplot( Data[which(group == "AMP"),], aes(x=prosthesis_partofbody, y= key_press, group = interaction(soa, sensorimotor_state))) + 
  geom_smooth(method="lm", se = FALSE, alpha = 0.1, aes(lty= fct_rev(sensorimotor_state), color=soa)) + 
  scale_y_continuous(breaks = seq(-1,1,by = 0.2), limits = c(-1,1)) +
  theme(axis.text=element_text(size=14)) +
  labs( x = "Prosthesis Ownership", y = "Illusion Experience")  +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14)) + 
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank() , axis.line = element_line(colour = "black")) 

#Supplementary materials analyses
mlm_amp_som <- lmer(formula = key_press ~  sensorimotor_state*soa*limb*consistency_amputation*constraint*perspective+
               (1|subject_id), data = Data[which(group == "AMP"),])
anova(mlm_amp_som)

#Create a table
table_mlm_amp_som <- kable(anova(mlm_amp_som), digits = 3, format = "pandoc")

#### Data analysis in BID individuals ####
#Test the need for a multilevel approach
interceptonly_bid <- gls(key_press ~ 1,  data = Data[which(group == "BID"),], method = "ML")
summary(interceptonly_bid)
randominterceptonly_bid <-  lme(key_press ~ 1, data = Data[which(group == "BID"),], random = ~1|subject_id, method = "ML")
anova(interceptonly_bid, randominterceptonly_bid)

#Linear mixed model 
mlm_bid <- lmer(formula = key_press ~  sensorimotor_state*soa +
               (1|subject_id), data = Data[which(group == "BID"),])
anova(mlm_bid)

#Post-hoc effectSOA 
emm_bid_soa <- emmeans(mlm_bid, ~ soa)
contrast(emm_bid_soa, method = "pairwise",  adjust = "bonferroni")

#Plot effect sensorimotor state
library(yarrr)
pirateplot(data = Data[which(group == "BID"),], key_press ~ sensorimotor_state, theme = 4, ylab = "Illusion Experience", xlab = "Sensorimotor state",
           cex.lab = 1.3, cex.axis = 1.3,cex.names = 1.3) 

#Supplementary materials analyses
mlm_bid_som <- lmer(formula = key_press ~  sensorimotor_state*soa*limb*consistency_amputation*constraint*perspective +
               (1|subject_id), data = Data[which(group == "BID"),])
anova(mlm_bid_som)

#Create table
table_bid_som <- kable(anova(mlm_bid_som), digits = 3, format = "pandoc")

#### Data analyses in able-bodied individuals ####
#Test the need for a multilevel approach
interceptonly_hc <- gls(key_press ~ 1,  data = Data[which(group == "HC"),], method = "ML")
summary(interceptonly_hc)
randominterceptonly_hc <-  lme(key_press ~ 1, data = Data[which(group == "HC"),], random = ~1|subject_id, method = "ML" )
anova(interceptonly_hc, randominterceptonly_hc)

#Linear mixed model
mlm_hc <- lmer(formula = key_press ~ soa*sensorimotor_state +
              (1|subject_id), data = Data[which(group == "HC"),])
anova(mlm_hc)

#Plot 2-way interaction sensorimotor state by soa
pirateplot(data = Data[which(group == "HC"),], key_press ~ sensorimotor_state*soa,  theme = 4, ylab = "Illusion Experience", xlab = "Sensorimotor state")

#Post-hoc test soa
emm_hc_soa<- emmeans(mlm_hc,~ soa, pbkrtest.limit = 3328, merTest.limit = 3328) 
contrast(emm_hc_soa, method = "pairwise", adjust = "bonferroni")

#Post-hoc test sensorimotor state by soa
emm_hc_sensorimotorstate_soa <- emmeans(mlm_hc, ~ sensorimotor_state*soa, pbkrtest.limit = 3328, merTest.limit = 3328 ) 
contrast(emm_hc_sensorimotorstate_soa, method = "pairwise", by = "soa", adjust = "bonferroni")

#Supplementary materials analyses
mlm_hc_som <- lmer(formula = key_press ~  sensorimotor_state*soa*limb*consistency_amputation*constraint*perspective+
               (1|subject_id), data = Data[which(group == "AMP"),])
anova(mlm_hc_som)

#Create table
table <- kable(anova(mlm3), digits = 3, format = "pandoc")