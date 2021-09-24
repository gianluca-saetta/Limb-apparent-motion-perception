#Set working directory. The file "Study2_Dataset.csv" should be in the same folder of this script.  
setwd(getwd())

#Load csv file in R
Data <- read.csv("Study2_Dataset_new.csv", header = TRUE, stringsAsFactors = FALSE)

#### Demographic info ####
attach(Data)
Data_demo_AMP <- aggregate(data = Data[which(group == "AMP"),], age ~ subject_id +  gender + amputation_side,  FUN = unique) 
Data_demo_BID <- aggregate(data = Data[which(group == "BID"),], age ~ subject_id +  gender + amputation_side,  FUN = unique)
Data_demo_HC <- aggregate(data = Data[which(group == "HC"),], age ~ subject_id + gender + amputation_side ,  FUN = unique) #only males 

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

#Follow the three-step procedure described in 
#Sommet, N., & Morselli, D. (2017). Keep calm and learn multilevel logistic modeling: A simplified three-step procedure using stata, R, Mplus, and SPSS.
#International Review of Social Psychology, 30, 203-218.

#Center "SOA"
grand_mean_soa <- mean(Data$soa, na.rm=T)
Data$soa_gmc <- Data$soa - grand_mean_soa
Data$soa_gmc <- round(Data$soa_gmc ,1)

##########################################################################
#INDIVIDUALS WITH AMPUTATION
##########################################################################

#Center "sensorimotor state"
Data$sensorimotor_state_gmc <- NA
Data$sensorimotor_state_gmc[Data[which(group == "AMP"),"sensorimotor_state"] == "Prosthesis_off"] <- -0.5
Data$sensorimotor_state_gmc[Data[which(group == "AMP"),"sensorimotor_state"] == "Prosthesis_on"] <- 0.5

#Center "Prosthesis part of the body" 
grand_mean_prosthesis_partofbody <- mean(Data$prosthesis_partofbody, na.rm=T)
Data$prosthesis_partofbody_gmc <- Data$prosthesis_partofbody - grand_mean_prosthesis_partofbody
Data$prosthesis_partofbody_gmc <- round(Data$prosthesis_partofbody_gmc ,1)

#Center "Limb"
Data$limb_gmc <- NA
Data$limb_gmc[Data$limb == "Leg"] <- -0.5
Data$limb_gmc[Data$limb == "Arm"] <- 0.5

library(lme4)
#Run an empty model, that is, a model containing no predictors, 
# and calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).
M0_amp <- glmer(key_press ~ ( 1 | subject_id), data= Data[which(group == "AMP"),], family = "binomial")
summary(M0_amp)

icc_amp <- M0_amp@theta[1]^2/ (M0_amp@theta[1]^2 + (3.14159^2/3))
icc_amp

#N = 7424 observations are nested in K = 29 subhects;
#The fixed intercept is in the "Fixed effects" part of the output: (Intercept) 0.615; 
#The random intercept variance (i.e., the level-2 residual) is in the "Random effects" part of the output: classes (Intercept) 1.693;
#The intraclass correlation coefficient is ICC = 0.3397644; 
#This indicates that 34% of the chance of perceiving the sensorimotorically guided path is explained by between-subjects differences 

#Build the intermediate models 
CIM_amp <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc + prosthesis_partofbody_gmc + (1 | subject_id),  data = Data[which(group == "AMP"),], family = "binomial")
summary(CIM_amp)
paste("FYI: The deviance of the CIM is:", CIM_amp@devcomp$cmp[[8]])

AIM_amp <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc + prosthesis_partofbody_gmc + (1 + sensorimotor_state_gmc || subject_id),  data = Data[which(group == "AMP"),], family = "binomial")
summary(AIM_amp)
paste("FYI: The deviance of the AIM is:", AIM_amp@devcomp$cmp[[8]])

anova(CIM_amp, AIM_amp)

#The likelihood-ratio test is given by the anova function, and referred by the parameter Chisq = 0, Chi Df = 1, p < 0.9986.
#Thus, allowing the effect of "sensorimotor_state_gmc" to vary between subjects does not  improve the fit and it is better not to take the random slope into account. 
FM_amp <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc* prosthesis_partofbody_gmc + (1 |subject_id),
            data = Data[which(group == "AMP"),], family = "binomial",
            control=glmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e5)))
summary(FM_amp)

#Exclude the factor Limb from analyses

FM_amp_limb <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc* prosthesis_partofbody_gmc+limb_gmc + (1 |subject_id),
                data = Data[which(group == "AMP"),], family = "binomial")

FM_amp_limb_interaction <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc* prosthesis_partofbody_gmc*limb_gmc + (1 |subject_id),
                     data = Data[which(group == "AMP"),], family = "binomial")

anova(FM_amp_limb,FM_amp_limb_interaction)

#Calculate Odd ratios and create table for manuscript
library(sjPlot)
tab_model(FM_amp)

#Plot
library(ggeffects)

library(sjPlot)
library(sjmisc)
library(ggplot2)
library(grid)

set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          axis.title.size = 1.5,  #To change axis title size
          axis.textsize.x = 1,  #To change x axis text size
          axis.textsize.y = 1, #To change y axis text siv
          legend.title.size = 1,
)  


Figure4 <- sjPlot::plot_model(FM_amp, type = "pred", terms = c("prosthesis_partofbody_gmc","soa_gmc", "sensorimotor_state_gmc"),
                   axis.title = c("Prosthesis ownership","Sensorimotorically Guided Perception Predicted Probability", "papa"), 
                   title =  "",  legend.title = "SOA") +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = c(0.9, 0.2)) +
  theme(legend.text = element_text(colour="black", size=25, 
  )) 

#Saving 25.2 x 15.1 in image
ggsave("Figure4.pdf",
       dpi = 320, 
       limitsize = FALSE)

##########################################################################
#INDIVIDUALS WITH BODY INTEGRITY DYSPHORIA
##########################################################################

#Center "sensorimotor state"
Data$sensorimotor_state_gmc <- NA
Data$sensorimotor_state_gmc[Data[which(group == "BID"),"sensorimotor_state"] == "Pretending"] <- -0.5
Data$sensorimotor_state_gmc[Data[which(group == "BID"),"sensorimotor_state"] == "Sitting_Normally"] <- 0.5

library(lme4)
#Run an empty model, that is, a model containing no predictors, 
# and calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).
M0_bid <- glmer(key_press ~ ( 1 | subject_id), data= Data[which(group == "BID"),], family = "binomial")
summary(M0_bid)

icc_bid <- M0_bid@theta[1]^2/ (M0_bid@theta[1]^2 + (3.14159^2/3))
icc_bid

#N = 2560 observations are nested in K = 10 classes;
#The fixed intercept is in the "Fixed effects" part of the output: (Intercept) 0.3220; 
#The random intercept variance (i.e., the level-2 residual) is in the "Random effects" part of the output: classes (Intercept) 0.8328;
#The intraclass correlation coefficient is ICC = .2019976; 
#This indicates that 20% of the chance of perceiving the sensorimotorically guided path is explained by between-subjects differences 

#Build the intermediate models 
CIM_bid <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc +  (1 | subject_id),  data = Data[which(group == "BID"),], family = "binomial")
summary(CIM_bid)
paste("FYI: The deviance of the CIM is:", CIM_bid@devcomp$cmp[[8]])

AIM_bid <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc +  (1 + sensorimotor_state_gmc || subject_id),  data = Data[which(group == "BID"),], family = "binomial")
summary(AIM_bid)
paste("FYI: The deviance of the AIM is:", CIM_bid@devcomp$cmp[[8]])

anova(CIM_bid, AIM_bid)

#The likelihood-ratio test is given by the anova function, and referred by the parameter Chisq = 20.961, Chi Df = 1, p < 0.0001.
#Thus, allowing the effect of "sensorimotor_state_gmc" to vary between subjects  improve the fit and it is better  to take the random slope into account. 
FM_bid <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc + (1 + sensorimotor_state_gmc || subject_id),
                data = Data[which(group == "BID"),], family = "binomial")
summary(FM_bid)

#Calculate Odd ratios and create table for manuscript
library(sjPlot)
tab_model(FM_bid)

#Exclude the factor Limb from analyses

FM_bid_limb <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc+limb_gmc + (1 |subject_id),
                     data = Data[which(group == "BID"),], family = "binomial")

FM_bid_limb_interaction <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*limb_gmc + (1 |subject_id),
                                 data = Data[which(group == "BID"),], family = "binomial")

anova(FM_bid_limb,FM_bid_limb_interaction)

##########################################################################
# HEALTHY INDIVIDUALS
##########################################################################

#Center "sensorimotor state"
Data$sensorimotor_state_gmc <- NA
Data$sensorimotor_state_gmc[Data[which(group == "HC"),"sensorimotor_state"] == "Sitting_on_Leg"] <- -0.5
Data$sensorimotor_state_gmc[Data[which(group == "HC"),"sensorimotor_state"] == "Sitting_Normally"] <- 0.5

library(lme4)
#Run an empty model, that is, a model containing no predictors, 
# and calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).
M0_hc <- glmer(key_press ~ ( 1 | subject_id), data= Data[which(group == "HC"),], family = "binomial")
summary(M0_hc)

icc_hc <- M0_hc@theta[1]^2/ (M0_hc@theta[1]^2 + (3.14159^2/3))
icc_hc

#N = 7168 observations are nested in K = 29 participants;
#The fixed intercept is in the "Fixed effects" part of the output: (Intercept) 0.06622; 
#The random intercept variance (i.e., the level-2 residual) is in the "Random effects" part of the output: classes (Intercept) 0.3489;
#The intraclass correlation coefficient is ICC = 0.09588802; 
#This indicates that 10% of the chance of perceiving the sensorimotorically guided path is explained by between-subjects differences 

#Build the intermediate models 
CIM_hc <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc + limb_gmc+  (1 | subject_id),  data = Data[which(group == "HC"),], family = "binomial")
summary(CIM_hc)
paste("FYI: The deviance of the CIM is:", CIM_hc@devcomp$cmp[[8]])

AIM_hc <- glmer(key_press ~ soa_gmc + sensorimotor_state_gmc +limb_gmc +(1 + sensorimotor_state_gmc || subject_id),  data = Data[which(group == "HC"),], family = "binomial")
summary(AIM_hc)
paste("FYI: The deviance of the AIM is:", AIM_hc@devcomp$cmp[[8]])

anova(CIM_hc, AIM_hc)

#The likelihood-ratio test is given by the anova function, and referred by the parameter Chisq = 20.961, Chi Df = 1, p < 0.0001.
#Thus, allowing the effect of "sensorimotor_state_gmc" to vary between subjects  improves the fit and it is better  to take the random slope into account. 
FM_hc <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*limb_gmc + (1 + sensorimotor_state_gmc || subject_id),
                data = Data[which(group == "HC"),], family = "binomial")
summary(FM_hc)


#Calculate Odd ratios and create table for manuscript
library(sjPlot)
tab_model(FM_hc)

#Plots
library(sjPlot)
library(sjmisc)
library(ggplot2)

set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          axis.title.size = 3,  #To change axis title size
          axis.textsize.x = 1.5,  #To change x axis text size
          axis.textsize.y = 1.5, #To change y axis text siv
          legend.title.size = 1,
)  

#Main effect of Limb
fit <-glmer(key_press ~ soa_gmc*as.factor(limb_gmc)*sensorimotor_state_gmc + (1 + sensorimotor_state_gmc || subject_id),
            data = Data[which(group == "HC"),], family = "binomial")
plot_model(fit,type = "pred", terms = c("limb_gmc"), 
                   axis.title = c("Limb","Sensorimotorically Guided Perception Predicted Probability "), 
                   title =  "",  legend.title = "") +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1))

#SOA by Sensorimotor State
Figure5a <- sjPlot::plot_model(FM_hc, type = "pred", terms = c("soa_gmc", "sensorimotor_state_gmc"),
                   axis.title = c("SOA","Sensorimotorically Guided Perception Predicted Probability "), 
                   title =  "",  legend.title = "") +
  scale_x_continuous(breaks=c(-300, -200, -100, 0, 100, 200, 300)) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = c(0.65, 0.33)) +
  theme(legend.text = element_text(colour="black", size=40, 
  ))

ggsave("Figure5a.png",
       dpi = 320, 
       limitsize = FALSE)

#Sensorimotor State by Limb 

Figure5b <-sjPlot::plot_model(fit, type = "pred", terms = c("limb_gmc", "sensorimotor_state_gmc"),
                   axis.title = c("Limb","Sensorimotorically Guided Perception Predicted Probability "), 
                   title =  "",  legend.title = "") +
  scale_x_continuous(breaks=c(-0.5,0.5)) +
  scale_x_continuous(limits=c(-1, 1)) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = c(0.75, 0.2)) +
  theme(legend.text = element_text(colour="black", size=40, 
  ))
ggsave("Figure5b.png",
       dpi = 320, 
       limitsize = FALSE)


#Create Table 3 of the manuscript^3
library(sjPlot)
tab_model(FM_amp, FM_bid, FM_hc, file = "table3.doc")

#Supplementary materials analyses 

#Center "Consistency Amputation"
Data$consistency_amputation_gmc <- NA
Data$consistency_amputation_gmc[Data$consistency_amputation == "Not_Consistent"] <- -0.5
Data$consistency_amputation_gmc[Data$consistency_amputation == "Consistent"] <- 0.5

#Center "Constraint"
Data$constraint_gmc <- NA
Data$constraint_gmc[Data$constraint == "O"] <- -0.5
Data$constraint_gmc[Data$constraint == "B"] <- 0.5

#Center "Perspective"
Data$perspective_gmc <- NA
Data$perspective_gmc[Data$perspective == "3"] <- -0.5
Data$perspective_gmc[Data$perspective == "1"] <- 0.5



##########################################################################
# Supplementary Analyses INDIVIDUALS WITH AMPUTATION
##########################################################################
FM_amp_mod1 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*limb_gmc + (1 |subject_id),
                data = Data[which(group == "AMP"),], family = "binomial")
summary(FM_amp_mod1)

FM_amp_mod2 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*consistency_amputation_gmc + (1 |subject_id),
                     data = Data[which(group == "AMP"),], family = "binomial")
summary(FM_amp_mod2)

FM_amp_mod3 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*constraint_gmc + (1 |subject_id),
                     data = Data[which(group == "AMP"),], family = "binomial")
summary(FM_amp_mod3)

FM_amp_mod4 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*perspective_gmc + (1 |subject_id),
                     data = Data[which(group == "AMP"),], family = "binomial")
summary(FM_amp_mod4)

tab_model(FM_amp_mod1, FM_amp_mod2, FM_amp_mod3, FM_amp_mod4, file = "table2sm.doc")

##########################################################################
# Supplementary Analyses INDIVIDUALS WITH BODY INTEGRITY DYSPHORIA
##########################################################################
FM_bid_mod1 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*limb_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "BID"),], family = "binomial")
summary(FM_bid_mod1)

FM_bid_mod2 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*consistency_amputation_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "BID"),], family = "binomial")
summary(FM_bid_mod2)

FM_bid_mod3 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*constraint_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "BID"),], family = "binomial")
summary(FM_bid_mod3)

FM_bid_mod4 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*perspective_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "BID"),], family = "binomial")
summary(FM_bid_mod4)

tab_model(FM_bid_mod1, FM_bid_mod2, FM_bid_mod3, FM_bid_mod4, file = "table3sm.doc")

##########################################################################
# Supplementary Analyses ABLE-BODIED INDIVIDUALS
##########################################################################
FM_hc_mod1 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*limb_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "HC"),], family = "binomial")
summary(FM_hc_mod1)

FM_hc_mod2 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*consistency_amputation_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "HC"),], family = "binomial")
summary(FM_hc_mod2)

FM_hc_mod3 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*constraint_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "HC"),], family = "binomial")
summary(FM_hc_mod3)

FM_hc_mod4 <- glmer(key_press ~ soa_gmc*sensorimotor_state_gmc*perspective_gmc + (1 + sensorimotor_state_gmc || subject_id),
                     data = Data[which(group == "HC"),], family = "binomial")
summary(FM_hc_mod4)

tab_model(FM_hc_mod1, FM_hc_mod2, FM_hc_mod3, FM_hc_mod4, file = "table4sm.doc")

