#Set working directory. The file "Study1_Dataset.xlsx" should be in the same folder of this script.  
setwd(getwd())

#Load excel files in R
library(readxl)
Data <- read_excel("Study1_Dataset.xlsx")

#Demographic info 
Data_demo <- aggregate(data = Data, Age ~ ID + Sex + tDCS,  FUN = unique) 

#All the participants
table(Data_demo$Sex) 
mean(Data_demo$Age)
sd(Data_demo$Age)

#Participants in the tDCS experimental group
table(Data_demo[which(Data_demo$tDCS == "Real" ),]$Sex)  
mean(Data_demo[which(Data_demo$tDCS == "Real" ),]$Age) 
sd(Data_demo[which(Data_demo$tDCS == "Real" ),]$Age) 

#Participants in the tDCS experimental group
table(Data_demo[which(Data_demo$tDCS == "Sham" ),]$Sex)
mean(Data_demo[which(Data_demo$tDCS == "Sham" ),]$Age)  
sd(Data_demo[which(Data_demo$tDCS == "Sham" ),]$Age) 

#Between group age differences
t.test(Data_demo[which(Data_demo$tDCS == "Real" ),]$Age ,Data_demo[which(Data_demo$tDCS == "Sham" ),]$Age) 

#Follow the three-step procedure described in 
#Sommet, N., & Morselli, D. (2017). Keep calm and learn multilevel logistic modeling: A simplified three-step procedure using stata, R, Mplus, and SPSS.
#International Review of Social Psychology, 30, 203-218.

##########################################################################
#PRELIMINARY PHASE: Centering Variables 
##########################################################################
#Center "SOA"
grand_mean_SOA <- mean(Data$SOA, na.rm=T)
Data$SOA_gmc <- Data$SOA - grand_mean_SOA
Data$SOA_gmc <- round(Data$SOA_gmc ,1)

#Center "tDCS"
Data$tDCS_gmc <- NA
Data$tDCS_gmc[Data$tDCS == "Real"] <- -0.5
Data$tDCS_gmc[Data$tDCS == "Sham"] <- 0.5

library(lme4)
#Run an empty model, that is, a model containing no predictors, 
# and calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).
M0 <- glmer(RESP ~ ( 1 | ID), data= Data, family = "binomial")
summary(M0)

icc <- M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3))
icc

#########
#RESULTS#
#########
#N = 2304 observations are nested in K = 24 classes;
#The fixed intercept is in the "Fixed effects" part of the output: (Intercept) -0.106; 
#The random intercept variance (i.e., the level-2 residual) is in the "Random effects" part of the output: classes (Intercept) 0.9152;
#The intraclass correlation coefficient is ICC = .2176; 
#This indicates that 22% of the chance of perceiving the sensorimotorically guided path is explained by between-subjects differences 

#############################################################################
#STEP #2: Building the Intermediate Models 
#############################################################################
CIM <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 | ID), data = Data, family = "binomial")
summary(CIM)
paste("FYI: The deviance of the CIM is:", CIM@devcomp$cmp[[8]])

AIM <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 + tDCS_gmc || ID), data = Data, family = "binomial")
summary(AIM)
paste("FYI: The deviance of the AIM is:", AIM@devcomp$cmp[[8]])

anova(CIM, AIM)

#RESULTS#
#########
#The likelihood-ratio test is given by the anova function, and referred by the parameter Chisq = 0, Chi Df = 1, p < 0.9986.
#Thus, allowing the effect of "tDCS_gmc" to vary between subjects does not  improve the fit and it is better not to take the random slope into account. 

#Run the final model (adding the cross-level interaction(s)). 
FM <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 | ID) + SOA_gmc:tDCS_gmc, data = Data, family = "binomial")
summary(FM)

#Exclude the factor Limb 
FM_limb <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 | ID) + SOA_gmc:tDCS_gmc+ Limb, data = Data, family = "binomial")
FM_limb_interaction <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 | ID) + SOA_gmc:tDCS_gmc* Limb, data = Data, family = "binomial")

anova(FM_limb,FM_limb_interaction)
#Calculate the first simple slope
#namely the effect of "tDCS_gmc" when "SOA_gmc" is fast (+ 1 SD)
sd_SOA_gmc <- sd(Data$SOA_gmc)
Data$SOA_p1SD <- Data$SOA_gmc + sd_SOA_gmc
FM_p1SD <- glmer(RESP ~ SOA_p1SD + tDCS_gmc + (1 | ID) + SOA_p1SD:tDCS_gmc, data = Data, family = "binomial")
summary(FM_p1SD)

#Calculate the second simple slope
#namely the effect of "tDCS_gmc" when "SOA_gmc" is slow (-1 SD).
Data$SOA_m1SD <- Data$SOA_gmc - sd_SOA_gmc
FM_m1SD <- glmer(RESP ~ SOA_m1SD + tDCS_gmc + (1 | ID) + SOA_m1SD:tDCS_gmc, data = Data, family = "binomial")
summary(FM_m1SD)

#Compare the coefficient estimates obtained in the final model,
#with (glmer) or without (glm) the use of multilevel modelling.
GLMER <- glmer(RESP ~ SOA_gmc + tDCS_gmc + (1 | ID) + SOA_gmc:tDCS_gmc, data = Data, family = "binomial")
GLM <-  glm(RESP ~ SOA_gmc + tDCS_gmc + SOA_gmc:tDCS_gmc, data = Data, family = "binomial")
summary(GLMER)
summary(GLM)

#Calculate Odd-ratios
OR <- exp(fixef(FM))
CI <- exp(confint(FM,parm="beta_",method="Wald")) 

OR_m1SD <- exp(fixef(FM_m1SD))
CI_m1SD <- exp(confint(FM_m1SD,parm="beta_", method="Wald"))

OR_p1SD <- exp(fixef(FM_p1SD))
CI_p1SD <- exp(confint(FM_p1SD,parm="beta_", method="Wald"))

OR.CI<-rbind(cbind(OR,CI), cbind(OR_m1SD,CI_m1SD)[3,], cbind(OR_p1SD,CI_p1SD)[3,])
rownames(OR.CI)<-c(rownames(cbind(OR,CI)), "tDCS_gmc_m1SD", "tDCS_gmc_p1SD")
OR.CI

#Plots
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(grid)

set_theme(base = theme_classic(), #To remove the background color and the grids
          theme.font = 'serif',   #To change the font type
          axis.title.size = 2.5,  #To change axis title size
          axis.textsize.x = 1.5,  #To change x axis text size
          axis.textsize.y = 1.5,  #To change y axis text size
          legend.title.size = 2.5,
          )  


Figure2 <- sjPlot::plot_model(FM, type = "pred", terms = c("SOA_gmc", "tDCS_gmc"),
           axis.title = c("SOA","Sensorimotorically Guided Perception Predicted Probability "), 
           title =  "",  legend.title = "tDCS") +
  scale_x_continuous(breaks=c(-300, -200, -100, 0, 100, 200, 300)) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = c(0.85, 0.2)) +
  theme(legend.text = element_text(colour="black", size=30, 
                                   )) 

#Save plot Saving 17.6 x 11.6 in image
ggsave("Figure2.pdf")


#Create Table 1 of the manuscript
library(sjPlot)
tab_model(FM, FM_p1SD, FM_m1SD, file = "table1.doc")

#Supplementary materials analyses 

Data$Limb_gmc[Data$Limb == "Lower"] <- -0.5
Data$Limb_gmc[Data$Limb == "Upper"] <- 0.5

Data$Laterality_gmc[Data$Laterality == "L"] <- -0.5
Data$Laterality_gmc[Data$Laterality == "R"] <- 0.5

FM_sm_mod1 <- glmer(RESP ~ SOA_gmc*tDCS_gmc*Limb_gmc + (1 | ID), data = Data, family = "binomial")
summary(FM_sm_mod1)

FM_sm_Lmod2 <- glmer(RESP ~ SOA_gmc*tDCS_gmc*Laterality_gmc + (1 | ID), data = Data, family = "binomial")
summary(FM_sm_mod1)

#Create Table 1 sm of the supplementary materials
tab_model(FM_sm_Limb, FM_sm_Laterality, file = "table1sm.doc")