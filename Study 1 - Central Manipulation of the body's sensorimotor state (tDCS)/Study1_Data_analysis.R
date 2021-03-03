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

#Define factors
Data$tDCS <- as.factor(Data$tDCS)
Data$SOA <- as.factor(Data$SOA)

#Test the need for a multilevel approach by modeling a random intercept for each participants. This follows the methods described in 
#Field, A., Miles, J., & Field, Z. (2012). Discovering statistics using R. Sage publications.
library(lmerTest)
library(multilevel)
interceptonly <- gls(RESP ~ 1,  data = Data, method = "ML")
summary(interceptonly)
randominterceptonly <-  lme(RESP ~ 1, data = Data, random = ~1|ID, method = "ML" )
anova(interceptonly, randominterceptonly)

#Linear mixed model 
mlm <- lmer(formula = RESP ~ tDCS*SOA +
                                (1|ID), data = Data)
summary(mlm)
anova(mlm)

#Report Mean and sd in a talble (supplemetary materials)
# This table produces mean and sd  for each combination of the levels of the factors tDCS and SOA
library(doBy)
table_mean_sd <- summaryBy(RESP ~ tDCS + SOA , data = Data,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

#Post-hoc tests
#Main effect of SOA 
library(emmeans)
emm <- emmeans(mlm, ~ SOA) 
contrast(emm, method = "pairwise",  adjust = "bonferroni")

#tDCS by SOA interaction
emm <- emmeans(mlm, ~ tDCS*SOA) 
contrast(emm, method = "pairwise", by = "SOA", adjust = "bonferroni")

#Plot
Data$RESP[Data$RESP == 0] = -1
pirateplot(data = Data, RESP ~ tDCS*SOA, theme = 4, ylab = "Illusion Experience")

#Supplementary materials analyses 
mlm_som <- lmer(formula = RESP ~ tDCS*SOA*Laterality +
              (1|ID), data = Data)
summary(mlm_som)
aov_mlm_som <- anova(mlm_som)

#Create a table formatted according the the APA style
library(knitr)
table2sm <- kable(
  aov_mlm_som, 
  digits = 3, 
  format = "pandoc")