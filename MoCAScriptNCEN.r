##### Script for "Longitudinal measurement properties of the Montreal Cognitive Assessment", submitted to Journal of Clinical and Experimental Neuropsychology

##### Install required packages
install.packages("foreign", "car", "mirt", "lavaan", "semTools", "xtable")

##### Load required packages
library(foreign)
library(car)
library(mirt)
library(lavaan)
library(semTools)
library(xtable)

##### Read in data and prepare data
mydir <- ""
mydat <- read.spss(paste0(mydir, "HS Data_EnvirAnalysis.sav"))
mydat <- as.data.frame(mydat)

##### MoCA variable names at time points 1 to 4
MoCAT1Names <- c("S6_MoCA_Trail", "S6_MoCA_Cube", "S6_MoCA_ClockShape", "S6_MoCA_ClockNumber",       
 "S6_MoCA_ClockHand", "S6_MoCA_Naming", "S6_MoCA_DigitSpan", "S6_MoCA_GoNoGo1",
 "S6_MoCA_Serial7", "S6_MoCA_LogicalMemory", "S6_MoCA_VerbalFluency", "S6_MoCA_Abstract",          
 "S6_MoCA_Memory", "S6_MoCA_Orient")

MoCAT2Names <- c("T2_S6_MoCA_Trail", "T2_S6_MoCA_Cube", "T2_S6_MoCA_ClockShape", "T2_S6_MoCA_ClockNumber",       
 "T2_S6_MoCA_ClockHand", "T2_S6_MoCA_Naming", "T2_S6_MoCA_DigitSpan", "T2_S6_MoCA_GoNoGo1",           
 "T2_S6_MoCA_Serial7", "T2_S6_MoCA_LogicalMemory", "T2_S6_MoCA_VerbalFluency", "T2_S6_MoCA_Abstract",          
 "T2_S6_MoCA_Memory", "T2_S6_MoCA_Orient")

MoCAT3Names <- c("T3_S6_MoCA_Trail", "T3_S6_MoCA_Cube", "T3_S6_MoCA_ClockShape", "T3_S6_MoCA_ClockNumber",       
 "T3_S6_MoCA_ClockHand", "T3_S6_MoCA_Naming", "T3_S6_MoCA_DigitSpan", "T3_S6_MoCA_GoNoGo1",           
 "T3_S6_MoCA_Serial7", "T3_S6_MoCA_LogicalMemory", "T3_S6_MoCA_VerbalFluency", "T3_S6_MoCA_Abstract",          
 "T3_S6_MoCA_Memory", "T3_S6_MoCA_Orient")

MoCAT4Names <- c("T4_S6_MoCA_Trail", "T4_S6_MoCA_Cube", "T4_S6_MoCA_ClockShape", "T4_S6_MoCA_ClockNumber",       
 "T4_S6_MoCA_ClockHand", "T4_S6_MoCA_Naming", "T4_S6_MoCA_DigitSpan", "T4_S6_MoCA_GoNoGo1",
 "T4_S6_MoCA_Serial7", "T4_S6_MoCA_LogicalMemory", "T4_S6_MoCA_VerbalFluency", "T4_S6_MoCA_Abstract",          
 "T4_S6_MoCA_Memory", "T4_S6_MoCA_Orient")

##### Define MoCA data objects
MoCAT1 <- mydat[,MoCAT1Names]
MoCAT2 <- mydat[,MoCAT2Names]
MoCAT3 <- mydat[,MoCAT3Names]
MoCAT4 <- mydat[,MoCAT4Names]

##### Recode missing value indicator 9999 to R NAs
for(i in 1:14) MoCAT1[,i] <- recode(MoCAT1[,i], '9999 = NA')
for(i in 1:14) MoCAT2[,i] <- recode(MoCAT2[,i], '9999 = NA')
for(i in 1:14) MoCAT3[,i] <- recode(MoCAT3[,i], '9999 = NA')
for(i in 1:14) MoCAT4[,i] <- recode(MoCAT4[,i], '9999 = NA')

##### Recode incorrect input for items 8 and 9 at time points 1 and 4, resp.
MoCAT1[,9] <- recode(MoCAT1[,9], '4 = 3')
MoCAT4[,8] <- recode(MoCAT4[,8], '2 = 1')

##### Recode item 14 (Orient) 0 and 1 to 2, since too few observations for EDU/NOEDU groups at some time points
for(i in 14:14) MoCAT1[,i] <- recode(MoCAT1[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT2[,i] <- recode(MoCAT2[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT3[,i] <- recode(MoCAT3[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT4[,i] <- recode(MoCAT4[,i], 'c(0, 1) = 2')

##### General check for missing data
apply(MoCAT1, 2, function(x) sum(is.na(x)))
apply(MoCAT2, 2, function(x) sum(is.na(x)))
apply(MoCAT3, 2, function(x) sum(is.na(x)))
apply(MoCAT4, 2, function(x) sum(is.na(x)))

##### Identify how many observations had all missing at each time point
##### These observations must be removed before the analysis
sum(apply(MoCAT1, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT2, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT3, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT4, 1, function(x) sum(is.na(x))) == 14)

##### Create objects indicating which cases should be kept
NAadjT1 <- !(apply(MoCAT1, 1, function(x) sum(is.na(x))) == 14)
NAadjT2 <- !(apply(MoCAT2, 1, function(x) sum(is.na(x))) == 14)
NAadjT3 <- !(apply(MoCAT3, 1, function(x) sum(is.na(x))) == 14)
NAadjT4 <- !(apply(MoCAT4, 1, function(x) sum(is.na(x))) == 14)

##### Total cases at each time point and across all time points
sum(NAadjT1)
sum(NAadjT2)
sum(NAadjT3)
sum(NAadjT4)
sum(NAadjT1 & NAadjT2  & NAadjT3 & NAadjT4)

##### Define vector of observations that are valid at all time points
NAadjT1toT4 <- (NAadjT1 & NAadjT2  & NAadjT3 & NAadjT4)

##### Define education level vector
EDUC <- mydat$E3_EduLevel  
EDUC <- recode(EDUC, "'no formal education' = 'NOEDU'; 9999 = NA; else='EDU'")

##### Define vector indicating valid cases
NAadjEDU <- !is.na(EDUC)

##### Define single factor models at each individual time point

cfaMoCAT1 <- '
	cogT1 =~ NA*S6_MoCA_Trail + lambda.1_1*S6_MoCA_Trail
	cogT1 =~ NA*S6_MoCA_Cube + lambda.2_1*S6_MoCA_Cube
	cogT1 =~ NA*S6_MoCA_ClockShape + lambda.3_1*S6_MoCA_ClockShape
	cogT1 =~ NA*S6_MoCA_ClockNumber + lambda.4_1*S6_MoCA_ClockNumber
	cogT1 =~ NA*S6_MoCA_ClockHand + lambda.5_1*S6_MoCA_ClockHand
	cogT1 =~ NA*S6_MoCA_Naming + lambda.6_1*S6_MoCA_Naming
	cogT1 =~ NA*S6_MoCA_DigitSpan + lambda.7_1*S6_MoCA_DigitSpan
	cogT1 =~ NA*S6_MoCA_GoNoGo1 + lambda.8_1*S6_MoCA_GoNoGo1
	cogT1 =~ NA*S6_MoCA_Serial7 + lambda.9_1*S6_MoCA_Serial7
	cogT1 =~ NA*S6_MoCA_LogicalMemory + lambda.10_1*S6_MoCA_LogicalMemory
	cogT1 =~ NA*S6_MoCA_VerbalFluency + lambda.11_1*S6_MoCA_VerbalFluency
	cogT1 =~ NA*S6_MoCA_Abstract + lambda.12_1*S6_MoCA_Abstract
	cogT1 =~ NA*S6_MoCA_Memory + lambda.13_1*S6_MoCA_Memory
	cogT1 =~ NA*S6_MoCA_Orient + lambda.14_1*S6_MoCA_Orient
'

cfaMoCAT2 <- '
	cogT2 =~ NA*T2_S6_MoCA_Trail + lambda.1_1*T2_S6_MoCA_Trail
	cogT2 =~ NA*T2_S6_MoCA_Cube + lambda.2_1*T2_S6_MoCA_Cube
	cogT2 =~ NA*T2_S6_MoCA_ClockShape + lambda.3_1*T2_S6_MoCA_ClockShape
	cogT2 =~ NA*T2_S6_MoCA_ClockNumber + lambda.4_1*T2_S6_MoCA_ClockNumber
	cogT2 =~ NA*T2_S6_MoCA_ClockHand + lambda.5_1*T2_S6_MoCA_ClockHand
	cogT2 =~ NA*T2_S6_MoCA_Naming + lambda.6_1*T2_S6_MoCA_Naming
	cogT2 =~ NA*T2_S6_MoCA_DigitSpan + lambda.7_1*T2_S6_MoCA_DigitSpan
	cogT2 =~ NA*T2_S6_MoCA_GoNoGo1 + lambda.8_1*T2_S6_MoCA_GoNoGo1
	cogT2 =~ NA*T2_S6_MoCA_Serial7 + lambda.9_1*T2_S6_MoCA_Serial7
	cogT2 =~ NA*T2_S6_MoCA_LogicalMemory + lambda.10_1*T2_S6_MoCA_LogicalMemory
	cogT2 =~ NA*T2_S6_MoCA_VerbalFluency + lambda.11_1*T2_S6_MoCA_VerbalFluency
	cogT2 =~ NA*T2_S6_MoCA_Abstract + lambda.12_1*T2_S6_MoCA_Abstract
	cogT2 =~ NA*T2_S6_MoCA_Memory + lambda.13_1*T2_S6_MoCA_Memory
	cogT2 =~ NA*T2_S6_MoCA_Orient + lambda.14_1*T2_S6_MoCA_Orient
'

cfaMoCAT3 <- '
	cogT3 =~ NA*T3_S6_MoCA_Trail + lambda.1_1*T3_S6_MoCA_Trail
	cogT3 =~ NA*T3_S6_MoCA_Cube + lambda.2_1*T3_S6_MoCA_Cube
	cogT3 =~ NA*T3_S6_MoCA_ClockShape + lambda.3_1*T3_S6_MoCA_ClockShape
	cogT3 =~ NA*T3_S6_MoCA_ClockNumber + lambda.4_1*T3_S6_MoCA_ClockNumber
	cogT3 =~ NA*T3_S6_MoCA_ClockHand + lambda.5_1*T3_S6_MoCA_ClockHand
	cogT3 =~ NA*T3_S6_MoCA_Naming + lambda.6_1*T3_S6_MoCA_Naming
	cogT3 =~ NA*T3_S6_MoCA_DigitSpan + lambda.7_1*T3_S6_MoCA_DigitSpan
	cogT3 =~ NA*T3_S6_MoCA_GoNoGo1 + lambda.8_1*T3_S6_MoCA_GoNoGo1
	cogT3 =~ NA*T3_S6_MoCA_Serial7 + lambda.9_1*T3_S6_MoCA_Serial7
	cogT3 =~ NA*T3_S6_MoCA_LogicalMemory + lambda.10_1*T3_S6_MoCA_LogicalMemory
	cogT3 =~ NA*T3_S6_MoCA_VerbalFluency + lambda.11_1*T3_S6_MoCA_VerbalFluency
	cogT3 =~ NA*T3_S6_MoCA_Abstract + lambda.12_1*T3_S6_MoCA_Abstract
	cogT3 =~ NA*T3_S6_MoCA_Memory + lambda.13_1*T3_S6_MoCA_Memory
	cogT3 =~ NA*T3_S6_MoCA_Orient + lambda.14_1*T3_S6_MoCA_Orient
'

cfaMoCAT4 <- '
	cogT4 =~ NA*T4_S6_MoCA_Trail + lambda.1_1*T4_S6_MoCA_Trail
	cogT4 =~ NA*T4_S6_MoCA_Cube + lambda.2_1*T4_S6_MoCA_Cube
	cogT4 =~ NA*T4_S6_MoCA_ClockShape + lambda.3_1*T4_S6_MoCA_ClockShape
	cogT4 =~ NA*T4_S6_MoCA_ClockNumber + lambda.4_1*T4_S6_MoCA_ClockNumber
	cogT4 =~ NA*T4_S6_MoCA_ClockHand + lambda.5_1*T4_S6_MoCA_ClockHand
	cogT4 =~ NA*T4_S6_MoCA_Naming + lambda.6_1*T4_S6_MoCA_Naming
	cogT4 =~ NA*T4_S6_MoCA_DigitSpan + lambda.7_1*T4_S6_MoCA_DigitSpan
	cogT4 =~ NA*T4_S6_MoCA_GoNoGo1 + lambda.8_1*T4_S6_MoCA_GoNoGo1
	cogT4 =~ NA*T4_S6_MoCA_Serial7 + lambda.9_1*T4_S6_MoCA_Serial7
	cogT4 =~ NA*T4_S6_MoCA_LogicalMemory + lambda.10_1*T4_S6_MoCA_LogicalMemory
	cogT4 =~ NA*T4_S6_MoCA_VerbalFluency + lambda.11_1*T4_S6_MoCA_VerbalFluency
	cogT4 =~ NA*T4_S6_MoCA_Abstract + lambda.12_1*T4_S6_MoCA_Abstract
	cogT4 =~ NA*T4_S6_MoCA_Memory + lambda.13_1*T4_S6_MoCA_Memory
	cogT4 =~ NA*T4_S6_MoCA_Orient + lambda.14_1*T4_S6_MoCA_Orient
'


##### Configural model for all time points combined
cfaT1T2T3T4measEq <- '
cogT1 =~ S6_MoCA_Trail + S6_MoCA_Cube + S6_MoCA_ClockShape + S6_MoCA_ClockNumber + S6_MoCA_ClockHand  + S6_MoCA_Naming + S6_MoCA_DigitSpan + S6_MoCA_GoNoGo1 + S6_MoCA_Serial7 + S6_MoCA_LogicalMemory + S6_MoCA_VerbalFluency + S6_MoCA_Abstract + S6_MoCA_Memory + S6_MoCA_Orient

cogT2 =~ T2_S6_MoCA_Trail + T2_S6_MoCA_Cube + T2_S6_MoCA_ClockShape + T2_S6_MoCA_ClockNumber + T2_S6_MoCA_ClockHand + T2_S6_MoCA_Naming + T2_S6_MoCA_DigitSpan + T2_S6_MoCA_GoNoGo1 + T2_S6_MoCA_Serial7 + T2_S6_MoCA_LogicalMemory + T2_S6_MoCA_VerbalFluency + T2_S6_MoCA_Abstract + T2_S6_MoCA_Memory + T2_S6_MoCA_Orient

cogT3 =~ T3_S6_MoCA_Trail + T3_S6_MoCA_Cube + T3_S6_MoCA_ClockShape + T3_S6_MoCA_ClockNumber + T3_S6_MoCA_ClockHand +  T3_S6_MoCA_Naming + T3_S6_MoCA_DigitSpan + T3_S6_MoCA_GoNoGo1 + T3_S6_MoCA_Serial7 + T3_S6_MoCA_LogicalMemory + T3_S6_MoCA_VerbalFluency + T3_S6_MoCA_Abstract + T3_S6_MoCA_Memory + T3_S6_MoCA_Orient

cogT4 =~ T4_S6_MoCA_Trail + T4_S6_MoCA_Cube + T4_S6_MoCA_ClockShape +  T4_S6_MoCA_ClockNumber + T4_S6_MoCA_ClockHand + T4_S6_MoCA_Naming + T4_S6_MoCA_DigitSpan + T4_S6_MoCA_GoNoGo1 + T4_S6_MoCA_Serial7 + T4_S6_MoCA_LogicalMemory + T4_S6_MoCA_VerbalFluency + T4_S6_MoCA_Abstract + T4_S6_MoCA_Memory + T4_S6_MoCA_Orient
'

##### Naming of latent variables over time, used in the model definitions below
longFacNames <- list(FU = c("cogT1", "cogT2", "cogT3", "cogT4"))

##### Define configural model from semTools
cfaT1T2T3T4ThetaConfig <- measEq.syntax(configural.model = cfaT1T2T3T4measEq,
data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], 
ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names), 
parameterization = "theta",
missing = "pairwise",
ID.fac = "std.lv",
ID.cat = "Wu.Estabrook.2016",
longFacNames = longFacNames)

cat(as.character(cfaT1T2T3T4ThetaConfig))
summary(cfaT1T2T3T4ThetaConfig)		

##### Define threshold, loading and intercept invariance model from semTools
cfaT1T2T3T4ThetaThreshLoadInter <- measEq.syntax(configural.model = cfaT1T2T3T4measEq,
data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], 
ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names), 
parameterization = "theta",
missing = "pairwise",
ID.fac = "std.lv",
ID.cat = "Wu.Estabrook.2016",
longFacNames = longFacNames, 
long.equal = c("thresholds", "loadings", "intercepts"))

cat(as.character(cfaT1T2T3T4ThetaThreshLoadInter))
summary(cfaT1T2T3T4ThetaThreshLoadInter)

##### Define threshold, loading, intercept and residual variance invariance model (full invariance) from semTools
cfaT1T2T3T4ThetaThreshLoadInterRes <- measEq.syntax(configural.model = cfaT1T2T3T4measEq,
data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], 
ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names), 
parameterization = "theta",
missing = "pairwise",
ID.fac = "std.lv",
ID.cat = "Wu.Estabrook.2016",
longFacNames = longFacNames, 
long.equal = c("thresholds", "loadings", "intercepts", "residuals"))

cat(as.character(cfaT1T2T3T4ThetaThreshLoadInterRes))
summary(cfaT1T2T3T4ThetaThreshLoadInterRes)


##### Steps 1 and 2: Basic model estimation in education and non-education groups

##### Step 1: evaluate configural models

estT1EDU <- cfa(cfaMoCAT1, data = cbind(MoCAT1)[NAadjT1 & NAadjEDU,][EDUC[NAadjT1 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT2EDU <- cfa(cfaMoCAT2, data = cbind(MoCAT2)[NAadjT2 & NAadjEDU,][EDUC[NAadjT2 & NAadjEDU] == "EDU",], ordered = c(MoCAT2Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT3EDU <- cfa(cfaMoCAT3, data = cbind(MoCAT3)[NAadjT3 &  NAadjEDU,][EDUC[NAadjT3 & NAadjEDU] == "EDU",], ordered = c(MoCAT3Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT4EDU <- cfa(cfaMoCAT4, data = cbind(MoCAT4)[ NAadjT4 & NAadjEDU,][EDUC[NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

estT1NOEDU <- cfa(cfaMoCAT1, data = cbind(MoCAT1)[NAadjT1 & NAadjEDU,][EDUC[NAadjT1 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT2NOEDU <- cfa(cfaMoCAT2, data = cbind(MoCAT2)[NAadjT2 & NAadjEDU,][EDUC[NAadjT2 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT2Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT3NOEDU <- cfa(cfaMoCAT3, data = cbind(MoCAT3)[NAadjT3 & NAadjEDU,][EDUC[NAadjT3 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT3Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT4NOEDU <- cfa(cfaMoCAT4, data = cbind(MoCAT4)[NAadjT4 & NAadjEDU,][EDUC[NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Additional model fit statistics 
resT1EDU <- lavResiduals(estT1EDU)
resT2EDU <- lavResiduals(estT2EDU)
resT3EDU <- lavResiduals(estT3EDU)
resT4EDU <- lavResiduals(estT4EDU)
resT1NOEDU <- lavResiduals(estT1NOEDU)
resT2NOEDU <- lavResiduals(estT2NOEDU)
resT3NOEDU <- lavResiduals(estT3NOEDU)
resT4NOEDU <- lavResiduals(estT4NOEDU)



##### Configural model: Education group
estT1T2T3T4EDUThetaConfig <- cfa(as.character(cfaT1T2T3T4ThetaConfig), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
resT1T2T3T4EDUThetaConfig <- lavResiduals(estT1T2T3T4EDUThetaConfig)
print(fitMeasures(estT1T2T3T4EDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
resT1T2T3T4EDUThetaConfig$summary

##### Configural model: Non-education group
estT1T2T3T4NOEDUThetaConfig <- cfa(as.character(cfaT1T2T3T4ThetaConfig), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
resT1T2T3T4NOEDUThetaConfig <- lavResiduals(estT1T2T3T4NOEDUThetaConfig)
print(fitMeasures(estT1T2T3T4NOEDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
resT1T2T3T4NOEDUThetaConfig$summary

##### Step 2: compare models with different levels of invariance
##### Education: model estimation
estT1T2T3T4EDUThetaThreshLoadInter <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInter), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

estT1T2T3T4EDUThetaThreshLoadInterRes <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterRes), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Education: model fit
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterRes, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

##### Education: test equality of models
anova(estT1T2T3T4EDUThetaConfig, estT1T2T3T4EDUThetaThreshLoadInter)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterRes)

##### No education: model estimation
estT1T2T3T4NOEDUThetaThreshLoadInter <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInter), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names), parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

estT1T2T3T4NOEDUThetaThreshLoadInterRes <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterRes), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names), parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### No education: model fit
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterRes, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

##### No education: test equality of models
anova(estT1T2T3T4NOEDUThetaConfig, estT1T2T3T4NOEDUThetaThreshLoadInter)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterRes)


##### Step 3: Invariance of thresholds, loadings, and intercepts
##### We release the restrictions on the parameters for each item in sequence, and estimate a maximally invariant model with respect to each item 
##### We release the loadings and thresholds at each time point but fix the intercept to 0 for identification and also fix the residual variance to 1 for identification
##### Model definitions in the following file
source(paste0(mydir, "mocaBaselineSearch.r"))

##### Education group: Partial invariance models
estT1T2T3T4EDUThetaThreshLoadInter <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInter), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterTrail <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterTrail), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterCube <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterCube), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterClockShape <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockShape), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterClockNumber <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockNumber), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterClockHand <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockHand), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterNaming <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterNaming), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterDigitSpan <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterDigitSpan), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterGoNoGo1 <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterGoNoGo1), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterSerial7 <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterSerial7), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterLogicalMemory <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterLogicalMemory), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterVerbalFluency <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterVerbalFluency), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterAbstract <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterAbstract), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterMemory <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterMemory), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4EDUThetaThreshLoadInterOrient <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterOrient), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Education group: Partial invariance tests
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterTrail)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterCube)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterClockShape)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterClockNumber)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterClockHand)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterNaming)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterDigitSpan)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterGoNoGo1)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterSerial7)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterLogicalMemory)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterVerbalFluency)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterAbstract)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterMemory)
anova(estT1T2T3T4EDUThetaThreshLoadInter, estT1T2T3T4EDUThetaThreshLoadInterOrient)

##### Invariant: Trail, ClockShape, ClockNumber, Naming, DigitSpan, GoNoGo1, Serial7, VerbalFluency (1, 3, 4, 6, 7, 8, 9, 11)
##### Non-invariant: Cube, ClockHand, LogicalMemory, Abstract, Memory, Orient (2, 5, 10, 12, 13, 14)

##### Model with parameters for non-invariant items freed
source(paste0(mydir, "mocaEDUFree.r"))

estT1T2T3T4EDUThetaThreshLoadInterFree <- cfa(as.character(cfaT1T2T3T4EDUThetaThreshLoadInterFree), data = cbind(MoCAT1,
 MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Model is not statistically significantly different to the configural model
anova(estT1T2T3T4EDUThetaConfig, estT1T2T3T4EDUThetaThreshLoadInterFree)
anova(estT1T2T3T4EDUThetaThreshLoadInterFree, estT1T2T3T4EDUThetaThreshLoadInter)

##### Model fit statistics
print(fitMeasures(estT1T2T3T4EDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterTrail, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterCube, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterClockShape, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterClockNumber, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterNaming, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterDigitSpan, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterGoNoGo1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterSerial7, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterLogicalMemory, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterVerbalFluency, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterMemory, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterOrient, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

resT1T2T3T4EDUThetaThreshLoadInterFree <- lavResiduals(estT1T2T3T4EDUThetaThreshLoadInterFree)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterFree, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
resT1T2T3T4EDUThetaThreshLoadInterFree$summary


##### Non-education group: Partial invariance models
estT1T2T3T4NOEDUThetaThreshLoadInter <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInter), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterTrail <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterTrail), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterCube <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterCube), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterClockShape <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockShape), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterClockNumber <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockNumber), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterClockHand <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterClockHand), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterNaming <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterNaming), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterDigitSpan <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterDigitSpan), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterGoNoGo1 <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterGoNoGo1), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterSerial7 <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterSerial7), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterLogicalMemory <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterLogicalMemory), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterVerbalFluency <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterVerbalFluency), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterAbstract <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterAbstract), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterMemory <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterMemory), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")
estT1T2T3T4NOEDUThetaThreshLoadInterOrient <- cfa(as.character(cfaT1T2T3T4ThetaThreshLoadInterOrient), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Non-education group: Partial invariance tests
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterTrail)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterCube)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterClockShape)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterClockNumber)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterClockHand)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterNaming)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterDigitSpan)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterGoNoGo1)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterSerial7)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterLogicalMemory)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterVerbalFluency)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterAbstract)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterMemory)
anova(estT1T2T3T4NOEDUThetaThreshLoadInter, estT1T2T3T4NOEDUThetaThreshLoadInterOrient)

##### Invariant: Trail, Cube, Naming, DigitSpan, GoNoGo1, Serial7, LogicalMemory, VerbalFluency (1, 2, 6, 7, 8, 9, 10, 11)
##### Non-invariant: ClockShape, ClockNumber, ClockHand, Abstract, Memory, Orient (3, 4, 5, 12, 13, 14)

##### Model with parameters for non-invariant items freed 
source(paste0(mydir, "mocaNOEDUFree.r"))

estT1T2T3T4NOEDUThetaThreshLoadInterFree <- cfa(as.character(cfaT1T2T3T4NOEDUThetaThreshLoadInterFree), data = cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], ordered = c(MoCAT1Names, MoCAT2Names, MoCAT3Names, MoCAT4Names),  parameterization = "theta", std.lv = TRUE, estimator = "WLSMV", missing = "pairwise")

##### Model is not statistically significantly different to the configural model
anova(estT1T2T3T4NOEDUThetaConfig, estT1T2T3T4NOEDUThetaThreshLoadInterFree)
anova(estT1T2T3T4NOEDUThetaThreshLoadInterFree, estT1T2T3T4NOEDUThetaThreshLoadInter)

##### Model fit
print(fitMeasures(estT1T2T3T4NOEDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterTrail, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterCube, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterClockShape, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterClockNumber, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterNaming, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterDigitSpan, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterGoNoGo1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterSerial7, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterLogicalMemory, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterVerbalFluency, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterMemory, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterOrient, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

resT1T2T3T4NOEDUThetaThreshLoadInterFree <- lavResiduals(estT1T2T3T4NOEDUThetaThreshLoadInterFree)


##### The final models partial invariance models
##### Education group

print(fitMeasures(estT1T2T3T4EDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4EDUThetaThreshLoadInterFree, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

resT1T2T3T4EDUThetaConfig$summary
resT1T2T3T4EDUThetaThreshLoadInter$summary
resT1T2T3T4EDUThetaThreshLoadInterFree$summary

##### No education group
resT1T2T3T4NOEDUThetaThreshLoadInter <- lavResiduals(estT1T2T3T4NOEDUThetaThreshLoadInter)

print(fitMeasures(estT1T2T3T4NOEDUThetaConfig, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInter, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)
print(fitMeasures(estT1T2T3T4NOEDUThetaThreshLoadInterFree, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"), output = "text"), add.h0 = TRUE)

resT1T2T3T4NOEDUThetaConfig$summary
resT1T2T3T4NOEDUThetaThreshLoadInter$summary
resT1T2T3T4NOEDUThetaThreshLoadInterFree$summary


##### Step 4: Prepare figures and tables 
##### MoCA item names
MoCANames <- c("Trail Making", "Cube", "Clock Shape", "Clock Number", "Clock Hand", "Naming", "Digit Span", "Attention", "Serial Subtraction", "Sentence Repetition", "Verbal Fluency", "Abstraction", "Delayed Recall", "Orientation")
MoCACats <- as.numeric(apply(cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], 2, function(x) length(table(x)))[1:14])
ninvEDU <- c(2, 5, 10, 12, 13, 14)
ninvNOEDU <- c(3, 4, 5, 12, 13, 14)

##### Parameters and standard errors for the two groups
parEDUT1T2T3T4 <- lavInspect(estT1T2T3T4EDUThetaThreshLoadInterFree, what = "est", add.labels = TRUE, add.class = TRUE, list.by.group = TRUE, drop.list.single.group = TRUE)
seEDUT1T2T3T4 <- lavInspect(estT1T2T3T4EDUThetaThreshLoadInterFree, what = "se", add.labels = TRUE, add.class = TRUE, list.by.group = TRUE, drop.list.single.group = TRUE)
parNOEDUT1T2T3T4 <- lavInspect(estT1T2T3T4NOEDUThetaThreshLoadInterFree, what = "est", add.labels = TRUE, add.class = TRUE, list.by.group = TRUE, drop.list.single.group = TRUE)
seNOEDUT1T2T3T4 <- lavInspect(estT1T2T3T4NOEDUThetaThreshLoadInterFree, what = "se", add.labels = TRUE, add.class = TRUE, list.by.group = TRUE, drop.list.single.group = TRUE)

##### Objects to facilitate plotting
LambdaEDU <- matrix(NA, nrow = 6, ncol = 4)
seLambdaEDU <- matrix(NA, nrow = 6, ncol = 4)
LambdaNOEDU <- matrix(NA, nrow = 6, ncol = 4)
seLambdaNOEDU <- matrix(NA, nrow = 6, ncol = 4)
 
tauEDU <- data.frame(T1 = parEDUT1T2T3T4$tau[c(2, 5, 15:16, 18:28),],
T2 = parEDUT1T2T3T4$tau[28 + c(2, 5, 15:16, 18:28),],
T3 = parEDUT1T2T3T4$tau[56 + c(2, 5, 15:16, 18:28),],
T4 = parEDUT1T2T3T4$tau[84 + c(2, 5, 15:16, 18:28),])

tauNOEDU <- data.frame(T1 = parNOEDUT1T2T3T4$tau[c(3, 4, 5, 18:28),],
T2 = parNOEDUT1T2T3T4$tau[28 + c(3, 4, 5, 18:28),], 
T3 = parNOEDUT1T2T3T4$tau[56 + c(3, 4, 5, 18:28),], 
T4 = parNOEDUT1T2T3T4$tau[84 + c(3, 4, 5, 18:28),])

setauEDU <- data.frame(T1 = seEDUT1T2T3T4$tau[c(2, 5, 15:16, 18:28),],
T2 = seEDUT1T2T3T4$tau[28 + c(2, 5, 15:16, 18:28),],
T3 = seEDUT1T2T3T4$tau[56 + c(2, 5, 15:16, 18:28),],
T4 = seEDUT1T2T3T4$tau[84 + c(2, 5, 15:16, 18:28),])

setauNOEDU <- data.frame(T1 = seNOEDUT1T2T3T4$tau[c(3, 4, 5, 18:28),],
T2 = seNOEDUT1T2T3T4$tau[28 + c(3, 4, 5, 18:28),], 
T3 = seNOEDUT1T2T3T4$tau[56 + c(3, 4, 5, 18:28),], 
T4 = seNOEDUT1T2T3T4$tau[84 + c(3, 4, 5, 18:28),])

for(i in 1:6){
	LambdaEDU[i,] <- coef((estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + ninvEDU[i] - 1]
	seLambdaEDU[i,] <- sqrt(diag(vcov(estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + ninvEDU[i] - 1])
	LambdaNOEDU[i,] <- coef((estT1T2T3T4NOEDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + ninvNOEDU[i] - 1]
	seLambdaNOEDU[i,] <- sqrt(diag(vcov(estT1T2T3T4NOEDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + ninvNOEDU[i] - 1])
}

LambdaEDU[1,] <- coef((estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 1]
seLambdaEDU[1,] <- sqrt(diag(vcov(estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 1])
LambdaEDU[2,] <- coef((estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 4]
seLambdaEDU[2,] <- sqrt(diag(vcov(estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 4])
LambdaEDU[3,] <- coef((estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 9]
seLambdaEDU[3,] <- sqrt(diag(vcov(estT1T2T3T4EDUThetaThreshLoadInterFree))[c(1, 15, 29, 43) + 9])


##### Plots like article 

par(mfrow = c(2, 3))
for(i in 1:6){
	plot(c(2014, 2015, 2016, 2017), LambdaEDU[i,], main = MoCANames[ninvEDU[i]], ylim = c(0, 2), ylab = "Factor loading estimate", xlab = "Time point", type = "b", pch = 4, lty = 1, lwd = 1.5, xaxt = "n")
	axis(1, at = 2014:2017)
	arrows(2014:2017, LambdaEDU[i,] - qnorm(0.975) * seLambdaEDU[i,], 2014:2017, LambdaEDU[i,] + qnorm(0.975) * seLambdaEDU[i,], code = 3, length = 0.02, angle = 90)
}

par(mfrow = c(2, 3))
for(i in 1:6){
	plot(c(2014, 2015, 2016, 2017), LambdaNOEDU[i,], main = MoCANames[ninvNOEDU[i]], ylim = c(0, 2), ylab = "Factor loading estimate", xlab = "Time point", type = "b", pch = 4, lty = 1, lwd = 1.5, xaxt = "n")
	axis(1, at = 2014:2017)
	arrows(2014:2017, LambdaNOEDU[i,] - qnorm(0.975) * seLambdaNOEDU[i,], 2014:2017, LambdaNOEDU[i,] + qnorm(0.975) * seLambdaNOEDU[i,], code = 3, length = 0.02, angle = 90)
}

taunames <- c("Tau 1", "Tau 2", "Tau 3", "Tau 4", "Tau 5")
par(mfrow = c(2, 3))
k <- 1
for(i in 1:6){
	for(j in 1:(MoCACats[ninvEDU[i]] - 1)){
		if(j == 1){
			plot(c(2014, 2015, 2016, 2017), tauEDU[k,], main = MoCANames[ninvEDU[i]], ylim = c(-4, 2), ylab = "Threshold estimate", xlab = "Time point", type = "b", pch = j, lty = j, lwd = 1.5, xaxt = "n")
			axis(1, at = 2014:2017)
			if(i == 5){
				legend("bottomleft", legend = taunames[1:((MoCACats[ninvEDU[i]] - 1))], pch = 1:((MoCACats[ninvEDU[i]] - 1)), lty = 1:((MoCACats[ninvEDU[i]] - 1)), bty = "n", lwd = 1.5, inset = 0)
				#legend(2013.9, -2.3, legend = taunames[1:2], pch = 1:2, lty = 1:2, bty = "n", lwd = 1.5, inset = 0)
				#legend(2014.9, -2.3, legend = taunames[3:4], pch = 3:4, lty = 3:4, bty = "n", lwd = 1.5, inset = 0)
				#legend(2015.9, -2.3, legend = taunames[5], pch = 5, lty = 5, bty = "n", lwd = 1.5, inset = 0)
			} else legend("topleft", legend = taunames[1:((MoCACats[ninvEDU[i]] - 1))], pch = 1:((MoCACats[ninvEDU[i]] - 1)), lty = 1:((MoCACats[ninvEDU[i]] - 1)), bty = "n", lwd = 1.5, inset = 0)
		} else{
			par(new = TRUE)
			plot(c(2014, 2015, 2016, 2017), tauEDU[k,], main = "", ylim = c(-4, 2), ylab = "", xlab = "", type = "b", pch = j, lty = j, lwd = 1.5, axes = FALSE)
		}
		#arrows(2014:2017, as.numeric(tauEDU[k,] - qnorm(0.975) * setauEDU[k,]), 2014:2017, as.numeric(tauEDU[k,] + qnorm(0.975) * setauEDU[k,]), code = 3, length = 0.02, angle = 90)
		k <- k + 1
	}
}

par(mfrow = c(2, 3))
k <- 1
for(i in 1:6){
	for(j in 1:(MoCACats[ninvNOEDU[i]] - 1)){
		if(j == 1){
			plot(c(2014, 2015, 2016, 2017), tauNOEDU[k,], main = MoCANames[ninvNOEDU[i]], ylim = c(-2.5, 2), ylab = "Threshold estimate", xlab = "Time point", type = "b", pch = j, lty = j, lwd = 1.5, xaxt = "n")
			axis(1, at = 2014:2017)
			if(i == 6){
				legend(2014, 2.25, legend = taunames[1:2], pch = 1:2, lty = 1:2, bty = "n", lwd = 1.5, inset = 0)
				legend(2015.5, 2.25, legend = taunames[3:4], pch = 3:4, lty = 3:4, bty = "n", lwd = 1.5, inset = 0)
			} else if(i == 5){
				legend(2014, -1, legend = taunames[1:3], pch = 1:3, lty = 1:3, bty = "n", lwd = 1.5, inset = 0)
				legend(2015.5, -1, legend = taunames[4:5], pch = 4:5, lty = 4:5, bty = "n", lwd = 1.5, inset = 0)
			} else legend("topleft", legend = taunames[1:((MoCACats[ninvNOEDU[i]] - 1))], pch = 1:((MoCACats[ninvNOEDU[i]] - 1)), lty = 1:((MoCACats[ninvNOEDU[i]] - 1)), bty = "n", lwd = 1.5, inset = 0)
		} else{
			par(new = TRUE)
			plot(c(2014, 2015, 2016, 2017), tauNOEDU[k,], main = "", ylim = c(-2.5, 2), ylab = "", xlab = "", type = "b", pch = j, lty = j, lwd = 1.5, axes = FALSE)
		}
		#arrows(2014:2017, as.numeric(tauNOEDU[k,] - qnorm(0.975) * setauNOEDU[k,]), 2014:2017, as.numeric(tauNOEDU[k,] + qnorm(0.975) * setauNOEDU[k,]), code = 3, length = 0.02, angle = 90)
		k <- k + 1
	}
}

##### ML factor scores from different models
mlEDUDIFT1T2T3T4PartialScalar <- lavPredict(estT1T2T3T4EDUThetaThreshLoadInterFree, method = "ML")
mlNOEDUDIFT1T2T3T4PartialScalar <- lavPredict(estT1T2T3T4NOEDUThetaThreshLoadInterFree, method = "ML")

mlEDUDIFT1T2T3T4Scalar <- lavPredict(estT1T2T3T4EDUThetaThreshLoadInter, method = "ML")
mlNOEDUDIFT1T2T3T4Scalar <- lavPredict(estT1T2T3T4NOEDUThetaThreshLoadInter, method = "ML")

##### Scatter plots and correlation between different factor scores
par(mfrow = c(1, 4))
plot(mlEDUDIFT1T2T3T4PartialScalar[,1], mlEDUDIFT1T2T3T4Scalar[,1], ylim = c(-5, 4), xlim = c(-5, 4), xlab = "Partial invariance", ylab = "Full invariance", main = "2014")
plot(mlEDUDIFT1T2T3T4PartialScalar[,2], mlEDUDIFT1T2T3T4Scalar[,2], ylim = c(-5, 4), xlim = c(-5, 4), xlab = "Partial invariance", ylab = "Full invariance", main = "2015")
plot(mlEDUDIFT1T2T3T4PartialScalar[,3], mlEDUDIFT1T2T3T4Scalar[,3], ylim = c(-5, 4), xlim = c(-5, 4), xlab = "Partial invariance", ylab = "Full invariance", main = "2016")
plot(mlEDUDIFT1T2T3T4PartialScalar[,4], mlEDUDIFT1T2T3T4Scalar[,4], ylim = c(-5, 4), xlim = c(-5, 4), xlab = "Partial invariance", ylab = "Full invariance", main = "2017")

cor(mlEDUDIFT1T2T3T4PartialScalar[,1], mlEDUDIFT1T2T3T4Scalar[,1])
cor(mlEDUDIFT1T2T3T4PartialScalar[,2], mlEDUDIFT1T2T3T4Scalar[,2])
cor(mlEDUDIFT1T2T3T4PartialScalar[,3], mlEDUDIFT1T2T3T4Scalar[,3])
cor(mlEDUDIFT1T2T3T4PartialScalar[,4], mlEDUDIFT1T2T3T4Scalar[,4])

##### Descriptive statistics for MoCA scores
mydata <- cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4, EDUC)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,]

mydataEDU <- mydata[mydata$EDUC == "EDU",-57]
mydataNOEDU <- mydata[mydata$EDUC == "NOEDU",-57]

sumEDUNArm <- apply(mydataEDU, 1, function(x) sum(is.na(x))) == 0
sumNOEDUNArm <- apply(mydataNOEDU, 1, function(x) sum(is.na(x))) == 0

sumNOEDU <- cbind(apply(mydataNOEDU[sumNOEDUNArm, 1:14], 1, sum), apply(mydataNOEDU[sumNOEDUNArm,15:28], 1, sum), apply(mydataNOEDU[sumNOEDUNArm,29:42], 1, sum), apply(mydataNOEDU[sumNOEDUNArm,43:56], 1, sum))
sumEDU <- cbind(apply(mydataEDU[sumEDUNArm, 1:14], 1, sum), apply(mydataEDU[sumEDUNArm,15:28], 1, sum), apply(mydataEDU[sumEDUNArm,29:42], 1, sum), apply(mydataEDU[sumEDUNArm,43:56], 1, sum))

xtable(matrix(c(apply(sumEDU, 2, mean),
apply(sumEDU, 2, sd),
apply(sumNOEDU, 2, mean),
apply(sumNOEDU, 2, sd),
apply(rbind(sumEDU, sumNOEDU), 2, mean),
apply(rbind(sumEDU, sumNOEDU), 2, sd)), ncol = 6), digits = 2)


##### Classification consistency between different scores for several types of classification 

limitsSUMNOEDU <- apply(sumNOEDU, 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]
limitsSUMEDU <- apply(sumEDU, 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]

limitsMLNOEDU <- apply(mlNOEDUDIFT1T2T3T4PartialScalar[sumNOEDUNArm,], 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]
limitsMLEDU <- apply(mlEDUDIFT1T2T3T4PartialScalar[sumEDUNArm,], 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]

limitsMLNOEDUScalar <- apply(mlNOEDUDIFT1T2T3T4Scalar[sumNOEDUNArm,], 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]
limitsMLEDUScalar <- apply(mlEDUDIFT1T2T3T4Scalar[sumEDUNArm,], 2, quantile, probs = c(0.02, 0.07, 0.16))[,1]

majorMLEDU <- mlEDUDIFT1T2T3T4PartialScalar[sumEDUNArm,] < limitsMLEDU[1]
majorMLNOEDU <- mlNOEDUDIFT1T2T3T4PartialScalar[sumNOEDUNArm,] < limitsMLNOEDU[1]
petersenMLEDU <- mlEDUDIFT1T2T3T4PartialScalar[sumEDUNArm,] < limitsMLEDU[2]
petersenMLNOEDU <- mlNOEDUDIFT1T2T3T4PartialScalar[sumNOEDUNArm,] < limitsMLNOEDU[2]
mildMLEDU <- mlEDUDIFT1T2T3T4PartialScalar[sumEDUNArm,] < limitsMLEDU[3]
mildMLNOEDU <- mlNOEDUDIFT1T2T3T4PartialScalar[sumNOEDUNArm,] < limitsMLNOEDU[3]

majorMLEDUScalar <- mlEDUDIFT1T2T3T4Scalar[sumEDUNArm,] < limitsMLEDUScalar[1]
majorMLNOEDUScalar <- mlNOEDUDIFT1T2T3T4Scalar[sumNOEDUNArm,] < limitsMLNOEDUScalar[1]
petersenMLEDUScalar <- mlEDUDIFT1T2T3T4Scalar[sumEDUNArm,] < limitsMLEDUScalar[2]
petersenMLNOEDUScalar <- mlNOEDUDIFT1T2T3T4Scalar[sumNOEDUNArm,] < limitsMLNOEDUScalar[2]
mildMLEDUScalar <- mlEDUDIFT1T2T3T4Scalar[sumEDUNArm,] < limitsMLEDUScalar[3]
mildMLNOEDUScalar <- mlNOEDUDIFT1T2T3T4Scalar[sumNOEDUNArm,] < limitsMLNOEDUScalar[3]

majorSUMEDU <- sumEDU < limitsSUMEDU[1]
majorSUMNOEDU <- sumNOEDU < limitsSUMNOEDU[1]
petersenSUMEDU <- sumEDU < limitsSUMEDU[2]
petersenSUMNOEDU <- sumNOEDU < limitsSUMNOEDU[2]
mildSUMEDU <- sumEDU < limitsSUMEDU[3]
mildSUMNOEDU <- sumNOEDU < limitsSUMNOEDU[3]

##### Tables
xtable(100 * matrix(c(
c(mean(majorSUMEDU[,1] == majorMLEDU[,1]), mean(majorSUMEDU[,2] == majorMLEDU[,2]), mean(majorSUMEDU[,3] == majorMLEDU[,3]), mean(majorSUMEDU[,4] == majorMLEDU[,4])),
c(mean(petersenSUMEDU[,1] == petersenMLEDU[,1]), mean(petersenSUMEDU[,2] == petersenMLEDU[,2]), mean(petersenSUMEDU[,3] == petersenMLEDU[,3]), mean(petersenSUMEDU[,4] == petersenMLEDU[,4])),
c(mean(mildSUMEDU[,1] == mildMLEDU[,1]), mean(mildSUMEDU[,2] == mildMLEDU[,2]), mean(mildSUMEDU[,3] == mildMLEDU[,3]), mean(mildSUMEDU[,4] == mildMLEDU[,4])),

c(sum((majorSUMEDU[,1] == FALSE) & (majorMLEDU[,1] == TRUE)) / 474, sum((majorSUMEDU[,2] == FALSE) & (majorMLEDU[,2] == TRUE)) / 474, sum((majorSUMEDU[,3] == FALSE) & (majorMLEDU[,3] == TRUE)) / 474, sum((majorSUMEDU[,4] == FALSE) & (majorMLEDU[,4] == TRUE)) / 474),
c(sum((petersenSUMEDU[,1] == FALSE) & (petersenMLEDU[,1] == TRUE)) / 474, sum((petersenSUMEDU[,2] == FALSE) & (petersenMLEDU[,2] == TRUE)) / 474, sum((petersenSUMEDU[,3] == FALSE) & (petersenMLEDU[,3] == TRUE)) / 474, sum((petersenSUMEDU[,4] == FALSE) & (petersenMLEDU[,4] == TRUE)) / 474),
c(sum((mildSUMEDU[,1] == FALSE) & (mildMLEDU[,1] == TRUE)) / 474, sum((mildSUMEDU[,2] == FALSE) & (mildMLEDU[,2] == TRUE)) / 474, sum((mildSUMEDU[,3] == FALSE) & (mildMLEDU[,3] == TRUE)) / 474, sum((mildSUMEDU[,4] == FALSE) & (mildMLEDU[,4] == TRUE)) / 474),

c(sum((majorSUMEDU[,1] == TRUE) & (majorMLEDU[,1] == FALSE)) / 474, sum((majorSUMEDU[,2] == TRUE) & (majorMLEDU[,2] == FALSE)) / 474, sum((majorSUMEDU[,3] == TRUE) & (majorMLEDU[,3] == FALSE)) / 474, sum((majorSUMEDU[,4] == TRUE) & (majorMLEDU[,4] == FALSE)) / 474),
c(sum((petersenSUMEDU[,1] == TRUE) & (petersenMLEDU[,1] == FALSE)) / 474, sum((petersenSUMEDU[,2] == TRUE) & (petersenMLEDU[,2] == FALSE)) / 474, sum((petersenSUMEDU[,3] == TRUE) & (petersenMLEDU[,3] == FALSE)) / 474, sum((petersenSUMEDU[,4] == TRUE) & (petersenMLEDU[,4] == FALSE)) / 474),
c(sum((mildSUMEDU[,1] == TRUE) & (mildMLEDU[,1] == FALSE)) / 474, sum((mildSUMEDU[,2] == TRUE) & (mildMLEDU[,2] == FALSE)) / 474, sum((mildSUMEDU[,3] == TRUE) & (mildMLEDU[,3] == FALSE)) / 474, sum((mildSUMEDU[,4] == TRUE) & (mildMLEDU[,4] == FALSE)) / 474),

c(mean(majorSUMNOEDU[,1] == majorMLNOEDU[,1]), mean(majorSUMNOEDU[,2] == majorMLNOEDU[,2]), mean(majorSUMNOEDU[,3] == majorMLNOEDU[,3]), mean(majorSUMNOEDU[,4] == majorMLNOEDU[,4])),
c(mean(petersenSUMNOEDU[,1] == petersenMLNOEDU[,1]), mean(petersenSUMNOEDU[,2] == petersenMLNOEDU[,2]), mean(petersenSUMNOEDU[,3] == petersenMLNOEDU[,3]), mean(petersenSUMNOEDU[,4] == petersenMLNOEDU[,4])),
c(mean(mildSUMNOEDU[,1] == mildMLNOEDU[,1]), mean(mildSUMNOEDU[,2] == mildMLNOEDU[,2]), mean(mildSUMNOEDU[,3] == mildMLNOEDU[,3]), mean(mildSUMNOEDU[,4] == mildMLNOEDU[,4])),

c(sum((majorSUMNOEDU[,1] == FALSE) & (majorMLNOEDU[,1] == TRUE)) / 301, sum((majorSUMNOEDU[,2] == FALSE) & (majorMLNOEDU[,2] == TRUE)) / 301, sum((majorSUMNOEDU[,3] == FALSE) & (majorMLNOEDU[,3] == TRUE)) / 301, sum((majorSUMNOEDU[,4] == FALSE) & (majorMLNOEDU[,4] == TRUE)) / 301),
c(sum((petersenSUMNOEDU[,1] == FALSE) & (petersenMLNOEDU[,1] == TRUE)) / 301, sum((petersenSUMNOEDU[,2] == FALSE) & (petersenMLNOEDU[,2] == TRUE)) / 301, sum((petersenSUMNOEDU[,3] == FALSE) & (petersenMLNOEDU[,3] == TRUE)) / 301, sum((petersenSUMNOEDU[,4] == FALSE) & (petersenMLNOEDU[,4] == TRUE)) / 301),
c(sum((mildSUMNOEDU[,1] == FALSE) & (mildMLNOEDU[,1] == TRUE)) / 301, sum((mildSUMNOEDU[,2] == FALSE) & (mildMLNOEDU[,2] == TRUE)) / 301, sum((mildSUMNOEDU[,3] == FALSE) & (mildMLNOEDU[,3] == TRUE)) / 301, sum((mildSUMNOEDU[,4] == FALSE) & (mildMLNOEDU[,4] == TRUE)) / 301),

c(sum((majorSUMNOEDU[,1] == TRUE) & (majorMLNOEDU[,1] == FALSE)) / 301, sum((majorSUMNOEDU[,2] == TRUE) & (majorMLNOEDU[,2] == FALSE)) / 301, sum((majorSUMNOEDU[,3] == TRUE) & (majorMLNOEDU[,3] == FALSE)) / 301, sum((majorSUMNOEDU[,4] == TRUE) & (majorMLNOEDU[,4] == FALSE)) / 301),
c(sum((petersenSUMNOEDU[,1] == TRUE) & (petersenMLNOEDU[,1] == FALSE)) / 301, sum((petersenSUMNOEDU[,2] == TRUE) & (petersenMLNOEDU[,2] == FALSE)) / 301, sum((petersenSUMNOEDU[,3] == TRUE) & (petersenMLNOEDU[,3] == FALSE)) / 301, sum((petersenSUMNOEDU[,4] == TRUE) & (petersenMLNOEDU[,4] == FALSE)) / 301),
c(sum((mildSUMNOEDU[,1] == TRUE) & (mildMLNOEDU[,1] == FALSE)) / 301, sum((mildSUMNOEDU[,2] == TRUE) & (mildMLNOEDU[,2] == FALSE)) / 301, sum((mildSUMNOEDU[,3] == TRUE) & (mildMLNOEDU[,3] == FALSE)) / 301, sum((mildSUMNOEDU[,4] == TRUE) & (mildMLNOEDU[,4] == FALSE)) / 301)
), ncol = 6), digits = 1)

xtable(100 * matrix(c(
c(sum((majorSUMEDU[,1] == FALSE) & (majorMLEDU[,1] == FALSE)) / 474, sum((majorSUMEDU[,2] == FALSE) & (majorMLEDU[,2] == FALSE)) / 474, sum((majorSUMEDU[,3] == FALSE) & (majorMLEDU[,3] == FALSE)) / 474, sum((majorSUMEDU[,4] == FALSE) & (majorMLEDU[,4] == FALSE)) / 474),
c(sum((petersenSUMEDU[,1] == FALSE) & (petersenMLEDU[,1] == FALSE)) / 474, sum((petersenSUMEDU[,2] == FALSE) & (petersenMLEDU[,2] == FALSE)) / 474, sum((petersenSUMEDU[,3] == FALSE) & (petersenMLEDU[,3] == FALSE)) / 474, sum((petersenSUMEDU[,4] == FALSE) & (petersenMLEDU[,4] == FALSE)) / 474),
c(sum((mildSUMEDU[,1] == FALSE) & (mildMLEDU[,1] == FALSE)) / 474, sum((mildSUMEDU[,2] == FALSE) & (mildMLEDU[,2] == FALSE)) / 474, sum((mildSUMEDU[,3] == FALSE) & (mildMLEDU[,3] == FALSE)) / 474, sum((mildSUMEDU[,4] == FALSE) & (mildMLEDU[,4] == FALSE)) / 474),

c(sum((majorSUMEDU[,1] == FALSE) & (majorMLEDU[,1] == TRUE)) / 474, sum((majorSUMEDU[,2] == FALSE) & (majorMLEDU[,2] == TRUE)) / 474, sum((majorSUMEDU[,3] == FALSE) & (majorMLEDU[,3] == TRUE)) / 474, sum((majorSUMEDU[,4] == FALSE) & (majorMLEDU[,4] == TRUE)) / 474),
c(sum((petersenSUMEDU[,1] == FALSE) & (petersenMLEDU[,1] == TRUE)) / 474, sum((petersenSUMEDU[,2] == FALSE) & (petersenMLEDU[,2] == TRUE)) / 474, sum((petersenSUMEDU[,3] == FALSE) & (petersenMLEDU[,3] == TRUE)) / 474, sum((petersenSUMEDU[,4] == FALSE) & (petersenMLEDU[,4] == TRUE)) / 474),
c(sum((mildSUMEDU[,1] == FALSE) & (mildMLEDU[,1] == TRUE)) / 474, sum((mildSUMEDU[,2] == FALSE) & (mildMLEDU[,2] == TRUE)) / 474, sum((mildSUMEDU[,3] == FALSE) & (mildMLEDU[,3] == TRUE)) / 474, sum((mildSUMEDU[,4] == FALSE) & (mildMLEDU[,4] == TRUE)) / 474),

c(sum((majorSUMEDU[,1] == TRUE) & (majorMLEDU[,1] == FALSE)) / 474, sum((majorSUMEDU[,2] == TRUE) & (majorMLEDU[,2] == FALSE)) / 474, sum((majorSUMEDU[,3] == TRUE) & (majorMLEDU[,3] == FALSE)) / 474, sum((majorSUMEDU[,4] == TRUE) & (majorMLEDU[,4] == FALSE)) / 474),
c(sum((petersenSUMEDU[,1] == TRUE) & (petersenMLEDU[,1] == FALSE)) / 474, sum((petersenSUMEDU[,2] == TRUE) & (petersenMLEDU[,2] == FALSE)) / 474, sum((petersenSUMEDU[,3] == TRUE) & (petersenMLEDU[,3] == FALSE)) / 474, sum((petersenSUMEDU[,4] == TRUE) & (petersenMLEDU[,4] == FALSE)) / 474),
c(sum((mildSUMEDU[,1] == TRUE) & (mildMLEDU[,1] == FALSE)) / 474, sum((mildSUMEDU[,2] == TRUE) & (mildMLEDU[,2] == FALSE)) / 474, sum((mildSUMEDU[,3] == TRUE) & (mildMLEDU[,3] == FALSE)) / 474, sum((mildSUMEDU[,4] == TRUE) & (mildMLEDU[,4] == FALSE)) / 474),

c(sum((majorSUMEDU[,1] == TRUE) & (majorMLEDU[,1] == TRUE)) / 474, sum((majorSUMEDU[,2] == TRUE) & (majorMLEDU[,2] == TRUE)) / 474, sum((majorSUMEDU[,3] == TRUE) & (majorMLEDU[,3] == TRUE)) / 474, sum((majorSUMEDU[,4] == TRUE) & (majorMLEDU[,4] == TRUE)) / 474),
c(sum((petersenSUMEDU[,1] == TRUE) & (petersenMLEDU[,1] == TRUE)) / 474, sum((petersenSUMEDU[,2] == TRUE) & (petersenMLEDU[,2] == TRUE)) / 474, sum((petersenSUMEDU[,3] == TRUE) & (petersenMLEDU[,3] == TRUE)) / 474, sum((petersenSUMEDU[,4] == TRUE) & (petersenMLEDU[,4] == TRUE)) / 474),
c(sum((mildSUMEDU[,1] == TRUE) & (mildMLEDU[,1] == TRUE)) / 474, sum((mildSUMEDU[,2] == TRUE) & (mildMLEDU[,2] == TRUE)) / 474, sum((mildSUMEDU[,3] == TRUE) & (mildMLEDU[,3] == TRUE)) / 474, sum((mildSUMEDU[,4] == TRUE) & (mildMLEDU[,4] == TRUE)) / 474),

c(sum((majorSUMNOEDU[,1] == FALSE) & (majorMLNOEDU[,1] == FALSE)) / 301, sum((majorSUMNOEDU[,2] == FALSE) & (majorMLNOEDU[,2] == FALSE)) / 301, sum((majorSUMNOEDU[,3] == FALSE) & (majorMLNOEDU[,3] == FALSE)) / 301, sum((majorSUMNOEDU[,4] == FALSE) & (majorMLNOEDU[,4] == FALSE)) / 301),
c(sum((petersenSUMNOEDU[,1] == FALSE) & (petersenMLNOEDU[,1] == FALSE)) / 301, sum((petersenSUMNOEDU[,2] == FALSE) & (petersenMLNOEDU[,2] == FALSE)) / 301, sum((petersenSUMNOEDU[,3] == FALSE) & (petersenMLNOEDU[,3] == FALSE)) / 301, sum((petersenSUMNOEDU[,4] == FALSE) & (petersenMLNOEDU[,4] == FALSE)) / 301),
c(sum((mildSUMNOEDU[,1] == FALSE) & (mildMLNOEDU[,1] == FALSE)) / 301, sum((mildSUMNOEDU[,2] == FALSE) & (mildMLNOEDU[,2] == FALSE)) / 301, sum((mildSUMNOEDU[,3] == FALSE) & (mildMLNOEDU[,3] == FALSE)) / 301, sum((mildSUMNOEDU[,4] == FALSE) & (mildMLNOEDU[,4] == FALSE)) / 301),

c(sum((majorSUMNOEDU[,1] == FALSE) & (majorMLNOEDU[,1] == TRUE)) / 301, sum((majorSUMNOEDU[,2] == FALSE) & (majorMLNOEDU[,2] == TRUE)) / 301, sum((majorSUMNOEDU[,3] == FALSE) & (majorMLNOEDU[,3] == TRUE)) / 301, sum((majorSUMNOEDU[,4] == FALSE) & (majorMLNOEDU[,4] == TRUE)) / 301),
c(sum((petersenSUMNOEDU[,1] == FALSE) & (petersenMLNOEDU[,1] == TRUE)) / 301, sum((petersenSUMNOEDU[,2] == FALSE) & (petersenMLNOEDU[,2] == TRUE)) / 301, sum((petersenSUMNOEDU[,3] == FALSE) & (petersenMLNOEDU[,3] == TRUE)) / 301, sum((petersenSUMNOEDU[,4] == FALSE) & (petersenMLNOEDU[,4] == TRUE)) / 301),
c(sum((mildSUMNOEDU[,1] == FALSE) & (mildMLNOEDU[,1] == TRUE)) / 301, sum((mildSUMNOEDU[,2] == FALSE) & (mildMLNOEDU[,2] == TRUE)) / 301, sum((mildSUMNOEDU[,3] == FALSE) & (mildMLNOEDU[,3] == TRUE)) / 301, sum((mildSUMNOEDU[,4] == FALSE) & (mildMLNOEDU[,4] == TRUE)) / 301),

c(sum((majorSUMNOEDU[,1] == TRUE) & (majorMLNOEDU[,1] == FALSE)) / 301, sum((majorSUMNOEDU[,2] == TRUE) & (majorMLNOEDU[,2] == FALSE)) / 301, sum((majorSUMNOEDU[,3] == TRUE) & (majorMLNOEDU[,3] == FALSE)) / 301, sum((majorSUMNOEDU[,4] == TRUE) & (majorMLNOEDU[,4] == FALSE)) / 301),
c(sum((petersenSUMNOEDU[,1] == TRUE) & (petersenMLNOEDU[,1] == FALSE)) / 301, sum((petersenSUMNOEDU[,2] == TRUE) & (petersenMLNOEDU[,2] == FALSE)) / 301, sum((petersenSUMNOEDU[,3] == TRUE) & (petersenMLNOEDU[,3] == FALSE)) / 301, sum((petersenSUMNOEDU[,4] == TRUE) & (petersenMLNOEDU[,4] == FALSE)) / 301),
c(sum((mildSUMNOEDU[,1] == TRUE) & (mildMLNOEDU[,1] == FALSE)) / 301, sum((mildSUMNOEDU[,2] == TRUE) & (mildMLNOEDU[,2] == FALSE)) / 301, sum((mildSUMNOEDU[,3] == TRUE) & (mildMLNOEDU[,3] == FALSE)) / 301, sum((mildSUMNOEDU[,4] == TRUE) & (mildMLNOEDU[,4] == FALSE)) / 301),

c(sum((majorSUMNOEDU[,1] == TRUE) & (majorMLNOEDU[,1] == TRUE)) / 301, sum((majorSUMNOEDU[,2] == TRUE) & (majorMLNOEDU[,2] == TRUE)) / 301, sum((majorSUMNOEDU[,3] == TRUE) & (majorMLNOEDU[,3] == TRUE)) / 301, sum((majorSUMNOEDU[,4] == TRUE) & (majorMLNOEDU[,4] == TRUE)) / 301),
c(sum((petersenSUMNOEDU[,1] == TRUE) & (petersenMLNOEDU[,1] == TRUE)) / 301, sum((petersenSUMNOEDU[,2] == TRUE) & (petersenMLNOEDU[,2] == TRUE)) / 301, sum((petersenSUMNOEDU[,3] == TRUE) & (petersenMLNOEDU[,3] == TRUE)) / 301, sum((petersenSUMNOEDU[,4] == TRUE) & (petersenMLNOEDU[,4] == TRUE)) / 301),
c(sum((mildSUMNOEDU[,1] == TRUE) & (mildMLNOEDU[,1] == TRUE)) / 301, sum((mildSUMNOEDU[,2] == TRUE) & (mildMLNOEDU[,2] == TRUE)) / 301, sum((mildSUMNOEDU[,3] == TRUE) & (mildMLNOEDU[,3] == TRUE)) / 301, sum((mildSUMNOEDU[,4] == TRUE) & (mildMLNOEDU[,4] == TRUE)) / 301)
), ncol = 8), digits = 1)

##### Additional descriptive statistics
descdat <- data.frame(Stroke = mydat$D6.1Stroke, PD = mydat$D25.1PD, Depression = mydat$D27.1Depression, Schizophrenia = mydat$D28.1SZ, Anxiety = mydat$D29.1Anexity, Bipolar = mydat$D30.1Bipolar, Education = mydat$E3_EduLevel)

apply(descdat[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,], 2, table)
apply(descdat[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU",], 2, table)
apply(descdat[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,][EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "NOEDU",], 2, table)

round(100 * c(306, 135, 90, 8, 11, 43, 5, 20, 2, 5, 1) / 550, 1)
round(100 * c(306, 135, 90, 8, 11, 43) / 1028, 1)
round(100 * c(23, 6, 4, 2, 6, 1) / 478, 1)
round(100 * c(66, 11, 24, 4, 11, 2) / 1028, 1)
