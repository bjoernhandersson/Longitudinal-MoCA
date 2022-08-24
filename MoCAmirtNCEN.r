#Load required packages
library(foreign)
library(car)
library(mirt)
library(lamle)
library(mvtnorm)
library(xtable)
#Read in data and prepare data
try(mydat <- read.spss("C:\\Users\\bjorn\\Dropbox (UiO)\\Data\\HS Data MocA\\HS Data_EnvirAnalysis.sav"))
try(mydat <- read.spss("C:\\Users\\bjorand\\Dropbox (UiO)\\Data\\HS Data MocA\\HS Data_EnvirAnalysis.sav"))
try(mydat <- read.spss("D:\\Dropbox (UiO)\\Data\\HS Data MocA\\HS Data_EnvirAnalysis.sav"))
mydat <- as.data.frame(mydat)
dim(mydat)
names(mydat)

#MoCA variable names at time points 1 to 4
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

##Define MoCA data objects
MoCAT1 <- mydat[,MoCAT1Names]
MoCAT2 <- mydat[,MoCAT2Names]
MoCAT3 <- mydat[,MoCAT3Names]
MoCAT4 <- mydat[,MoCAT4Names]


#Check data, identifies some recoding needed
apply(MoCAT1, 2, table)
apply(MoCAT2, 2, table)
apply(MoCAT3, 2, table)
apply(MoCAT4, 2, table)

#Recode missing value indicator 9999 to R NAs
for(i in 1:14) MoCAT1[,i] <- recode(MoCAT1[,i], '9999 = NA')
for(i in 1:14) MoCAT2[,i] <- recode(MoCAT2[,i], '9999 = NA')
for(i in 1:14) MoCAT3[,i] <- recode(MoCAT3[,i], '9999 = NA')
for(i in 1:14) MoCAT4[,i] <- recode(MoCAT4[,i], '9999 = NA')

#Recode incorrect input for items 8 and 9 at time points 1 and 4, resp.
MoCAT1[,9] <- recode(MoCAT1[,9], '4 = 3')
MoCAT4[,8] <- recode(MoCAT4[,8], '2 = 1')

#Recode item 14 (Orient) 0 and 1 to 2, since too few observations for EDU/NOEDU groups at some time points
for(i in 14:14) MoCAT1[,i] <- recode(MoCAT1[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT2[,i] <- recode(MoCAT2[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT3[,i] <- recode(MoCAT3[,i], 'c(0, 1) = 2')
for(i in 14:14) MoCAT4[,i] <- recode(MoCAT4[,i], 'c(0, 1) = 2')

#General check for missing data
apply(MoCAT1, 2, function(x) sum(is.na(x)))
apply(MoCAT2, 2, function(x) sum(is.na(x)))
apply(MoCAT3, 2, function(x) sum(is.na(x)))
apply(MoCAT4, 2, function(x) sum(is.na(x)))

#Identify how many observations had all missing at each time point
#These observations must be removed before the analysis
sum(apply(MoCAT1, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT2, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT3, 1, function(x) sum(is.na(x))) == 14)
sum(apply(MoCAT4, 1, function(x) sum(is.na(x))) == 14)

#Create objects indicating which cases should be kept
NAadjT1 <- !(apply(MoCAT1, 1, function(x) sum(is.na(x))) == 14)
NAadjT2 <- !(apply(MoCAT2, 1, function(x) sum(is.na(x))) == 14)
NAadjT3 <- !(apply(MoCAT3, 1, function(x) sum(is.na(x))) == 14)
NAadjT4 <- !(apply(MoCAT4, 1, function(x) sum(is.na(x))) == 14)

#Total cases at each time point and across all time points
sum(NAadjT1)
sum(NAadjT2)
sum(NAadjT3)
sum(NAadjT4)
sum(NAadjT1 & NAadjT2  & NAadjT3 & NAadjT4)

#Define vector of observations that are valid at all time points
NAadjT1toT4 <- (NAadjT1 & NAadjT2  & NAadjT3 & NAadjT4)

##Defin education level vector
EDUC <- mydat$E3_EduLevel  # revised by Hao
EDUC <- recode(EDUC, "'no formal education' = 'NOEDU'; 9999 = NA;  
               else='EDU'")

#Define vector indicating valid cases
NAadjEDU <- !is.na(EDUC)

MoCAT1toT4 <- cbind(MoCAT1, MoCAT2, MoCAT3, MoCAT4, EDUC)[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU,-57]
MoCAT1toT4 <- as.matrix(MoCAT1toT4) + 1
MoCAT1toT4[,c(14, 28, 42, 56)] <- MoCAT1toT4[,c(14, 28, 42, 56)] - 2

groupdef <- numeric(1028)
groupdef[which(EDUC[NAadjT1 & NAadjT2 & NAadjT3 & NAadjT4 & NAadjEDU] == "EDU")] <- 1

MoCAT1toT4EDU <- MoCAT1toT4[groupdef == 1,]
MoCAT1toT4NOEDU <- MoCAT1toT4[groupdef == 0,]


MoCA18D <- '
	F1 = 1-14
	F2 = 15-28
	F3 = 29-42
	F4 = 43-56
	
	F5 = 1, 15, 29, 43
	F6 = 2, 16, 30, 44
	F7 = 3, 17, 31, 45
	F8 = 4, 18, 32, 46
	F9 = 5, 19, 33, 47
	F10 = 6, 20, 34, 48
	F11 = 7, 21, 35, 49
	F12 = 8, 22, 36, 50
	F13 = 9, 23, 37, 51
	F14 = 10, 24, 38, 52
	F15 = 11, 25, 39, 53
	F16 = 12, 26, 40, 54
	F17 = 13, 27, 41, 55
	F18 = 14, 28, 42, 56
	
	COV = F2*F2, F3*F3, F4*F4, F1*F2*F3*F4
	MEAN = F2, F3, F4
	
	CONSTRAIN = (1, 15, 29, 43, a1, a2, a3, a4)
	CONSTRAIN = (2, 16, 30, 44, a1, a2, a3, a4)
	CONSTRAIN = (3, 17, 31, 45, a1, a2, a3, a4)
	CONSTRAIN = (4, 18, 32, 46, a1, a2, a3, a4)
	CONSTRAIN = (5, 19, 33, 47, a1, a2, a3, a4)
	CONSTRAIN = (6, 20, 34, 48, a1, a2, a3, a4)
	CONSTRAIN = (7, 21, 35, 49, a1, a2, a3, a4)
	CONSTRAIN = (8, 22, 36, 50, a1, a2, a3, a4)
	CONSTRAIN = (9, 23, 37, 51, a1, a2, a3, a4)
	CONSTRAIN = (10, 24, 38, 52, a1, a2, a3, a4)
	CONSTRAIN = (11, 25, 39, 53, a1, a2, a3, a4)
	CONSTRAIN = (12, 26, 40, 54, a1, a2, a3, a4)
	CONSTRAIN = (13, 27, 41, 55, a1, a2, a3, a4)
	CONSTRAIN = (14, 28, 42, 56, a1, a2, a3, a4)


	CONSTRAIN = (1, 15, 29, 43, d1)	
	CONSTRAIN = (2, 16, 30, 44, d1)
	CONSTRAIN = (3, 17, 31, 45, d1)
	CONSTRAIN = (4, 18, 32, 46, d1)
	CONSTRAIN = (5, 19, 33, 47, d1)
	
	CONSTRAIN = (6, 20, 34, 48, d1)
	CONSTRAIN = (6, 20, 34, 48, d2)
	CONSTRAIN = (6, 20, 34, 48, d3)
	
	CONSTRAIN = (7, 21, 35, 49, d1)
	CONSTRAIN = (7, 21, 35, 49, d2)
	
	CONSTRAIN = (8, 22, 36, 50, d1)
	
	CONSTRAIN = (9, 23, 37, 51, d1)
	CONSTRAIN = (9, 23, 37, 51, d2)
	CONSTRAIN = (9, 23, 37, 51, d3)
	
	CONSTRAIN = (10, 24, 38, 52, d1)
	CONSTRAIN = (10, 24, 38, 52, d2)

	CONSTRAIN = (11, 25, 39, 53, d1)
	
	CONSTRAIN = (12, 26, 40, 54, d1)
	CONSTRAIN = (12, 26, 40, 54, d2)

	CONSTRAIN = (13, 27, 41, 55, d1)
	CONSTRAIN = (13, 27, 41, 55, d2)
	CONSTRAIN = (13, 27, 41, 55, d3)
	CONSTRAIN = (13, 27, 41, 55, d4)
	CONSTRAIN = (13, 27, 41, 55, d5)
	
	CONSTRAIN = (14, 28, 42, 56, d1)
	CONSTRAIN = (14, 28, 42, 56, d2)
	CONSTRAIN = (14, 28, 42, 56, d3)
	CONSTRAIN = (14, 28, 42, 56, d4)
'


mirtMoCA18DEDU <- mirt(MoCAT1toT4EDU, MoCA18D, itemtype = "graded", method = "MHRM", TOL = 0.0001, draws = 5, SE = TRUE, SE.type = "FMHRM", technical = list(MHRM_SE_draws = 20000, NCYCLES = 4000, MHDRAWS = 20))

mirtMoCA18DNOEDU <- mirt(MoCAT1toT4NOEDU, MoCA18D, itemtype = "graded", method = "MHRM", TOL = 0.0001, draws = 5, SE = TRUE, SE.type = "FMHRM", technical = list(MHRM_SE_draws = 20000, NCYCLES = 4000, MHDRAWS = 20))

mirtMoCA18DEDUpars <- mirt(MoCAT1toT4EDU, MoCA18D, itemtype = "graded", method = "MHRM", TOL = 0.0001, draws = 5, SE = FALSE, SE.type = "FMHRM", technical = list(MHRM_SE_draws = 20000, NCYCLES = 10000, MHDRAWS = 20), pars = 'values')

mirtMoCA18DNOEDUpars <- mirt(MoCAT1toT4NOEDU, MoCA18D, itemtype = "graded", method = "MHRM", TOL = 0.0001, draws = 5, SE = FALSE, SE.type = "FMHRM", technical = list(MHRM_SE_draws = 20000, NCYCLES = 10000, MHDRAWS = 20), pars = 'values')


##### lamle: Model definition
lamleMoCA18D <- matrix(NA, 56, 18)
lamleMoCA18D[1:14, 1] <- 1
lamleMoCA18D[15:28, 2] <- 1
lamleMoCA18D[29:42, 3] <- 1
lamleMoCA18D[43:56, 4] <- 1
lamleMoCA18D[c(1, 15, 29, 43), 5] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 1, 6] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 2, 7] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 3, 8] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 4, 9] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 5, 10] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 6, 11] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 7, 12] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 8, 13] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 9, 14] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 10, 15] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 11, 16] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 12, 17] <- 1
lamleMoCA18D[c(1, 15, 29, 43) + 13, 18] <- 1


##### lamle: Define restrictions on items
ParEqMoCA18D <- vector("list", 7)
ParEqMoCA18D[[1]] <- vector("list", 14)
ParEqMoCA18D[[2]] <- vector("list", 14)
ParEqMoCA18D[[3]] <- ParEqMoCA18D[[4]] <- ParEqMoCA18D[[5]] <- ParEqMoCA18D[[6]] <- ParEqMoCA18D[[7]] <- vector ("list", 1)
ParEqMoCA18D[[1]][[1]] <- list(items = c(1, 15, 29, 43), dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[2]] <- list(items = c(1, 15, 29, 43) + 1, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[3]] <- list(items = c(1, 15, 29, 43) + 2, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[4]] <- list(items = c(1, 15, 29, 43) + 3, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[5]] <- list(items = c(1, 15, 29, 43) + 4, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[6]] <- list(items = c(1, 15, 29, 43) + 5, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[7]] <- list(items = c(1, 15, 29, 43) + 6, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[8]] <- list(items = c(1, 15, 29, 43) + 7, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[9]] <- list(items = c(1, 15, 29, 43) + 8, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[10]] <- list(items = c(1, 15, 29, 43) + 9, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[11]] <- list(items = c(1, 15, 29, 43) + 10, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[12]] <- list(items = c(1, 15, 29, 43) + 11, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[13]] <- list(items = c(1, 15, 29, 43) + 12, dimension = c(1, 2, 3, 4))
ParEqMoCA18D[[1]][[14]] <- list(items = c(1, 15, 29, 43) + 13, dimension = c(1, 2, 3, 4))

ParEqMoCA18D[[2]][[1]] <- list(items = c(1, 15, 29, 43))
ParEqMoCA18D[[2]][[2]] <- list(items = c(1, 15, 29, 43) + 1)
ParEqMoCA18D[[2]][[3]] <- list(items = c(1, 15, 29, 43) + 2)
ParEqMoCA18D[[2]][[4]] <- list(items = c(1, 15, 29, 43) + 3)
ParEqMoCA18D[[2]][[5]] <- list(items = c(1, 15, 29, 43) + 4)
ParEqMoCA18D[[2]][[6]] <- list(items = c(1, 15, 29, 43) + 5)
ParEqMoCA18D[[2]][[7]] <- list(items = c(1, 15, 29, 43) + 6)
ParEqMoCA18D[[2]][[8]] <- list(items = c(1, 15, 29, 43) + 7)
ParEqMoCA18D[[2]][[9]] <- list(items = c(1, 15, 29, 43) + 8)
ParEqMoCA18D[[2]][[10]] <- list(items = c(1, 15, 29, 43) + 9)
ParEqMoCA18D[[2]][[11]] <- list(items = c(1, 15, 29, 43) + 10)
ParEqMoCA18D[[2]][[12]] <- list(items = c(1, 15, 29, 43) + 11)
ParEqMoCA18D[[2]][[13]] <- list(items = c(1, 15, 29, 43) + 12)
ParEqMoCA18D[[2]][[14]] <- list(items = c(1, 15, 29, 43) + 13)

ParFixMoCA18D <- vector("list", 7)
ParFixMoCA18D[[1]] <- ParFixMoCA18D[[2]] <- ParFixMoCA18D[[3]] <- ParFixMoCA18D[[4]] <- vector("list", 1)
ParFixMoCA18D[[5]] <- vector("list", 2)
ParFixMoCA18D[[5]][[1]] <- list(mean = 1, value = 0, groups = 1)
ParFixMoCA18D[[5]][[2]] <- list(mean = 5:18, value = rep(0, 14), groups = c(1))
ParFixMoCA18D[[6]] <- vector("list", 2)
ParFixMoCA18D[[6]][[1]] <- list(variance = 1, value = 1, groups = 1)
ParFixMoCA18D[[6]][[2]] <- list(variance = 5:18, value = rep(1, 14), groups = c(1))
ParFixMoCA18D[[7]] <- vector("list", 1)
ParFixMoCA18D[[7]][[1]] <- list(covariance = (1:153)[-c(1, 2, 3, 18, 19, 34)], value = rep(0, 147), groups = c(1))


mirtMoCA18DEDUGRMSV <- c(mirtMoCA18DEDUpars[1:280, 6][mirtMoCA18DEDUpars[1:280, 9]],
rep(0.851, 14 * 3), 
c(0, 0, 0), c(1, 1, 1), c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25))

mirtMoCA18DNOEDUGRMSV <- c(mirtMoCA18DNOEDUpars[1:280, 6][mirtMoCA18DNOEDUpars[1:280, 9]],
rep(0.851, 14 * 3), 
c(0, 0, 0), c(1, 1, 1), c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25))



estMoCA18DEDUGRMLap1SV <- lamle(y = as.matrix(MoCAT1toT4EDU), itemdim = lamleMoCA18D, parequal = ParEqMoCA18D, parfix = ParFixMoCA18D, accuracy = 1, tol = 1e-4, maxit = 500, optimizer = "BFGS", obsinfo = FALSE, maxdiff = 0.25, step.length = c(0.5, 1), first.step = 25, inithess = "NCW", startval = mirtMoCA18DEDUGRMSV, starttheta = NULL)
estMoCA18DEDUGRMLap2SV <- lamle(y = as.matrix(MoCAT1toT4EDU), itemdim = lamleMoCA18D, parequal = ParEqMoCA18D, parfix = ParFixMoCA18D, accuracy = 2, tol = 1e-4, maxit = 500, optimizer = "BFGS", obsinfo = FALSE, maxdiff = 0.25, step.length = c(0.5, 1), first.step = 5, inithess = "crossprod", startval = estMoCA18DEDUGRMLap1SV$par, starttheta = estMoCA18DEDUGRMLap1SV$map)

estMoCA18DNOEDUGRMLap1SV <- lamle(y = as.matrix(MoCAT1toT4NOEDU), itemdim = lamleMoCA18D, parequal = ParEqMoCA18D, parfix = ParFixMoCA18D, accuracy = 1, tol = 1e-4, maxit = 500, optimizer = "BFGS", obsinfo = FALSE, maxdiff = 0.25, step.length = c(0.5, 1), first.step = 25, inithess = "NCW", startval = mirtMoCA18DNOEDUGRMSV, starttheta = NULL)

estMoCA18DNOEDUGRMLap2SV <- lamle(y = as.matrix(MoCAT1toT4NOEDU), itemdim = lamleMoCA18D, parequal = ParEqMoCA18D, parfix = ParFixMoCA18D, accuracy = 2, tol = 1e-4, maxit = 500, optimizer = "BFGS", obsinfo = FALSE, maxdiff = 0.25, step.length = c(0.5, 1), first.step = 25, inithess = "NCW", startval = mirtMoCA18DNOEDUGRMSV, starttheta = NULL)

estMoCA18DNOEDUGRMLap2SV <- lamle(y = as.matrix(MoCAT1toT4NOEDU), itemdim = lamleMoCA18D, parequal = ParEqMoCA18D, parfix = ParFixMoCA18D, accuracy = 2, tol = 1e-4, maxit = 500, optimizer = "BFGS", obsinfo = FALSE, maxdiff = 0.25, step.length = c(0.5, 1), first.step = 5, inithess = "crossprod", startval = estMoCA18DEDUGRMLap2SV$par, starttheta = NULL)

lamle.fit(estMoCA18DEDUGRMLap2SV, as.matrix(MoCAT1toT4EDU), N = 100)
lamle.fit(estMoCA18DNOEDUGRMLap2SV, as.matrix(MoCAT1toT4NOEDU), N = 100)


