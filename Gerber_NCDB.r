
read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

# filter for only T1-2 N0-1 M0 cases with mastectomy ER+ and available Oncotype testing
dat <- read[(read$TNM_PATH_T == 2 | read$TNM_PATH_T  == 1) & (read$TNM_PATH_N == 0 | read$TNM_PATH_N  == 1) & read$TNM_CLIN_M == 0 & read$RX_SUMM_SURG_PRIM_SITE > 29 & read$RX_SUMM_SURG_PRIM_SITE <= 80 & read$CS_SITESPECIFIC_FACTOR_23 < 101 & read$CS_SITESPECIFIC_FACTOR_1 == 10 & !is.na(read$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(read$PUF_VITAL_STATUS),]


dat <- read[(read$TNM_PATH_T == 2 | read$TNM_PATH_T  == 1) & (read$TNM_PATH_N  == 1) & read$TNM_CLIN_M == 0 & read$RX_SUMM_SURG_PRIM_SITE > 29 & read$RX_SUMM_SURG_PRIM_SITE <= 80 & read$CS_SITESPECIFIC_FACTOR_23 < 101 & read$CS_SITESPECIFIC_FACTOR_1 == 10 & !is.na(read$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(read$PUF_VITAL_STATUS),]

###########################################
# Here are some relevant variables (see complete list seperately)
# [1] "PUF_CASE_ID"                 "FACILITY_TYPE_CD"           
# [3] "AGE"                         "DX_DEFSURG_STARTED_DAYS"                     
# [5] "RACE"                        "INSURANCE_STATUS"           
# [7] "YEAR_OF_DIAGNOSIS"           "HISTOLOGY"                  
# [9] "GRADE"                       "TNM_CLIN_T"                 
# [11] "TNM_CLIN_N"                  "TNM_CLIN_M"                 
# [13] "TNM_CLIN_STAGE_GROUP"        "TNM_PATH_T"                 
# [15] "TNM_PATH_N"                  "TNM_PATH_M"                 
# [17] "TNM_PATH_STAGE_GROUP"        "TNM_EDITION_NUMBER"         
# [19] "RX_SUMM_RADIATION"           "RAD_LOCATION_OF_RX"         
# [21] "RAD_TREAT_VOL"               "RAD_REGIONAL_RX_MODALITY"   
# [23] "RAD_REGIONAL_DOSE_CGY"       "RAD_BOOST_RX_MODALITY"      
# [25] "RAD_BOOST_DOSE_CGY"          "RAD_NUM_TREAT_VOL"          
# [27] "RX_SUMM_SURGRAD_SEQ"         "PUF_VITAL_STATUS"           
# [29] "DX_LASTCONTACT_DEATH_MONTHS" "RX_SUMM_IMMUNOTHERAPY"      
# [31] "RX_SUMM_SYSTEMIC_SUR_SEQ"    "DX_SYSTEMIC_STARTED_DAYS"   
# [33] "DX_CHEMO_STARTED_DAYS"       "CDCC_TOTAL"                 
# [35] "CROWFLY"                     "DX_RAD_STARTED_DAYS"       


# RX_SUMM_SURG_PRIM_SITE  "SITE SPECIFIC SURGERY"
# Nipple Sparing Mastectomy (Subcutaneous): 30
# Mastectomy) 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 73-75, 78, 50 - 59, 63, 60- 61, 64-69, 70-72, 80)
# essentially 30 - 80

# DX_HORMONE_STARTED_DAYS

# Oncotype is in CS Factor 23
# read$CS_SITESPECIFIC_FACTOR_23
#########################################
# install dependencies
# install.packages('tableone')
# install.packages('survival')
#install.packages("gdtools")
#install.packages("gdtools", repos = "https://cloud.r-project.org/", type="source")

#install.packages("ReporteRs")
# install.packages("magrittr")
library(tableone)
library(survival)
#library(ReporteRs)
#library(magrittr)

########################################

#########################################
# split data into comparative groups
# neoadjuvant vs. no induction chemotherapy mastectomy
C_Date <- dat$DX_CHEMO_STARTED_DAYS
R_ <- dat$RX_SUMM_RADIATION 
R_Date <- dat$DX_RAD_STARTED_DAYS
S_Date <- dat$DX_SURG_STARTED_DAYS
H_Date <- dat$DX_HORMONE_STARTED_DAYS

R_Leng <- dat$RAD_ELAPSED_RX_DAYS

CS_Seq <- dat$RX_SUMM_SYSTEMIC_SUR_SEQ 
RS_Seq <- dat$RX_SUMM_SURGRAD_SEQ


# MASTECTOMY RT regardless of neoadjuvant
PMRT <- dat[RS_Seq == 3, ]
PMRT<-subset(PMRT, (!is.na(PMRT[,1])))

NoPMRT <- dat[R_ == 0, ]
NoPMRT<-subset(NoPMRT, (!is.na(NoPMRT[,1])))

# final coding of the treatment groups
PMRT$P_RT <- 'PMRT'
NoPMRT$P_RT <- 'NoPMRT'

# final dataframe
MASTDX <- rbind(PMRT,NoPMRT)

#recode for Oncotype
MASTDX$ONCDX<-NULL
MASTDX$ONCDX[MASTDX$CS_SITESPECIFIC_FACTOR_23 <= 25] <- "LowRisk"
MASTDX$ONCDX[MASTDX$CS_SITESPECIFIC_FACTOR_23 > 25] <- "HighRisk"

# descriptive statistics
listVars<-(c("AGE",'ONCDX',"YEAR_OF_DIAGNOSIS","TNM_PATH_T", "TNM_PATH_N","RACE"))

catVars<-(c("TNM_PATH_T",'TNM_PATH_N','ONCDX','YEAR_OF_DIAGNOSIS',"RACE")) 
table1 <- CreateTableOne(vars = listVars, data = MASTDX, factorVars = catVars, strata = 'P_RT')

write.csv(print(table1),'H:\\Gerber\\NCDB\\BreastPMRT.csv')
 
#########################################
# survival analysis for entire cohort and by treatment
attach(MASTDX)

#find the follow up and censor time
followup_time = MASTDX["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = MASTDX["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of All T1-2N1M0 ER+ Breast Cancer")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival by PMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 1.0, c("PMRT","NoPMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model
coxmodel <- coxph(Surv(followup_time, censor)~AGE + CDCC_TOTAL + as.factor(P_RT)+ as.factor(ONCDX)+ as.factor(droplevels(TNM_PATH_T))+ as.factor(droplevels(TNM_PATH_N)))
coxmodel
detach(MASTDX)

#########################################
#analysis for high risk cohort and by treatment
HIGHRISK <- MASTDX[MASTDX$ONCDX == "HighRisk",]
attach(HIGHRISK)

#find the follow up and censor time
followup_time = HIGHRISK["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = HIGHRISK["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of High Risk By Oncotype T1-2N0-1M0 ER+ Breast Cancer")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival of High Risk patients by PMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 1.0, c("PMRT","NoPMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model
coxmodel <- coxph(Surv(followup_time, censor)~SEX + AGE + CDCC_TOTAL + as.factor(P_RT)+ as.factor(droplevels(TNM_PATH_T))+ as.factor(droplevels(TNM_PATH_N)))
coxmodel
detach(HIGHRISK)

#########################################
#analysis for high risk cohort and by treatment
ULTRARISK <- MASTDX[MASTDX$CS_SITESPECIFIC_FACTOR_23 > 25 & MASTDX$AGE < 50,]
attach(ULTRARISK)

#find the follow up and censor time
followup_time = ULTRARISK["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = ULTRARISK["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of High Risk Premenopausal T1-2N0-1M0 ER+ Breast Cancer")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival of Young (<50) High Risk patients by PMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 1.0, c("PMRT","NoPMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model
coxmodel <- coxph(Surv(followup_time, censor)~SEX + AGE + CDCC_TOTAL + as.factor(P_RT)+ as.factor(droplevels(TNM_PATH_T))+ as.factor(droplevels(TNM_PATH_N)))
coxmodel
detach(ULTRARISK)
######################################
#analysis for low risk cohort and by treatment
LOWRISK <- MASTDX[MASTDX$ONCDX == "LowRisk",]
attach(LOWRISK)

#find the follow up and censor time
followup_time = MASTDX["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = MASTDX["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of Low Risk T1-2N0-1M0 ER+ Breast Cancer")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival of Low Risk patients by PMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 1.0, c("PMRT","NoPMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model
coxmodel <- coxph(Surv(followup_time, censor)~SEX + AGE + CDCC_TOTAL + as.factor(P_RT)+ as.factor(ONCDX)+ as.factor(droplevels(TNM_PATH_T))+ as.factor(droplevels(TNM_PATH_N)))
coxmodel
detach(LOWRISK)

########################################

#analysis for high risk cohort and by treatment
THIRTYHIGHRISK <- MASTDX[MASTDX$CS_SITESPECIFIC_FACTOR_23 > 40 ,]
attach(THIRTYHIGHRISK)

#find the follow up and censor time
followup_time = THIRTYHIGHRISK["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = THIRTYHIGHRISK["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of Very High Risk (>30) T1-2N0-1M0 ER+ Breast Cancer")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival of >35 Risk patients by PMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 1.0, c("PMRT","NoPMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model
coxmodel <- coxph(Surv(followup_time, censor)~SEX + AGE + CDCC_TOTAL + as.factor(P_RT)+ as.factor(droplevels(TNM_PATH_T))+ as.factor(droplevels(TNM_PATH_N)))
coxmodel
detach(THIRTYHIGHRISK)

#########################################
# survival analysis for CCRT w/ and w/o IMRT

#########################################
# propensity score matched cohort analysis

# KM survival analysis

# Cox Model

