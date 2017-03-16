##
# ##
# 1)	Let’s look at stage grouping for N1mi patients and look more into patients with inconsistencies
	# Only those with "documented T1mic" disease, N1 disease
	# Can exclude those patients with M1 disease 
	
# 4)	Chemo should be added to current table 1
    # whether they recieved chemotherapy or not
	
# 2)	Is there a way to quantify number of nodes involved in N1mi patients (ie just one node with microscopic dx or multiple nodes).
    # look into number of nodes removed

# 3)	Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table

# 5)	Look at PMRT in N1mi patients <50 years old
    # and those with higher risk features
#
read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# Exclude patients with previous cancer history
dats1 <- dats[dats$SEQUENCE == 1 | dats$SEQUENCE == 0 ,]
dim(dat3)

# Include AJCC pT1-T2 patients
dat2 <- dats1[dats1$TNM_PATH_T == '1' | dats1$TNM_PATH_T == '1A' | dats1$TNM_PATH_T == '1B' | dats1$TNM_PATH_T == '1C' | dats1$TNM_PATH_T == '1MI' | dats1$TNM_PATH_T == '2',]

# exclude M1 and N3, NX disease and >T3 disease 
dat3 <- dat2[dat2$TNM_CLIN_M != 1 & dat2$TNM_PATH_N != 3 & dat2$TNM_PATH_N != '3A'& dat2$TNM_PATH_N != '3B'& dat2$TNM_PATH_N != '3C'& dat2$TNM_PATH_N != 88 & dat2$TNM_PATH_N != 'X',]
dim(dat3)

# include patients who had documented mastectomy with negative margins
dat4 <- dat3[dat3$RX_SUMM_SURG_PRIM_SITE >= 30 & dat3$RX_SUMM_SURG_PRIM_SITE <= 80  & dat3$RX_SUMM_SURGICAL_MARGINS == 0,]
dim(dat4)

# include only patients treated with RT after Surgery
dat5 <- dat4[(dat4$RX_SUMM_SURGRAD_SEQ ==  3),]
dim(dat5)

dat <- dat5




#Participants were women with unilateral, early-stage breast cancer (stages T1 to 2, N0 to 1, M0) who were diagnosed in BC from January 1, 1989 to December 31, 2003. Patients who were men or who had bilateral or synchronous contralateral breast cancers diagnosed within 6 months; who had in situ disease alone; or who had clinical or pathologic stages T3 or T4, N2 or N3 (including patients with four or more positive nodes), or M1 disease were excluded. Patients who were treated with mastectomy, adjuvant chemotherapy, RT greater than 52 weeks after BCS, who were diagnosed outside BC, or who were not referred to the BCCA with newly diagnosed breast cancer were excluded.

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
# split data into nodal groups
# T1-T2N0 
# Vs.
# T1-T2N1mi
# Vs.
# T1-T2, N1 
# Vs. 
# T1-T2N2 

# Bin patients into different time N stages
# N0
N0 <- dat[(dat$TNM_PATH_N == 0 | dat$TNM_PATH_N == '0I-' | dat$TNM_PATH_N == '0I+'  | dat$TNM_PATH_N == '0M-'| dat$TNM_PATH_N == '0M+'), ]
N0$NODE_CAT <- 'T1-T2 N0'

# N1mi
N1mi <- dat[(dat$TNM_PATH_N =='1MI'), ]
N1mi$NODE_CAT <- 'T1-T2 N1mi'

# N1
N1 <- dat[(dat$TNM_PATH_N == 1 | dat$TNM_PATH_N == '1A' | dat$TNM_PATH_N == '1B'  | dat$TNM_PATH_N == '1C'), ]
N1$NODE_CAT <- 'T1-T2 N1'

# N2
N2 <- dat[(dat$TNM_PATH_N == 2 | dat$TNM_PATH_N == '2A' | dat$TNM_PATH_N == '2B'), ]
N2$NODE_CAT <- 'T1-T2 N2'


############### Create the tableone ##########
NODAL <- rbind(N0,N1mi)
NODAL <- rbind(NODAL,N1)
NODAL <- rbind(NODAL,N2)

NODAL$AGE_2=NULL
NODAL[NODAL$AGE <50 ,'AGE_2'] = "<50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_2'] = "50-65"
NODAL[NODAL$AGE >=65,'AGE_2'] = "> or = 65"

NODAL$RACE_2 = 'White'
NODAL[NODAL$RACE >1 ,'RACE_2'] = "Other"

NODAL$ERSTATUS = 'Unknown'
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

NODAL$CDCC_TOTAL_2 = NULL
NODAL[NODAL$CDCC_TOTAL ==0 ,'CDCC_TOTAL_2'] = "0"
NODAL[NODAL$CDCC_TOTAL ==1 ,'CDCC_TOTAL_2'] = "1"
NODAL[NODAL$CDCC_TOTAL ==2 ,'CDCC_TOTAL_2'] = "> or = 2"


# descriptive statistics
listVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","CROWFLY","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','RX_SUMM_RADIATION','RX_SUMM_CHEMO','RX_SUMM_SYSTEMIC_SUR_SEQ','RX_SUMM_HORMONE','REGIONAL_NODES_EXAMINED','REGIONAL_NODES_POSITIVE'))

catVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','RX_SUMM_RADIATION','RX_SUMM_CHEMO','RX_SUMM_SYSTEMIC_SUR_SEQ','RX_SUMM_HORMONE','REGIONAL_NODES_POSITIVE','REGIONAL_NODES_EXAMINED')) 
table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'NODE_CAT')

write.csv(print(table1),'H:\\Gerber\\NCDB\\N1Mi_TableOne_1_12_2017_ChemoHrom_Nodes.csv')
 

# 3)	Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table
########################## Create the new data set ################

# include only patients treated with RT after Surgery
dat6 <- dat4[(dat4$RX_SUMM_SURGRAD_SEQ ==  3 | dat4$RX_SUMM_SURGRAD_SEQ ==  0 ),]
dim(dat6)

dat <- dat6
N1mi_P <- dat[(dat$TNM_PATH_N =='1MI'), ]

N1mi_P$PMRT <- 'No PMRT'
N1mi_P[N1mi_P$RX_SUMM_SURGRAD_SEQ == 3 ,'PMRT'] = "PMRT"


N1mi_P$AGE_2=NULL
N1mi_P[N1mi_P$AGE <50 ,'AGE_2'] = "<50"
N1mi_P[N1mi_P$AGE <65 & N1mi_P$AGE >=50,'AGE_2'] = "50-65"
N1mi_P[N1mi_P$AGE >=65,'AGE_2'] = "> or = 65"

N1mi_P$RACE_2 = 'White'
N1mi_P[N1mi_P$RACE >1 ,'RACE_2'] = "Other"
N1mi_P$ERSTATUS = 'Unknown'
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

N1mi_P$CDCC_TOTAL_2 = NULL
N1mi_P[N1mi_P$CDCC_TOTAL ==0 ,'CDCC_TOTAL_2'] = "0"
N1mi_P[N1mi_P$CDCC_TOTAL ==1 ,'CDCC_TOTAL_2'] = "1"
N1mi_P[N1mi_P$CDCC_TOTAL ==2 ,'CDCC_TOTAL_2'] = "> or = 2"


# descriptive statistics
listVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","CROWFLY","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','RX_SUMM_RADIATION','RX_SUMM_CHEMO','RX_SUMM_SYSTEMIC_SUR_SEQ','RX_SUMM_HORMONE','REGIONAL_NODES_EXAMINED','REGIONAL_NODES_POSITIVE'))

catVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','RX_SUMM_RADIATION','RX_SUMM_CHEMO','RX_SUMM_SYSTEMIC_SUR_SEQ','RX_SUMM_HORMONE','REGIONAL_NODES_POSITIVE','REGIONAL_NODES_EXAMINED')) 
table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'PMRT')

write.csv(print(table1),'H:\\Gerber\\NCDB\\N1Mi_TableOne_1_12_2017_PMRT.csv')
 
 
########################## Create plot for all N1Mic ################
 

N1mi_P$AGE_2=NULL
N1mi_P[N1mi_P$AGE <50 ,'AGE_2'] = "<50"
N1mi_P[N1mi_P$AGE <65 & N1mi_P$AGE >=50,'AGE_2'] = "50-65"
N1mi_P[N1mi_P$AGE >=65,'AGE_2'] = "> or = 65"

N1mi_P$RACE_2 = 'White'
N1mi_P[N1mi_P$RACE >1 ,'RACE_2'] = "Other"
N1mi_P$ERSTATUS = 'Unknown'
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
N1mi_P[N1mi_P$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

N1mi_P$CDCC_TOTAL_2 = NULL
N1mi_P[N1mi_P$CDCC_TOTAL ==0 ,'CDCC_TOTAL_2'] = "0"
N1mi_P[N1mi_P$CDCC_TOTAL ==1 ,'CDCC_TOTAL_2'] = "1"
N1mi_P[N1mi_P$CDCC_TOTAL ==2 ,'CDCC_TOTAL_2'] = "> or = 2"

attach(N1mi_P)

#find the follow up and censor time
followup_time = N1mi_P["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = N1mi_P["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()



# stratify by PMRT
subset <- survfit(Surv(followup_time, censor)~PMRT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival Following \n of T1-T2 N1Mic by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(PMRT))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT))

detach(N1mi_P)



########################## Create plot for patients younger than 50 ################
N1mi_P_YOUNG <- N1mi_P[N1mi_P$AGE_2 == "<50",]
 
attach(N1mi_P_YOUNG)

#find the follow up and censor time
followup_time = N1mi_P_YOUNG["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = N1mi_P_YOUNG["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()



# stratify by PMRT
subset <- survfit(Surv(followup_time, censor)~PMRT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival Following \n of T1-T2 N1Mic of <50 by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("PMRT","No PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL)+ + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(PMRT))

detach(N1mi_P_YOUNG)



########################## Create the forest plot ################

NODAL <- rbind(N0,N1mi)
NODAL <- rbind(NODAL,N1)
NODAL <- rbind(NODAL,N2)

NODAL$AGE_2=NULL
NODAL[NODAL$AGE <50 ,'AGE_2'] = "<50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_2'] = "50-65"
NODAL[NODAL$AGE >=65,'AGE_2'] = "> or = 65"



NODAL$RACE_2 = 'White'
NODAL[NODAL$RACE >1 ,'RACE_2'] = "Other"
NODAL$ERSTATUS = 'Unknown'
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"


attach(NODAL)

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()



# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~NODE_CAT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival Following \n of T1-T2 Tumors',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("N0","N1","N1mi","N2"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)



# stratify by PMRT
subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival Following \n of Node Positive T1-T2 Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("PMRT","No PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)


detach(NODAL)





##################
# N0
NODAL <- N0


attach(NODAL)

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N0 Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)


detach(NODAL)

##################
# N1
NODAL <- N1


attach(NODAL)

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N1 Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)





detach(NODAL)

##################
# N1mi
NODAL <- N1mi

NODAL$AGE_2=NULL
NODAL[NODAL$AGE <50 ,'AGE_2'] = "<50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_2'] = "50-65"
NODAL[NODAL$AGE >=65,'AGE_2'] = "> or = 65"


NODAL$RACE_2 = 'White'
NODAL[NODAL$RACE >1 ,'RACE_2'] = "Other"
NODAL$ERSTATUS = 'Unknown'
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

attach(NODAL)

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N1mi Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)


# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ as.factor(RX_SUMM_RADIATION) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(RX_SUMM_SURGICAL_MARGINS) + CROWFLY + as.factor(FACILITY_LOCATION_CD) + as.factor(FACILITY_TYPE_CD))





detach(NODAL)

##################
# N2
NODAL <- N2


attach(NODAL)

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N2 Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)









results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(P_RT)[2]))

cochrane_from_rmeta <- 
  structure(list(
    mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531), 
    lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
    upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -11L), 
    class = "data.frame")

tabletext<-cbind(
  c("", "Study", "Auckland", "Block", 
    "Doran", "Gamsu", "Morrison", "Papageorgiou", 
    "Tauesch", NA, "Summary"),
  c("Deaths", "(steroid)", "36", "1", 
    "4", "14", "3", "1", 
    "8", NA, NA),
  c("Deaths", "(placebo)", "60", "5", 
    "11", "20", "7", "7", 
    "10", NA, NA),
  c("", "OR", "0.58", "0.16", 
    "0.25", "0.70", "0.35", "0.14", 
    "1.02", NA, "0.53"))

# draw a forest plot of the cox model
forestplot(tabletext, 
           graph.pos = 4,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "11" = gpar(lwd=1, columns=c(1:3,5), col = "#000044"),
                             "12" = gpar(lwd=1, lty=2, columns=c(1:3,5), col = "#000044")),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(FALSE,FALSE,rep(FALSE,8),FALSE),
           clip=c(0.1,2.5), 
           xlog=TRUE,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", hrz_lines = "#444444"))
