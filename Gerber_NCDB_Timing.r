
read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# Women aged >= 18
dat1 <- dats[dats$AGE > 17,]
dim(dat1)

# AJCC Stage I-II
dat2 <- dat1[dat1$TNM_PATH_STAGE_GROUP == '1A' | dat1$TNM_PATH_STAGE_GROUP == '1' | dat1$TNM_PATH_STAGE_GROUP == '1B' | dat1$TNM_PATH_STAGE_GROUP == '2' | dat1$TNM_PATH_STAGE_GROUP == '2A' | dat1$TNM_PATH_STAGE_GROUP == '2B',]
dim(dat2)

# Exclude patients with previous or subsequent cancer history
dat3 <- dat2[dat2$SEQUENCE == 0,]
dim(dat3)

dat4 <- dat3[!dat3$TNM_PATH_M == 1 & !dat3$TNM_PATH_N == 2& !dat3$TNM_PATH_N == '2A' & !dat3$TNM_PATH_N == '2B' & !dat3$TNM_PATH_N == 3 & !dat3$TNM_PATH_N == '3A' & !dat3$TNM_PATH_N == '3B' & !dat3$TNM_PATH_N == '3C',]
dim(dat4)


dat5 <- dat4[dat4$RX_SUMM_SURG_PRIM_SITE > 0 & dat4$RX_SUMM_SURG_PRIM_SITE < 30 ,]
dim(dat5)

dat6 <- dat5[!is.na(dat5$DX_RAD_STARTED_DAYS) & !is.na(dat5$DX_SURG_STARTED_DAYS),]
dim(dat6)

dat7 <- dat6[dat6$DX_SURG_STARTED_DAYS < 181,]
dim(dat7)


dat8 <- dat7[dat7$RX_SUMM_CHEMO == 0 | dat7$RX_SUMM_CHEMO >81 & dat7$RX_SUMM_CHEMO != 99,]
dim(dat8)

dat9 <- dat8[dat8$RX_SUMM_SURGRAD_SEQ == 3,]
dim(dat9)


dat10 <- dat9[!is.na(dat9$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(dat9$PUF_VITAL_STATUS),]
dim(dat10)

#dat11 <- dat10[!is.na(dat10$DX_HORMONE_STARTED_DAYS),]
#dim(dat11)


#dat12 <- dat11[dat11$DX_HORMONE_STARTED_DAYS > dat11$DX_RAD_STARTED_DAYS,]
#dim(dat12)

dat <- dat10

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
# split data into comparative groups
# neoadjuvant vs. no induction chemotherapy mastectomy
R_Date <- dat$DX_RAD_STARTED_DAYS
S_Date <- dat$DX_SURG_STARTED_DAYS
H_Date <- dat$DX_HORMONE_STARTED_DAYS
R_Leng <- dat$RAD_ELAPSED_RX_DAYS
RS_Seq <- dat$RX_SUMM_SURGRAD_SEQ


# Bin patients into different time P_RTs
#<50 days
L_50 <- dat[(R_Date < S_Date + 63) & (R_Date >= S_Date) , ]
L_50 <-subset(L_50, (!is.na(L_50[,1])))
L_50$P_RT = "<9 weeks"

# 50-100
L_100_G_50 <- dat[(R_Date >= S_Date + 63) &  (R_Date < S_Date + 126), ]
L_100_G_50 <-subset(L_100_G_50, (!is.na(L_100_G_50[,1])))
L_100_G_50$P_RT = "9-18 weeks"

#>200 days
G_200 <- dat[(R_Date > S_Date + 126) , ]
G_200 <-subset(G_200, (!is.na(G_200[,1])))
G_200$P_RT = ">18 weeks"



############### Create the tableone ##########
TIME <- rbind(L_50,L_100_G_50)
TIME <- rbind(TIME,G_200)

TIME$AGE_2=NULL
TIME[TIME$AGE <50 ,'AGE_2'] = "<50"
TIME[TIME$AGE <65 & TIME$AGE >=50,'AGE_2'] = "50-65"
TIME[TIME$AGE >=65,'AGE_2'] = "> or = 65"

TIME$RACE_2 = 'White'
TIME[TIME$RACE >1 ,'RACE_2'] = "Other"

TIME$ERSTATUS = 'Unknown'
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

TIME$CDCC_TOTAL_2 = NULL
TIME[TIME$CDCC_TOTAL ==0 ,'CDCC_TOTAL_2'] = "0"
TIME[TIME$CDCC_TOTAL ==1 ,'CDCC_TOTAL_2'] = "1"
TIME[TIME$CDCC_TOTAL ==2 ,'CDCC_TOTAL_2'] = "> or = 2"

TIME$Education_2 = 'Not Available'
TIME[TIME$NO_HSD_QUAR_12 ==1 ,'Education_2'] = "> or = 21%"
TIME[TIME$NO_HSD_QUAR_12 ==2 ,'Education_2'] = "13 - 20.9%"
TIME[TIME$NO_HSD_QUAR_12 ==3 ,'Education_2'] = "7 - 12.9%"
TIME[TIME$NO_HSD_QUAR_12 ==4 ,'Education_2'] = "<7%"

TIME$MED_INC_QUAR_12 = 'Not Available'
TIME[TIME$MED_INC_QUAR_12 ==1 ,'Income_2'] = "<$38K"
TIME[TIME$MED_INC_QUAR_12 ==2 ,'Income_2'] = "$38 - $48K"
TIME[TIME$MED_INC_QUAR_12 ==3 ,'Income_2'] = "$48 - 63K"
TIME[TIME$MED_INC_QUAR_12 ==4 ,'Income_2'] = ">63K"

TIME$INSURANCE_STATUS = 'Unknown'
TIME[TIME$INSURANCE_STATUS ==1 ,'INSURANCE_2'] = "Private Insurance"
TIME[TIME$INSURANCE_STATUS ==2 ,'INSURANCE_2'] = "Medicaid"
TIME[TIME$INSURANCE_STATUS ==3 ,'INSURANCE_2'] = "Medicare"
TIME[TIME$INSURANCE_STATUS ==4 ,'INSURANCE_2'] = "Other Govt"
TIME[TIME$INSURANCE_STATUS ==0 ,'INSURANCE_2'] = "Not Insured"


# descriptive statistics
listVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","CROWFLY","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','Education_2','INSURANCE_2','Income_2'))

catVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','Education_2','INSURANCE_2','Income_2')) 
table1 <- CreateTableOne(vars = listVars, data = TIME, factorVars = catVars, strata = 'P_RT')

write.csv(print(table1),'H:\\Gerber\\NCDB\\BreastRTTiming_TableOne.csv')
 

############### Create the plot of HR #############
# Establish Cutoff
cutoff_l = c(7,14,21,28,35,42,49,56,63,70,77,84,112,140,168,196,224,252)
results = NULL

for (i in 1:18){

cutoff = cutoff_l[i]
EARLY <- dat[(R_Date < S_Date + cutoff) & (R_Date >= S_Date) , ]
EARLY <-subset(EARLY, (!is.na(EARLY[,1])))

LATE <- dat[(R_Date > S_Date + cutoff) , ]
LATE <-subset(LATE, (!is.na(LATE[,1])))


# final coding of the treatment groups
EARLY$P_RT <- 'Early'
LATE$P_RT <- 'Late'

# final dataframeTDX
TIME <- rbind(EARLY,LATE)
TIME$AGE_2=NULL
TIME[TIME$AGE <50 ,'AGE_2'] = "<50"
TIME[TIME$AGE <65 & TIME$AGE >=50,'AGE_2'] = "50-65"
TIME[TIME$AGE >=65,'AGE_2'] = "> or = 65"
#########################################
# survival analysis for entire cohort and by treatment
attach(TIME)

#find the follow up and censor time
followup_time = TIME["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = TIME["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of All T1-2N0-1M0  Breast Cancer \n Who Did not get Chemotherapy \n Stratified by Treatment Time)")

x11()


# stratify by some variable
subset <- survfit(Surv(followup_time, censor)~P_RT)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival by Timing of RT',lwd=.5,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("<9weeks",">18 weeks",">9-18 weeks"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)

# Cox Model

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ as.factor(P_RT) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)))

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(P_RT)[2]))
}

results2 = results[1:18,]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 
detach(TIME)


########################## Create the forest plot ################

TIME <- rbind(L_50,L_100_G_50)
TIME <- rbind(TIME,G_200)
TIME$AGE_2=NULL
TIME[TIME$AGE <50 ,'AGE_2'] = "<50"
TIME[TIME$AGE <65 & TIME$AGE >=50,'AGE_2'] = "50-65"
TIME[TIME$AGE >=65,'AGE_2'] = "> or = 65"


TIME$RACE_2 = 'White'
TIME[TIME$RACE >1 ,'RACE_2'] = "Other"
TIME$ERSTATUS = 'Unknown'
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Borderline"
TIME[TIME$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"


attach(TIME)

#find the follow up and censor time
followup_time = TIME["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = TIME["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

x11()

# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ as.factor(P_RT) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(RX_SUMM_SURGICAL_MARGINS) + CROWFLY + as.factor(FACILITY_LOCATION_CD) + as.factor(FACILITY_TYPE_CD))




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
