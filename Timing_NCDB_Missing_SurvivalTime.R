read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# AJCC Stage I-II
dat2 <- dats[dats$TNM_PATH_STAGE_GROUP == '1A' | dats$TNM_PATH_STAGE_GROUP == '1' | dats$TNM_PATH_STAGE_GROUP == '1B' | dats$TNM_PATH_STAGE_GROUP == '2' | dats$TNM_PATH_STAGE_GROUP == '2A' | dats$TNM_PATH_STAGE_GROUP == '2B',]
dim(dat2)

# patients who recieved lumpectomy
dat3 <- dat2[dat2$RX_SUMM_SURG_PRIM_SITE > 0 & dat2$RX_SUMM_SURG_PRIM_SITE < 30 ,]
dim(dat3)

# patients with known time elapsed from date of diagnosis to date of surgery and radiation
dat4 <- dat3[!is.na(dat3$DX_RAD_STARTED_DAYS) & !is.na(dat3$DX_SURG_STARTED_DAYS),]
dim(dat4)

# patients who had radiation after surgery
dat5 <- dat4[dat4$RX_SUMM_SURGRAD_SEQ == 3,]
dim(dat5)

# patients who did not have chemotherapy 
dat6 <- dat5[dat5$RX_SUMM_CHEMO == 0 | dat5$RX_SUMM_CHEMO >81 & dat5$RX_SUMM_CHEMO != 99,]
dim(dat6)


# Exclude patients with previous or subsequent cancer history
dat7 <- dat6[dat6$SEQUENCE == 0,]
dim(dat7)

# exclude patients miscoded as M1 and N2 
dat8 <- dat7[!dat7$TNM_PATH_M == 1 & !dat7$TNM_PATH_N == 2& !dat7$TNM_PATH_N == '2A' & !dat7$TNM_PATH_N == '2B' & !dat7$TNM_PATH_N == 3 & !dat7$TNM_PATH_N == '3A' & !dat7$TNM_PATH_N == '3B' & !dat7$TNM_PATH_N == '3C',]
dim(dat8)

dat <- dat8
write.csv(dat,'H:/Gerber/Step9.csv')


# patients with follow up information
dat9 <- dat8[!is.na(dat8$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(dat8$PUF_VITAL_STATUS),]
dim(dat9)


#########################################
# load libraries 
library(tableone)



#########################################

# Bin patients into those missing and those not missing survival data
Complete <- dat8[!is.na(dat8$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(dat8$PUF_VITAL_STATUS),]
Complete$P_RT = "Complete Survival Data"

Missing <- dat8[is.na(dat8$DX_LASTCONTACT_DEATH_MONTHS) | is.na(dat8$PUF_VITAL_STATUS),]
Missing$P_RT = "Missing Survival Data"


############### Create Comparison Table and Descriptive Statistics ##########
TIME <- rbind(Complete,Missing)


#########################################
# recode age, race, ER status, and Charlson Deyo Score

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

# List all variables and categorical variables
listVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","CROWFLY","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','DX_RAD_STARTED_DAYS','DX_DEFSURG_STARTED_DAYS'))

catVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2')) 
table1 <- CreateTableOne(vars = listVars, data = TIME, factorVars = catVars, strata = 'P_RT')

write.csv(print(table1),'H:/Gerber/NCDBBreastRTTiming_MissingData.csv')

summary(Complete$DX_DEFSURG_STARTED_DAYS)
summary(Missing$DX_DEFSURG_STARTED_DAYS)
summary(Complete$DX_RAD_STARTED_DAYS)
summary(Missing$DX_RAD_STARTED_DAYS)

summary(Complete$CROWFLY)
summary(Missing$CROWFLY)


 