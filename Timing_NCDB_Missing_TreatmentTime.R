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

write.csv(dat,'H:/Gerber/Step4.csv')

dat <- dat3


#########################################
# install dependencies
library(tableone)

########################################
# Bin patients into those missing and those not missing treatment time data
Complete <- dat3[!is.na(dat3$DX_RAD_STARTED_DAYS) & !is.na(dat3$DX_SURG_STARTED_DAYS),]
Complete$P_RT = "Complete Timing Data"

Missing <- dat3[is.na(dat3$DX_RAD_STARTED_DAYS) | is.na(dat3$DX_SURG_STARTED_DAYS),]
Missing$P_RT = "Missing Timing Data"

############### Create the tableone ##########
TIME <- rbind(Complete,Missing)


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

# descriptive statistics
listVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","CROWFLY","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','DX_RAD_STARTED_DAYS','DX_DEFSURG_STARTED_DAYS','DX_LASTCONTACT_DEATH_MONTHS','PUF_VITAL_STATUS'))

catVars<-(c("AGE_2","YEAR_OF_DIAGNOSIS","TNM_PATH_T","TNM_PATH_N","TNM_PATH_STAGE_GROUP",'HISTOLOGY',"RACE_2","GRADE","ERSTATUS","LYMPH_VASCULAR_INVASION","RX_SUMM_SURGICAL_MARGINS","FACILITY_LOCATION_CD","FACILITY_TYPE_CD",'CDCC_TOTAL_2','DX_DEFSURG_STARTED_DAYS','PUF_VITAL_STATUS')) 
table1 <- CreateTableOne(vars = listVars, data = TIME, factorVars = catVars, strata = 'P_RT')


#########################################
write.csv(print(table1),'H:/Gerber/NCDB/BreastRTTiming_Timing_Missing.csv')
 
