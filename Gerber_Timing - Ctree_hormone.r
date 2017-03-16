
read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# include patients who had documented lumpectomy 
dats1 <- dats[dats$RX_SUMM_SURG_PRIM_SITE < 30 & dats$YEAR_OF_DIAGNOSIS < 2012, ] 
dim(dats1)

# Exclude patients with previous cancer history
dats2 <- dats1[dats1$SEQUENCE == 1 | 
     dats1$SEQUENCE == 0 
,]
dim(dats2)


# AJCC TNM T1-T2, N0-N2
dat3T <- dats2[
	dats2$TNM_PATH_T == '1'  | 
	dats2$TNM_PATH_T == '1A' |
	dats2$TNM_PATH_T == '1B' |
	dats2$TNM_PATH_T == '1C' |
	dats2$TNM_PATH_T == '1MI'|
	dats2$TNM_PATH_T == '2' 
	,]

dim(dat3T)
	
dat3N <- dat3T[
	dat3T$TNM_PATH_N == '0'   | 
	dat3T$TNM_PATH_N == '0M+' | 
	dat3T$TNM_PATH_N == '0M-' | 
	dat3T$TNM_PATH_N == '0I+' | 
	dat3T$TNM_PATH_N == '0I-' | 
	dat3T$TNM_PATH_N == '1'   |
	dat3T$TNM_PATH_N == '1A'  |
	dat3T$TNM_PATH_N == '1B'  |
	dat3T$TNM_PATH_N == '1C'  |
	dat3T$TNM_PATH_N == '1MI' |
	dat3T$TNM_PATH_N == '2'  ,]
dim(dat3N)

dat3 <- dat3N[
	dat3N$TNM_PATH_M != '1' ,]
	
dim(dat3)

# # Include patients with documented  (Table 1)
# Start of Radiation after Start day of Surgery and with known timing
dat5A <- dat3[ 
	(dat3$DX_RAD_STARTED_DAYS > dat3$DX_SURG_STARTED_DAYS)
	,]
dim(dat5A)

dat5 <- dat5A[ 
	(!is.na(dat5A$DX_RAD_STARTED_DAYS) & !is.na(dat5A$DX_SURG_STARTED_DAYS))
	,]
dim(dat5)


# # Include patients with documented  (Table 1)
# No Chemotherapy Therapy (No systemic Delivered) 
dat6 <- dat5[ (dat5$RX_SUMM_CHEMO == 0 | dat5$RX_SUMM_CHEMO >= 82 & dat5$RX_SUMM_CHEMO <= 87),]
dim(dat6)

NODAL <- dat6

NODAL <- NODAL[!is.na(NODAL$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(NODAL$PUF_VITAL_STATUS),]
dim(NODAL)

 # install.packages("party")
 # install.packages("ReporteRs")
 # install.packages("magrittr")
 install.packages("forestmodel")
 install.packages("tableone")
 install.packages("survival")
 # FOREST PLOT
library("forestmodel")
library(tableone)
library(party)
library(survival)
#library(ReporteRs)
#library(magrittr)

########################################

#########################################

############### Create the tableone ##########
# split data into nodal groups
# T1-T2N0 
# Vs.
# T1-T2N1mi
# Vs.
# T1-T2, N1 
# Vs. 
# T1-T2N2 
# recode variables as needed


NODAL$NODE_CAT="Unknown SURVIVAL DATA"
NODAL[(!is.na(NODAL$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(NODAL$PUF_VITAL_STATUS)),'NODE_CAT'] = 'FULL SURVIVAL DATA'  

# T-stage
NODAL$TNM_PATH_T_Recode  = NODAL$TNM_PATH_T
NODAL[
NODAL$TNM_PATH_T ==1    |
NODAL$TNM_PATH_T =='1A' |
NODAL$TNM_PATH_T =='1B' |
NODAL$TNM_PATH_T =='1C' |
NODAL$TNM_PATH_T =='1MI'
,'TNM_PATH_T_Recode'] = 1

# N-stage
NODAL$TNM_PATH_N_Recode  = NODAL$TNM_PATH_N
NODAL[
NODAL$TNM_PATH_N ==1    |
NODAL$TNM_PATH_N =='1A' |
NODAL$TNM_PATH_N =='1B' |
NODAL$TNM_PATH_N =='1C' |
NODAL$TNM_PATH_N =='1MI'
,'TNM_PATH_N_Recode'] = 1

NODAL[
NODAL$TNM_PATH_N ==0    |
NODAL$TNM_PATH_N =='0M+' |
NODAL$TNM_PATH_N =='0M-' |
NODAL$TNM_PATH_N =='0I+' |
NODAL$TNM_PATH_N =='0I-'
,'TNM_PATH_N_Recode'] = 0

# M-stage
NODAL$TNM_PATH_M_Recode  = NODAL$TNM_PATH_M
NODAL[
NODAL$TNM_PATH_M =='X'    |
NODAL$TNM_PATH_M ==' '    |
NODAL$TNM_PATH_M ==88 
,'TNM_PATH_M_Recode'] = 'X'

# Age
NODAL$AGE_Recode=NULL
NODAL[NODAL$AGE <50 ,'AGE_Recode']                 = "1. <50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_Recode'] = "2. 50-65"
NODAL[NODAL$AGE >=65,'AGE_Recode']                 = "3. > or = 65"

# recode radiation sequencing
NODAL$RX_SUMM_SURGRAD_SEQ_Recode = NULL
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==0 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "No RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==2 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Neoadjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==3 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Adjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==4 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Before and After Surgery"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==5 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-Operative"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==6 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-operative and additional RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==9 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Unknown"

# Race
NODAL$RACE_Recode = 'White'
NODAL[NODAL$RACE >1 ,'RACE_Recode'] = "Other"

# ER Status
NODAL$ERSTATUS = '3. Unknown'
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "1. Positive"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "3. Unknown"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "2. Negative"

# YEAR OF DIAGNOSIS
NODAL$YEAR_OF_DIAGNOSIS_Recode = "2012+"
NODAL[NODAL$YEAR_OF_DIAGNOSIS <2012 ,'YEAR_OF_DIAGNOSIS_Recode'] = "Before 2012"


# Grade
NODAL$GRADE_Recode = NULL
NODAL[NODAL$GRADE ==1 ,'GRADE_Recode'] = "1. Well Differentiated"
NODAL[NODAL$GRADE ==2 ,'GRADE_Recode'] = "2. Moderately Differentiated"
NODAL[NODAL$GRADE ==3 ,'GRADE_Recode'] = "3. Poorly Differentiated"
NODAL[NODAL$GRADE ==4 ,'GRADE_Recode'] = "3. Poorly Differentiated"
NODAL[NODAL$GRADE ==9 ,'GRADE_Recode'] = "4. Unknown"

# LVI
NODAL$LYMPH_VASCULAR_INVASION_Recode = NULL
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==0 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Absent"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==1 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Present"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==8 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==9 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"
NODAL[is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"


# Charlson-Deyo
NODAL$CDCC_TOTAL_Recode = NULL
NODAL[NODAL$CDCC_TOTAL ==0 ,'CDCC_TOTAL_Recode'] = "0"
NODAL[NODAL$CDCC_TOTAL ==1 ,'CDCC_TOTAL_Recode'] = "1"
NODAL[NODAL$CDCC_TOTAL ==2 ,'CDCC_TOTAL_Recode'] = "2+"


# RX SUMMARY CHEMO
NODAL$RX_SUMM_CHEMO_Recode = NULL
NODAL[NODAL$RX_SUMM_CHEMO ==0 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==1 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==2 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==3 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==82 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==85 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==86 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==87 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==88 ,'RX_SUMM_CHEMO_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_CHEMO ==99 ,'RX_SUMM_CHEMO_Recode'] = "Unknown"


# RX SUMM SYSTEMIC SUR SEQ 
NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ_Recode = NULL
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==0 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "No systemic therapy"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==2 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==3 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Adjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==4 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==5 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==6 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==9 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
NODAL[is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"

# RX_SUMM_HORMONE
NODAL$RX_SUMM_HORMONE_Recode = NULL
NODAL[NODAL$RX_SUMM_HORMONE ==0 ,'RX_SUMM_HORMONE_Recode'] = "None"
NODAL[NODAL$RX_SUMM_HORMONE ==1 ,'RX_SUMM_HORMONE_Recode'] = "Hormonal Therapy"
NODAL[NODAL$RX_SUMM_HORMONE ==82 ,'RX_SUMM_HORMONE_Recode'] = "None"
NODAL[NODAL$RX_SUMM_HORMONE ==85 ,'RX_SUMM_HORMONE_Recode'] = "None"
NODAL[NODAL$RX_SUMM_HORMONE ==86 ,'RX_SUMM_HORMONE_Recode'] = "None"
NODAL[NODAL$RX_SUMM_HORMONE ==87 ,'RX_SUMM_HORMONE_Recode'] = "None"
NODAL[NODAL$RX_SUMM_HORMONE ==88 ,'RX_SUMM_HORMONE_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_HORMONE ==99 ,'RX_SUMM_HORMONE_Recode'] = "Unknown"


# RX_SUMM_SURGICAL_MARGINS
NODAL$RX_SUMM_SURGICAL_MARGINS_Recode = NULL
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==0 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Negative"
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==1 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==2 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==3 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==7 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==9 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"
NODAL[is.na(NODAL$RX_SUMM_SURGICAL_MARGINS),'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"


#recode facility
# FACILITY_TYPE_CD_Recode
NODAL$FACILITY_TYPE_CD_Recode = NULL
NODAL[NODAL$FACILITY_TYPE_CD ==1 & !is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Community Cancer Program"
NODAL[NODAL$FACILITY_TYPE_CD ==2 & !is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Comprehensive Community Cancer Program"
NODAL[NODAL$FACILITY_TYPE_CD ==3 & !is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Academic/Research Program"
NODAL[NODAL$FACILITY_TYPE_CD ==4 & !is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Integrated Network Cancer Program"
NODAL[NODAL$FACILITY_TYPE_CD ==9 & !is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Other or unknown"
NODAL[is.na(NODAL$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Unknown"


#recode facility type
# FACILITY_LOCATION_CD_Recode
NODAL$FACILITY_LOCATION_CD_Recode = NULL
NODAL[NODAL$FACILITY_LOCATION_CD ==1 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "New England"
NODAL[NODAL$FACILITY_LOCATION_CD ==2 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Middle Atlantic"
NODAL[NODAL$FACILITY_LOCATION_CD ==3 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "South Atlantic"
NODAL[NODAL$FACILITY_LOCATION_CD ==4 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "East North Central"
NODAL[NODAL$FACILITY_LOCATION_CD ==5 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "East South Central"
NODAL[NODAL$FACILITY_LOCATION_CD ==6 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "West North Central"
NODAL[NODAL$FACILITY_LOCATION_CD ==7 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "West South Central"
NODAL[NODAL$FACILITY_LOCATION_CD ==8 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Mountain"
NODAL[NODAL$FACILITY_LOCATION_CD ==9 & !is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Pacific"
NODAL[is.na(NODAL$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Unknown"


#recode histology
# HISTOLOGY
# A count of all histologies
 # 8000   8001   8004   8005   8010   8012   8013   8020   8021   8022   8031 
   # 233      8      4      2   1712      5      4      7     13    111      1 
  # 8032   8033   8035   8041   8045   8046   8050   8052   8070   8071   8072 
    # 76     52      9     27      1      2    889      4    110     12      1 
  # 8074   8075   8076   8082   8123   8140   8141   8147   8154   8190   8200 
    # 10      1      1      4      6   1075     16      2      3      1    260 
  # 8201   8211   8230   8231   8240   8243   8246   8247   8249   8251   8255 
   # 703   1747    169      2      2      1     58      1      3      2    219 
  # 8260   8290   8310   8315   8323   8341   8343   8401   8410   8430   8453 
    # 65      1     11     15     37      5     14    668      2      3      5 
  # 8460   8480   8481   8490   8500   8501   8502   8503   8504   8507   8508 
     # 6   5975     75     59 282047    538     47    685    163    956      3 
  # 8510   8512   8513   8514   8520   8521   8522   8523   8524   8525   8530 
   # 994      8     83      1  37322    602  24755  12853   1681      6    155 
  # 8540   8541   8543   8550   8560   8562   8570   8571   8572   8573   8574 
    # 59   1130    121     19     78      2     34     11     36     29     30 
  # 8575   8980   9020 
  # 1431     87    189 
  
  # ICD-O-3 groupings are here: http://codes.iarc.fr/codegroup/1
  #
NODAL$HISTOLOGY_Recode = 'Other'
NODAL[NODAL$HISTOLOGY >= 8500 &  NODAL$HISTOLOGY <= 8500 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Ductal"
NODAL[NODAL$HISTOLOGY >= 8520 &  NODAL$HISTOLOGY <= 8520 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Lobular"
NODAL[NODAL$HISTOLOGY >= 8522 &  NODAL$HISTOLOGY <= 8523 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Mixed Ductal and Lobular"

# timing
NODAL$R_Date <- NODAL$DX_RAD_STARTED_DAYS
NODAL$S_Date <- NODAL$DX_SURG_STARTED_DAYS
NODAL$H_Date <- NODAL$DX_HORMONE_STARTED_DAYS
NODAL$R_Leng <- NODAL$RAD_ELAPSED_RX_DAYS
NODAL$RS_Seq <- NODAL$RX_SUMM_SURGRAD_SEQ

NODAL$followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
NODAL$censor = 1-censor 


# interval from RT to surgery
# if >0, then RT after surgery
NODAL$interval = NODAL$R_Date - NODAL$S_Date 

# Interval from surgery to hormones
# if >0, then hormones after surgery
NODAL$interval_S_H = NODAL$H_Date - NODAL$S_Date 

# Interval from Hormones to RT 
# if >0, then hormones before RT
NODAL$interval_H_RT = NODAL$R_Date - NODAL$H_Date

NODAL$HORM_RT_SEQ = NULL
NODAL[NODAL$interval_H_RT > 0 & !is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "1. Hormones Before RT"
NODAL[NODAL$interval_H_RT <= 0 & !is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "2. Hormones During or After RT"
NODAL[is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "3. Unknown"
NODAL[NODAL$RX_SUMM_HORMONE_Recode == 'None' & !is.na(NODAL$RX_SUMM_HORMONE_Recode),'HORM_RT_SEQ'] = "4. No Hormones"

# Stratify into those who start hormones before RT vs. during AND after RT

# CROWFLY
NODAL$CROWFLY_Recode = '1. <10'
NODAL[NODAL$CROWFLY >= 10 &  NODAL$CROWFLY <= 20 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "2. 10-20"
NODAL[NODAL$CROWFLY >= 20 &  NODAL$CROWFLY <= 50 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "3. 20-50"
NODAL[NODAL$CROWFLY >= 50 &  NODAL$CROWFLY <= 100 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "4. 50-100"
NODAL[NODAL$CROWFLY >= 100 !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "5. >100"



# period until RT
NODAL$P_RT = NULL
NODAL[(NODAL$interval >=0 & NODAL$interval <42 & !is.na(NODAL$interval)),'P_RT'] = "1. <6 weeks"
NODAL[(NODAL$interval >=42 & NODAL$interval <84  & !is.na(NODAL$interval)),'P_RT'] = "2. 6-12 weeks"
NODAL[(NODAL$interval >=84 & NODAL$interval <126 & !is.na(NODAL$interval) ),'P_RT'] = "3. 12-18 weeks"
NODAL[(NODAL$interval >=126  & !is.na(NODAL$interval)) ,'P_RT'] = "4. >18 weeks"

# CROWFLYO
NODAL$CROWFLY_Recode = '1. <10'
NODAL[NODAL$CROWFLY >= 10 &  NODAL$CROWFLY <= 20 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "2. 10-20"
NODAL[NODAL$CROWFLY >= 20 &  NODAL$CROWFLY <= 50 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "3. 20-50"
NODAL[NODAL$CROWFLY >= 50 &  NODAL$CROWFLY <= 100 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "4. 50-100"
NODAL[NODAL$CROWFLY >= 100 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "5. >100"

# descriptive statistics
listVars<-(c('P_RT', "AGE_Recode","RACE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","TNM_PATH_M_Recode","ERSTATUS","GRADE_Recode","HISTOLOGY_Recode","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode','HORM_RT_SEQ'))

catVars<-(c("P_RT","AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","TNM_PATH_M_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'HORM_RT_SEQ',includeNA=TRUE)
 write.csv(print(table1),'H:/Gerber/NCDB/Timing_TableOne_2_4_2017_Timing_Hormone.csv')

 hist(NODAL$interval,1000,xlim=c(-100,300),main='time from surgery to RT')
 
 hist(NODAL$interval_S_H,1000,xlim=c(-100,300),main='time from surgery to Hormones')
 
 hist(NODAL$interval_H_RT,1000,xlim=c(-100,300),main='time from hormones to RT')
 
 
# remove neoadjuvant hormone therapy 
NODAL <- NODAL[(NODAL$interval_S_H > 0),]
dim(NODAL)



 
##################### Univariate Survival ###############
# define the group as patients who recieved hormones before RT (but after surgery)
NODAL_S_H_RT <- NODAL[NODAL$HORM_RT_SEQ == '1. Hormones Before RT',]
NODAL_S_RT_H <- NODAL[NODAL$HORM_RT_SEQ == '2. Hormones During or After RT',]
  
 hist(NODAL_S_H_RT$interval_S_H,1000,xlim=c(-100,300),main='time from surgery to Hormones')
 hist(NODAL_S_H_RT$interval,1000,xlim=c(-100,300),main='time from surgery to RT')


 
UNI_Results <- NULL

par(mfrow = c(15,1))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_N_Recode)),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_LOCATION_CD_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ CROWFLY,data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ P_RT,data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_S_H,data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_H_RT,data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ interval,data=NODAL_S_H_RT)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))




UNI_Results

#################### Multivariate Survival ###############



coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode)+
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_HORMONE_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_LOCATION_CD_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ as.factor(P_RT)
+ as.factor(HORM_RT_SEQ)
+ as.factor(CROWFLY_Recode),data=NODAL)

forest_model(coxmodel)

################## Univariate ANOVA ##################

UNI_Results_2 <- NULL

glmmodel <- glm( interval ~ CROWFLY, family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(AGE_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(RACE_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(droplevels(TNM_PATH_T_Recode)), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(droplevels(TNM_PATH_N_Recode)), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(ERSTATUS), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(GRADE_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(HISTOLOGY_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(LYMPH_VASCULAR_INVASION_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(CDCC_TOTAL_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(RX_SUMM_HORMONE_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(FACILITY_LOCATION_CD_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ as.factor(FACILITY_TYPE_CD_Recode), family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

glmmodel <- glm( interval~ CROWFLY, family='gaussian',data=NODAL)
UNI_Results_2 = rbind(UNI_Results_2,cbind(glmmodel$coefficients,confint(glmmodel),coef(summary(glmmodel))[,4]))

UNI_Results_2
################## Multivariate ANOVA ##################
 
glmmodel <- glm(interval ~ as.factor(AGE_Recode)+
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_HORMONE_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_LOCATION_CD_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL)

forest_model(glmmodel)
######## timing HR diagram ##########
 
############### Create the plot of HR #############

NODAL2 <- NODAL[NODAL$RX_SUMM_HORMONE_Recode == "None" ,] 

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

for (i in 1:length(cutoff_l)){

cutoff = cutoff_l[i]
NODAL2$P_RT2 = NULL
NODAL2[NODAL2$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL2[NODAL2$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_LOCATION_CD_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL2)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL2$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved hormonal therapy with no hormones "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 


############ ctree ##################




# univariate

#find the follow up and censor time

library(party)
# first implement the cutoffs
NODAL_ct <- ctree(Surv(followup_time, censor) ~ interval,data=NODAL)
plot(NODAL_ct,type='simple')
NODAL_ct@tree$psplit

# randomly sample 50% of the rows

splits = NULL

for (i in 1:100) {
NODAL_R <- NODAL[sample(nrow(NODAL), round(dim(NODAL)[1]/2)), ]
NODAL_ct_R <- ctree(Surv(followup_time, censor) ~ interval,data=NODAL_R)
splits = rbind(splits,NODAL_ct_R@tree$psplit)
print(i)
}

save(splits,file='H:\\Gerber\\NCDB\\splits_100.R')
load('H:\\Gerber\\NCDB\\splits_100.R')

splits2 = NULL
for (i in 1:dim(splits)[1]) {
splits2 = rbind(splits[i,]$splitpoint,splits2)
print(i)
}

hist(splits2, main='histogram of cutoffs chosen by inference tree \n N = 23 Iterations with 50% Sampling, ',20)



# NODAL_cf <- cforest(Surv(followup_time, censor) ~ interval,data=NODAL)

# treeresponse(NODAL_cf, OOB = TRUE)

# ### if you can't resist to look at individual trees ...
# party:::prettytree(bst@ensemble[[1]], names(bst@data@get("input")))

#plot(NODAL_ct,type='simple')
#plot(NODAL_ct)

install.packages('rpart')

# NODAL_rpart<-rpart(Surv(followup_time, censor) ~ interval+interval, 
   # method="exp", data=NODAL,control=rpart.control(minsplit=1,minsbucket=1, cp=0.001))
   
# printcp(NODAL_rpart) # display the results 
# plotcp(NODAL_rpart) # visualize cross-validation results 

# par(mfrow=c(1,2)) # two plots on one page 
# rsq.rpart(NODAL_rpart) # visualize cross-validation results  	

# plot(NODAL_rpart, 
  	# main="RPA for interval ")
# text(NODAL_rpart, use.n=TRUE, all=TRUE, cex=.8)

# summary(fit) # detailed summary of splits

# library(rpart)
###### COX MODEL #########
attach(NODAL)
coxmodel <- coxph(Surv(NODAL$followup_time, NODAL$censor)~ as.factor(AGE_Recode)+as.factor(CDCC_TOTAL)+ as.factor(RX_SUMM_RADIATION) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(RACE_Recode) + as.factor(GRADE_Recode) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION_Recode) + as.factor(RX_SUMM_SURGICAL_MARGINS_Recode) + CROWFLY + as.factor(FACILITY_LOCATION_CD_Recode) + as.factor(FACILITY_TYPE_CD_Recode)+as.factor(P_RT))




