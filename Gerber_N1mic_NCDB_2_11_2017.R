# -	MVA two ways as per email below
# -	Taking out any systemic therapy from all tables and analysis
# -	Taking out # of nodes and ALND vs SLN from UVA
# -	Propensity score analysis

# Then-- I would run multivariate model two ways:

# 1) with all significant variables on UVA (minus systemic therapy which we are omitting and likely number of nodes AND SLN V ALND which we'll omit). So this model would include
# Age
# T stage
# ER status
# Grade
# LVI
# Chemo
# Hormone therapy
# PMRT
# Facility location
# Facility type

# 2) A smaller model that includes variables with large HR and that we think make sense to be in the model. I propose:

# Age
# T stage
# ER status
# +/- LVI
# Grade
# Chemo
# PMRT

# (omitting hormone therapy as this overlaps with ER status which we are including in the model. Omitting facility location and type due to only a small subset of these categories being significant and only a small HR. Omitting LVI for small HR.)



read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# Exclude patients with previous cancer history
dats1 <- dats[dats$SEQUENCE == 1 | 
     dats$SEQUENCE == 0 
,]
dim(dats1)

# Include AJCC pT1-T2 patients
dat2 <- dats1[
dats1$TNM_PATH_T == '1'   | 
dats1$TNM_PATH_T == '1A'  | 
dats1$TNM_PATH_T == '1B'  | 
dats1$TNM_PATH_T == '1C'  | 
dats1$TNM_PATH_T == '1MI' | 
dats1$TNM_PATH_T == '2'
,]

dim(dat2)

# Incluyde AJCC pN0-N2 patients
dat2_N <- dat2[
dat2$TNM_PATH_N == 0      | 
dat2$TNM_PATH_N == '0I-'  | 
dat2$TNM_PATH_N == '0I+'  | 
dat2$TNM_PATH_N == '0M-'  | 
dat2$TNM_PATH_N == '0M+'  |
dat2$TNM_PATH_N =='1MI'   |
dat2$TNM_PATH_N == 1      | 
dat2$TNM_PATH_N == '1A'   | 
dat2$TNM_PATH_N == '1B'   | 
dat2$TNM_PATH_N == '1C'   |
dat2$TNM_PATH_N == 2      | 
dat2$TNM_PATH_N == '2A'   | 
dat2$TNM_PATH_N == '2B'
, ]

dim(dat2_N)



# exclude M1 disease
dat3 <- dat2_N[  dat2_N$TNM_PATH_M != 1 ,]
dim(dat3)

# include patients who had documented mastectomy 
dat4 <- dat3[dat3$RX_SUMM_SURG_PRIM_SITE >= 30 & 
dat3$RX_SUMM_SURG_PRIM_SITE <= 80 
,]
dim(dat4)

# # Include patients with documented  (Table 1)
# Radiation after Surgery (Post-mastectomy RT) (N = 317,167)
# AND
# No Radiation Therapy (No radiation Delivered) (N = 91,658)
dat5 <- dat4[ dat4$RX_SUMM_SURGRAD_SEQ == 0 | dat4$RX_SUMM_SURGRAD_SEQ == 3  | is.na(dat4$RX_SUMM_SURGRAD_SEQ) | dat4$RX_SUMM_SURGRAD_SEQ == 9,]
dim(dat5)

# # Include patients with documented  (Table 1)
# Chemotherapy after Surgery (Post-mastectomy RT) (N = 317,167)
# AND
# No Chemotherapy Therapy (No systemic Delivered) (N = 91,658)
dat6 <- dat5[ (dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 0 | dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 3 | is.na(dat5$RX_SUMM_SYSTEMIC_SUR_SEQ) | dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 9),]
dim(dat6)

#dat7 <- dat6[(dat6$DX_LASTCONTACT_DEATH_MONTHS < 6 & (dat6$PUF_VITAL_STATUS != 0))|dat6$DX_LASTCONTACT_DEATH_MONTHS >= 6 ,]
#dim(dat7)

NODAL <- dat6

save.image('H:/Gerber/NCDB/2_15_2017.R')
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
library(survminer)
library(MatchIt)

#install.packages('MatchIt')
#install.packages('survminer')
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


NODAL$NODE_CAT=NULL
NODAL[(NODAL$TNM_PATH_N == 0   | 
NODAL$TNM_PATH_N == '0I-'  | 
NODAL$TNM_PATH_N == '0I+'  | 
NODAL$TNM_PATH_N == '0M-'  | 
NODAL$TNM_PATH_N == '0M+'),'NODE_CAT'] = '1. T1-T2 N0'

NODAL[NODAL$TNM_PATH_N =='1MI','NODE_CAT'] = '2. T1-T2 N1mi'

NODAL[(NODAL$TNM_PATH_N == 2    | 
NODAL$TNM_PATH_N == '2A'    | 
NODAL$TNM_PATH_N == '2B'),'NODE_CAT'] = '4. T1-T2 N2'

NODAL[(NODAL$TNM_PATH_N == 1  | 
NODAL$TNM_PATH_N == '1A'  | 
NODAL$TNM_PATH_N == '1B'  | 
NODAL$TNM_PATH_N == '1C'),'NODE_CAT'] = '3. T1-T2 N1'


# T-stage
NODAL$TNM_PATH_T_Recode  = NODAL$TNM_PATH_T
NODAL[
NODAL$TNM_PATH_T ==1    |
NODAL$TNM_PATH_T =='1A' |
NODAL$TNM_PATH_T =='1B' |
NODAL$TNM_PATH_T =='1C' |
NODAL$TNM_PATH_T =='1MI'
,'TNM_PATH_T_Recode'] = 1

# Age
NODAL$AGE_Recode=NULL
NODAL[NODAL$AGE <50 ,'AGE_Recode']                 = "1. <50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_Recode'] = "2. 50-65"
NODAL[NODAL$AGE >=65,'AGE_Recode']                 = "3. > or = 65"



# recode PMRT
NODAL$PMRT = NULL
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==  3 ,'PMRT'] = "PMRT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==  0 ,'PMRT'] = "No PMRT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==  9 | is.na(NODAL$RX_SUMM_SURGRAD_SEQ) ,'PMRT'] = "Unknown"

# recode radiation sequencing
NODAL$RX_SUMM_SURGRAD_SEQ_Recode = NULL
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==0 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "No RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==2 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Neoadjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==3 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Adjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==4 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Before and After Surgery"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==5 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-Operative"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==6 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-operative and additional RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==9 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Unknown"

# Race # DO NOT CHANGE ORDER, It matters for White --> Hispanic
NODAL$RACE_Recode = '5. Other'
NODAL[NODAL$RACE ==1 ,'RACE_Recode'] = "1. White"
NODAL[(NODAL$SPANISH_HISPANIC_ORIGIN >=1 & NODAL$SPANISH_HISPANIC_ORIGIN <=6) | (NODAL$SPANISH_HISPANIC_ORIGIN ==8),'RACE_Recode'] = "3. Hispanic"
NODAL[NODAL$RACE ==2 ,'RACE_Recode'] = "2. Black"

NODAL[NODAL$RACE >=4 & NODAL$RACE <=97,'RACE_Recode'] = "4. East Asian, South Asian, Pacific Islander"

# HISPANICRACE
NODAL$HISPANIC_Recode = NULL
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==1 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==2 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==3 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==4 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==5 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==6 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==7 ,'HISPANIC_Recode'] = "Other"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==8 ,'HISPANIC_Recode'] = "Hispanic"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==9 ,'HISPANIC_Recode'] = "Other"
NODAL[NODAL$SPANISH_HISPANIC_ORIGIN ==0 ,'HISPANIC_Recode'] = "Non-Hispanic"

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
NODAL[NODAL$RX_SUMM_CHEMO ==0 ,'RX_SUMM_CHEMO_Recode'] = "1. No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==1 ,'RX_SUMM_CHEMO_Recode'] = "2. Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==2 ,'RX_SUMM_CHEMO_Recode'] = "2. Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==3 ,'RX_SUMM_CHEMO_Recode'] = "2. Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==82 ,'RX_SUMM_CHEMO_Recode'] = "1. No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==85 ,'RX_SUMM_CHEMO_Recode'] = "1. No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==86 ,'RX_SUMM_CHEMO_Recode'] = "1. No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==87 ,'RX_SUMM_CHEMO_Recode'] = "1. No Chemotherapy"
NODAL[NODAL$RX_SUMM_CHEMO ==88 ,'RX_SUMM_CHEMO_Recode'] = "3. Unknown"
NODAL[NODAL$RX_SUMM_CHEMO ==99 ,'RX_SUMM_CHEMO_Recode'] = "3. Unknown"


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

# SCOPE_REG_LN_2012
NODAL$RX_SUMM_SCOPE_REG_LN_SUR_Recode = NULL
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'RX_SUMM_SCOPE_REG_LN_SUR_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'RX_SUMM_SCOPE_REG_LN_SUR_Recode'] = "ALND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'RX_SUMM_SCOPE_REG_LN_SUR_Recode'] = "Unknown"

# SCOPE_REG_LN_2012
NODAL$SCOPE_REG_LN_2012_Recode = "Unknown"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "Unknown"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==2 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "SLND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==3 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "ALND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==5 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "ALND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==6 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "ALND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==7 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "ALND"
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "ALND"
NODAL[is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'SCOPE_REG_LN_2012_Recode'] = "Unknown"

# SCOPE REGIONAL LN ALL DATES (DO NOT MESS WITH THE ORDER, ORDER MATTERS)
NODAL$LN_SAMPLING_Recode = NULL
NODAL[

(NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012)) |
(NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012)) |
(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR)  ) ,'LN_SAMPLING_Recode'] = "4. Unknown"

NODAL[(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR) ) |
(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR)),'LN_SAMPLING_Recode'] = "4. Unknown"


NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'LN_SAMPLING_Recode'] = "1. SLND and/or ALND Performed"

NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==2 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'LN_SAMPLING_Recode'] = "2. SLND"

NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 >=3 & NODAL$RX_SUMM_SCOPE_REG_LN_2012 <=9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'LN_SAMPLING_Recode'] = "3. ALND"


# regional nodes
NODAL$REGIONAL_NODES_EXAMINED_Recode = NODAL$REGIONAL_NODES_EXAMINED
NODAL[NODAL$REGIONAL_NODES_EXAMINED >= 95 ,'REGIONAL_NODES_EXAMINED_Recode'] = NA

NODAL$REGIONAL_NODES_EXAMINED_Recode_2 = NULL
NODAL[NODAL$REGIONAL_NODES_EXAMINED < 95 ,'REGIONAL_NODES_EXAMINED_Recode_2'] = 'Positive Nodes'

NODAL$REGIONAL_NODES_POSITIVE_Recode = NODAL$REGIONAL_NODES_POSITIVE
NODAL[NODAL$REGIONAL_NODES_POSITIVE >= 95 ,'REGIONAL_NODES_POSITIVE_Recode'] = NA

NODAL$REGIONAL_NODES_POSITIVE_Recode_2 = NULL
NODAL[NODAL$REGIONAL_NODES_EXAMINED < 95 ,'REGIONAL_NODES_POSITIVE_Recode_2'] = 'Positive Nodes'


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

# histology
NODAL$HISTOLOGY_Recode = 'Other'
NODAL[NODAL$HISTOLOGY >= 8500 &  NODAL$HISTOLOGY <= 8500 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Ductal"
NODAL[NODAL$HISTOLOGY >= 8520 &  NODAL$HISTOLOGY <= 8520 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Lobular"
NODAL[NODAL$HISTOLOGY >= 8522 &  NODAL$HISTOLOGY <= 8523 & !is.na(NODAL$HISTOLOGY),'HISTOLOGY_Recode'] = "Mixed Ductal and Lobular"

# CROWFLY
NODAL$CROWFLY_Recode = '1. <10'
NODAL[NODAL$CROWFLY >= 10 &  NODAL$CROWFLY <= 20 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "2. 10-20"
NODAL[NODAL$CROWFLY >= 20 &  NODAL$CROWFLY <= 50 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "3. 20-50"
NODAL[NODAL$CROWFLY >= 50 &  NODAL$CROWFLY <= 100 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "4. 50-100"
NODAL[NODAL$CROWFLY >= 100 & !is.na(NODAL$CROWFLY),'CROWFLY_Recode'] = "5. >100"

# SURVIVAL DATA
NODAL$followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
NODAL$censor = 1-censor 


# Education
NODAL$EDUCATION_Recode = NULL
NODAL[NODAL$NO_HSD_QUAR_12 == 1 & !is.na(NODAL$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "1. >21%"
NODAL[NODAL$NO_HSD_QUAR_12 == 2 & !is.na(NODAL$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "2. 13-21%"
NODAL[NODAL$NO_HSD_QUAR_12 == 3 & !is.na(NODAL$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "3. 7-12.9%"
NODAL[NODAL$NO_HSD_QUAR_12 == 4 & !is.na(NODAL$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "4. <7%"
NODAL[is.na(NODAL$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "Unknown"


# Education2000
NODAL$EDUCATION2000_Recode = NULL
NODAL[NODAL$NO_HSD_QUAR_00 == 1 & !is.na(NODAL$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "1. >21%"
NODAL[NODAL$NO_HSD_QUAR_00 == 2 & !is.na(NODAL$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "2. 13-21%"
NODAL[NODAL$NO_HSD_QUAR_00 == 3 & !is.na(NODAL$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "3. 7-12.9%"
NODAL[NODAL$NO_HSD_QUAR_00 == 4 & !is.na(NODAL$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "4. <7%"
NODAL[is.na(NODAL$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "Unknown"


# Income
NODAL$INCOME_Recode = NULL
NODAL[NODAL$MED_INC_QUAR_12 ==1 & !is.na(NODAL$MED_INC_QUAR_12),'INCOME_Recode'] = "1. <38K"
NODAL[NODAL$MED_INC_QUAR_12 == 2 & !is.na(NODAL$MED_INC_QUAR_12),'INCOME_Recode'] = "2. 38K-48K"
NODAL[NODAL$MED_INC_QUAR_12 == 3 & !is.na(NODAL$MED_INC_QUAR_12),'INCOME_Recode'] = "4. 48K-63K"
NODAL[NODAL$MED_INC_QUAR_12 == 4 & !is.na(NODAL$MED_INC_QUAR_12),'INCOME_Recode'] = "5. >63K"
NODAL[is.na(NODAL$MED_INC_QUAR_12),'INCOME_Recode'] = "Unknown"

# Income2000
NODAL$INCOME2000_Recode = NULL
NODAL[NODAL$MED_INC_QUAR_00 ==1 & !is.na(NODAL$MED_INC_QUAR_00),'INCOME2000_Recode'] = "1. <30K"
NODAL[NODAL$MED_INC_QUAR_00 == 2 & !is.na(NODAL$MED_INC_QUAR_00),'INCOME2000_Recode'] = "2. 30K-35K"
NODAL[NODAL$MED_INC_QUAR_00 == 3 & !is.na(NODAL$MED_INC_QUAR_00),'INCOME2000_Recode'] = "4. 35K-46K"
NODAL[NODAL$MED_INC_QUAR_00 == 4 & !is.na(NODAL$MED_INC_QUAR_00),'INCOME2000_Recode'] = "5. >46K"
NODAL[is.na(NODAL$MED_INC_QUAR_00),'INCOME2000_Recode'] = "Unknown"

# Insurance
NODAL$INSURANCE_STATUS_Recode = NULL
NODAL[NODAL$INSURANCE_STATUS ==1,'INSURANCE_STATUS_Recode'] = "1. Private"
NODAL[NODAL$INSURANCE_STATUS == 2 ,'INSURANCE_STATUS_Recode'] = "3. Medicaid"
NODAL[NODAL$INSURANCE_STATUS == 3 ,'INSURANCE_STATUS_Recode'] = "2. Medicare"
NODAL[NODAL$INSURANCE_STATUS == 4 ,'INSURANCE_STATUS_Recode'] = "5. Other Gvt"
NODAL[NODAL$INSURANCE_STATUS == 0 ,'INSURANCE_STATUS_Recode'] = "4. Uninsured"
NODAL[NODAL$INSURANCE_STATUS == 9 ,'INSURANCE_STATUS_Recode'] = "6. Unknown"


# descriptive statistics
listVars<-(c('PMRT',"AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode","LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


catVars<-(c('PMRT',"AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode","LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'NODE_CAT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_14_2017_Allpatients_Nodes.csv')


# install.packages('survminer')
library(survminer)
ggsurvplot(subset, main='  Overall Survival of All T1-2N0 Supraglottic Larynx \n              Stratified by treatment',xlab='Months',pval = TRUE,
                                                surv.scale=c('percent'),
                                                pval.size = 8,
                                                                font.main = c(24),
                                                                font.legend = c(24),
                                                                font.x = c(24),
                                                                font.y = c(24),
                                                                font.tickslab = c(24),
                                                                risk.table.fontsize = 6,
                                                                break.time.by = 12,
                                                                legend.title='',
                                                                risk.table.title='',
                                                                legend=c('bottom'),
           risk.table = TRUE,
                                  
#           risk.table.col = "strata",
           risk.table.height = 0.3,
           ggtheme = theme_bw(),
           legend.labs = c( "No IMRT","IMRT"))
 
 
 
 
# 3) Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table
########################## Create the new data set ################

# include only patients treated with N1mi and known RT sequencing
N1mi_P <- NODAL[(NODAL$TNM_PATH_N =='1MI') & (NODAL$RX_SUMM_SURGRAD_SEQ != 9) & (!is.na(NODAL$RX_SUMM_SURGRAD_SEQ)),]

# descriptive statistics
listVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))

catVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'PMRT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_PMRTN1Mic.csv')
#  3) Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
  
    # create this comparison table for LN Dissection 
########################## Create the new data set ################

# include only patients treated with RT after Surgery

# descriptive statistics
listVars<-(c("YEAR_OF_DIAGNOSIS_Recode",'REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2'))

catVars<-(c("YEAR_OF_DIAGNOSIS_Recode",'REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode_2'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'LN_SAMPLING_Recode',includeNA=TRUE)

summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='1. SLND and/or ALND Performed','REGIONAL_NODES_EXAMINED_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='2. SLND','REGIONAL_NODES_EXAMINED_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='3. ALND','REGIONAL_NODES_EXAMINED_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='4. Unknown','REGIONAL_NODES_EXAMINED_Recode'])

summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='1. SLND and/or ALND Performed','REGIONAL_NODES_POSITIVE_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='2. SLND','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_POSITIVE_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='3. ALND','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_POSITIVE_Recode'])
summary(N1mi_P[N1mi_P$LN_SAMPLING_Recode=='4. Unknown','REGIONAL_NODES_POSITIVE_Recode'])


# SCOPE REGIONAL LN ALL DATES (DO NOT MESS WITH THE ORDER, ORDER MATTERS)
NODAL$LN_SAMPLING_Recode = NULL
NODAL[

(NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012)) |
(NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012)) |
(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR)  ) ,'LN_SAMPLING_Recode'] = "4. Unknown"

NODAL[(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR) ) |
(NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR)),'LN_SAMPLING_Recode'] = "4. Unknown"


NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==1 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'LN_SAMPLING_Recode'] = "1. SLND and/or ALND Performed"

NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 ==2 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'LN_SAMPLING_Recode'] = "2. SLND"

NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_2012 >=3 & NODAL$RX_SUMM_SCOPE_REG_LN_2012 <=9 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_2012),'LN_SAMPLING_Recode'] = "3. ALND"



write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_1_28_2017_ScopeLN.csv')
 
##################### Univariate Survival ###############
UNI_Results <- NULL



coxmodel <- coxph(Surv(followup_time, censor)~ PMRT,data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LN_SAMPLING_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_CHEMO_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_HORMONE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CROWFLY_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INSURANCE_STATUS_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INCOME_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(EDUCATION_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

UNI_Results<-round(UNI_Results,3)
colnames(UNI_Results) = c('HR','2.5% CI','97.5% CI','p')
# write all univariate and only the significant variables
write.csv(UNI_Results,'H:/Gerber/N1Mi_2_10_2017_UnivariateResults.csv')
write.csv(UNI_Results[UNI_Results[,4]<0.05,],'H:/Gerber/N1Mi_2_10_2017_Significant_UnivariateResults.csv')

# significant values
# PMRT, age, race, charlson, T stage, ER status, Grade, histology, LVI, chemo, hormone, facility, distance, insurance, income, education


#################### Multivariate Survival ###############
#install.packages('forestmodel')
library(forestmodel)

# eliminate income = unknown and education = unknown because cox model does not converge
N1mi_P_Modified


N1mi_P_Modified <- N1mi_P[(N1mi_P$INCOME_Recode !='Unknown') & (N1mi_P$EDUCATION_Recode != 'Unknown') ,]


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT)
+ as.factor(AGE_Recode)
+ as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
+ as.factor(RX_SUMM_HORMONE_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ as.factor(CROWFLY_Recode)
+ as.factor(INSURANCE_STATUS_Recode)
+ as.factor(INCOME_Recode)
+ as.factor(EDUCATION_Recode)
,data=N1mi_P_Modified)

forest_model(coxmodel)


# 1) with all significant variables on UVA (minus systemic therapy which we are omitting and likely number of nodes AND SLN V ALND which we'll omit). So this model would include
# Age
# T stage
# ER status
# Grade
# LVI
# Chemo
# Hormone therapy
# PMRT
# Facility location
# Facility type


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT)
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N1mi_P_Modified)

forest_model(coxmodel)


# 2) A smaller model that includes variables with large HR and that we think make sense to be in the model. I propose:

# Age
# T stage
# ER status
# +/- LVI
# Grade
# Chemo
# PMRT

# (omitting hormone therapy as this overlaps with ER status which we are including in the model. Omitting facility location and type due to only a small subset of these categories being significant and only a small HR. Omitting LVI for small HR.) # i added back T-stage


############# Propensity Score Analysis ##############
N1mi_P2<- N1mi_P[N1mi_P_Modified$PMRT != 'Unknown',]

N1mi_P2$PMRT_Binary = NULL
N1mi_P2[N1mi_P2$RX_SUMM_SURGRAD_SEQ ==  3 ,'PMRT_Binary'] = 1
N1mi_P2[N1mi_P2$RX_SUMM_SURGRAD_SEQ ==  0 ,'PMRT_Binary'] = 0

# N1mi_P2 <- N1mi_P2[!is.na(N1mi_P2$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(N1mi_P2$PUF_VITAL_STATUS),]

# SURVIVAL DATA
N1mi_P2$followup_time = N1mi_P2["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = N1mi_P2["PUF_VITAL_STATUS"][[1]]
N1mi_P2$censor = 1-censor 

# significant values
# PMRT, age, race, charlson, T stage, ER status, Grade, histology, LVI, chemo, hormone, facility, distance, insurance, income, education

#install.packages('MatchIt')
library(MatchIt)
# select variables
myvars<-(c('PMRT_Binary',"AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


matchdata <- N1mi_P2[myvars]

# Create the match based on stage, age, chemo using the nearest neighbor method etc...)
m.out=matchit(PMRT_Binary ~ as.factor(AGE_Recode) +as.factor(RACE_Recode) + as.factor(CDCC_TOTAL_Recode) + as.factor(TNM_PATH_T_Recode) + as.factor(ERSTATUS) + as.factor(GRADE_Recode)+ as.factor(HISTOLOGY_Recode) + as.factor(LYMPH_VASCULAR_INVASION_Recode) + as.factor(LN_SAMPLING_Recode) +as.factor(RX_SUMM_HORMONE_Recode) + as.factor(FACILITY_TYPE_CD_Recode) + as.factor(CROWFLY_Recode) + as.factor(RX_SUMM_CHEMO_Recode)+as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)+as.factor(YEAR_OF_DIAGNOSIS)+as.factor(INSURANCE_STATUS_Recode)+as.factor(INCOME_Recode)+as.factor(EDUCATION_Recode), data = matchdata, method = "nearest")
summary(m.out);

N1mi_P_Matched <- match.data(m.out)

# table 1 #
listVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


catVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


table1 <- CreateTableOne(vars = listVars, data = N1mi_P_Matched, factorVars = catVars, strata = 'PMRT_Binary',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_PMRTMatched.csv')


##################### Univariate Survival ###############
UNI_Results <- NULL

coxmodel <- coxph(Surv(followup_time, censor)~ PMRT_Binary,data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LN_SAMPLING_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_CHEMO_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_HORMONE_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CROWFLY_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INSURANCE_STATUS_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INCOME_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(EDUCATION_Recode),data=N1mi_P_Matched)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

UNI_Results<-round(UNI_Results,3)
colnames(UNI_Results) = c('HR','2.5% CI','97.5% CI','p')
# write all univariate and only the significant variables
write.csv(UNI_Results,file='H:/Gerber/N1Mi_2_10_2017_Matched_UnivariateResults.csv')
write.csv(UNI_Results[UNI_Results[,4]<0.05,],file='H:/Gerber/N1Mi_2_10_2017_Matched_Significant_UnivariateResults.csv')


#################### Multivariate Survival ###############
# age, charlson, t, er, grade, histology, lvi, chemo, hormone, facility, distnace, inusrance, income ,education 

N1mi_P_Matched_Mod <- N1mi_P2[(N1mi_P_Matched$INCOME_Recode !='Unknown') & (N1mi_P_Matched$EDUCATION_Recode != 'Unknown') ,]

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT)
+ as.factor(AGE_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
+ as.factor(RX_SUMM_HORMONE_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ as.factor(CROWFLY_Recode)
+ as.factor(INSURANCE_STATUS_Recode)
+ as.factor(INCOME_Recode)
+ as.factor(EDUCATION_Recode)
,data=N1mi_P_Matched_Mod)

forest_model(coxmodel)


# 1) with all significant variables on UVA (minus systemic therapy which we are omitting and likely number of nodes AND SLN V ALND which we'll omit). So this model would include
# Age
# T stage
# ER status
# Grade
# LVI
# Chemo
# Hormone therapy
# PMRT
# Facility location
# Facility type


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT)
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N1mi_P2_Modified)

forest_model(coxmodel)



############# Younger and ER Negative ##############
N1mi_P3<- N1mi_P_Modified[N1mi_P_Modified$AGE_Recode == '1. <50' & N1mi_P_Modified$ERSTATUS == '2. Negative',]


# table 1 #
listVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


catVars<-(c("AGE_Recode","RACE_Recode",'CDCC_TOTAL_Recode',"TNM_PATH_T_Recode","ERSTATUS","GRADE_Recode",'HISTOLOGY_Recode',"LYMPH_VASCULAR_INVASION_Recode",'LN_SAMPLING_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode',"YEAR_OF_DIAGNOSIS",'INSURANCE_STATUS_Recode','INCOME_Recode','EDUCATION_Recode'))


table1 <- CreateTableOne(vars = listVars, data = N1mi_P3, factorVars = catVars, strata = 'PMRT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_PMRTMatched_Negative_Young.csv')


##################### Univariate Survival ###############
UNI_Results <- NULL

coxmodel <- coxph(Surv(followup_time, censor)~ PMRT,data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LN_SAMPLING_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_CHEMO_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_HORMONE_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CROWFLY_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INSURANCE_STATUS_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INCOME_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(EDUCATION_Recode),data=N1mi_P3)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

UNI_Results<-round(UNI_Results,3)
colnames(UNI_Results) = c('HR','2.5% CI','97.5% CI','p')
# write all univariate and only the significant variables
write.csv(UNI_Results,file='H:/Gerber/N1Mi_2_10_2017_Matched_UnivariateResults_Age50Negative.csv')
write.csv(UNI_Results[UNI_Results[,4]<0.05,],file='H:/Gerber/N1Mi_2_10_2017_Matched_Significant_UnivariateResults_Age50Negative.csv')


#################### Multivariate Survival ###############
# age, charlson, t, er, grade, histology, lvi, chemo, hormone, facility, distnace, inusrance, income ,education 

N1mi_P3 <- N1mi_P3[(N1mi_P3$INCOME_Recode !='Unknown') & (N1mi_P3$EDUCATION_Recode != 'Unknown') & (N1mi_P3$INSURANCE_STATUS_Recode != '6. Unknown') ,]

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(FACILITY_TYPE_CD_Recode)
+ as.factor(INSURANCE_STATUS_Recode)
+ as.factor(INCOME_Recode)
+ as.factor(EDUCATION_Recode)
,data=N1mi_P3)

forest_model(coxmodel)


##########################################
# plot the KM plots with number at risk tables
par(mfrow=c(2,2))
N0<-NODAL[NODAL$NODE_CAT == '1. T1-T2 N0' & NODAL$PMRT!= 'Unknown' ,]
fit1 = survfit(Surv(N0$followup_time,N0$censor)~N0$PMRT)
cox1 = coxph(Surv(N0$followup_time,N0$censor)~N0$PMRT)
cox1 <- coxph(Surv(N0$followup_time,N0$censor)~N0$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N0)


N1mi<-NODAL[NODAL$NODE_CAT == '2. T1-T2 N1mi' & NODAL$PMRT!= 'Unknown' ,]
fit2 = survfit(Surv(N1mi$followup_time,N1mi$censor)~N1mi$PMRT)
cox2 <- coxph(Surv(N1mi$followup_time,N1mi$censor)~N1mi$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N1mi)


N1<-NODAL[NODAL$NODE_CAT == '3. T1-T2 N1' & NODAL$PMRT!= 'Unknown' ,]
fit3 = survfit(Surv(N1$followup_time,N1$censor)~N1$PMRT)
cox3 = coxph(Surv(N1$followup_time,N1$censor)~N1$PMRT)
cox3 <- coxph(Surv(N1$followup_time,N1$censor)~N1$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N1)

N2<-NODAL[NODAL$NODE_CAT == '4. T1-T2 N2' & NODAL$PMRT!= 'Unknown' ,]
fit4 = survfit(Surv(N2$followup_time,N2$censor)~N2$PMRT)
cox4 = coxph(Surv(N2$followup_time,N2$censor)~N2$PMRT)
cox4 <- coxph(Surv(N2$followup_time,N2$censor)~N2$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=N2)

library(survminer)
ggsurvplot(fit4, main='  Overall Survival of T1-T2 N2 \nBreast Cancer Patients in NCDB',xlab='Months',pval = TRUE, 
			surv.scale=c('percent'),
			pval.size = 8,
				font.main = c(36),
				font.legend = c(24),
				font.x = c(24),
				font.y = c(24),
				font.tickslab = c(24),
				risk.table.fontsize = 6,
				break.time.by = 12,
				legend.title='',
				risk.table.title='',
				legend=c('bottom'),
           risk.table = TRUE,
		   
#           risk.table.col = "strata",
           risk.table.height = 0.2, 
           ggtheme = theme_bw(),
           legend.labs = c( "No PMRT","PMRT"))

		   
ggsurvplot(fit1, main='  Overall Survival of T1-T2 N0 \nBreast Cancer Patients in NCDB',xlab='Months',pval = TRUE, 
			surv.scale=c('percent'),
			pval.size = 8,
				font.main = c(36),
				font.legend = c(24),
				font.x = c(24),
				font.y = c(24),
				font.tickslab = c(24),
				risk.table.fontsize = 6,
				break.time.by = 12,
				legend.title='',
				risk.table.title='',
				legend=c('bottom'),
           risk.table = TRUE,
		   
#           risk.table.col = "strata",
           risk.table.height = 0.2, 
           ggtheme = theme_bw(),
           legend.labs = c( "No PMRT","PMRT"))

		   
ggsurvplot(fit2, main='  Overall Survival of T1-T2 N1mi \nBreast Cancer Patients in NCDB',xlab='Months',pval = TRUE, 
			surv.scale=c('percent'),
			pval.size = 8,
				font.main = c(36),
				font.legend = c(24),
				font.x = c(24),
				font.y = c(24),
				font.tickslab = c(24),
				risk.table.fontsize = 6,
				break.time.by = 12,
				legend.title='',
				risk.table.title='',
				legend=c('bottom'),
           risk.table = TRUE,
		   
#           risk.table.col = "strata",
           risk.table.height = 0.2, 
           ggtheme = theme_bw(),
           legend.labs = c( "No PMRT","PMRT"))
		   
ggsurvplot(fit3, main='  Overall Survival of T1-T2 N1 \nBreast Cancer Patients in NCDB',xlab='Months',pval = TRUE, 
			surv.scale=c('percent'),
			pval.size = 8,
				font.main = c(36),
				font.legend = c(24),
				font.x = c(24),
				font.y = c(24),
				font.tickslab = c(24),
				risk.table.fontsize = 6,
				break.time.by = 12,
				legend.title='',
				risk.table.title='',
				legend=c('bottom'),
           risk.table = TRUE,
		   
#           risk.table.col = "strata",
           risk.table.height = 0.2, 
           ggtheme = theme_bw(),
           legend.labs = c( "No PMRT","PMRT"))


# fit 5 YOUNG <50 patients
YOUNG<-N1mi[N1mi$AGE_Recode == '1. <50' & N1mi$PMRT!= 'Unknown' ,]
fit5 = survfit(Surv(YOUNG$followup_time,YOUNG$censor)~YOUNG$PMRT)
cox5u = coxph(Surv(YOUNG$followup_time,YOUNG$censor)~YOUNG$PMRT)
cox5 <- coxph(Surv(YOUNG$followup_time,YOUNG$censor)~YOUNG$PMRT
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=YOUNG)


# fit 6 ER Negative patients
ERNEG<-N1mi[N1mi$ERSTATUS == '2. Negative' & N1mi$PMRT!= 'Unknown' ,]
fit6 = survfit(Surv(ERNEG$followup_time,ERNEG$censor)~ERNEG$PMRT)
cox6u = coxph(Surv(ERNEG$followup_time,ERNEG$censor)~ERNEG$PMRT)
cox6 <- coxph(Surv(ERNEG$followup_time,ERNEG$censor)~ERNEG$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=ERNEG)

# fit 7 LVI
LVI<-N1mi[N1mi$LYMPH_VASCULAR_INVASION_Recode == 'Present' & N1mi$PMRT!= 'Unknown' ,]
fit7 = survfit(Surv(LVI$followup_time,LVI$censor)~LVI$PMRT)
cox7u = coxph(Surv(LVI$followup_time,LVI$censor)~LVI$PMRT)
cox7 <- coxph(Surv(LVI$followup_time,LVI$censor)~LVI$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=LVI)

# fit 8 T-Stage
TSTAGE<-N1mi[N1mi$TNM_PATH_T_Recode == '2' & N1mi$PMRT!= 'Unknown' ,]
fit8 = survfit(Surv(TSTAGE$followup_time,TSTAGE$censor)~TSTAGE$PMRT)
cox8u = coxph(Surv(TSTAGE$followup_time,TSTAGE$censor)~TSTAGE$PMRT)
cox8 <- coxph(Surv(TSTAGE$followup_time,TSTAGE$censor)~TSTAGE$PMRT
+ as.factor(AGE_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=TSTAGE)


# fit 9 T-Stage
GRADEHI<-N1mi[N1mi$GRADE_Recode == '3. Poorly Differentiated' & N1mi$PMRT!= 'Unknown' ,]
fit9 = survfit(Surv(GRADEHI$followup_time,GRADEHI$censor)~GRADEHI$PMRT)
cox9u = coxph(Surv(GRADEHI$followup_time,GRADEHI$censor)~GRADEHI$PMRT)
cox9 <- coxph(Surv(GRADEHI$followup_time,GRADEHI$censor)~GRADEHI$PMRT
+ as.factor(AGE_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(ERSTATUS)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=GRADEHI)



# fit 10 YOUNG and ER Status Grade
YOUNGNEG<-N1mi[N1mi$AGE_Recode == '1. <50' & N1mi$ERSTATUS == '2. Negative'  & N1mi$PMRT!= 'Unknown' ,]
fit10 = survfit(Surv(YOUNGNEG$followup_time,YOUNGNEG$censor)~YOUNGNEG$PMRT)
cox10u = coxph(Surv(YOUNGNEG$followup_time,YOUNGNEG$censor)~YOUNGNEG$PMRT)
cox10 <- coxph(Surv(YOUNGNEG$followup_time,YOUNGNEG$censor)~YOUNGNEG$PMRT
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=YOUNGNEG)

# fit 11 OLD
OLD<-N1mi[N1mi$AGE_Recode == '3. > or = 65'& N1mi$PMRT!= 'Unknown' ,]
fit11 = survfit(Surv(OLD$followup_time,OLD$censor)~OLD$PMRT)
cox11u = coxph(Surv(OLD$followup_time,OLD$censor)~OLD$PMRT)
cox11 <- coxph(Surv(OLD$followup_time,OLD$censor)~OLD$PMRT
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=OLD)


# fit 12 Charlson Score
GOODPERF<-N1mi[N1mi$CDCC_TOTAL_Recode == '0'& N1mi$PMRT!= 'Unknown' ,]
fit12 = survfit(Surv(GOODPERF$followup_time,GOODPERF$censor)~GOODPERF$PMRT)
cox12u = coxph(Surv(GOODPERF$followup_time,GOODPERF$censor)~GOODPERF$PMRT)
cox12 <- coxph(Surv(GOODPERF$followup_time,GOODPERF$censor)~GOODPERF$PMRT
+ as.factor(AGE_Recode)
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(RX_SUMM_CHEMO_Recode)
,data=GOODPERF)


		   
ggsurvplot(fit10, main='  Overall Survival of T1-T2 N1 \nBreast Cancer Patients in NCDB',xlab='Months',pval = TRUE, 
			surv.scale=c('percent'),
			pval.size = 8,
				font.main = c(36),
				font.legend = c(24),
				font.x = c(24),
				font.y = c(24),
				font.tickslab = c(24),
				risk.table.fontsize = 6,
				break.time.by = 12,
				legend.title='',
				risk.table.title='',
				legend=c('bottom'),
           risk.table = TRUE,
		   
#           risk.table.col = "strata",
           risk.table.height = 0.2, 
           ggtheme = theme_bw(),
           legend.labs = c( "No PMRT","PMRT"))









