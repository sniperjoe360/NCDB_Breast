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

NODAL <- dat6

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
NODAL$TNM_PATH_N == '0M+'),'NODE_CAT'] = 'T1-T2 N0'

NODAL[NODAL$TNM_PATH_N =='1MI','NODE_CAT'] = 'T1-T2 N1mi'

NODAL[(NODAL$TNM_PATH_N == 2    | 
NODAL$TNM_PATH_N == '2A'    | 
NODAL$TNM_PATH_N == '2B'),'NODE_CAT'] = 'T1-T2 N2'

NODAL[(NODAL$TNM_PATH_N == 1  | 
NODAL$TNM_PATH_N == '1A'  | 
NODAL$TNM_PATH_N == '1B'  | 
NODAL$TNM_PATH_N == '1C'),'NODE_CAT'] = 'T1-T2 N1'


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

# SCOPE_REG_LN_2012
NODAL$RX_SUMM_SCOPE_REG_LN_SUR_Recode = NULL
NODAL[NODAL$RX_SUMM_SCOPE_REG_LN_SUR ==0 & !is.na(NODAL$RX_SUMM_SCOPE_REG_LN_SUR),'RX_SUMM_SCOPE_REG_LN_SUR_Recode'] = "None"
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


# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'NODE_CAT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_Allpatients_Nodes.csv')
 
 
# 3) Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table
########################## Create the new data set ################

# include only patients treated with RT after Surgery

N1mi_P <- NODAL[(NODAL$TNM_PATH_N =='1MI'), ]
# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'PMRT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_PMRTN1Mic.csv')
 
#  3) Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
  
    # create this comparison table for LN Dissection 
########################## Create the new data set ################

# include only patients treated with RT after Surgery

# descriptive statistics
listVars<-(c("YEAR_OF_DIAGNOSIS_Recode",'REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2'))

catVars<-(c("YEAR_OF_DIAGNOSIS_Recode",'REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode_2'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'SCOPE_REG_LN_2012_Recode',includeNA=TRUE)

summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='ALND','REGIONAL_NODES_EXAMINED_Recode'])
summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='SLND','REGIONAL_NODES_EXAMINED_Recode'])
summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='Unknown','REGIONAL_NODES_EXAMINED_Recode'])

summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='ALND','REGIONAL_NODES_POSITIVE_Recode'])
summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='SLND','REGIONAL_NODES_POSITIVE_Recode'])
summary(N1mi_P[N1mi_P$SCOPE_REG_LN_2012_Recode=='Unknown','REGIONAL_NODES_POSITIVE_Recode'])


write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_1_28_2017_ScopeLN.csv')
 
##################### Univariate Survival ###############
UNI_Results <- NULL


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=N1mi_P)
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

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_HORMONE_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_LOCATION_CD_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ CROWFLY,data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

coxmodel <- coxph(Surv(followup_time, censor)~ PMRT,data=N1mi_P)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),coef(summary(coxmodel))[,5]))

UNI_Results

#################### Multivariate Survival ###############
install.packages('forestmodel')
library(forestmodel)
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
+ as.factor(FACILITY_LOCATION_CD_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
,data=N1mi_P)

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
,data=N1mi_P)

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
N1mi_P2<- N1mi_P[N1mi_P$PMRT != 'Unknown',]

N1mi_P2$PMRT_Binary = NULL
N1mi_P2[N1mi_P2$RX_SUMM_SURGRAD_SEQ ==  3 ,'PMRT_Binary'] = 1
N1mi_P2[N1mi_P2$RX_SUMM_SURGRAD_SEQ ==  0 ,'PMRT_Binary'] = 0

N1mi_P2 <- N1mi_P2[!is.na(N1mi_P2$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(N1mi_P2$PUF_VITAL_STATUS),]

# SURVIVAL DATA
N1mi_P2$followup_time = N1mi_P2["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = N1mi_P2["PUF_VITAL_STATUS"][[1]]
N1mi_P2$censor = 1-censor 


#install.packages('MatchIt')
library(MatchIt)
# select variables
myvars <- c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT_Binary','FACILITY_TYPE_CD_Recode','CROWFLY_Recode','followup_time','censor')
matchdata <- N1mi_P2[myvars]

# Create the match based on stage, age, chemo using the nearest neighbor method etc...)
m.out=matchit(PMRT_Binary ~ as.factor(AGE_Recode) + as.factor(CDCC_TOTAL_Recode) + as.factor(TNM_PATH_T_Recode) + as.factor(ERSTATUS) + as.factor(CDCC_TOTAL_Recode) + as.factor(GRADE_Recode) + as.factor(LYMPH_VASCULAR_INVASION_Recode) + +as.factor(RX_SUMM_HORMONE_Recode) + as.factor(RX_SUMM_CHEMO_Recode)+as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)+as.factor(SCOPE_REG_LN_2012_Recode)+as.factor(YEAR_OF_DIAGNOSIS_Recode), data = matchdata, method = "nearest");
summary(m.out);

N1mi_P_Matched <- match.data(m.out)

# table 1 #
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P_Matched, factorVars = catVars, strata = 'PMRT_Binary',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_2_10_2017_PMRTMatched.csv')

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
+ as.factor(FACILITY_LOCATION_CD_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
,data=N1mi_P_Matched)

forest_model(coxmodel)



