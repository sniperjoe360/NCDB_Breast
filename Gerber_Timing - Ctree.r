# TO DO after meeting 2/3/17:

# Remove sequence of hormone therapy from table and analyses
# Add all patients to TABLE 1
	# Done Manually
# Median off histogram for 1000 iterations
# Include patients with missing survival. 

# Univariate model;
# Crowfly as per JCO
# Type of facilty 
# Region
# Margins
# Race as per JCO
# Charleson Deyo
# T
# N
# Education (new)
# Income level (new)
# Hispanic ethnicity new)
# Insurance status (new)
# TAKE OUT REGION DONE


read<-read.csv("C:\\Research\\NCDB/BreastAll.csv",strip.white=TRUE)

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

dat7 <- dat6[!is.na(dat6$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(dat6$PUF_VITAL_STATUS),]

NODAL <- dat7


dim(NODAL)

 #install.packages("party")
 # install.packages("ReporteRs")
 # install.packages("magrittr")
 #install.packages("forestmodel")
 #install.packages("tableone")
 #install.packages("survival")
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
NODAL$RACE_Recode = NULL
NODAL[NODAL$RACE ==1 ,'RACE_Recode'] = "White"
NODAL[NODAL$RACE ==2 ,'RACE_Recode'] = "Black"
NODAL[NODAL$RACE >2 ,'RACE_Recode'] = "Other"

# Race
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
NODAL[NODAL$INSURANCE_STATUS == 2 ,'INSURANCE_STATUS_Recode'] = "2. Medicaid"
NODAL[NODAL$INSURANCE_STATUS == 3 ,'INSURANCE_STATUS_Recode'] = "3. Medicare"
NODAL[NODAL$INSURANCE_STATUS == 4 ,'INSURANCE_STATUS_Recode'] = "4. Other Gvt"
NODAL[NODAL$INSURANCE_STATUS == 0 ,'INSURANCE_STATUS_Recode'] = "5. Not Insured"
NODAL[NODAL$INSURANCE_STATUS == 9 ,'INSURANCE_STATUS_Recode'] = "6. Unknown"

################ SPECIALLY DEFINED VARIABLES ########

# timing
NODAL$R_Date <- NODAL$DX_RAD_STARTED_DAYS
NODAL$S_Date <- NODAL$DX_SURG_STARTED_DAYS
NODAL$C_Date <- NODAL$DX_CHEMO_STARTED_DAYS
NODAL$H_Date <- NODAL$DX_HORMONE_STARTED_DAYS
NODAL$R_Leng <- NODAL$RAD_ELAPSED_RX_DAYS
NODAL$RS_Seq <- NODAL$RX_SUMM_SURGRAD_SEQ

NODAL$followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
NODAL$censor = 1-censor 

# period until RT
NODAL$P_RT = NULL
NODAL[(NODAL$interval >=0 & NODAL$interval <42),'P_RT'] = "1. <6 weeks"
NODAL[(NODAL$interval >=42 & NODAL$interval <84 ),'P_RT'] = "2. 6-12 weeks"
NODAL[(NODAL$interval >=84 & NODAL$interval <126 ),'P_RT'] = "3. 12-18 weeks"
NODAL[(NODAL$interval >=126 ) ,'P_RT'] = "4. >18 weeks"

# interval from RT to surgery
# if >0, then RT after surgery
NODAL$interval = NODAL$R_Date - NODAL$S_Date 

# Interval from surgery to hormones
# if >0, then hormones after surgery
NODAL$interval_S_H = NODAL$H_Date - NODAL$S_Date 


# Interval from Hormones to RT 
# if >0, then hormones before RT
NODAL$interval_H_RT = NODAL$R_Date - NODAL$H_Date

# Stratify into those who start hormones before RT vs. during AND after RT

NODAL$HORM_RT_SEQ = NULL
NODAL[NODAL$interval_H_RT > 0 & !is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "1. Hormones Before RT"
NODAL[NODAL$interval_H_RT <= 0 & !is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "2. Hormones During or After RT"
NODAL[is.na(NODAL$interval_H_RT) ,'HORM_RT_SEQ'] = "3. Unknown"
NODAL[NODAL$RX_SUMM_HORMONE_Recode == 'None' & !is.na(NODAL$RX_SUMM_HORMONE_Recode),'HORM_RT_SEQ'] = "4. No Hormones"

# descriptive statistics
listVars<-(c('P_RT', "AGE_Recode","RACE_Recode","HISPANIC_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","ERSTATUS","GRADE_Recode","HISTOLOGY_Recode","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_HORMONE_Recode','HORM_RT_SEQ','RX_SUMM_SURGICAL_MARGINS_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

catVars<-(c("P_RT","AGE_Recode","RACE_Recode","HISPANIC_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','FACILITY_TYPE_CD_Recode','EDUCATION2000_Recode','EDUCATION_Recode','INCOME2000_Recode','INCOME_Recode','INSURANCE_STATUS_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'HORM_RT_SEQ',includeNA=TRUE)
 #write.csv(print(table1),'C:\\Research\\NCDB/Timing_TableOne_2_4_2017_Timing_Hormone.csv')

 
 ################### plot some histograms ############
 # for Patients with Adjuvant Hormones before RT
 
 x11()
 NODAL1<- NODAL[NODAL$HORM_RT_SEQ == '1. Hormones Before RT',]
 NODAL2<- NODAL[NODAL$HORM_RT_SEQ  == '2. Hormones During or After RT',]

   par(mfrow=c(3,1))


     hist(NODAL$interval_S_H,1000,xlim=c(0,300),ylim=c(0,12000),add=F,col=2, main='Time to treatment initiation from surgery \n in all patients')
	legend(200,8000,c('Hormone initiation','RT initiation'),col=c(2,3),lwd=2)
	 hist(NODAL$interval,1000,xlim=c(0,300),ylim=c(0,12000),add=T, col=3)
	 	


 hist(NODAL2$interval_S_H,1000,xlim=c(0,300),ylim=c(0,12000),add=F, col=2,main='Time to treatment initiation from surgery \n in patients with hormones during or after RT')
 legend(200,8000,c('Hormone initiation','RT initiation'),col=c(2,3),lwd=2)
hist(NODAL2$interval,1000,xlim=c(0,300),ylim=c(0,12000),add=T, col=3)
		
 hist(NODAL1$interval,1000,xlim=c(0,300),ylim=c(0,3000),add=F, col=3, main='Time to treatment initiation from surgery  \n in patients with hormones before RT')
legend(200,2000,c('Hormone initiation','RT initiation'),col=c(2,3),lwd=2)
  hist(NODAL1$interval_S_H,1000,xlim=c(0,300),ylim=c(0,3000),add=T, col=2,)


  
##################### Univariate Survival ###############
# define the groups as patients who recieved hormones before RT (but after surgery) and no hormones
NODAL_S_H_RT <- NODAL[NODAL$HORM_RT_SEQ == '1. Hormones Before RT',]
NODAL_S_RT_H <- NODAL[NODAL$HORM_RT_SEQ == '2. Hormones During or After RT',]
NODAL_None <- NODAL[NODAL$HORM_RT_SEQ == '4. No Hormones',]
 NODAL_HORM <- NODAL[NODAL$HORM_RT_SEQ != '4. No Hormones',]
 NODAL_ERPOS <- NODAL[NODAL$ERSTATUS == '1. Positive',]
 NODAL_ERPOS_None <- NODAL_ERPOS[NODAL_ERPOS$HORM_RT_SEQ == '4. No Hormones',]
 NODAL_ERPOS_HORM <- NODAL_ERPOS[NODAL_ERPOS$HORM_RT_SEQ != '4. No Hormones',]

 NODAL_None_ERPOS <- NODAL_None[NODAL_None$ERSTATUS == '1. Positive',]
  NODAL_None_ERNEG <- NODAL_None[NODAL_None$ERSTATUS == '2. Negative',]

 NODAL_ERNEG <- NODAL[NODAL$ERSTATUS == '2. Negative',]
 NODAL_ERNEG_NoCHEMO <- NODAL_ERNEG[NODAL_ERNEG$RX_SUMM_CHEMO == 0,]
 

 
table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'HORM_RT_SEQ',includeNA=TRUE)
 
UNI_Results <- NULL


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_N_Recode)),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_LOCATION_CD_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ CROWFLY,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ P_RT,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_S_H,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_H_RT,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ INCOME2000_Recode,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ INCOME_Recode,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ HISPANIC_Recode,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ EDUCATION2000_Recode,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INSURANCE_STATUS_Recode),data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ EDUCATION_Recode,data=NODAL_None)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

UNI_Results<-round(UNI_Results,3)
colnames(UNI_Results) = c('HR','2.5% CI','97.5% CI','p')
UNI_Results
#################### Multivariate Survival ###############



coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode)+
+ as.factor(droplevels(TNM_PATH_T_Recode))
+ as.factor(droplevels(TNM_PATH_N_Recode))
# + as.factor(ERSTATUS)
+ as.factor(RACE_Recode)
+ as.factor(HISPANIC_Recode)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ as.factor(P_RT)
# + as.factor(HORM_RT_SEQ)
# + as.factor(EDUCATION2000_Recode)
# + as.factor(INCOME2000_Recode)
+ as.factor(INSURANCE_STATUS_Recode)
+ as.factor(CROWFLY_Recode),data=NODAL_ERNEG_NoCHEMO)

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
 x11()
 par(mfrow=c(2,2))
 
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_None
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved no hormones "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 


############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

for (i in 1:length(cutoff_l)){

cutoff = cutoff_l[i]
NODAL_ERNEG_NoCHEMO$P_RT2 = NULL
NODAL_ERNEG_NoCHEMO[NODAL_ERNEG_NoCHEMO$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_ERNEG_NoCHEMO[NODAL_ERNEG_NoCHEMO$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
# + as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_ERNEG_NoCHEMO)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_ERNEG_NoCHEMO$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n in All ER Negative patients "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_None_ERPOS
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved no hormones and ER Positive "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

 
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_None_ERNEG
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved no hormones and ER Negative "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

 ##### timing HR diagram ##########
  x11()
 par(mfrow=c(2,2))

############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_HORM
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved hormones "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

NODAL_test<-NODAL_ERPOS
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

for (i in 1:length(cutoff_l)){

cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
# + as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n in All ER Positive Patients"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_ERPOS_HORM
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n ER Positive Patients with hormones"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

 
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_ERPOS_None
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n in All ER Positive Patients without hormones"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

 
 ##### timing HR diagram ##########
  x11()
 par(mfrow=c(2,2))

############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_HORM
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n who recieved hormones "
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

NODAL_test<-NODAL_ERPOS
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

for (i in 1:length(cutoff_l)){

cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
# + as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n in All ER Positive Patients"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_S_H_RT
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n patients with hormones before RT"
)

# hack: we draw arrows but with very special "arrowheads"
arrows(cutoff_l, results2[,2], cutoff_l, results[,3], length=0.05, angle=90, code=3,cex=3)
abline(h=1,lwd=2)
text(cutoff_l,results[,3], labels=paste(results[,4]), cex= 0.7 ,pos=3)
 

 
############### Create the plot of HR #############

# Establish Cutoff
cutoff_l = c(7,14,28,42,56,70,84,98,112,126,154,182,210)
results = NULL

NODAL_test <- NODAL_S_RT_H
for (i in 1:length(cutoff_l)){


cutoff = cutoff_l[i]
NODAL_test$P_RT2 = NULL
NODAL_test[NODAL_test$interval < cutoff ,'P_RT2'] = 'EARLY'
NODAL_test[NODAL_test$interval >= cutoff ,'P_RT2'] = 'LATE'
#########################################
coxmodel <- coxph( (Surv(followup_time, censor))~ AGE_Recode+as.factor(CDCC_TOTAL)+ as.factor(P_RT2)+ as.factor(droplevels(TNM_PATH_T_Recode)) + as.factor(RACE_Recode)
+ as.factor(droplevels(TNM_PATH_N_Recode))
#+ as.factor(ERSTATUS)
+ as.factor(GRADE_Recode)
+ as.factor(HISTOLOGY_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)+ as.factor(CDCC_TOTAL_Recode)
+ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode)
+ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode)
+ as.factor(FACILITY_TYPE_CD_Recode)
+ CROWFLY,data=NODAL_test)

results = rbind(results,c(exp(coxmodel$coefficients[5]),exp(confint(coxmodel))[5,],table(NODAL_test$P_RT2)[2]))
print(cutoff)
}

results2 = results[1:length(cutoff_l),]
plot(cutoff_l,results2[,1],
    ylim=range(0.5,3),
    pch=19, xlab="Days from surgery since initiating RT", ylab="Hazard Ratio +/- 95% CI",
    main="Hazard Ratio of Death with Various Cutoffs for RT Initiation Time \n Taken from Adjusted Cox Model of patients \n in patients with hormones after or during RT"
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
Peter Wu <speterwu@gmail.com>
	
Feb 8 (1 day ago)
	
to me
# NCDB_HN.R: Data analysis for the NCDB Larynx Project with Dr. Hu and Givi
read<-read.csv("C:/Research/NCDB_HN/Larynx.csv",strip.white=TRUE)

dim(read)

# filter for only T1-2 NX M0 cases
dat <- read[

(
(read$TNM_CLIN_T == 1 | 
 read$TNM_CLIN_T == '1A' |
 read$TNM_CLIN_T == '1B' |
  read$TNM_CLIN_T == 2),]

dat2<-dat[!is.na(dat$TNM_CLIN_T),]

dat3 <- dat2[dat2$TNM_CLIN_N == 0 & !is.na(dat2$TNM_CLIN_N),] 

dat4 <- dat3[dat3$TNM_CLIN_M != 1,]

# only glottic larynx
dat5 <- dat4[dat4$PRIMARY_SITE == 'C320',]


# Exclude patients with previous cancer history
dats2 <- dat5[dat5$SEQUENCE == 1 | 
     dat5$SEQUENCE == 0 
,]
dim(dats2)


# Exclude patients with no radiation and those with chemo
dats3 <- dats2[dats2$RX_SUMM_RADIATION != 0 & dats2$RX_SUMM_CHEMO == 0 & dats2$RX_SUMM_SURG_PRIM_SITE == 0,]
dim(dats3)


# include patients with complete survival data
dat7 <- dats3[!is.na(dats3$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(dats3$PUF_VITAL_STATUS),]

larynx <- dat7
dim(larynx)

 #install.packages("party")
 # install.packages("ReporteRs")
 # install.packages("magrittr")
 #install.packages("forestmodel")
 #install.packages("tableone")
 #install.packages("survival")
 # FOREST PLOT
library("forestmodel")
library(tableone)
library(party)
library(survival)
#library(ReporteRs)

############################

#recode for IMRT
larynx$IMRT <- "No IMRT"
larynx$IMRT[larynx$RAD_REGIONAL_RX_MODALITY == 31] = "IMRT"



larynx$NODE_CAT="Unknown SURVIVAL DATA"
larynx[(!is.na(larynx$DX_LASTCONTACT_DEATH_MONTHS)& !is.na(larynx$PUF_VITAL_STATUS)),'NODE_CAT'] = 'FULL SURVIVAL DATA'  

larynx$RX_SUMM_RADIATION_Recode = NULL
larynx[larynx$RX_SUMM_RADIATION ==1 ,'RX_SUMM_RADIATION_Recode'] = "EBRT"
larynx[larynx$RX_SUMM_RADIATION ==2 ,'RX_SUMM_RADIATION_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_RADIATION == 3 ,'RX_SUMM_RADIATION_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_RADIATION == 4 ,'RX_SUMM_RADIATION_Recode'] = "EBRT"
larynx[larynx$RX_SUMM_RADIATION == 5 ,'RX_SUMM_RADIATION_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_RADIATION == 9 ,'RX_SUMM_RADIATION_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_RADIATION == 0 ,'RX_SUMM_RADIATION_Recode'] = "No RT"


# T-stage
larynx$TNM_CLIN_T_Recode  = larynx$TNM_CLIN_T
larynx[
larynx$TNM_CLIN_T ==1    |
larynx$TNM_CLIN_T =='1A' |
larynx$TNM_CLIN_T =='1B' |
larynx$TNM_CLIN_T =='1C' |
larynx$TNM_CLIN_T =='1MI'
,'TNM_CLIN_T_Recode'] = 1

# N-stage
larynx$TNM_CLIN_N_Recode  = larynx$TNM_CLIN_N
larynx[
larynx$TNM_CLIN_N ==1    |
larynx$TNM_CLIN_N =='1A' |
larynx$TNM_CLIN_N =='1B' |
larynx$TNM_CLIN_N =='1C' |
larynx$TNM_CLIN_N =='1MI'
,'TNM_CLIN_N_Recode'] = 1

larynx[
larynx$TNM_CLIN_N ==0    |
larynx$TNM_CLIN_N =='0M+' |
larynx$TNM_CLIN_N =='0M-' |
larynx$TNM_CLIN_N =='0I+' |
larynx$TNM_CLIN_N =='0I-'
,'TNM_CLIN_N_Recode'] = 0

# M-stage
larynx$TNM_CLIN_M_Recode  = larynx$TNM_CLIN_M
larynx[
larynx$TNM_CLIN_M =='X'    |
larynx$TNM_CLIN_M ==' '    |
larynx$TNM_CLIN_M ==88 
,'TNM_CLIN_M_Recode'] = 'X'

# Age
larynx$AGE_Recode=NULL
larynx[larynx$AGE <50 ,'AGE_Recode']                 = "1. <50"
larynx[larynx$AGE <65 & larynx$AGE >=50,'AGE_Recode'] = "2. 50-65"
larynx[larynx$AGE >=65,'AGE_Recode']                 = "3. > or = 65"

# recode radiation sequencing
larynx$RX_SUMM_SURGRAD_SEQ_Recode = NULL
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==0 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "No RT"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==2 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Neoadjuvant RT"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==3 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Adjuvant RT"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==4 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Before and After Surgery"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==5 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-Operative"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==6 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-operative and additional RT"
larynx[larynx$RX_SUMM_SURGRAD_SEQ ==9 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Unknown"

# Race
larynx$RACE_Recode = NULL
larynx[larynx$RACE ==1 ,'RACE_Recode'] = "White"
larynx[larynx$RACE ==2 ,'RACE_Recode'] = "Black"
larynx[larynx$RACE >2 ,'RACE_Recode'] = "Other"

# Race
larynx$HISPANIC_Recode = NULL
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==1 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==2 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==3 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==4 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==5 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==6 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==7 ,'HISPANIC_Recode'] = "Other"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==8 ,'HISPANIC_Recode'] = "Hispanic"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==9 ,'HISPANIC_Recode'] = "Other"
larynx[larynx$SPANISH_HISPANIC_ORIGIN ==0 ,'HISPANIC_Recode'] = "Non-Hispanic"


# ER Status
larynx$ERSTATUS = '3. Unknown'
larynx[larynx$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "1. Positive"
larynx[larynx$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "3. Unknown"
larynx[larynx$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "2. Negative"

# YEAR OF DIAGNOSIS
larynx$YEAR_OF_DIAGNOSIS_Recode = "2012+"
larynx[larynx$YEAR_OF_DIAGNOSIS <2012 ,'YEAR_OF_DIAGNOSIS_Recode'] = "Before 2012"


# Grade
larynx$GRADE_Recode = NULL
larynx[larynx$GRADE ==1 ,'GRADE_Recode'] = "1. Well Differentiated"
larynx[larynx$GRADE ==2 ,'GRADE_Recode'] = "2. Moderately Differentiated"
larynx[larynx$GRADE ==3 ,'GRADE_Recode'] = "3. Poorly Differentiated"
larynx[larynx$GRADE ==4 ,'GRADE_Recode'] = "3. Poorly Differentiated"
larynx[larynx$GRADE ==9 ,'GRADE_Recode'] = "4. Unknown"

# LVI
larynx$LYMPH_VASCULAR_INVASION_Recode = NULL
larynx[larynx$LYMPH_VASCULAR_INVASION ==0 & !is.na(larynx$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Absent"
larynx[larynx$LYMPH_VASCULAR_INVASION ==1 & !is.na(larynx$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Present"
larynx[larynx$LYMPH_VASCULAR_INVASION ==8 & !is.na(larynx$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"
larynx[larynx$LYMPH_VASCULAR_INVASION ==9 & !is.na(larynx$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"
larynx[is.na(larynx$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"


# Charlson-Deyo
larynx$CDCC_TOTAL_Recode = NULL
larynx[larynx$CDCC_TOTAL ==0 ,'CDCC_TOTAL_Recode'] = "0"
larynx[larynx$CDCC_TOTAL ==1 ,'CDCC_TOTAL_Recode'] = "1"
larynx[larynx$CDCC_TOTAL ==2 ,'CDCC_TOTAL_Recode'] = "2+"


# RX SUMMARY CHEMO
larynx$RX_SUMM_CHEMO_Recode = NULL
larynx[larynx$RX_SUMM_CHEMO ==0 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==1 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==2 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==3 ,'RX_SUMM_CHEMO_Recode'] = "Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==82 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==85 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==86 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==87 ,'RX_SUMM_CHEMO_Recode'] = "No Chemotherapy"
larynx[larynx$RX_SUMM_CHEMO ==88 ,'RX_SUMM_CHEMO_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_CHEMO ==99 ,'RX_SUMM_CHEMO_Recode'] = "Unknown"


# RX SUMM SYSTEMIC SUR SEQ 
larynx$RX_SUMM_SYSTEMIC_SUR_SEQ_Recode = NULL
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==0 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "No systemic therapy"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==2 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==3 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Adjuvant"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==4 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==5 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==6 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_SYSTEMIC_SUR_SEQ ==9 & !is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"
larynx[is.na(larynx$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"

# RX_SUMM_HORMONE
larynx$RX_SUMM_HORMONE_Recode = NULL
larynx[larynx$RX_SUMM_HORMONE ==0 ,'RX_SUMM_HORMONE_Recode'] = "None"
larynx[larynx$RX_SUMM_HORMONE ==1 ,'RX_SUMM_HORMONE_Recode'] = "Hormonal Therapy"
larynx[larynx$RX_SUMM_HORMONE ==82 ,'RX_SUMM_HORMONE_Recode'] = "None"
larynx[larynx$RX_SUMM_HORMONE ==85 ,'RX_SUMM_HORMONE_Recode'] = "None"
larynx[larynx$RX_SUMM_HORMONE ==86 ,'RX_SUMM_HORMONE_Recode'] = "None"
larynx[larynx$RX_SUMM_HORMONE ==87 ,'RX_SUMM_HORMONE_Recode'] = "None"
larynx[larynx$RX_SUMM_HORMONE ==88 ,'RX_SUMM_HORMONE_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_HORMONE ==99 ,'RX_SUMM_HORMONE_Recode'] = "Unknown"


# RX_SUMM_SURGICAL_MARGINS
larynx$RX_SUMM_SURGICAL_MARGINS_Recode = NULL
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==0 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Negative"
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==1 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==2 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==3 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Positive Macroscopic/Microscopic"
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==7 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"
larynx[larynx$RX_SUMM_SURGICAL_MARGINS ==9 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"
larynx[is.na(larynx$RX_SUMM_SURGICAL_MARGINS),'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown"


#recode facility
# FACILITY_TYPE_CD_Recode
larynx$FACILITY_TYPE_CD_Recode = NULL
larynx[larynx$FACILITY_TYPE_CD ==1 & !is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Community Cancer Program"
larynx[larynx$FACILITY_TYPE_CD ==2 & !is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Comprehensive Community Cancer Program"
larynx[larynx$FACILITY_TYPE_CD ==3 & !is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Academic/Research Program"
larynx[larynx$FACILITY_TYPE_CD ==4 & !is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Integrated Network Cancer Program"
larynx[larynx$FACILITY_TYPE_CD ==9 & !is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Other or unknown"
larynx[is.na(larynx$FACILITY_TYPE_CD),'FACILITY_TYPE_CD_Recode'] = "Unknown"


#recode facility type
# FACILITY_LOCATION_CD_Recode
larynx$FACILITY_LOCATION_CD_Recode = NULL
larynx[larynx$FACILITY_LOCATION_CD ==1 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "New England"
larynx[larynx$FACILITY_LOCATION_CD ==2 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Middle Atlantic"
larynx[larynx$FACILITY_LOCATION_CD ==3 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "South Atlantic"
larynx[larynx$FACILITY_LOCATION_CD ==4 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "East North Central"
larynx[larynx$FACILITY_LOCATION_CD ==5 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "East South Central"
larynx[larynx$FACILITY_LOCATION_CD ==6 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "West North Central"
larynx[larynx$FACILITY_LOCATION_CD ==7 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "West South Central"
larynx[larynx$FACILITY_LOCATION_CD ==8 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Mountain"
larynx[larynx$FACILITY_LOCATION_CD ==9 & !is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Pacific"
larynx[is.na(larynx$FACILITY_LOCATION_CD),'FACILITY_LOCATION_CD_Recode'] = "Unknown"

larynx$HISTOLOGY_Recode = 'Other'
larynx[larynx$HISTOLOGY >= 8500 &  larynx$HISTOLOGY <= 8500 & !is.na(larynx$HISTOLOGY),'HISTOLOGY_Recode'] = "Ductal"
larynx[larynx$HISTOLOGY >= 8520 &  larynx$HISTOLOGY <= 8520 & !is.na(larynx$HISTOLOGY),'HISTOLOGY_Recode'] = "Lobular"
larynx[larynx$HISTOLOGY >= 8522 &  larynx$HISTOLOGY <= 8523 & !is.na(larynx$HISTOLOGY),'HISTOLOGY_Recode'] = "Mixed Ductal and Lobular"


# CROWFLY
larynx$CROWFLY_Recode = '1. <10'
larynx[larynx$CROWFLY >= 10 &  larynx$CROWFLY <= 20 & !is.na(larynx$CROWFLY),'CROWFLY_Recode'] = "2. 10-20"
larynx[larynx$CROWFLY >= 20 &  larynx$CROWFLY <= 50 & !is.na(larynx$CROWFLY),'CROWFLY_Recode'] = "3. 20-50"
larynx[larynx$CROWFLY >= 50 &  larynx$CROWFLY <= 100 & !is.na(larynx$CROWFLY),'CROWFLY_Recode'] = "4. 50-100"
larynx[larynx$CROWFLY >= 100 & !is.na(larynx$CROWFLY),'CROWFLY_Recode'] = "5. >100"


# Education
larynx$EDUCATION_Recode = NULL
larynx[larynx$NO_HSD_QUAR_12 == 1 & !is.na(larynx$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "1. >21%"
larynx[larynx$NO_HSD_QUAR_12 == 2 & !is.na(larynx$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "2. 13-21%"
larynx[larynx$NO_HSD_QUAR_12 == 3 & !is.na(larynx$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "3. 7-12.9%"
larynx[larynx$NO_HSD_QUAR_12 == 4 & !is.na(larynx$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "4. <7%"
larynx[is.na(larynx$NO_HSD_QUAR_12),'EDUCATION_Recode'] = "Unknown"


# Education2000
larynx$EDUCATION2000_Recode = NULL
larynx[larynx$NO_HSD_QUAR_00 == 1 & !is.na(larynx$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "1. >21%"
larynx[larynx$NO_HSD_QUAR_00 == 2 & !is.na(larynx$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "2. 13-21%"
larynx[larynx$NO_HSD_QUAR_00 == 3 & !is.na(larynx$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "3. 7-12.9%"
larynx[larynx$NO_HSD_QUAR_00 == 4 & !is.na(larynx$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "4. <7%"
larynx[is.na(larynx$NO_HSD_QUAR_00),'EDUCATION2000_Recode'] = "Unknown"


# Income
larynx$INCOME_Recode = NULL
larynx[larynx$MED_INC_QUAR_12 ==1 & !is.na(larynx$MED_INC_QUAR_12),'INCOME_Recode'] = "1. <38K"
larynx[larynx$MED_INC_QUAR_12 == 2 & !is.na(larynx$MED_INC_QUAR_12),'INCOME_Recode'] = "2. 38K-48K"
larynx[larynx$MED_INC_QUAR_12 == 3 & !is.na(larynx$MED_INC_QUAR_12),'INCOME_Recode'] = "4. 48K-63K"
larynx[larynx$MED_INC_QUAR_12 == 4 & !is.na(larynx$MED_INC_QUAR_12),'INCOME_Recode'] = "5. >63K"
larynx[is.na(larynx$MED_INC_QUAR_12),'INCOME_Recode'] = "Unknown"

# Income2000
larynx$INCOME2000_Recode = NULL
larynx[larynx$MED_INC_QUAR_00 ==1 & !is.na(larynx$MED_INC_QUAR_00),'INCOME2000_Recode'] = "1. <30K"
larynx[larynx$MED_INC_QUAR_00 == 2 & !is.na(larynx$MED_INC_QUAR_00),'INCOME2000_Recode'] = "2. 30K-35K"
larynx[larynx$MED_INC_QUAR_00 == 3 & !is.na(larynx$MED_INC_QUAR_00),'INCOME2000_Recode'] = "4. 35K-46K"
larynx[larynx$MED_INC_QUAR_00 == 4 & !is.na(larynx$MED_INC_QUAR_00),'INCOME2000_Recode'] = "5. >46K"
larynx[is.na(larynx$MED_INC_QUAR_00),'INCOME2000_Recode'] = "Unknown"

# Insurance
larynx$INSURANCE_STATUS_Recode = NULL
larynx[larynx$INSURANCE_STATUS ==1,'INSURANCE_STATUS_Recode'] = "1. Private"
larynx[larynx$INSURANCE_STATUS == 2 ,'INSURANCE_STATUS_Recode'] = "2. Medicaid"
larynx[larynx$INSURANCE_STATUS == 3 ,'INSURANCE_STATUS_Recode'] = "3. Medicare"
larynx[larynx$INSURANCE_STATUS == 4 ,'INSURANCE_STATUS_Recode'] = "4. Other Gvt"
larynx[larynx$INSURANCE_STATUS == 0 ,'INSURANCE_STATUS_Recode'] = "5. Not Insured"
larynx[larynx$INSURANCE_STATUS == 9 ,'INSURANCE_STATUS_Recode'] = "6. Unknown"

#survival
larynx$followup_time = larynx["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = larynx["PUF_VITAL_STATUS"][[1]]
larynx$censor = 1-censor 

# descriptive statistics
listVars<-(c("AGE_Recode","RACE_Recode","HISPANIC_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_CLIN_T_Recode","GRADE_Recode","HISTOLOGY_Recode","RX_SUMM_CHEMO_Recode","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','EDUCATION_Recode','INCOME2000_Recode','INCOME_Recode','INSURANCE_STATUS_Recode','FACILITY_TYPE_CD_Recode','CROWFLY_Recode'))

catVars<-(c("AGE_Recode","RACE_Recode","HISPANIC_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_CLIN_T_Recode","RX_SUMM_CHEMO_Recode","GRADE_Recode","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','FACILITY_TYPE_CD_Recode','EDUCATION2000_Recode','EDUCATION_Recode','INCOME2000_Recode','INCOME_Recode','INSURANCE_STATUS_Recode','CROWFLY_Recode'))

table1 <- CreateTableOne(vars = listVars, data = larynx, factorVars = catVars, strata = 'IMRT',includeNA=TRUE)
 #############################
print(table1)

larynx_test <-larynx

# create survival object
allpts <- survfit(Surv(followup_time, censor)~ 1,data=larynx_test)
plot(allpts, xlab="Time", ylab="Survival Probability (%)",lwd=2, main="Kaplan Meier Plot for Survival of All T1-2N0 with RT Alone")

x11()

# stratify by IMRT
subset <- survfit(Surv(followup_time, censor)~IMRT,data=larynx_test)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival of All T1-2N0 Larynx \n Treated with Definitive RT Stratified by IMRT',lwd=2,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 1.5)
legend(80, 1.0, c("IMRT","Non-IMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=1)
logrank <- survdiff(Surv(followup_time, censor)~IMRT,data=larynx_test)
pval <- 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
text(30,0.07,paste("logrank ","p =",round(pval,4),sep = ' '))

#SURVIVAL ANALYSIS #######
UNI_Results <- NULL


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(IMRT),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_N_Recode)),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(HISTOLOGY_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(CDCC_TOTAL_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_LOCATION_CD_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ CROWFLY,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ P_RT,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_S_H,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval_H_RT,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ interval,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ INCOME2000_Recode,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ INCOME_Recode,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ HISPANIC_Recode,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))


coxmodel <- coxph(Surv(followup_time, censor)~ EDUCATION2000_Recode,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(INSURANCE_STATUS_Recode),data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

coxmodel <- coxph(Surv(followup_time, censor)~ EDUCATION_Recode,data=larynx)
UNI_Results = rbind(UNI_Results,cbind(exp(coxmodel$coefficients),exp(confint(coxmodel)),round(coef(summary(coxmodel))[,5],2)))

UNI_Results<-round(UNI_Results,3)
colnames(UNI_Results) = c('HR','2.5% CI','97.5% CI','p')
UNI_Results
#################### Multivariate Survival ###############



coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode) + as.factor(IMRT) + as.factor(droplevels(TNM_CLIN_T_Recode))
+ as.factor(RACE_Recode)
+ as.factor(HISPANIC_Recode)
+ as.factor(GRADE_Recode)
+ as.factor(LYMPH_VASCULAR_INVASION_Recode)
+ as.factor(CDCC_TOTAL_Recode)
,data=larynx)

forest_model(coxmodel)



#########################################
# propensity score matched cohort analysis

# KM survival analysis

# Cox Model