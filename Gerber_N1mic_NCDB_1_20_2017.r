
# 1)	INCLUDE/EXCLUDE
# a.	Step 3. INCLUDING t1 T2 N1 N2
# b.	Excluding M1. 
# c.	Step 5: excluding neoadj and intra op RT and chemo. KEEP UNKNOWN
# 2)	Edit table 1 with collapsed variables
# 3)	Edit Nodal table as discussed. Pre 2012 post 2012 unknown as rows and columns. Median, range
# 4)	Univariate analysis for all variables (collapsed as discussed)


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
dat3 <- dat2_N[	dat2_N$TNM_PATH_M != 1 ,]
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
dat5 <- dat4[	dat4$RX_SUMM_SURGRAD_SEQ == 0 | dat4$RX_SUMM_SURGRAD_SEQ == 3,]
dim(dat5)

# # Include patients with documented  (Table 1)
# Chemotherapy after Surgery (Post-mastectomy RT) (N = 317,167)
# AND
# No Chemotherapy Therapy (No systemic Delivered) (N = 91,658)
dat6 <- dat5[	(dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 0 | dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 3) & !is.na(dat5$RX_SUMM_SYSTEMIC_SUR_SEQ),]
dim(dat6)



dat <- dat6
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


					
# N-Stage
# N0
N0 <- dat[(
	dat$TNM_PATH_N == 0      | 
	dat$TNM_PATH_N == '0I-'  | 
	dat$TNM_PATH_N == '0I+'  | 
	dat$TNM_PATH_N == '0M-'  | 
	dat$TNM_PATH_N == '0M+'
							), ]
N0$NODE_CAT <- 'T1-T2 N0'
N0$TNM_PATH_N_Recode <- '0'

# N1mi
N1mi <- dat[(
	dat$TNM_PATH_N =='1MI'
							), ]
N1mi$NODE_CAT <- 'T1-T2 N1mi'
N1mi$TNM_PATH_N_Recode <- 'N1mi'


# N1
N1 <- dat[(
	dat$TNM_PATH_N == 1     | 
	dat$TNM_PATH_N == '1A'  | 
	dat$TNM_PATH_N == '1B'  | 
	dat$TNM_PATH_N == '1C'
							), ]
N1$NODE_CAT <- 'T1-T2 N1'
N1$TNM_PATH_N_Recode <- 'N1'


# N2
N2 <- dat[(
	dat$TNM_PATH_N == 2    | 
	dat$TNM_PATH_N == '2A' | 
	dat$TNM_PATH_N == '2B'
							), ]
N2$NODE_CAT <- 'T1-T2 N2'
N2$TNM_PATH_N_Recode <- 'N2'

# combine the N-stage sheets
NODAL <- rbind(N0,N1mi)
NODAL <- rbind(NODAL,N1)
NODAL <- rbind(NODAL,N2)

# T-stage
NODAL$TNM_PATH_T_Recode  = NODAL$TNM_PATH_T
NODAL[
	NODAL$TNM_PATH_T ==1    |
	NODAL$TNM_PATH_T =='1A' |
	NODAL$TNM_PATH_T =='1B' |
	NODAL$TNM_PATH_T =='1C' 
	,'TNM_PATH_T_Recode'] = 1

# Age
NODAL$AGE_Recode=NULL
NODAL[NODAL$AGE <50 ,'AGE_Recode']                 = "<50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_Recode'] = "50-65"
NODAL[NODAL$AGE >=65,'AGE_Recode']                 = "> or = 65"


# recode PMRT
NODAL$PMRT = 'No PMRT'
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==  3 ,'PMRT'] = "PMRT"

# recode radiation sequencing
NODAL$RX_SUMM_SURGRAD_SEQ_Recode = NULL
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==0 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "No RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==2 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Neoadjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==3 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Adjuvant RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==4 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Before and After Surgery"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==5 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-Operative"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==6 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Intra-operative and additional RT"
NODAL[NODAL$RX_SUMM_SURGRAD_SEQ ==9 ,'RX_SUMM_SURGRAD_SEQ_Recode'] = "Unknown Sequence"

# Race
NODAL$RACE_Recode = 'White'
NODAL[NODAL$RACE >1 ,'RACE_Recode'] = "Other"

# ER Status
NODAL$ERSTATUS = 'Unknown'
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==10 ,'ERSTATUS'] = "Positive"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==30 ,'ERSTATUS'] = "Unknown"
NODAL[NODAL$CS_SITESPECIFIC_FACTOR_1 ==20 ,'ERSTATUS'] = "Negative"

# YEAR OF DIAGNOSIS
NODAL$YEAR_OF_DIAGNOSIS_Recode = "2012+"
NODAL[NODAL$YEAR_OF_DIAGNOSIS <2012 ,'YEAR_OF_DIAGNOSIS_Recode'] = "Before 2012"


# Grade
NODAL$GRADE_Recode = NULL
NODAL[NODAL$GRADE ==1 ,'GRADE_Recode'] = "Well Differentiated"
NODAL[NODAL$GRADE ==2 ,'GRADE_Recode'] = "Moderately Differentiated"
NODAL[NODAL$GRADE ==3 ,'GRADE_Recode'] = "Poorly Differentiated"
NODAL[NODAL$GRADE ==4 ,'GRADE_Recode'] = "Poorly Differentiated"
NODAL[NODAL$GRADE ==9 ,'GRADE_Recode'] = "Unknown"

# LVI
NODAL$LYMPH_VASCULAR_INVASION_Recode = NULL
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==0 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Absent"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==1 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Present"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==8 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"
NODAL[NODAL$LYMPH_VASCULAR_INVASION ==9 & !is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"


# Charlson-Deyo
NODAL$CDCC_TOTAL_Recode = NULL
NODAL[NODAL$CDCC_TOTAL ==0 ,'CDCC_TOTAL_Recode'] = "0"
NODAL[NODAL$CDCC_TOTAL ==1 ,'CDCC_TOTAL_Recode'] = "1"
NODAL[NODAL$CDCC_TOTAL ==2 ,'CDCC_TOTAL_Recode'] = "> or = 2"


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
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==0 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "No systemic therapy"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==2 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==3 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Adjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==4 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Sandwich"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==5 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Intraoperative"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==6 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Intraoperative"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==9 ,'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Unknown"

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
NODAL[NODAL$RX_SUMM_SURGICAL_MARGINS ==9 ,'RX_SUMM_SURGICAL_MARGINS_Recode'] = "Unknown "


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


# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD','CROWFLY'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'NODE_CAT',includeNA=TRUE)

write.csv(print(table1),'H:\\Gerber\\NCDB\\N1Mi_TableOne_1_25_2017_Allpatients_Nodes.csv')
 
 
# 3)	Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table
########################## Create the new data set ################

# include only patients treated with RT after Surgery

N1mi_P <- NODAL[(NODAL$TNM_PATH_N =='1MI'), ]
# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD','CROWFLY'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'PMRT',includeNA=TRUE)

write.csv(print(table1),'H:\\Gerber\\NCDB\\N1Mi_TableOne_1_28_2017_PMRTN1Mic.csv')
 
  3)	Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
  
    # create this comparison table for LN Dissection 
########################## Create the new data set ################

# include only patients treated with RT after Surgery

# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode",'REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode",'REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode_2'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'SCOPE_REG_LN_2012_Recode',includeNA=TRUE)

write.csv(print(table1),'H:\\Gerber\\NCDB\\N1Mi_TableOne_1_28_2017_ScopeLN.csv')
 
 ############################################## COX MODEL ###############
 
attach(NODAL)

# univariate

#find the follow up and censor time
followup_time = NODAL["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = NODAL["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

factors<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD','CROWFLY'))

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_Recode))

for (factor in 1:length(factors)){



}

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N1mi Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)


# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ as.factor(RX_SUMM_RADIATION) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(RX_SUMM_SURGICAL_MARGINS) + CROWFLY + as.factor(FACILITY_LOCATION_CD) + as.factor(FACILITY_TYPE_CD))