
read<-read.csv("H:/Gerber/NCDB/BreastAll.csv",strip.white=TRUE)

dim(read)

# Exclude men
dats <- read[read$SEX == 2,]
dim(dats)

# include patients who had documented lumpectomy 
dats1 <- dats[dats$RX_SUMM_SURG_PRIM_SITE < 30, ] 
dim(dats1)

# Exclude patients with previous cancer history
dats2 <- dats1[dats1$SEQUENCE == 1 | 
     dats1$SEQUENCE == 0 
,]
dim(dats2)

# exclude M1 disease and Stage IV disease
dat3 <- dats2[  dats2$TNM_PATH_M != 1 & dats2$TNM_PATH_STAGE != 1,]
dim(dat3)

# # Include patients with documented  (Table 1)
# Radiation after Surgery (Post-mastectomy RT) (N = 317,167)
# AND
# No Radiation Therapy (No radiation Delivered) (N = 91,658)
dat5 <- dat3[ dat3$RX_SUMM_SURGRAD_SEQ == 0 | dat3$RX_SUMM_SURGRAD_SEQ == 3  | is.na(dat3$RX_SUMM_SURGRAD_SEQ) | dat3$RX_SUMM_SURGRAD_SEQ == 9,]
dim(dat5)

# # Include patients with documented  (Table 1)
# Chemotherapy after Surgery (Post-mastectomy RT) (N = 317,167)
# AND
# No Chemotherapy Therapy (No systemic Delivered) (N = 91,658)
dat6 <- dat5[ (dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 0 | dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 3 | is.na(dat5$RX_SUMM_SYSTEMIC_SUR_SEQ) | dat5$RX_SUMM_SYSTEMIC_SUR_SEQ == 9),]
dim(dat6)

#exclue missing timing
dat7 <- dat6[!is.na(dat6$DX_RAD_STARTED_DAYS) & !is.na(dat6$DX_SURG_STARTED_DAYS),]
dim(dat7)

NODAL <- dat7

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

# Age
NODAL$AGE_Recode=NULL
NODAL[NODAL$AGE <50 ,'AGE_Recode']                 = "<50"
NODAL[NODAL$AGE <65 & NODAL$AGE >=50,'AGE_Recode'] = "50-65"
NODAL[NODAL$AGE >=65,'AGE_Recode']                 = "> or = 65"


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
NODAL[is.na(NODAL$LYMPH_VASCULAR_INVASION) ,'LYMPH_VASCULAR_INVASION_Recode'] = "Unknown/Indeterminate"


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
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==0 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "No systemic therapy"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==2 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Neoadjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==3 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Adjuvant"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==4 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Sandwich"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==5 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Intraoperative"
NODAL[NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ ==6 & !is.na(NODAL$RX_SUMM_SYSTEMIC_SUR_SEQ),'RX_SUMM_SYSTEMIC_SUR_SEQ_Recode'] = "Intraoperative"
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
NODAL$HISTOLOGY_Recode = NULL
NODAL[NODAL$HISTOLOGY >= 8000 &  NODAL$HISTOLOGY <= 8001] = "Unknown"

NODAL[
(NODAL$HISTOLOGY >= 8004 &  NODAL$HISTOLOGY <= 8004) | 
(NODAL$HISTOLOGY >= 8031 &  NODAL$HISTOLOGY <= 8035)] = "Spindle/Giant Cell"
NODAL[NODAL$HISTOLOGY >= 8005 &  NODAL$HISTOLOGY <= 8005] = "Clear"
NODAL[NODAL$HISTOLOGY >= 8041 &  NODAL$HISTOLOGY <= 8045] = "Small Cell"

NODAL[
(NODAL$HISTOLOGY >= 8010 &  NODAL$HISTOLOGY <= 8010) |
(NODAL$HISTOLOGY >= 8020 &  NODAL$HISTOLOGY <= 8022) |
(NODAL$HISTOLOGY >= 8230 &  NODAL$HISTOLOGY <= 8231)  ] = "Carcinoma NOS"
NODAL[(NODAL$HISTOLOGY >= 8012 &  NODAL$HISTOLOGY <= 8013) | (NODAL$HISTOLOGY == 8046)] = "Large Cell"
NODAL[NODAL$HISTOLOGY >= 8000 &  NODAL$HISTOLOGY <= 8001] = "Unknown"

NODAL[NODAL$HISTOLOGY >= 8050 &  NODAL$HISTOLOGY <= 8052] = "Papilloma"

NODAL[NODAL$HISTOLOGY >= 8070 &  NODAL$HISTOLOGY <= 8076] = "Squamous Cell"

NODAL[NODAL$HISTOLOGY >= 8082 &  NODAL$HISTOLOGY <= 8082] = "Lymphoepithelial"
NODAL[NODAL$HISTOLOGY >= 8123 &  NODAL$HISTOLOGY <= 8123] = "Basoloid"

NODAL[NODAL$HISTOLOGY >= 8140 &  NODAL$HISTOLOGY <= 8211] = "Adenocarcinoma"

# work in progress
# then print the mean median and mode of the LN
# then apply this to the timing study 



# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_STAGE_GROUP","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_STAGE_GROUP","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode'))

table1 <- CreateTableOne(vars = listVars, data = NODAL, factorVars = catVars, strata = 'NODE_CAT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/Timing_TableOne_1_25_2017_Allpatients_Nodes.csv')
 
 
# 3) Among N1mi patients: create a table with PMRT as one column and no PMRT as second column and compare patient characteristics such as T stage, grade, LVI, age, chemo, endocrine therapy, margins etc.
    # create this comparison table
########################## Create the new data set ################

# include only patients treated with RT after Surgery

N1mi_P <- NODAL[(NODAL$TNM_PATH_N =='1MI'), ]
# descriptive statistics
listVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode','CROWFLY'))

catVars<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR_Recode','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD_Recode','FACILITY_TYPE_CD_Recode'))

table1 <- CreateTableOne(vars = listVars, data = N1mi_P, factorVars = catVars, strata = 'PMRT',includeNA=TRUE)

write.csv(print(table1),'H:/Gerber/NCDB/N1Mi_TableOne_1_28_2017_PMRTN1Mic.csv')
 
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


write.csv(print(table1),'H:/Gerber/NCDB/Timing_TableOne_1_28_2017_ScopeLN.csv')
 
 ############################################## COX MODEL ###############
 
attach(N1mi_P)

# univariate

#find the follow up and censor time
followup_time = N1mi_P["DX_LASTCONTACT_DEATH_MONTHS"][[1]]
censor  = N1mi_P["PUF_VITAL_STATUS"][[1]]
censor = 1-censor 

factors<-(c("AGE_Recode","YEAR_OF_DIAGNOSIS_Recode","TNM_PATH_T_Recode","TNM_PATH_N_Recode","RACE_Recode","GRADE_Recode","ERSTATUS","LYMPH_VASCULAR_INVASION_Recode",'CDCC_TOTAL_Recode','RX_SUMM_CHEMO_Recode','RX_SUMM_SYSTEMIC_SUR_SEQ_Recode','RX_SUMM_HORMONE_Recode','REGIONAL_NODES_EXAMINED_Recode','REGIONAL_NODES_EXAMINED_Recode_2','REGIONAL_NODES_POSITIVE_Recode','REGIONAL_NODES_POSITIVE_Recode_2','RX_SUMM_SURGICAL_MARGINS_Recode','RX_SUMM_SCOPE_REG_LN_SUR','SCOPE_REG_LN_2012_Recode','PMRT','FACILITY_LOCATION_CD','FACILITY_TYPE_CD','CROWFLY'))

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ (AGE_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(YEAR_OF_DIAGNOSIS_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(droplevels(TNM_PATH_T_Recode)))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(NODE_CAT))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RACE_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(GRADE_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(ERSTATUS))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(LYMPH_VASCULAR_INVASION_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_CHEMO_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SYSTEMIC_SUR_SEQ_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_HORMONE_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ (REGIONAL_NODES_EXAMINED_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(REGIONAL_NODES_EXAMINED_Recode_2))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ (REGIONAL_NODES_POSITIVE_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(REGIONAL_NODES_POSITIVE_Recode_2))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SURGICAL_MARGINS_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(RX_SUMM_SCOPE_REG_LN_SUR))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(SCOPE_REG_LN_2012_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(PMRT))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_LOCATION_CD_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(FACILITY_TYPE_CD_Recode))

coxmodel

allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ (CROWFLY))

coxmodel

x11()

subset <- survfit(Surv(followup_time, censor)~RX_SUMM_RADIATION)
plot(subset, xlab="Months", ylab="Survival Probability (%)", main='Overall Survival \n of N1mi Tumors by PMRT',lwd=4,col=c(1:length(names(subset$strata))),lty=1,cex.axis = 2,cex.lab = 1.5,cex.main = 2)
legend(80, 0.2, c("No PMRT","PMRT"),lwd=4,col=c(1:length(names(subset$strata))),lty=2)


# create survival object
allpts <- survfit(Surv(followup_time, censor, type='right')~ 1)
coxmodel <- coxph(Surv(followup_time, censor)~ as.factor(AGE_2)+as.factor(CDCC_TOTAL)+ as.factor(RX_SUMM_RADIATION) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(droplevels(TNM_PATH_STAGE_GROUP)) + as.factor(RACE_2) + as.factor(GRADE) + as.factor(ERSTATUS) + as.factor(LYMPH_VASCULAR_INVASION) + as.factor(RX_SUMM_SURGICAL_MARGINS) + CROWFLY + as.factor(FACILITY_LOCATION_CD) + as.factor(FACILITY_TYPE_CD))