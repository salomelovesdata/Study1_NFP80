#DISCLAIMER : 
# IF YOU ARE USING THE RAW DATA (DIRECTLY EXPORTED FROM TIVIAN) - DO NOT RUN ALL THE CODE AT ONCE. ONLY ONE MODULE AT ONCE.
# ALWAYS RUN STEP 1 AND 2 BEFORE RUNNING ANY OTHER STEP.
# IF YOU ALREADY HAVE THE "CLEAN_RECODED.CSV" FILE, GO DIRECTLY TO STEP 3 AND FURTHER ON.

#Number 1 step : remove careless responders
source("PageTime_cleancode.R")
#you now have a new dataset called "filtered_data.csv"

#Number 2 : recode, rename, treat NAs etc
source("recoding and renaming file_get clean file.R")
#you now have a new dataset called "clean_recoded.csv"


#Number 3 : facultative - Get descriptives
source("Descriptives.R")

#Number 4 : facultative - Get correlations
source("correlations.R")

#Number 5 : facultative - Get frequencies
source("frequencies.R")

#Number 6 : facultative - Get MI
source("Measurement Invariance_languages.R")

#Number 7 : Scale building - Omega and CFA
source("Scale building_Omega_CFA.R")

#to be continued, for example with regressions...



