# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("filtered_data_test.csv", sep = ",", na = c('NA','-77', '-99', '-66'))
view(mydata)  


names(mydata)

#. Rename variables ----

#General variables
mydata <- reshape::rename(mydata, c("lfdn" = "userID",  #!!!Attention: check, whether this really corresponds to how ppt ID is labelled
                                    
                                    "mogWFH" = "WFHmog",
                                    "workPct" = "Wpct",
                                    "curWork" = "Wcur",
                                    "leadPos" = "Wlead",
                                    "freelanc" = "FREELANCE",
                                    
                                    "timeOrg" = "ORGtime",
                                    "sectOrg" = "ORGsect",
                                    "sizeOrg" = "ORGsize",
                                    
                                    "hourWeek" = "Whour",
                                    "opiWFH" = "WFHopi",
                                    "flexDay" = "FLEXd",
                                    "flexHour" = "FLEXh",
                                    
                                    "comDay" = "COMUd",
                                    "timeHin" = "COMUhin",
                                    "timeRuc" = "COMUruc",
                                    "auto" = "COMU1",
                                    "beifahre" = "COMU2",
                                    "bus" = "COMU3",
                                    "zug" = "COMU4",
                                    "velo" = "COMU5",
                                    "fuss" = "COMU6",
                                    "zweirad" = "COMU7",
                                    "v_261" = "COMU8",
                                    
                                    "optWFH" = "WFHwish",
                                    "wfh_Mon" = "WFHwish_m",
                                    "wfh_Tue" = "WFHwish_tu",
                                    "wfh_Wed" = "WFHwish_w",
                                    "wfh_Thu" = "WFHwish_th",
                                    "wfh_Fri" = "WFHwish_f",
                                    "wfh_sat" = "WFHwish_sa",
                                    "wfh_Sun" = "WFHwish_su",
                                    "wfh_oth" = "WFHwish_non",
                                    
                                    "cruicial" = "WFHcruicial",
                                    
                                    "dayWFH" = "WFHday_o",
                                    "realWFH" = "WFHday_r",
                                    "daySite" = "AGday_r",
                                    "decMaker" = "WFHdec",
                                    
                                    "ort_Home" = "WFHloc1",
                                    "ort_Cafe" = "WFHloc2",
                                    "ort_Share" = "WFHloc3",
                                    "ort_Oth" = "WFHloc4",
                                    "v_263" = "WFHloc5",
                                    
                                    "normWFH" = "WFHnorm",
                                    
                                    "negAf" = "AFn",
                                    "posAf" = "AFp",
                                    
                                    "lonely" = "LONE",
                                    
                                    "absent" = "ABSENT",
                                    "present" = "PRESENT",
                                    
                                    "advWFH1" = "WFHadv1",
                                    "advWFH2" = "WFHadv2",
                                    "advWFH3" = "WFHadv3",
                                    "advWFH4" = "WFHadv4",
                                    "advWFH5" = "WFHadv5",
                                    "advWFH6" = "WFHadv6",
                                    "advWFH7" = "WFHadv7",
                                    "advWFH8" = "WFHadv8",
                                    "advWFH_other" = "WFHadv9",
                                    "advWFH_text" = "WFHadv10",
                                    
                                    "advBuro1" = "AGadv1",
                                    "advBuro2" = "AGadv2",
                                    "advBuro3" = "AGadv3",
                                    "advBuro4" = "AGadv4",
                                    "advBuro5" = "AGadv5",
                                    "advBuro_other" = "AGadv6",
                                    "advBuro_text" = "AGadv7",
                                    
                                    "aussWFH" = "WFHauss",
                                    "ergWFH1" = "WFHerg1", 
                                    "ergWFH2" = "WFHerg2",
                                    "ergWFH3" = "WFHerg3",
                                    
                                    "ergO1" = "AGerg1",
                                    "ergO2" = "AGerg2", 
                                    "ergO3" = "AGerg3",
                                    
                                    "deskShar" = "AGdesk",
                                    
                                    "healWFH1" = "WFHhealth1",
                                    "healWFH2" = "WFHhealth2", 
                                    "healWFH3" = "WFHhealth3",
                                    
                                    "effWFH1" = "WFHfuture1",
                                    "effWFH2" = "WFHfuture2",
                                    "effWFH3" = "WFHfuture3", 
                                    "effWFH4" = "WFHfuture4",
                                    "effWFH5" = "WFHfuture5",
                                    "effWFH6" = "WFHfuture6",
                                    "effWFH7" = "WFHfuture7",
                                    "v_262" = "WFHfuture8",
                                    
                                    "covid1" = "WFHexp1",
                                    "covid2" = "WFHexp2", 
                                    
                                    "agWFH1" = "WFHculture1",
                                    "agWFH2" = "WFHculture2",
                                    "agWFH3" = "WFHculture3",
                                    
                                    "nation" = "NATION",
                                    "education" = "EDUCATION",
                                    "salary" = "BRUTTO",
                                    "region" = "REGION",
                                    "lifeSit1" = "LIVING1", 
                                    "lifeSit2" = "LIVING2",
                                    "lifeSit3" = "LIVING3",
                                    "lifeSit4" = "LIVING4",
                                    "lifeSit5" = "LIVING5", 
                                    "lifeSit6" = "LIVING6",
                                    "hausPer" = "HOUSEH1",
                                    "hausKind" = "HOUSEH2",
                                    "ageKind" = "HOUSEH3"
))

#Exhaustion
mydata <- reshape::rename(mydata, c("exh1" = "EXH1", 
                                    "exh2" = "EXH2",
                                    "exh3" = "EXH3",
                                    "exh4" = "EXH4"))

#Overall Justice
mydata <- reshape::rename(mydata, c("JOv1" = "JO1", 
                                    "JOv2" = "JO2",
                                    "JOv3" = "JO3"))



#JOD preferences
mydata <- reshape::rename(mydata, c("JODpref1" = "JODP1", 
                                    "JODpref2" = "JODP2",
                                    "JODpref3" = "JODP3"))


#Procedural justice
mydata <- reshape::rename(mydata, c("dupl1_JOP1" = "JOP1"))

#Interpersonal and informational justice
mydata <- reshape::rename(mydata, c("JSI1" = "JSInp", 
                                    "JSI2" = "JSInf"))

#Satisfaction
mydata <- reshape::rename(mydata, c("workSat" = "SAT"))

#Commitment
mydata <- reshape::rename(mydata, c("orgAC" = "COM"))

#Trust
mydata <- reshape::rename(mydata, c("trustS" = "TRUSTs", 
                                    "trustO" = "TRUSTo"))

names(mydata)


#. Recode incorrectly labelled scales ----

#!!! Attention:check this again

mydata$JOD1r <- plyr::revalue(as.character(mydata$JOD1), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) #assuming "old value" = "new value"
mydata$JOD2r <- plyr::revalue(as.character(mydata$JOD2), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 
mydata$JOD3r <- plyr::revalue(as.character(mydata$JOD3), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JODfr <- plyr::revalue(as.character(mydata$JODf), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 

mydata$JODP1r <- plyr::revalue(as.character(mydata$JODP1), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JODP2r <- plyr::revalue(as.character(mydata$JODP2), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JODP3r <- plyr::revalue(as.character(mydata$JODP3), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JOP1r <- plyr::revalue(as.character(mydata$JOP1), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JOP2r <- plyr::revalue(as.character(mydata$JOP2), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JOP3r <- plyr::revalue(as.character(mydata$JOP3), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JOP4r <- plyr::revalue(as.character(mydata$JOP4), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JOPfr <- plyr::revalue(as.character(mydata$JOPf), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JSInfr <- plyr::revalue(as.character(mydata$JSInf), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 
mydata$JSInpr <- plyr::revalue(as.character(mydata$JSInp), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JSIfr <- plyr::revalue(as.character(mydata$JSIf), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "7" = "6")) 

mydata$JO1r <- plyr::revalue(as.character(mydata$JO1), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 
mydata$JO2r <- plyr::revalue(as.character(mydata$JO2), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 
mydata$JO3r <- plyr::revalue(as.character(mydata$JO3), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 

#Salome code to recode everything linked to days

#Comuting days
mydata$COMUdr <- mydata$COMUd - 1

#Wished number of WFH days
new_values <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7)
mydata$WFHwishr <- new_values[mydata$WFHwish]

#WFH days allowed
new_values <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7)
mydata$WFHday_or <- new_values[mydata$WFHday_o]

#real WFH days 
new_values <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7)
mydata$WFHday_rr <- new_values[mydata$WFHday_r]

#Office days
new_values <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7)
mydata$AGday_rr <- new_values[mydata$AGday_r]





names(mydata)

#***********************************************************************
# 02 Missing values, duplicated cases, and outliers ----


#. Check and delete duplicated cases again ----
#library(dplyr)
duplicates <- mydata %>% 
  group_by(p_0001) %>% 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates)) # Number of duplicates (0)

# Data frame without duplicates

mydata <- mydata %>% distinct(p_0001,.keep_all = T)

names(mydata)


#. Specify "Ich weiss es nicht" or "Betrifft mich nicht" as missing values
mydata <- mydata %>%
  mutate(
    ORGsect = ifelse(ORGsect == 15, NA, ORGsect), 
    ORGsize = ifelse(ORGsize == 8, NA, ORGsize),
    WFHopi = ifelse(WFHopi == 6, NA, WFHopi),
    WFHnorm = ifelse(WFHnorm == 6, NA, WFHnorm),
    EDUCATION = ifelse(EDUCATION == 8, NA, EDUCATION),
    WFHexp1 = ifelse(WFHexp1 == 7, NA, WFHexp1),
    BRUTTO = ifelse(BRUTTO == 10, NA, BRUTTO),
    LONE = ifelse(LONE == 6| LONE==7, NA, LONE),
    TRUSTo = ifelse(TRUSTo == 6, NA, TRUSTo),
    TRUSTs = ifelse(TRUSTs == 6, NA, TRUSTs),
    WFHdec = ifelse(WFHdec == 5, NA, WFHdec),
    
    JOD1r = ifelse(JOD1r == 6, NA, JOD1r),
    JOD2r = ifelse(JOD2r == 6, NA, JOD2r),
    JOD3r = ifelse(JOD3r == 6, NA, JOD3r),
    JODfr = ifelse(JODfr == 6, NA, JODfr),
    
    JODP1r = ifelse(JODP1r == 6, NA, JODP1r),
    JODP2r = ifelse(JODP2r == 6, NA, JODP2r),
    JODP3r = ifelse(JODP3r == 6, NA, JODP3r),
    
    JOP1r = ifelse(JOP1r == 6, NA, JOP1r),
    JOP2r = ifelse(JOP2r == 6, NA, JOP2r),
    JOP3r = ifelse(JOP3r == 6, NA, JOP3r),
    JOP4r = ifelse(JOP4r == 6, NA, JOP4r),
    JOPfr = ifelse(JOPfr == 6, NA, JOPfr),
    
    JSInpr = ifelse(JSInpr == 6, NA, JSInpr),
    JSInfr = ifelse(JSInfr == 6, NA, JSInfr),
    JSIfr = ifelse(JSIfr == 6, NA, JSIfr),
    
    JO1r = ifelse(JO1r == 6, NA, JO1r),
    JO2r = ifelse(JO2r == 6, NA, JO2r),
    JO3r = ifelse(JO3r == 6, NA, JO3r),
  )
sum(is.na(mydata))
#describe(mydata$LONE) #check whether LONE was recoded correctly --> seems to work! 


write.csv(mydata, "clean_recoded.csv", row.names = TRUE)