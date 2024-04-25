#Anne
#R code book collection
#***********************************************************************
#00 data frame labels with description ---- 

mydata #full (initial) data set 
mydata.nodup #data set without duplicates (build from mydata)
mydata.nomiss #data set without missing values (build from mydata.nodup)
mydata.nooutlier #data set without outliers (build from mydata.nodup) -> does this need to be built from mydata.nomissing instead? 
mydata.selected #data set with selected variables for my analysis (build from mydata.nooutlier)
mydata.descriptives #data set for descriptive analyses 

#data sets according to filter (built from mydata.valid)
mydata.full & mydata.full.valid
mydata.valid  & mydata.valid.valid 
mydata.noWFH & mydata.noWFH.valid 
mydata.couldbutnoWFH & mydata.couldbutnoWFH.valid
mydata.noAG & mydata.noAG.valid
mydata.nochildren & mydata.nochildren.valid
mydata.freel & mydata.freel.valid
mydata.alone & mydata.alone.valid
mydata.noWFH.freel & mydata.noWFH.freel.valid

mydata.valid #data set with only ppts who have valid page times in entire survey (built from merging filter data frames)
mydata.scale #data set with scales (built from mydata.valid)

#***********************************************************************
#01 set up ----

# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("15_4_2024.csv", sep = ";", na = c('NA','-77', '-99', '-66'))
view(mydata)  


mydata <- mydata %>% 
  filter(lfdn > 48) #Retrieve participants from before launch)

#. Rename variables ----

#!!! Logik: alles im Zusammenhang mit WFH beginnt mit WFH, alles im Zusammenhang mit Arbeiten vor Ort beginnt mit AG ('Arbeitgeber'), us. 
#!!! Attention: check whether original names are correct 

#General variables
mydata <- reshape::rename(mydata, c("lfdn" = "userID",  #!!!Attention: check, whether this really corresponds to how ppt ID is labelled
                                    
                                    "mogWFH" = "WFHmog",
                                    "workPct" = "Wpct",
                                    "curWork" = "Wcur",
                                    "leadPos" = "Wlead",
                                    "freelanc" = "ORGtime",
                                    
                                    "timeOrg" = "ORGsect",
                                    "sectOrg" = "ORGsize",
                                    
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
#!!! Question: here, recode into same or different variable? 

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

mydata$JO1r <- plyr::revalue(as.character(mydata$JO1), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 
mydata$JO2r <- plyr::revalue(as.character(mydata$JO2), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 
mydata$JO3r <- plyr::revalue(as.character(mydata$JO3), c("2" = "1", "3" = "2", "4" = "3", "5" = "4", "6" = "5", "1" = "6")) 

names(mydata)
#***********************************************************************
# 02 Missing values, duplicated cases, and outliers ----

#. Check and delete duplicated cases ----
#library(dplyr)
duplicates <- mydata %>% 
  group_by(userID) %>% #here I used 'userID' instead of 'p0001' Correct? 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates)) # Number of duplicates (0)

# Data frame without duplicates
mydata.nodup <- mydata %>% distinct(userID,.keep_all = T)
names(mydata.nodup)


#. Check and delete missing values ----

#!!! Question: Hier bin ich ein wenig überfragt, was die Reihenfolge der Schritte angeht... Außerdem: Wollen wir NA cases löschen? 

#Identifying which cases have missing values 
mydata.nodup[!complete.cases(mydata.nodup),]

# find location of missing values
which(is.na(mydata.nodup))

# count total missing values 
sum(is.na(mydata.nodup))
colSums(is.na(mydata.nodup)) #counts per column

#delete cases with missing values 
#mydata.nomissing <- na.omit(mydata.nodup) #listwise deletion

#Specify "Ich weiss es nicht" or "Betrifft mich nicht" as missing values
mydata.nodup <- mydata.nodup %>%
  mutate(
    sectOrg = ifelse(ORGsect == 15, NA, ORGsect), 
    sizeOrg = ifelse(ORGsize == 8, NA, ORGsize),
    opiWFH = ifelse(WFHopi == 6, NA, WFHopi),
    decMaker = ifelse(WFHdec == 5, NA, WFHdec),
    normWFH = ifelse(WFHnorm == 6, NA, WFHnorm),
    
    JOD1r = ifelse(JOD1r == 6, NA, JOD1r),
    JOD2r = ifelse(JOD2r == 6, NA, JOD2r),
    JOD3r = ifelse(JOD3r == 6, NA, JOD3r),
    JODfr = ifelse(JODfr == 6, NA, JODfr),
    
    JODpref1r = ifelse(JODP1r == 6, NA, JODP1r),
    JODpref2r = ifelse(JODP2r == 6, NA, JODP2r),
    JODpref3r = ifelse(JODP3r == 6, NA, JODP3r),
    
    JOP1r = ifelse(JOP1r == 6, NA, JOP1r),
    JOP2r = ifelse(JOP2r == 6, NA, JOP2r),
    JOP3r = ifelse(JOP3r == 6, NA, JOP3r),
    JOP4r = ifelse(JOP1r == 6, NA, JOP1r),
    JOPfr = ifelse(JOPfr == 6, NA, JOPfr),
    
    JSDInpr = ifelse(JSInpr == 6, NA, JSInpr),
    JSDInfr = ifelse(JSInfr == 6, NA, JSInfr),
    JSIfr = ifelse(JSIf == 6, NA, JSIf),
    
    JO1r = ifelse(JO1r == 6, NA, JO1r),
    JO2r = ifelse(JO2r == 6, NA, JO2r),
    JO3r = ifelse(JO3r == 6, NA, JO3r),
    )
sum(is.na(mydata.nodup))

#. Remove outliers from entire data set ----

#library(dplyr)
mydata.nooutlier <- mydata.nodup %>% 
  mutate(across(
    where(is.numeric),
    ~ ifelse(
      abs(as.numeric(scale(.x))) > 3,
      NA, 
      .x
    )
  ))

#***********************************************************************
#03 TO CHECK: Select variables relevant for my analyses ----

#!! Attention: Which variables to select here is still to be defined/debated

#. Descripitive analysis variables ---- 

mydata.descriptives <- mydata.nooutlier %>% select(c(userID,                     
                                                     p_0001,                    
                                                     p_0002,
                                                     p_0003,
                                                     p_0004,
                                                     p_0005,                   
                                                     WFHmog,
                                                     Wpct,
                                                     Wcur,
                                                     Wlead,
                                                     ORGtime,
                                                     ORGsect,
                                                     ORGsize,
                                                     sizeOrg,                   
                                                     Whour,
                                                     WFHopi,
                                                     FLEXd,
                                                     FLEXh,
                                                     COMUd,
                                                     COMUhin,
                                                     COMUruc,
                                                     COMU1,
                                                     COMU2,
                                                     COMU3,
                                                     COMU4,
                                                     COMU5,
                                                     COMU6,
                                                     COMU8,
                                                     WFHwish,
                                                     WFHwish_m,
                                                     WFHwish_tu,
                                                     WFHwish_w,
                                                     WFHwish_th,
                                                     WFHwish_f,
                                                     WFHwish_sa,
                                                     WFHwish_su,
                                                     WFHwish_non,
                                                     crucial,
                                                     WFHday_o,
                                                     WFHday_r,
                                                     AGday_r,
                                                     WFHdec,
                                                     WFHloc1,
                                                     WFHloc2,
                                                     WFHloc3,
                                                     WFHloc4,
                                                     WFHnorm,
                                                     JOD1,
                                                     JOD3,
                                                     JOD2,
                                                     JODf,
                                                     JODP1,
                                                     JODP3,
                                                     JODP2,
                                                     JOP1,
                                                     JOP2,
                                                     JOP3,
                                                     JOP4,
                                                     JOPf,
                                                     JSInp,
                                                     JSInf,
                                                     JSIf,
                                                     SDTr1,
                                                     SDTr2,
                                                     SDTr3,
                                                     SDTr4,
                                                     SDTc1,
                                                     SDTc2,
                                                     SDTc3,
                                                     SDTc4,
                                                     SDTa1,
                                                     SDTa2,
                                                     SDTa3,
                                                     SDTa4,
                                                     AFn,
                                                     AFp,
                                                     EXH1,
                                                     EXH2,
                                                     EXH3,
                                                     EXH4,
                                                     WLB,
                                                     SAT,
                                                     COM,
                                                     TRUSTs,
                                                     TRUSTo,
                                                     LONE,
                                                     ABSENT,
                                                     PRESENT,
                                                     WFHadv1,
                                                     WFHadv2,
                                                     WFHadv3,
                                                     WFHadv4,
                                                     WFHadv5,
                                                     WFHadv6,
                                                     WFHadv7,
                                                     WFHadv8,
                                                     WFHadv9,
                                                     WFHadv10,
                                                     AGadv1,
                                                     AGadv2,
                                                     AGadv3,
                                                     AGadv4,
                                                     AGadv5,
                                                     AGadv6,
                                                     AGadv7,
                                                     WFHauss,
                                                     WFHerg1,
                                                     WFHerg2,
                                                     WFHerg3,
                                                     AGerg1,
                                                     AGerg2,
                                                     AGerg3,
                                                     AGdesk,
                                                     WFHhealth1,
                                                     WFHhealth2,
                                                     WFHhealth3,
                                                     WFHfuture1,
                                                     WFHfuture2,
                                                     WFHfuture3,
                                                     WFHfuture4,
                                                     WFHfuture5,
                                                     WFHfuture6,
                                                     WFHfuture7,
                                                     WFHfuture8,
                                                     WFHexp1,
                                                     WFHexp2,
                                                     WFHculture1,
                                                     WFHculture2,              
                                                     WFHculture3,
                                                     NATION,
                                                     EDUCATION,
                                                     BRUTTO,
                                                     REGION,
                                                     LIVING1,
                                                     LIVING2,
                                                     LIVING3,
                                                     LIVING4,
                                                     LIVING5,
                                                     LIVING6,
                                                     HOUSEH1,
                                                     HOUSEH2,
                                                     HOUSEH3,
                                                     JO1,
                                                     JO2,
                                                     JO3,
                                                     JOD1r,
                                                     JOD2r,
                                                     JOD3r,
                                                     JODfr,
                                                     JODP1r,
                                                     JODP2r,
                                                     JODP3r,
                                                     JOP1r,
                                                     JOP2r,
                                                     JOP3r,
                                                     JOP4r,
                                                     JOPfr,
                                                     JSInfr,
                                                     JSInpr,
                                                     JO1r,
                                                     JO2r,
                                                     JO3r
                                                     )) 

summary (mydata.descriptives)
view(mydata.descriptives) 

#. Main analysis variables ----

mydata.selected <- mydata.nooutlier %>% select(c(userID, 
                                                 p_0001,
                                                 p_0002,
                                                 p_0003,
                                                 p_0004,
                                                 p_0005,                   
                                                 WFHmog,
                                                 Wpct,
                                                 Wcur,
                                                 Wlead,
                                                 ORGtime,
                                                 ORGsect,
                                                 ORGsize,
                                                 sizeOrg,                   
                                                 Whour,
                                                 WFHopi,
                                                 FLEXd,
                                                 FLEXh,
                                                 COMUd,
                                                 COMUhin,
                                                 COMUruc,
                                                 COMU1,
                                                 COMU2,
                                                 COMU3,
                                                 COMU4,
                                                 COMU5,
                                                 COMU6,
                                                 COMU8,
                                                 WFHwish,
                                                 WFHwish_m,
                                                 WFHwish_tu,
                                                 WFHwish_w,
                                                 WFHwish_th,
                                                 WFHwish_f,
                                                 WFHwish_sa,
                                                 WFHwish_su,
                                                 WFHwish_non,
                                                 crucial,
                                                 WFHday_o,
                                                 WFHday_r,
                                                 AGday_r,
                                                 WFHdec,
                                                 WFHloc1,
                                                 WFHloc2,
                                                 WFHloc3,
                                                 #WFHloc4,
                                                 WFHnorm,
                                                 JOD1,
                                                 JOD3,
                                                 JOD2,
                                                 JODf,
                                                 JODP1,
                                                 JODP3,
                                                 JODP2,
                                                 JOP1,
                                                 JOP2,
                                                 JOP3,
                                                 JOP4,
                                                 JOPf,
                                                 JSInp,
                                                 JSInf,
                                                 JSIf,
                                                 SDTr1,
                                                 SDTr2,
                                                 SDTr3,
                                                 SDTr4,
                                                 SDTc1,
                                                 SDTc2,
                                                 #QC1,
                                                 SDTc3,
                                                 SDTc4,
                                                 SDTa1,
                                                 SDTa2,
                                                 SDTa3,
                                                 SDTa4,
                                                 AFn,
                                                 AFp,
                                                 EXH1,
                                                 EXH2,
                                                 EXH3,
                                                 EXH4,
                                                 WLB,
                                                 SAT,
                                                 COM,
                                                 TRUSTs,
                                                 TRUSTo,
                                                 LONE,
                                                 ABSENT,
                                                 PRESENT,
                                                 WFHadv1,
                                                 WFHadv2,
                                                 WFHadv3,
                                                 WFHadv4,
                                                 WFHadv5,
                                                 WFHadv6,
                                                 WFHadv7,
                                                 WFHadv8,
                                                 #WFHadv9,
                                                 #WFHadv10,
                                                 AGadv1,
                                                 AGadv2,
                                                 AGadv3,
                                                 AGadv4,
                                                 AGadv5,
                                                 #AGadv6,
                                                 #AGadv7,
                                                 WFHauss,
                                                 WFHerg1,
                                                 WFHerg2,
                                                 WFHerg3,
                                                 AGerg1,
                                                 AGerg2,
                                                 AGerg3,
                                                 AGdesk,
                                                 WFHhealth1,
                                                 WFHhealth2,
                                                 WFHhealth3,
                                                 WFHfuture1,
                                                 WFHfuture2,
                                                 WFHfuture3,
                                                 WFHfuture4,
                                                 WFHfuture5,
                                                 WFHfuture6,
                                                 #WFHfuture7,
                                                 #WFHfuture8,
                                                 WFHexp1,
                                                 WFHexp2,
                                                 WFHculture1,
                                                 WFHculture2,              
                                                 WFHculture3,
                                                 NATION,
                                                 EDUCATION,
                                                 BRUTTO,
                                                 REGION,
                                                 LIVING1,
                                                 LIVING2,
                                                 LIVING3,
                                                 LIVING4,
                                                 LIVING5,
                                                 LIVING6,
                                                 HOUSEH1,
                                                 HOUSEH2,
                                                 HOUSEH3,
                                                 JO1,
                                                 JO2,
                                                 JO3,
                                                 rts276638,
                                                 rts276640,
                                                 rts276644,
                                                 rts276645,
                                                 rts276663,
                                                 rts276891,
                                                 rts276892,
                                                 rts276894,
                                                 rts276896,
                                                 rts276897,
                                                 rts276898,
                                                 rts276915,
                                                 rts276916,
                                                 rts276921,
                                                 rts276922,
                                                 rts276923,
                                                 rts276940,
                                                 rts276941,
                                                 rts276943,
                                                 rts276944,
                                                 rts276946,
                                                 rts276947,
                                                 rts277030,
                                                 rts277059,
                                                 rts277060,
                                                 rts277376,
                                                 rts280445,
                                                 rts280451,
                                                 rts280452,
                                                 rts280455,
                                                 JOD1r,
                                                 JOD2r,
                                                 JOD3r,
                                                 JODfr,
                                                 JODP1r,
                                                 JODP2r,
                                                 JODP3r,
                                                 JOP1r,
                                                 JOP2r,
                                                 JOP3r,
                                                 JOP4r,
                                                 JOPfr,
                                                 JSInfr,
                                                 JSInpr,
                                                 JO1r,
                                                 JO2r,
                                                 JO3r 
                                                 )) 

summary (mydata.selected)
view(mydata.selected)

#***********************************************************************
#04 Page time screening (2secs/item) ----

#Filter pages (not displayed to ppts) are: PGID 276642 (Seite 5), PGID 280452 (Seite 10.1), PGID 276917 (Seite 13), PGID 276942 (Seite 20), PGID 283970 (Seite 21), PGID 280446 (Seite 22), PGID 276945 (Seite 23), PGID 277029 (Seite 26.1), PGID 276890 (Seite 26.2)
#Text pages are: PGID 276638 (Seite 3), PGID 276893 (Seite 9), PGID 283969 (Seite 10), PGID 280451 (Seite 11), PGID 280455 (Seite 18), PGID 277376 (Seite 25)


#. Create data frames with only subgroups of ppts by filter ----

#!!! Question: Do I have to do even more combinations here? Or does 'the problem' solve itself, when I later on merge it by ID and delete duplicates? 

#Ppts, who filled in the full survey (meaning, no filter applies)
#no pages filtered out -> no page has PT = '0'



#################### Salome



# Function to calculate page time for each participant - takes the page_history column as input 
# and returns a dataframe with the page_id and page_time columns.




get_min_allowed_time_spent_on_page <- function(page_number) {
  # Here you would define the comparison value for each page_number
  # For example:
  retval = 0
  
  #print(page_number)

  if (page_number == "276640") {retval = 4 }
  else if (page_number == "276644") {retval = 16 }
  else if (page_number == "276645") {retval = 10 }
  else if (page_number == "276892") {retval = 8 }
  else if (page_number == "276894") {retval = 6 }
  else if (page_number == "276940") {retval = 9 }
  else if (page_number == "276897") {retval = 10 }
  else if (page_number == "276808") {retval = 7 }
  else if (page_number == "276915") {retval = 12 }
  else if (page_number == "277059") {retval = 8 }
  else if (page_number == "276916") {retval = 27 }
  else if (page_number == "276921") {retval = 5 }
  else if (page_number == "277060") {retval = 9 }
  else if (page_number == "276922") {retval = 13 }
  else if (page_number == "276923") {retval = 4 }
  else if (page_number == "276941") {retval = 4 }
  else if (page_number == "276943") {retval = 10 }
  else if (page_number == "276944") {retval = 9 }
  else if (page_number == "280445") {retval = 9 }
  else if (page_number == "276946") {retval = 4 }
  else if (page_number == "276947") {retval = 6 }
  else if (page_number == "276663") {retval = 10 }
  else if (page_number == "277030") {retval = 4 }
  else if (page_number == "276891") {retval = 2 }
  else if (page_number == "276896") {retval = 8 }
  
  
  return(retval)
}



# compute_failing_pages <- function(page_history) {
get_page_ids_history <- function(x, output) {
  
  
  page_ids_history = x["page_history"]$page_history
  page_ids_history = strsplit(page_ids_history, ",")
  
  
  timestamp_on_previous_page <- 0
  # page_id is just a name for elements within the list of lists
  for(page_id in page_ids_history) {
    # id is just aa name for elements within the inner list
    for (id in page_id)
      {
      time_spent_on_page <- 0
      min_allowed_time_spent_on_page <- get_min_allowed_time_spent_on_page(id)
      timestamp_on_current_page <- x[paste("rts", id, sep="")][[1]]
      
      if(! is.null(timestamp_on_current_page)) {
        time_spent_on_page <- timestamp_on_current_page - timestamp_on_previous_page
        timestamp_on_previous_page <- timestamp_on_current_page
        if(time_spent_on_page < min_allowed_time_spent_on_page) {
          x[paste("enough_time_on_", id, sep="")] <- 1
        
        }
        
      }
        
      }
  }
  
  return(x)
}


test_data = mydata

new_cols = c(
  "enough_time_on_276640",
  "enough_time_on_276644",
  "enough_time_on_276645",
  "enough_time_on_276892",
  "enough_time_on_276894",
  "enough_time_on_276940",
  "enough_time_on_276897",
  "enough_time_on_276808",
  "enough_time_on_276915",
  "enough_time_on_277059",
  "enough_time_on_276916",
  "enough_time_on_276921",
  "enough_time_on_277060",
  "enough_time_on_276922",
  "enough_time_on_276923",
  "enough_time_on_276941",
  "enough_time_on_276943",
  "enough_time_on_276944",
  "enough_time_on_280445",
  "enough_time_on_276946",
  "enough_time_on_276947",
  "enough_time_on_276663",
  "enough_time_on_277030",
  "enough_time_on_276891",
  "enough_time_on_276896"
)
test_data[, new_cols] = 0

new_list <- apply(test_data, 1, get_page_ids_history)

new_df <- as.data.frame(do.call(rbind, new_list))





#################################################


#.. Page times for ppts, who did not work onsite (filter page is PGID 283970) ----
#Filtered out page is 21.1


mydata.full$nb_failing_tests = 0
print(mydata.full$nb_failing_tests)

mydata.full[mydata.full$bool_page4 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page6 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page7 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page9.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page9.2 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page10.2 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page10.3 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page10.4 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page10.5 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page12 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page14 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page15 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page16 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page17 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page19 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page20.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page21.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page22.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page23.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page24 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page26 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.full[mydata.full$bool_page27 == "0", "nb_failing_tests"] = mydata.full[mydata.full$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.full.valid <- mydata.full[(mydata.full$nb_failing_tests>=1)]

view(mydata.full.valid)

#.. Page times for ppts, who did not work from home (filter pages are PGID 276893 and PGID 276942 and PGID 276945) ----
#Filtere out pages are 9.1, 9.2, 20.1, 23.1
mydata.noWFH <- mydata.noWFH  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page10.2 = ifelse(PT_page10.2 >= 10, 1,0),
    bool_page10.3 = ifelse(PT_page10.3 >= 7, 1,0),
    bool_page10.4 = ifelse(PT_page10.4 >= 12, 1,0), 
    bool_page10.5 = ifelse(PT_page10.5 >= 8, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.noWFH$nb_failing_tests = 0

mydata.noWFH[mydata.noWFH$bool_page4 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page6 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page7 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page10.2 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page10.3 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page10.4 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page10.5 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page12 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page14 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page15 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page16 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page17 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page19 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page21.1 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page22.1 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page24 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page26 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH[mydata.noWFH$bool_page27 == "0", "nb_failing_tests"] = mydata.noWFH[mydata.noWFH$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.noWFH.valid <- mydata.noWFH[(mydata.noWFH$nb_failing_tests>=1)]
view(mydata.noWFH.valid)

#.. Page times for ppts, who did not work from home and are freelancers (filter pages are PGID 276893 and PGID 276942 and PGID 276945 + PGID 283969) ----
#Filtere out pages are 9.1, 9.2, 20.1, 23.1 + 10.1 until 10.5 (including)
mydata.noWFH.freel <- mydata.noWFH.freel  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.noWFH.freel$nb_failing_tests = 0

mydata.noWFH.freel[mydata.noWFH.freel$bool_page4 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page6 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page7 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page12 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page14 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page15 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page16 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page17 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page19 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page21.1 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page22.1 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page24 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page26 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.noWFH.freel[mydata.noWFH.freel$bool_page27 == "0", "nb_failing_tests"] = mydata.noWFH.freel[mydata.noWFH.freel$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.noWFH.freel.valid <- mydata.noWFH.freel[(mydata.noWFH.freel$nb_failing_tests>=1)]
view(mydata.noWFH.freel.valid)

#.. Page times for ppts, who are freelancers (filter page is PGID 283969) ----
#Filtere out pages are 10.1 until 10.5 (including)
mydata.freel <- mydata.freel  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page9.1 = ifelse(PT_page9.1 >= 6, 1,0),
    bool_page9.2 = ifelse(PT_page9.2 >= 9, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page20.1 = ifelse(PT_page20.1 >= 10, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page23.1 = ifelse(PT_page23.1 >= 4, 1,0),
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.freel$nb_failing_tests = 0

mydata.freel[mydata.freel$bool_page4 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page6 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page7 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page9.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page9.2 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page12 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page14 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page15 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page16 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page17 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page19 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page20.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page21.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page22.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page23.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page24 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page26 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.freel[mydata.freel$bool_page27 == "0", "nb_failing_tests"] = mydata.freel[mydata.freel$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.freel.valid <- mydata.freel[(mydata.freel$nb_failing_tests>=1)]
view(mydata.freel.valid)

#.. Page times for ppts, who did not work onsite (filter page is PGID 283970) ----
#filtered out page is 21.1
mydata.noAG <- mydata.noAG  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page9.1 = ifelse(PT_page9.1 >= 6, 1,0),
    bool_page9.2 = ifelse(PT_page9.2 >= 9, 1,0),
    bool_page10.2 = ifelse(PT_page10.2 >= 10, 1,0),
    bool_page10.3 = ifelse(PT_page10.3 >= 7, 1,0),
    bool_page10.4 = ifelse(PT_page10.4 >= 12, 1,0), 
    bool_page10.5 = ifelse(PT_page10.5 >= 8, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page20.1 = ifelse(PT_page20.1 >= 10, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page23.1 = ifelse(PT_page23.1 >= 4, 1,0),
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.noAG$nb_failing_tests = 0

mydata.noAG[mydata.noAG$bool_page4 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page6 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page7 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page9.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page9.2 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page10.2 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page10.3 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page10.4 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page10.5 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page12 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page14 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page15 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page16 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page17 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page19 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page20.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page22.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page23.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page24 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page26 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.noAG[mydata.noAG$bool_page27 == "0", "nb_failing_tests"] = mydata.noAG[mydata.noAG$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.noAG.valid <- mydata.noAG[(mydata.noAG$nb_failing_tests>=1)]
view(mydata.noAG.valid)

#.. Page times for ppts, who could do but did not WFH (filter page is PGID 280446) ----
#Filtered out page is 22.1
mydata.couldbutnoWFH <- mydata.couldbutnoWFH  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page9.1 = ifelse(PT_page9.1 >= 6, 1,0),
    bool_page9.2 = ifelse(PT_page9.2 >= 9, 1,0),
    bool_page10.2 = ifelse(PT_page10.2 >= 10, 1,0),
    bool_page10.3 = ifelse(PT_page10.3 >= 7, 1,0),
    bool_page10.4 = ifelse(PT_page10.4 >= 12, 1,0), 
    bool_page10.5 = ifelse(PT_page10.5 >= 8, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page20.1 = ifelse(PT_page20.1 >= 10, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page23.1 = ifelse(PT_page23.1 >= 4, 1,0),
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.couldbutnoWFH$nb_failing_tests = 0

mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page4 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page6 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page7 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page9.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page9.2 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.2 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.3 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.4 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.5 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page12 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page14 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page15 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page16 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page17 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page19 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page20.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page21.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page23.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page24 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page27 == "0", "nb_failing_tests"] = mydata.couldbutnoWFH[mydata.couldbutnoWFH$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.couldbutnoWFH.valid <- mydata.couldbutnoWFH[(mydata.couldbutnoWFH$nb_failing_tests>=1)]
view(mydata.couldbutnoWFH.valid)

#.. Page times for ppts, who lived alone (filter page is PGID 277029) ----
#Filtered out page is 26.1.1
mydata.alone <- mydata.alone  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page9.1 = ifelse(PT_page9.1 >= 6, 1,0),
    bool_page9.2 = ifelse(PT_page9.2 >= 9, 1,0),
    bool_page10.2 = ifelse(PT_page10.2 >= 10, 1,0),
    bool_page10.3 = ifelse(PT_page10.3 >= 7, 1,0),
    bool_page10.4 = ifelse(PT_page10.4 >= 12, 1,0), 
    bool_page10.5 = ifelse(PT_page10.5 >= 8, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page20.1 = ifelse(PT_page20.1 >= 10, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page23.1 = ifelse(PT_page23.1 >= 4, 1,0),
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.2.1 = ifelse(PT_page26.2.1 >= 2, 1,0), 
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

mydata.alone$nb_failing_tests = 0

mydata.alone[mydata.alone$bool_page4 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page6 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page7 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page9.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page9.2 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page10.2 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page10.3 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page10.4 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page10.5 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page12 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page14 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page15 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page16 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page17 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page19 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page20.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page21.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page22.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page23.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page24 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page26 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page26.2.1 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page26.2.1 == "0", "nb_failing_tests"] + 1
mydata.alone[mydata.alone$bool_page27 == "0", "nb_failing_tests"] = mydata.alone[mydata.alone$bool_page27 == "0", "nb_failing_tests"] + 1

mydata.alone.valid <- mydata.alone[(mydata.alone$nb_failing_tests>=1)]
view(mydata.alone.valid)

#.. Page times for ppts, who had no children (filter page is PGID 276890) ----
#Filtered out page is 26.2.1
mydata.nochildren <- mydata.nochildren  %>%
  mutate(
    bool_page4 = ifelse(PT_page4 >= 4, 1,0), #for x insert page time in sec
    bool_page6 = ifelse(PT_page6 >= 16, 1,0),
    bool_page7 = ifelse(PT_page7 >= 10, 1,0),
    bool_page8 = ifelse(PT_page8 >= 8, 1,0), 
    bool_page9.1 = ifelse(PT_page9.1 >= 6, 1,0),
    bool_page9.2 = ifelse(PT_page9.2 >= 9, 1,0),
    bool_page10.2 = ifelse(PT_page10.2 >= 10, 1,0),
    bool_page10.3 = ifelse(PT_page10.3 >= 7, 1,0),
    bool_page10.4 = ifelse(PT_page10.4 >= 12, 1,0), 
    bool_page10.5 = ifelse(PT_page10.5 >= 8, 1,0),
    bool_page12 = ifelse(PT_page12 >= 27, 1,0),
    bool_page14 = ifelse(PT_page14 >= 5, 1,0),
    bool_page15 = ifelse(PT_page15 >= 9, 1,0),
    bool_page16 = ifelse(PT_page16 >= 13, 1,0), 
    bool_page17 = ifelse(PT_page17 >= 4, 1,0),
    bool_page19 = ifelse(PT_page19 >= 4, 1,0),
    bool_page20.1 = ifelse(PT_page20.1 >= 10, 1,0),
    bool_page21.1 = ifelse(PT_page21.1 >= 9, 1,0),
    bool_page22.1 = ifelse(PT_page22.1 >= 9, 1,0), 
    bool_page23.1 = ifelse(PT_page23.1 >= 4, 1,0),
    bool_page24 = ifelse(PT_page24 >= 6, 1,0),
    bool_page26 = ifelse(PT_page26 >= 10, 1,0),
    bool_page26.1.1 = ifelse(PT_page26.1.1 >= 4, 1,0),
    bool_page27 = ifelse(PT_page27 >= 8, 1,0)
  )

#@Salomé: Here, I tried changing NA values to 0 (for the code below), because I had many NA values in the page times (for whatever reason... or at least, i think that I did)
mydata.nochildren <- mydata.nochildren %>% mutate(bool_page4 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page6 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page7 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page8 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page9.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page9.2 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page10.2 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page10.3 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page10.4 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page10.5 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page12 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page14 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page15 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page16 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page17 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page19 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page20.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page21.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page22.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page23.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page24 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page26 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page26.1.1 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page27 = ifelse(is.na(bool_page4), 0, bool_page4),
                                                  bool_page4 = ifelse(is.na(bool_page4), 0, bool_page4))
                                                  
        
mydata.nochildren$nb_failing_tests = 0

mydata.nochildren[mydata.nochildren$bool_page4 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page4 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page6 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page6 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page7 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page7 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page9.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page9.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page9.2 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page9.2 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page10.2 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page10.2 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page10.3 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page10.3 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page10.4 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page10.4 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page10.5 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page10.5 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page12 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page12 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page14 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page14 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page15 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page15 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page16 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page16 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page17 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page17 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page19 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page19 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page20.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page20.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page21.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page21.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page22.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page22.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page23.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page23.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page24 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page24 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page26 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page26 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page26.1.1 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page26.1.1 == "0", "nb_failing_tests"] + 1
mydata.nochildren[mydata.nochildren$bool_page27 == "0", "nb_failing_tests"] = mydata.nochildren[mydata.nochildren$bool_page27 == "0", "nb_failing_tests"] + 1

print(mydata.nochildren$nb_failing_tests)
print(mydata.nochildren$PT_page9.1)
mydata.nochildren.valid <- mydata.nochildren[(mydata.nochildren$nb_failing_tests>=1)]
view(mydata.nochildren.valid)


#. OPTIONAL: iterate through all rows and check if the condition is true. Return ID when it's true. ----


#. Merge separate data frames again to get full data set with valid cases only

#@Salome: The problem here was that the number of variables was not the same across data sets -> I think that one first needs to delete all the bool and PT variables again, so, the number of variables is the same in all data sets again
mydata.valid.nf <- rbind(mydata.valid.valid, 
                      mydata.noWFH.valid, 
                      mydata.noWFH.freel.valid, 
                      mydata.freel.valid, 
                      mydata.couldbutnoWFH.valid, 
                      mydata.alone.valid, 
                      mydata.noAG.valid, 
                      mydata.nochildren.valid)

#check for and filter out duplicate cases again
duplicates.valid.nf <- mydata.valid.nf %>% 
  group_by(userID) %>% 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates.valid)) # Number of duplicates (0)

# Data frame without duplicates
mydata.valid <- mydata.valid.nf %>% distinct(userID,.keep_all = T)
names(mydata.valid)
view(mydata.valid)

#***********************************************************************
#05 Data preparation ----
#browse in codebook as needed

#***********************************************************************
#06 Scales and reliabilities ----


#. Creation of latent constructs ----

mydata.valid$JOP1r<-as.numeric(as.character(mydata.valid$JOP1r))
mydata.valid$JOP2r<-as.numeric(as.character(mydata.valid$JOP2r))
mydata.valid$J0P3r<-as.numeric(as.character(mydata.valid$JOP3r))
mydata.valid$JOP4r<-as.numeric(as.character(mydata.valid$JOP4r))

mydata.valid$EXH1<-as.numeric(as.character(mydata.valid$EXH1))
mydata.valid$EXH2<-as.numeric(as.character(mydata.valid$EXH2))
mydata.valid$EXH3<-as.numeric(as.character(mydata.valid$EXH3))
mydata.valid$EXH4<-as.numeric(as.character(mydata.valid$EXH4))

mydata.valid$JO1r<-as.numeric(as.character(mydata.valid$JO1r))
mydata.valid$JO2r<-as.numeric(as.character(mydata.valid$JO2r))
mydata.valid$JO3r<-as.numeric(as.character(mydata.valid$JO3r))

mydata.valid$TRUSTs<-as.numeric(as.character(mydata.valid$TRUSTs))
mydata.valid$TRUSTo<-as.numeric(as.character(mydata.valid$TRUSTo))

mydata.valid$JSInfr<-as.numeric(as.character(mydata.valid$JSInfr))
mydata.valid$JSInpr<-as.numeric(as.character(mydata.valid$JSInpr))

mydata.scale <- mydata.valid %>%
  rowwise() %>%
  mutate(
    JOP = mean(c(JOP1r, JOP2r, JOP3r, JOP4r), na.rm = TRUE),
    EXH = mean(c(EXH1, EXH2, EXH3, EXH4), na.rm = TRUE),
    JO = mean(c(JO1r, JO2r, JO3r), na.rm = TRUE),
    TRUST = mean(c(TRUSTs, TRUSTo), na.rm = TRUE),
    JSInt = mean(c(JSInfr, JSInpr), na.rm = TRUE),
  ) %>%
  ungroup()

describe(mydata.scale)

#. Get scale reliabilities: OMEGA ----
library(psych)
install.packages("GPArotation")
library(GPArotation)


omega_JOP <- mydata.scale[, c("JOP1r", "JOP2r", "JOP3r", "JOP4r")]
result_omega_JOP <- omega(omega_JOP)
print(result_omega_JOP)

omega_EXH <- mydata.scale[, c("EXH1", "EXH2", "EXH3", "EXH4")]
result_omega_EXH <- omega(omega_EXH)
print(result_omega_EXH)

omega_JO <- mydata.scale[, c("JO1r", "JO2r", "JO3r")]
result_omega_JO <- omega(omega_JO)
print(result_omega_JO)

omega_TRUST <- mydata.scale[, c("TRUSTs", "TRUSTo")] #!!! Not possible
result_omega_TRUST <- omega(omega_TRUST)
print(result_omega_TRUST)

omega_JSInt <- mydata.scale[, c("JSInpr", "JSInfr")] #!!! Not possible
result_omega_JSInt <- omega(omega_JSInt)
print(result_omega_JSInt)

#. Get scale descriptions (summary statistics table)----

install.packages("apaTables")
library(apaTables)

library(apaTables)

corr.table <- apa.cor.table(attitude, 
                        table.number=1)

print(table1)


apa.save(filename = "table1.doc",
         table1)

install.packages("vtable")
library(vtable)
sumtable(mydata.scale, vars = c('JOP',
                          'JOD1r',
                          'JOD2r',
                          'JOD3r',
                          'JODfr',
                          'JSInfr',
                          'JSInpr',
                          'JSInt',
                          'JO',
                          'TRUST',
                          'TRUSTo',
                          'TRUSTs',
                          'EXH'
                          ))

#***********************************************************************
#07 Description of the data set ----

#TO DO: check descriptives and frequencies
psych::describe(mydata.scale)
table(mydata.scale$variable) #!!! Attention:specify variables

# Save descriptive as excel file 
library(openxlsx)
output <- describe(mydata.scale)
write.xlsx(output, "Descriptives mydata.scale.xlsx")
describe(mydata.scale)

#Group data
#mydata.grouped <- group_by (mydata.scale, variable)

#***********************************************************************
#08 Correlations ----

#Placeholders that need replacing:
#mydata – name of your dataset
#var1, var2, 3rdvar, etc – general variable(s)
#xvar, yvar, zvar – x and y variables; z-axis variable
#depvar, indvar1, indvar2, etc – general variables
#catvar – name of your categorical variable
#intvar – name of your interval or continuous variable
#object(s) – whatever you want to call your object(s))
#filename – whatever you want to call your html file
#labels/title – any titles, axis labels, category labels


#. check whether all necessary packages are installed ----
req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})

#basic correlation command (correlation between 2 variables, accounting for NA)
#cor(mydata$var1, mydata$var2, use = "complete.obs")
#cor.test(mydata$var1, mydata$var2, use = "complete.obs")


#. Get correlations between total item set ----

apa.cor.table(mydata.scale, filename = "Correlations.mydata.scale.doc", show.conf.interval = FALSE)


#. Graphing correlations ----

mydata.scale$JOD1r<-as.numeric(as.character(mydata.scale$JOD1r))
mydata.scale$JOD2r<-as.numeric(as.character(mydata.scale$JOD2r))
mydata.scale$JOD3r<-as.numeric(as.character(mydata.scale$JOD3r))
mydata.scale$JODfr<-as.numeric(as.character(mydata.scale$JODfr))



#correlations for RQ1
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOD1r", "JOD2r", "JOD3r")], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Research question 1", show.p = TRUE, digits = 2, triangle = "lower", file = "RQ1_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

#correlations for H1
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO")], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis 1", show.p = TRUE, digits = 2, triangle = "lower", file = "H1_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

#correlations for H2-3
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO", "TRUST", "SAT","EXH","COM" )], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis 1", show.p = TRUE, digits = 2, triangle = "lower", file = "H2_H3_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.
names(mydata.scale)
#correlations for explorative analysis: TRUSTs and TRUSTo
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO", "TRUSTs","TRUSTo", "SAT","EXH","COM" )], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis 1", show.p = TRUE, digits = 2, triangle = "lower", file = "TRUSTs_TRUSTo_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

#***********************************************************************
#09 Data description: Summary statistics (descriptive information) ----

#. Item level: descriptive statistics ----
library(vtable)
descriptive.table <- sumtable(mydata.scale, vars = c(                    
                                'Wpct',
                                'Wcur',
                                'ORGtime',
                                'ORGsize',
                                'sizeOrg',                   
                                'Whour',
                                'FLEXd',
                                'FLEXh',
                                'COMUd',
                                'COMUhin',
                                'COMUruc',
                                'WFHwish',
                                'crucial',
                                'WFHday_o',
                                'WFHday_r',
                                'AGday_r',
                                'AFn',
                                'AFp',
                                'EXH1',
                                'EXH2',
                                'EXH3',
                                'EXH4',
                                'WLB',
                                'SAT',
                                'COM',
                                'TRUSTs',
                                'TRUSTo',
                                'LONE',
                                'ABSENT',
                                'PRESENT',
                                'WFHauss',
                                'WFHerg1',
                                'WFHerg2',
                                'WFHerg3',
                                'AGerg1',
                                'AGerg2',
                                'AGerg3',
                                'AGdesk',
                                'WFHhealth1',
                                'WFHhealth2',
                                'WFHhealth3',
                                'WFHexp1',
                                'WFHexp2',
                                'WFHculture1',
                                'WFHculture2',              
                                'WFHculture3',
                                'HOUSEH1',
                                'HOUSEH2',
                                'HOUSEH3'
))

install.packages("janitor")
library(janitor)
descriptive.frequencies <- mydata.selected %>% tabyl(p_0002,
                                   p_0003,
                                   p_0004,
                                   p_0005,
                                   WFHmog,
                                   Wlead,
                                   ORGsect,
                                   WFHopi,
                                   COMU1,
                                   COMU2,
                                   COMU3,
                                   COMU4,
                                   COMU5,
                                   COMU6,
                                   COMU8,
                                   WFHwish_m,
                                   WFHwish_tu,
                                   WFHwish_w,
                                   WFHwish_th,
                                   WFHwish_f,
                                   WFHwish_sa,
                                   WFHwish_su,
                                   WFHwish_non,
                                   WFHloc1,
                                   WFHloc2,
                                   WFHloc3,
                                   WFHloc4,
                                   WFHnorm,
                                   WFHadv1,
                                   WFHadv2,
                                   WFHadv3,
                                   WFHadv4,
                                   WFHadv5,
                                   WFHadv6,
                                   WFHadv7,
                                   WFHadv8,
                                   WFHadv9,
                                   WFHadv10,
                                   WFHfuture1,
                                   WFHfuture2,
                                   WFHfuture3,
                                   WFHfuture4,
                                   WFHfuture5,
                                   WFHfuture6,
                                   WFHfuture7,
                                   WFHfuture8,
                                   AGadv1,
                                   AGadv2,
                                   AGadv3,
                                   AGadv4,
                                   AGadv5,
                                   AGadv6,
                                   AGadv7,
                                   LIVING1,
                                   LIVING2,
                                   LIVING3,
                                   LIVING4,
                                   LIVING5,
                                   LIVING6,
                                   NATION,
                                   EDUCATION,
                                   BRUTTO,
                                   REGION,
                                   WFHdec)

JO1,
JO2,
JO3,
JOD1r,
JOD2r,
JOD3r,
JODfr,
JODP1r,
JODP2r,
JODP3r,
JOP1r,
JOP2r,
JOP3r,
JOP4r,
JOPfr,
JSInfr,
JSInpr,
JO1r,
JO2r,
JO3r

SDTr1,
SDTr2,
SDTr3,
SDTr4,
SDTc1,
SDTc2,
SDTc3,
SDTc4,
SDTa1,
SDTa2,
SDTa3,
SDTa4,


#***********************************************************************
#10 OLS regression ----


#. Check whether all necessary packages are installed ----

req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot", "stargazer", "coefplot", "dotwhisker", "visreg", "jtools", "ggfortify", "olsrr", "DescTools", "interactions")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})


#. Check whether data structure is adequate for OLS regression ----

#class(mydata.scale$JOP1) # Will tell you how R currently views the variable (double, factor…)
#mydata.scale$groupvar <- factor(mydata.scale$groupvar) # Will declare the variable as a factor variable

#!!! Question: DO I need to declare variables as factors in order to perform regression? 


#. EXH regression ----

#perform multiple regression analyses: EXH
RGh4_EXH_m <- lm(EXH ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_EXH_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: exhaustion
RGh4_EXH_i <- lm(EXH ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_EXH_i, standardized=TRUE) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(exh ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_EXH_m, RGh4_EXH_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

names(mydata.scale)


#. SAT regression ----

#perform multiple regression analyses: SAT
RGh4_SAT_m <- lm(SAT ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_SAT_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: satisfaction
RGh4_SAT_i <- lm(SAT ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_SAT_i, standardized=TRUE) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_SAT_m, RGh4_SAT_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))


#.TRUST regression ----

#perform multiple regression analyses: TRUST
RGh4_TRUST_m <- lm(TRUST ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUST_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustO
RGh4_TRUST_i<- lm(TRUST ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUST_i, standardized=TRUE) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUST_m, RGh4_TRUST_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))


#. COM regression ----

#perform multiple regression analyses: COM
RGh4_COM_m <- lm(COM ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_COM_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustO
RGh4_COM_i<- lm(COM ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_COM_i, standardized=TRUE) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_COM_m, RGh4_COM_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

#***********************************************************************
#11 Explorative OLS regression ----


#. TRUSTs regression ----

#perform multiple regression analyses: TRUSTs
RGh4_TRUSTs_m <- lm(TRUSTs ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUSTs_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustS
RGh4_TRUSTs_i <- lm(TRUSTs ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTs_i, standardized=TRUE) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUSTs_m, RGh4_TRUSTs_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))


#. TRUSTo regression ----

#perform multiple regression analyses: TRUSTo
RGh4_TRUSTo_m <- lm(TRUSTo ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUSTo_m, standardized=TRUE) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustS
RGh4_TRUSTo_i <- lm(TRUSTo ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTo_i, standardized=TRUE) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUSTo_m, RGh4_TRUSTo_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))


#. RQ1 regression (exploratory): perform multiple regression analyses with main effects only ----

RGrq1 <- lm(JODf ~ JOD1 + JOD2 + JOD3 + correlate1 + correlate2, data = mydata.scale) #retains main effects 
#create HTML table
tab_model(RGrq1, p.style = "stars")

#***********************************************************************
#12 TO DO: OLS regression tables ----

library(apaTables)


