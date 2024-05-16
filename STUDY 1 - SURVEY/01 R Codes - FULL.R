#Anne
#R code book collection
#***********************************************************************
#00 data frame labels with description ---- 

mydata #full (initial) data set 
mydata.nooutlier #data set without outliers (build from mydata.nodup) -> does this need to be built from mydata.nomissing instead? 
mydata.selected #data set with selected variables for my analysis (build from mydata.nooutlier)
mydata.clean #newly loaded data set (after cleaning)
mydata.scale #data set with scales (built from mydata.valid)

#***********************************************************************
#01 set up ----


#.  Working dictionary ----
getwd() #Print my current working directory
setwd(dir = "C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data") #change working dictionary to another path


#. Load and install packages ----
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan)


#. Clean up ----

rm(list=ls()) #delete environment
Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(scipen=999, max.print=9999)
cat("\014") #delete console
getOption("max.print")
options(scipen = 999)

#. Load data set ----

mydata <- read.csv("C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data_project_34947_2024_05_15.csv", sep = ";", na = c('NA','-77', '-99', '-66','-44'))
          #select(variable1, variable2, variable3) #by adding this line of code, one can directly select variables to retain in the data set 
names(mydata)
view(mydata)  


#. Retrieve participants from before launch) ----
mydata <- mydata %>% 
  filter(lfdn > 48) 


#. Check and delete duplicated cases ----
#library(dplyr)
duplicates <- mydata %>% 
  group_by(p_0001) %>% #here I used 'userID' instead of 'p0001' Correct? 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates)) # Number of duplicates (0)
mydata <- mydata[!duplicated(mydata$p_0001), ] #delete duplicated cases based on user_ID
#***********************************************************************
#02 Page time screening (2secs/item) ----

get_min_allowed_time_spent_on_page <- function(page_number) {
  # Here we define the comparison value for each page_number
  # For example:
  retval = 0 #initiate at 0 to avoid problems
  
  #print(page_number) - for debug
  
  if (page_number == "276640") {retval = 4 } #general info about work 1
  else if (page_number == "276644") {retval = 16 } #general info about work 2
  else if (page_number == "276645") {retval = 10 } #general info about work 3
  else if (page_number == "276892") {retval = 7 } #info about WFH 1 
  else if (page_number == "276894") {retval = 4 } #info about WFH 2
  else if (page_number == "276940") {retval = 8 } # day Site, HO decision maker & location normativness
  #else if (page_number == "280452") {retval = 2 } #intro justice - do we want to take this into account?
  else if (page_number == "276897") {retval = 9 } #JOD1
  else if (page_number == "276808") {retval = 7 } #JOD2
  else if (page_number == "276915") {retval = 11 } #JOP1
  else if (page_number == "277059") {retval = 7 } #JOif
  #else if (page_number == "280451") {retval = 1 } #intro SDT - do we want to take this into account?
  else if (page_number == "276916") {retval = 27 } #SDT1
  else if (page_number == "276921") {retval = 5 } #job affect
  else if (page_number == "277060") {retval = 9 } #exhaustion
  else if (page_number == "276922") {retval = 13 } #WLB/Satisfaction/Commitment/Trust
  else if (page_number == "276923") {retval = 4 } #absence & presence
  else if (page_number == "280455") {retval = 1 } #intro rest
  else if (page_number == "276941") {retval = 4 } #motivation to work from home/in the office
  else if (page_number == "276943") {retval = 9 } #ergonomics
  else if (page_number == "276944") {retval = 9 } #ergonomics office
  else if (page_number == "280445") {retval = 9 } #healthy homeoffice
  else if (page_number == "276946") {retval = 4 } #future wfh
  else if (page_number == "276947") {retval = 6 } #arbeitgeber
  else if (page_number == "276663") {retval = 10 } #demographic 1
  else if (page_number == "277030") {retval = 4 } #living situation
  else if (page_number == "276891") {retval = 2 } #demographic2
  else if (page_number == "276896") {retval = 7 } #overall justice
  
return(retval)
}

get_page_ids_history <- function(x, output) {
  
  page_ids_history = x[["page_history"]] 
  page_ids_history = strsplit(page_ids_history, ",")
  
  timestamp_on_previous_page <- 0
  for(page_id in page_ids_history) { 
    for (id in page_id) {
      time_spent_on_page <- 0
      min_allowed_time_spent_on_page <- get_min_allowed_time_spent_on_page(id)
      
      if ( paste("rts", id, sep="") %in% names(x) ) {
        timestamp_on_current_page <- x[paste("rts", id, sep="")][[1]]
        
        if(! is.null(timestamp_on_current_page)) {
          if(is.numeric(timestamp_on_current_page) && is.numeric(timestamp_on_previous_page)) {
            time_spent_on_page <- timestamp_on_current_page - timestamp_on_previous_page
            timestamp_on_previous_page <- timestamp_on_current_page
            if(time_spent_on_page < min_allowed_time_spent_on_page) {
              x[paste("enough_time_on_", id, sep="")] <- 1
            }
          }
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

new_list <- lapply(1:nrow(test_data), function(i) get_page_ids_history(test_data[i, ]))

new_df <- bind_rows(new_list)

calc_ratio <- function(x) {
  enough_time_cols <- startsWith(names(x), "enough_time_on_")
  page_ids_history = strsplit(x[["page_history"]], ",")
  num_pages = length(unlist(page_ids_history))
  num_enough_time = sum(x[enough_time_cols] == 1, na.rm = TRUE)
  return(num_enough_time / num_pages)
}

new_df$ratio = apply(new_df, 1, calc_ratio)

participants_with_high_ratio <- new_df$p_0001[new_df$ratio >= 0.2]  #adapt this to desired ratio. 
view(participants_with_high_ratio) #retrieve ID from careless responders. 

mydata <- mydata[!mydata$p_0001 %in% participants_with_high_ratio, ] #clean data (without careless responders)

write.csv(mydata, "C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data.CLEANED.2024-05-15.csv", row.names = TRUE) #export the new clean data set :)

#***********************************************************************
#03 Start with new data set (cleaned data) ----


#. Clean up ----

rm(list=ls()) #delete environment
cat("\014") #delete console


#. Load data set again ----
mydata <- read.csv("C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data.CLEANED.2024-05-15.csv", sep = ",", na = c('NA','-77', '-99', '-66','-44'))

#***********************************************************************
#04 Prepare data set ----


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
#mydata <- mydata %>% distinct(p_0001,.keep_all = T)


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

#. Check outliers again ----

#library(dplyr)
mydata.cleaned <- mydata %>% 
  mutate(across(
    where(is.numeric),
    ~ ifelse(
      abs(as.numeric(scale(.x))) > 3,
      NA, 
      .x
    )
  ))

#***********************************************************************
#05 TO CHECK: Select variables relevant for my analyses ----

#!! Attention: Which variables to select here is still to be defined/debated
names(mydata)
names(mydata.cleaned)
mydata.selected <- mydata.cleaned %>% select(c(userID,
                                               p_0001,
                                               p_0002,
                                               p_0003,
                                               p_0004,
                                               p_0005,
                                               WFHmog,
                                               Wpct,
                                               Wcur,
                                               Wlead,
                                               FREELANCE,
                                               ORGtime,
                                               ORGsect,
                                               ORGsize,
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
                                               COMU7,
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
                                               WFHloc5,
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
                                               WFHadv9,
                                               #WFHadv10,
                                               AGadv1,
                                               AGadv2,
                                               AGadv3,
                                               AGadv4,
                                               AGadv5,
                                               AGadv6,
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
                                               WFHfuture7,
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
                                               #rts276638,
                                               #rts276640,
                                               #rts276644,
                                               #rts276645,
                                               #rts276663,
                                               #rts276891,
                                               #rts276892,
                                               #rts276894,
                                               #rts276896,
                                               #rts276897,
                                               #rts276898,
                                               #rts276915,
                                               #rts276916,
                                               #rts276921,
                                               #rts276922,
                                               #rts276923,
                                               #rts276940,
                                               #rts276941,
                                               #rts276943,
                                               #rts276944,
                                               #rts276946,
                                               #rts276947,
                                               #rts277030,
                                               #rts277059,
                                               #rts277060,
                                               #rts277376,
                                               #rts280445,
                                               #rts280451,
                                               #rts280452,
                                               #rts280455,
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
                                               JSIfr,
                                               JO1r,
                                               JO2r,
                                               JO3r        
)) 

summary (mydata.selected)
view(mydata.selected)
names(mydata.selected)

#***********************************************************************
#05 Scales and reliabilities ----


#. Recode relevant variables into numeric
mydata.selected$JOP1r<-as.numeric(as.character(mydata.selected$JOP1r))
mydata.selected$JOP2r<-as.numeric(as.character(mydata.selected$JOP2r))
mydata.selected$JOP3r<-as.numeric(as.character(mydata.selected$JOP3r))
mydata.selected$JOP4r<-as.numeric(as.character(mydata.selected$JOP4r))

mydata.selected$EXH1<-as.numeric(as.character(mydata.selected$EXH1))
mydata.selected$EXH2<-as.numeric(as.character(mydata.selected$EXH2))
mydata.selected$EXH3<-as.numeric(as.character(mydata.selected$EXH3))
mydata.selected$EXH4<-as.numeric(as.character(mydata.selected$EXH4))

mydata.selected$JO1r<-as.numeric(as.character(mydata.selected$JO1r))
mydata.selected$JO2r<-as.numeric(as.character(mydata.selected$JO2r))
mydata.selected$JO3r<-as.numeric(as.character(mydata.selected$JO3r))

mydata.selected$JOD1r<-as.numeric(as.character(mydata.selected$JOD1r))
mydata.selected$JOD2r<-as.numeric(as.character(mydata.selected$JOD2r))
mydata.selected$JOD3r<-as.numeric(as.character(mydata.selected$JOD3r))
mydata.selected$JODfr<-as.numeric(as.character(mydata.selected$JODfr))

mydata.selected$JODfr<-as.numeric(as.character(mydata.selected$JODfr))

mydata.selected$TRUSTs<-as.numeric(as.character(mydata.selected$TRUSTs))
mydata.selected$TRUSTo<-as.numeric(as.character(mydata.selected$TRUSTo))

mydata.selected$JSInfr<-as.numeric(as.character(mydata.selected$JSInfr))
mydata.selected$JSInpr<-as.numeric(as.character(mydata.selected$JSInpr))

mydata.selected$SDTr1<-as.numeric(as.character(mydata.selected$SDTr1))
mydata.selected$SDTr2<-as.numeric(as.character(mydata.selected$SDTr2))
mydata.selected$SDTr3<-as.numeric(as.character(mydata.selected$SDTr3))
mydata.selected$SDTr4<-as.numeric(as.character(mydata.selected$SDTr4))

mydata.selected$SDTa1<-as.numeric(as.character(mydata.selected$SDTa1))
mydata.selected$SDTa2<-as.numeric(as.character(mydata.selected$SDTa2))
mydata.selected$SDTa3<-as.numeric(as.character(mydata.selected$SDTa3))
mydata.selected$SDTa4<-as.numeric(as.character(mydata.selected$SDTa4))

mydata.selected$SDTc1<-as.numeric(as.character(mydata.selected$SDTc1))
mydata.selected$SDTc2<-as.numeric(as.character(mydata.selected$SDTc2))
mydata.selected$SDTc3<-as.numeric(as.character(mydata.selected$SDTc3))
mydata.selected$SDTc4<-as.numeric(as.character(mydata.selected$SDTc4))




#. Recode character into numeric variables ----

mydata.selected$JOP1r<-as.numeric(mydata.selected$JOP1r)
mydata.selected$JOP2r<-as.numeric(mydata.selected$JOP2r)
mydata.selected$JOP3r<-as.numeric(mydata.selected$JOP3r)
mydata.selected$JOP4r<-as.numeric(mydata.selected$JOP4r)

mydata.selected$JODP1r<-as.numeric(mydata.selected$JODP1r)
mydata.selected$JODP2r<-as.numeric(mydata.selected$JODP2r)
mydata.selected$JODP3r<-as.numeric(mydata.selected$JODP3r)

mydata.selected$JOPfr<-as.numeric(mydata.selected$JOPfr)

mydata.selected$EXH1<-as.numeric(mydata.selected$EXH1)
mydata.selected$EXH2<-as.numeric(mydata.selected$EXH2)
mydata.selected$EXH3<-as.numeric(mydata.selected$EXH3)
mydata.selected$EXH4<-as.numeric(mydata.selected$EXH4)

mydata.selected$JO1r<-as.numeric(mydata.selected$JO1r)
mydata.selected$JO2r<-as.numeric(mydata.selected$JO2r)
mydata.selected$JO3r<-as.numeric(mydata.selected$JO3r)

mydata.selected$JODfr<-as.numeric(mydata.selected$JODfr)

mydata.selected$TRUSTs<-as.numeric(mydata.selected$TRUSTs)
mydata.selected$TRUSTo<-as.numeric(mydata.selected$TRUSTo)

mydata.selected$JOD1r<-as.numeric(mydata.selected$JOD1r)
mydata.selected$JOD2r<-as.numeric(mydata.selected$JOD2r)
mydata.selected$JOD3r<-as.numeric(mydata.selected$JOD3r)

mydata.selected$JSInfr<-as.numeric(mydata.selected$JSInfr)
mydata.selected$JSInpr<-as.numeric(mydata.selected$JSInpr)

mydata.selected$JSIfr<-as.numeric(mydata.selected$JSIfr)

mydata.selected$SDTr1<-as.numeric(mydata.selected$SDTr1)
mydata.selected$SDTr2<-as.numeric(mydata.selected$SDTr2)
mydata.selected$SDTr3<-as.numeric(mydata.selected$SDTr3)
mydata.selected$SDTr4<-as.numeric(mydata.selected$SDTr4)

mydata.selected$SDTa1<-as.numeric(mydata.selected$SDTa1)
mydata.selected$SDTa2<-as.numeric(mydata.selected$SDTa2)
mydata.selected$SDTa3<-as.numeric(mydata.selected$SDTa3)
mydata.selected$SDTa4<-as.numeric(mydata.selected$SDTa4)

mydata.selected$SDTc1<-as.numeric(mydata.selected$SDTc1)
mydata.selected$SDTc2<-as.numeric(mydata.selected$SDTc2)
mydata.selected$SDTc3<-as.numeric(mydata.selected$SDTc3)
mydata.selected$SDTc4<-as.numeric(mydata.selected$SDTc4)


#. Create latent constructs ----

mydata.scale <- mydata.selected %>%
  rowwise() %>%
  mutate(
    JOP = mean(c(JOP1r, JOP2r, JOP3r, JOP4r), na.rm = TRUE),
    EXH = mean(c(EXH1, EXH2, EXH3, EXH4), na.rm = TRUE),
    JO = mean(c(JO1r, JO2r, JO3r), na.rm = TRUE),
    TRUST = mean(c(TRUSTs, TRUSTo), na.rm = TRUE),
    JSInt = mean(c(JSInfr, JSInpr), na.rm = TRUE),
    
    SDTr = mean(c(SDTr1,SDTr2,SDTr3,SDTr4), na.rm = TRUE),
    SDTa = mean(c(SDTa1,SDTa2,SDTa3,SDTa4), na.rm = TRUE),
    SDTc = mean(c(SDTc1,SDTc2,SDTc3,SDTc4), na.rm = TRUE),
    
    SDT = mean(c(SDTr1,SDTr2,SDTr3,SDTr4,
                 SDTa1,SDTa2,SDTa3,SDTa4,
                 SDTc1,SDTc2,SDTc3,SDTc4), na.rm = TRUE)
  ) %>%
  ungroup()

names(mydata.scale)
describe(mydata.scale)

#.. create a new file with scales to use for RWA later on ----
write.csv(mydata.scale, "C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data.RWA.2024-05-15.csv", row.names = TRUE) #export the new clean data set :)
#mydata.scale <- read.csv("C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data.RWA.2024-05-15.csv", sep = ",", na = c('NA','-77', '-99', '-66','-44'))


#. Scale reliabilities: OMEGA ----

#install.packages("GPArotation")
library(GPArotation)
library(psych)

#.. Omega JOP ---- 
omega_JOP <- mydata.scale[, c("JOP1r", "JOP2r", "JOP3r", "JOP4r")]
result_omega_JOP <- omega(omega_JOP)
print(result_omega_JOP)

#.. Omega EXH----
omega_EXH <- mydata.scale[, c("EXH1", "EXH2", "EXH3", "EXH4")]
result_omega_EXH <- omega(omega_EXH)
print(result_omega_EXH)

#.. Omega JO ----
omega_JO <- mydata.scale[, c("JO1r", "JO2r", "JO3r")]
result_omega_JO <- omega(omega_JO)
print(result_omega_JO)

#.. Omega TRUST ----
omega_TRUST <- mydata.scale[, c("TRUSTs", "TRUSTo")] #!!! Not possible
result_omega_TRUST <- omega(omega_TRUST)
print(result_omega_TRUST)

#.. Omega JSInt ----
omega_JSInt <- mydata.scale[, c("JSInpr", "JSInfr")] #!!! Not possible
result_omega_JSInt <- omega(omega_JSInt)
print(result_omega_JSInt)

#.. Omega SDT scales & SDT ----
omega_SDTr <- mydata.scale[, c("SDTr1", "SDTr2","SDTr3","SDTr4")] 
result_omega_SDTr <- omega(omega_SDTr)
print(result_omega_SDTr)

omega_SDTa <- mydata.scale[, c("SDTa1", "SDTa2","SDTa3","SDTa4")] 
result_omega_SDTa <- omega(omega_SDTa)
print(result_omega_SDTa)

omega_SDTc <- mydata.scale[, c("SDTc1", "SDTc2","SDTc3","SDTc4")] 
result_omega_SDTc <- omega(omega_SDTc)
print(result_omega_SDTc)

omega_SDT <- mydata.scale[, c("SDTr1", "SDTr2","SDTr3","SDTr4",
                              "SDTa1", "SDTa2","SDTa3","SDTa4",
                              "SDTc1", "SDTc2","SDTc3","SDTc4")] 
result_omega_SDT <- omega(omega_SDT)
print(result_omega_SDT)


#. Scale fit: CFAs ----

pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

#.. CFA for SDT ----
model_SDT <- 'SDT_r =~ SDTr1 + SDTr2 + SDTr3 + SDTr4
              SDT_a =~ SDTa1 + SDTa2 + SDTa3 + SDTa4
              SDT_C =~ SDTc1 + SDTc2 + SDTc3 + SDTc4'

fit.SDT <- cfa(model_SDT, data = mydata.scale)

#install.packages("semPlot")
library(semPlot)
library(lavaan)
summary(fit.SDT, fit.measures = TRUE, standardized = TRUE)
semPaths(fit.SDT, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


#.. CFA for Exhaustion ----
model_Exhaustion <- 'Exhaustion =~ EXH1 + EXH2 + EXH3 + EXH4'

fit.Exhaustion <- cfa(model_Exhaustion, data = mydata.scale)

summary(fit.Exhaustion, fit.measures = TRUE, standardized = TRUE)
semPaths(fit.Exhaustion, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)

#.. CFA for JOP ----

model_JOP <- 'JOP_scale =~ JOP1r + JOP2r + JOP3r + JOP4r'

fit.JOP <- cfa(model_JOP, data = mydata.scale)

summary(fit.JOP, fit.measures = TRUE, standardized = TRUE)
semPaths(fit.JOP, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


#.. CFA for JOP+JSI ----

model_JOP_JSI <- 'JOP_scale =~ JOP1r + JOP2r + JOP3r + JOP4r 
                  JSI_scale =~ JSInfr + JSInpr'

fit.JOP.JSI <- cfa(model_JOP_JSI, data = mydata.scale)

summary(fit.JOP.JSI, fit.measures = TRUE, standardized = TRUE)

semPaths(fit.JOP.JSI, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)

#.. CFA for JOP+JSI+JO ----

model_JOP_JSI_JO <- 'JOP_scale =~ JOP1r + JOP2r + JOP3r + JOP4r 
                  JSI_scale =~ JSInfr + JSInpr
                  JO_Scale =~ JO1r + JO2r+ JO3r'

fit.JOP.JSI.JO <- cfa(model_JOP_JSI_JO, data = mydata.scale)

summary(fit.JOP.JSI.JO, fit.measures = TRUE, standardized = TRUE)

semPaths(fit.JOP.JSI.JO, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)


#.. CFA for JO ----

model_JO <- 'JO_scale =~ JO1r + JO2r + JO3r'

fit.JO <- cfa(model_JO, data = mydata.scale)

summary(fit.JO, fit.measures = TRUE, standardized = TRUE)
semPaths(fit.JO, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)

#***********************************************************************
#06 Descriptives and frequencies ----


#. Full data set descriptives ----

#Export descriptives to excel file
install.packages("openxlsx")
library(openxlsx)
library(psych)
output.descrptive <- describe(mydata.scale)
write.xlsx(output.descrptive, "Descriptives_mydata.scale.xlsx") #!! Attention: Only gives you variable names but no numbers
describe(mydata.scale)
names(mydata.scale)

#check name assignment
describe(mydata.scale$JSIf)

install.packages("readxl")
library(readxl)
Descriptives_mydata.scale=read_excel('Descriptives_mydata.scale.xlsx')
View(Descriptives_mydata.scale)

#. Descriptives statistics for justice ----

library(vtable)
descriptive.table <- sumtable(mydata.scale, vars = c(                    
  'JOP1r',
  'JOP2r',
  'JOP3r',
  'JOP4r',
  'JSInfr',
  'JSIntr',
  'JO1r',
  'JO2r',
  'JO3r',
  'JOD1r',
  'JOD2r',
  'JOD3r',
  'JODfr',
  'JOPfr',
  'JSIfr'
))

#. Descriptive statistics for specific constructs ----

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

#. Frequencies for distributive justice ----

mydata.scale$JODP1r<-as.numeric(mydata.scale$JODP1r)
mydata.scale$JODP2r<-as.numeric(mydata.scale$JODP2r)
mydata.scale$JODP3r<-as.numeric(mydata.scale$JODP3r)

# Get a summary 
summary_table_justice <- lapply(mydata.scale[, c(
                                                 'JOD1r',
                                                 'JOD2r',
                                                 'JOD3r',
                                                 'JODP1r',
                                                 'JODP2r',
                                                 'JODP3r'
                                                 
)], tabyl)

#Export file into .csv
combined_df_j <- bind_rows(summary_table_justice, .id = "var_name")
write.csv(combined_df_j, "frequencies_j.chtm", row.names = TRUE)

#converst .csv into .xsl
frequencies_justice <- read.csv("frequencies_j.chtm")
library(xlsx)
xlsx::write.xlsx(frequencies_justice, 
                 "Frequencies_justice.xls", 
                 col.names=TRUE, 
                 row.names=TRUE, 
                 sheetName="Frequencies_justice_table")


#. Frequencies for demographics ----

# Get a summary of your dataframe
summary_table <- lapply(mydata.scale[, c('WFHmog', 
                                         'Wcur', 
                                         'Wlead', 
                                         'FREELANCE', 
                                         'ORGsect', 
                                         'ORGsize', 
                                         'COMUd', 
                                         'COMU1', 
                                         'COMU2', 
                                         'COMU3', 
                                         'COMU4', 
                                         'COMU5', 
                                         'COMU6',
                                         'COMU7',
                                         'COMU8', 
                                         'WFHwish',
                                         'WFHwish_m', 
                                         'WFHwish_tu',
                                         'WFHwish_w', 
                                         'WFHwish_th', 
                                         'WFHwish_f', 
                                         'WFHwish_sa', 
                                         'WFHwish_su', 
                                         'WFHday_o', 
                                         'WFHday_r', 
                                         'AGday_r', 
                                         'WFHdec', 
                                         'WFHloc1',
                                         'WFHloc2', 
                                         'WFHloc3', 
                                         'WFHloc4',
                                         'WFHloc5',
                                         'WFHadv1',
                                         'WFHadv2', 
                                         'WFHadv3',
                                         'WFHadv4', 
                                         'WFHadv5',
                                         'WFHadv6', 
                                         'WFHadv7',
                                         'WFHadv8',
                                         'AGadv1', 
                                         'AGadv2', 
                                         'AGadv3',
                                         'AGadv4', 
                                         'AGadv5',
                                         'NATION',
                                         'EDUCATION', 
                                         'BRUTTO', 
                                         'REGION', 
                                         'LIVING1',
                                         'LIVING2', 
                                         'LIVING3',
                                         'LIVING4', 
                                         'LIVING5', 
                                         'LIVING6')], tabyl)


#Export file into .csv
combined_df <- bind_rows(summary_table, .id = "var_name")
write.csv(combined_df, "frequencies.chtm", row.names = TRUE)

#converst .csv into .xsl
frequencies <- read.csv("frequencies.chtm")
library(xlsx)
xlsx::write.xlsx(frequencies, 
                 "Frequencies.xls", 
                 col.names=TRUE, 
                 row.names=TRUE, 
                 sheetName="Frequencies_summary_table")

#***********************************************************************
#07 Correlations ----

#class(mydata.scale$)

#. Correlations for Justice variables (item level) ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", 
                                  "JOD1r", 
                                  "JOD2r", 
                                  "JOD3r",
                                  "JOP1r",
                                  "JOP2r",
                                  "JOP3r",
                                  "JOP4r",
                                  "JSInfr",
                                  "JSInpr",
                                  "JSIfr",
                                  "JOPfr"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "Justice Item Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_Justice items_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.


#. Correlations for Justice variables (scale level) ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", 
                                  "JOPfr",
                                  "JSIfr",
                                  "JOD1r", 
                                  "JOD2r", 
                                  "JOD3r",
                                  "JOP",
                                  "JO",
                                  "JSInt"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "Justice Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_Justice_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

sjPlot::tab_corr(mydata.scale[, c("JOPfr",
                                  "JOP1r", 
                                  "JOP2r", 
                                  "JOP3r",
                                  "JOP4r",
                                  "JOP"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "Procedural Justice Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_Procedural Justice_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

sjPlot::tab_corr(mydata.scale[, c("JSInfr",
                                  "JSInpr", 
                                  "JSInt", 
                                  "JSIfr"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "JSI Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_JSI_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

sjPlot::tab_corr(mydata.scale[, c("JOPfr",
                                  "JODfr", 
                                  "JSIfr",
                                  "JO"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "Jf and JO Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_Jf_JO_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.



#. Correlations for SDT variables (item level) ----
sjPlot::tab_corr(mydata.scale[, c("SDTr1", 
                                  "SDTr2", 
                                  "SDTr3", 
                                  "SDTr4",
                                  "SDTc1",
                                  "SDTc2",
                                  "SDTc3",
                                  "SDTc4",
                                  "SDTa1",
                                  "SDTa2",
                                  "SDTa3",
                                  "SDTa4"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "SDT Item Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_SDT items_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.


#. Correlations for SDT variables (scale level) ----
sjPlot::tab_corr(mydata.scale[, c("SDTr", 
                                  "SDTa", 
                                  "SDTc"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "SDT Scale Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_SDT scales_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

#.. Correlations for EXH ----
sjPlot::tab_corr(mydata.scale[, c("EXH1", 
                                  "EXH2", 
                                  "EXH3"
)], na.deletion = "listwise", corr.method = "pearson", 
title = "EXH Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_EXH_mydata.scale.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.



#. Other correlations ----

sjPlot::tab_corr(mydata.scale[, c("SDT",
                                  "SDTr",
                                  "SDTa",
                                  "SDTc",
                                  "JODfr",
                                  "JOD1r",
                                  "JOD2r",
                                  "JOD3r",
                                  "JOP",
                                  "JSInt",
                                  "JO",
                                  "TRUST",
                                  "EXH",
                                  "SAT",
                                  "COM",
                                  #"WFHmog",
                                  #"Wlead",
                                  "FLEXh",
                                  "FLEXd",
                                  "WLB",
                                  "LONE",
                                  "WFHexp1",
                                  "WFHexp2",
                                  "PRESENT",
                                  "WFHculture1",
                                  "WFHculture2",
                                  "WFHculture3",
                                  "WFHnorm")],
                 corr.method = "pearson",
                 title = "Selected Correlations",
                 show.p = TRUE, digits = 2, triangle = "upper", 
                 file = "Corr_SELECTED constructs_mydata.scale.htm")


#. Correlations between total item set ----

#Export to file works but, currently, Word just breaks down :D 

apa.cor.table(mydata.scale, filename = "Correlations.mydata.scale.doc", show.conf.interval = FALSE)
 

#***********************************************************************
#08 Hypotheses Testing: Correlations ----

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


#. correlations for RQ1 ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOD1r", "JOD2r", "JOD3r")], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Research question 1", show.p = TRUE, digits = 2, triangle = "lower", file = "RQ1_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.


#. correlations for H1 ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO")], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis 1", show.p = TRUE, digits = 2, triangle = "lower", file = "H1_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.


#. correlations for H2-3 ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO", "TRUST", "SAT","EXH","COM" )], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis 2/3", show.p = TRUE, digits = 2, triangle = "lower", file = "H2_H3_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.


#. correlations for explorative analysis: TRUSTs and TRUSTo ----
sjPlot::tab_corr(mydata.scale[, c("JODfr", "JOP", "JSInpr", "JSInfr", "JO", "TRUSTs","TRUSTo", "SAT","EXH","COM" )], na.deletion = "listwise", corr.method = "pearson", 
                 title = "Hypothesis expl. TRUSTs/TRUSTo", show.p = TRUE, digits = 2, triangle = "lower", file = "TRUSTs_TRUSTo_correlations.mydata.scale.htm") #correlation matrix
                 #If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.

#***********************************************************************
#09 Hypotheses testing: Regression unstandardized ----


#. Check whether all necessary packages are installed ----

req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot", "stargazer", "coefplot", "dotwhisker", "visreg", "jtools", "ggfortify", "olsrr", "DescTools", "interactions")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})

install.packages("lm.beta")
library(lm.beta)

#. Check whether data structure is adequate for OLS regression ----

#class(mydata.scale$JOP1) # Will tell you how R currently views the variable (double, factor…)
#mydata.scale$groupvar <- factor(mydata.scale$groupvar) # Will declare the variable as a factor variable


#. EXH regression ----

#perform multiple regression analyses: EXH
RGh4_EXH_m <- lm(EXH ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_EXH_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: exhaustion
RGh4_EXH_i <- lm(EXH ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_EXH_i) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(exh ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_EXH_m, RGh4_EXH_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_EXH_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_EXH_m,x=T,y=T)
reghelper::beta(RGh4_EXH_i,x=T,y=T)

#lm.beta(RGh4_EXH_m)
#lm.beta(RGh4_EXH_i)

#. SAT regression ----

#perform multiple regression analyses: SAT
RGh4_SAT_m <- lm(SAT ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_SAT_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: satisfaction
RGh4_SAT_i <- lm(SAT ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_SAT_i) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_SAT_m, RGh4_SAT_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_SAT_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_SAT_m,x=T,y=T)
reghelper::beta(RGh4_SAT_i,x=T,y=T)

#.TRUST regression ----

#perform multiple regression analyses: TRUST
RGh4_TRUST_m <- lm(TRUST ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUST_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustO
RGh4_TRUST_i<- lm(TRUST ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUST_i) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUST_m, RGh4_TRUST_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUST_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_TRUST_m,x=T,y=T)
reghelper::beta(RGh4_TRUST_i,x=T,y=T)

#lm.beta(RGh4_TRUST_m)
#lm.beta(RGh4_TRUST_i)

#. COM regression ----

#perform multiple regression analyses: COM
RGh4_COM_m <- lm(COM ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_COM_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustO
RGh4_COM_i<- lm(COM ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_COM_i) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_COM_m, RGh4_COM_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_COM_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_COM_m,x=T,y=T)
reghelper::beta(RGh4_COM_i,x=T,y=T)

#lm.beta(RGh4_COM_m)
#lm.beta(RGh4_COM_i)

#***********************************************************************
#11 Explorative regression ----


#. TRUSTs regression ----

#perform multiple regression analyses: TRUSTs
RGh4_TRUSTs_m <- lm(TRUSTs ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUSTs_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustS
RGh4_TRUSTs_i <- lm(TRUSTs ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTs_i) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUSTs_m, RGh4_TRUSTs_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUSTs_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_TRUSTs_m,x=T,y=T)
reghelper::beta(RGh4_TRUSTs_i,x=T,y=T)

#lm.beta(RGh4_TRUSTs_m)
#lm.beta(RGh4_TRUSTs_i)

#. TRUSTo regression ----

#perform multiple regression analyses: TRUSTo
RGh4_TRUSTo_m <- lm(TRUSTo ~ JODfr + JOP + JSInpr + JSInfr , data = mydata.scale) 
summary(RGh4_TRUSTo_m) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: trustS
RGh4_TRUSTo_i <- lm(TRUSTo ~ JODfr + JOP + JSInt + JODfr:JOP + JODfr:JSInt + JOP:JSInt + JODfr:JOP:JSInt, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTo_i) # Obtain a summary of the regression model 
#create HTML table
tab_model(RGh4_TRUSTo_m, RGh4_TRUSTo_i, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JOP"))
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JSInt"))
sjPlot::plot_model(RGh4_TRUSTo_i, type = "pred", terms = c("JODfr", "JOP", "JSInt"))

reghelper::beta(RGh4_TRUSTo_m,x=T,y=T)
reghelper::beta(RGh4_TRUSTo_i,x=T,y=T)

#lm.beta(RGh4_TRUSTo_m)
#lm.beta(RGh4_TRUSTo_i)


#. RQ1 regression (exploratory): perform multiple regression analyses with main effects only ----

#.. RQ1 by gender ----

# Correlation matrix for males
RQ1_male_data <- mydata.scale[mydata$p_0002 == 1, c("JODfr", "JOD1r", "JOD2r", "JOD3r")]
RQ1_male_corr <- cor(RQ1_male_data, 
                     na.deletion = "listwise", corr.method = "pearson", 
                     show.p = TRUE, digits = 2, triangle = "lower")
view(RQ1_male_data)

# Correlation matrix for females
RQ1_female_data <- mydata.scale[mydata$p_0002 == 2, c("JODfr", "JOD1r", "JOD2r", "JOD3r")]
RQ1_female_corr <- cor(RQ1_female_data, method = "pearson")

# Display correlation tables
tab_corr(RQ1_male_corr, 
         title = "Research question 1 (Males)", 
         show.p = TRUE, 
         digits = 2, 
         triangle = "lower", 
         file = "RQ1_correlations_male.html")

tab_corr(RQ1_female_corr, 
         title = "Research question 1 (Females)", 
         show.p = TRUE, 
         digits = 2, 
         triangle = "lower", 
         file = "RQ1_correlations_female.html")

RGrq1 <- lm(JODfr ~ JOD1r + JOD2r + JOD3r + correlate1 + correlate2, data = mydata.scale) #retains main effects 
#create HTML table
tab_model(RGrq1, p.style = "stars")

#***********************************************************************
#10 Hypotheses testing: Regression using scale() ----


#. Standardized constructs ----

mydata.scale$JOD1r_standardized <- scale(mydata.scale$JOD1r)
mydata.scale$JOD2r_standardized <- scale(mydata.scale$JOD2r)
mydata.scale$JOD3r_standardized <- scale(mydata.scale$JOD3r)

mydata.scale$JOP_standardized <- scale(mydata.scale$JOP)

mydata.scale$JSInpr_standardized <- scale(mydata.scale$JSInpr)
mydata.scale$JSInfr_standardized <- scale(mydata.scale$JSInfr)
mydata.scale$JSInt_standardized <- scale(mydata.scale$JSInt)

mydata.scale$JODfr_standardized <- scale(mydata.scale$JODfr)
mydata.scale$JOPfr_standardized <- scale(mydata.scale$JOPfr)
mydata.scale$JSIfr_standardized <- scale(mydata.scale$JSIfr)


mydata.scale$TRUST_standardized <- scale(mydata.scale$TRUST)
mydata.scale$SAT_standardized <- scale(mydata.scale$SAT)
mydata.scale$COM_standardized <- scale(mydata.scale$COM)
mydata.scale$EXH_standardized <- scale(mydata.scale$EXH)

mydata.scale$TRUSTs_standardized <- scale(mydata.scale$TRUSTs)
mydata.scale$TRUSTo_standardized <- scale(mydata.scale$TRUSTo)



#. Regression with standardized predictors only (no outcomes standardized) ---- 

#.. EXH regression ----
names(mydata.scale)

#perform multiple regression analyses: EXH
RGh4_EXH_m_std <- lm(EXH ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_EXH_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: exhaustion
RGh4_EXH_i_std <- lm(EXH ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_EXH_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(exh ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_EXH_m_std, RGh4_EXH_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_EXH_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_EXH_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_EXH_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. SAT regression ----

#perform multiple regression analyses: SAT
RGh4_SAT_m_std <- lm(SAT ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_SAT_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_SAT_i_std <- lm(SAT ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_SAT_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(SAT ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_SAT_m_std, RGh4_SAT_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_SAT_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_SAT_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_SAT_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))



#.. TRUST regression ----

#perform multiple regression analyses: TRUST
RGh4_TRUST_m_std <- lm(TRUST ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUST_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUST_i_std <- lm(TRUST ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUST_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUST ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUST_m_std, RGh4_TRUST_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUST_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUST_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUST_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. COM regression ----

#perform multiple regression analyses: COM
RGh4_COM_m_std <- lm(COM ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_COM_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_COM_i_std <- lm(COM ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_COM_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(COM ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_COM_m_std, RGh4_COM_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_COM_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_COM_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_COM_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. TRUSTs regression ----

#perform multiple regression analyses: TRUSTs
RGh4_TRUSTs_m_std <- lm(TRUSTs ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUSTs_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUSTs_i_std <- lm(TRUSTs ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTs_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUSTs ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUSTs_m_std, RGh4_TRUSTs_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTs_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUSTs_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUSTs_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. TRUSTo regression ----

#perform multiple regression analyses: TRUSTo
RGh4_TRUSTo_m_std <- lm(TRUSTo ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUSTo_m_std) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUSTo_i_std <- lm(TRUSTo ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTo_i_std) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUSTo ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUSTo_m_std, RGh4_TRUSTo_i_std, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTo_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUSTo_i_std, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUSTo_i_std, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#. Regression with standardized predictors and outcomes ---- 

#.. EXH regression ----

#perform multiple regression analyses: EXH
RGh4_EXH_m_std2 <- lm(EXH_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_EXH_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term: exhaustion
RGh4_EXH_i_std2 <- lm(EXH_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_EXH_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(exh ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_EXH_m_std2, RGh4_EXH_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_EXH_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_EXH_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_EXH_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. SAT regression ----

#perform multiple regression analyses: SAT
RGh4_SAT_m_std2 <- lm(SAT_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_SAT_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_SAT_i_std2 <- lm(SAT_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_SAT_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(SAT ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_SAT_m_std2, RGh4_SAT_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_SAT_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_SAT_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_SAT_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))



#.. TRUST regression ----

#perform multiple regression analyses: TRUST
RGh4_TRUST_m_std2 <- lm(TRUST_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUST_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUST_i_std2 <- lm(TRUST_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUST_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUST ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUST_m_std2, RGh4_TRUST_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUST_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUST_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUST_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#.. COM regression ----

#perform multiple regression analyses: COM
RGh4_COM_m_std2 <- lm(COM_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_COM_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_COM_i_std2 <- lm(COM_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_COM_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(COM ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_COM_m_std2, RGh4_COM_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_COM_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_COM_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_COM_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#. TRUSTs regression ----

#perform multiple regression analyses: TRUSTs
RGh4_TRUSTs_m_std2 <- lm(TRUSTs_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUSTs_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUSTs_i_std2 <- lm(TRUSTs_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTs_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUSTs ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUSTs_m_std2, RGh4_TRUSTs_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTs_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUSTs_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUSTs_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#. TRUSTo regression ----

#perform multiple regression analyses: TRUSTo
RGh4_TRUSTo_m_std2 <- lm(TRUSTo_standardized ~ JODfr_standardized + JOP_standardized + JSInpr_standardized + JSInfr_standardized , data = mydata.scale) 
summary(RGh4_TRUSTo_m_std2) # Obtain a summary of the regression model 
#perform multiple regression analyses with three-way interaction term
RGh4_TRUSTo_i_std2 <- lm(TRUSTo_standardized ~ JODfr_standardized + JOP_standardized + JSInt_standardized + JODfr_standardized:JOP_standardized + JODfr_standardized:JSInt_standardized + JOP_standardized:JSInt_standardized + JODfr_standardized:JOP_standardized:JSInt_standardized, data = mydata.scale) #retains main + interaction effects
summary(RGh4_TRUSTo_i_std2) # Obtain a summary of the regression model 
#test : RGIh4e <- lm(TRUSTo ~ ODf:JOP:JSInt, data = mydata) #retains main + interaction effects
#create HTML table
tab_model(RGh4_TRUSTo_m_std2, RGh4_TRUSTo_i_std2, p.style = "stars")
#plotting interaction results in a graph
sjPlot::plot_model(RGh4_TRUSTo_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized"))
sjPlot::plot_model(RGh4_TRUSTo_i_std2, type = "pred", terms = c("JODfr_standardized", "JSInt_standardized"))
sjPlot::plot_model(RGh4_TRUSTo_i_std2, type = "pred", terms = c("JODfr_standardized", "JOP_standardized", "JSInt_standardized"))


#***********************************************************************
#11 RWA: Basic ---- 

#. RWA for RQ1 ----
if(!require('rwa')) {
  install.packages('rwa')
  library('rwa')
}
library(ggplot2)

rwa(mydata.scale,"JODfr",c("JOD1r","JOD2r","JOD3r"),applysigns = FALSE, plot = TRUE)


#***********************************************************************
#12 RWA: Difference in predictor weights ---- 

pacman::p_load(boot,broom,knitr,purrr,rwa,tidyverse,dplyr)


#. Read in your data set (must be assigned to df) ----

df <- read.csv("C:/Users/annek/Dropbox/02 PhD NRP80/05 NRP80 - Papers/01 Papers - Justice/01 Justice - Data/data.RWA.csv", sep = ",", na = c('NA','-77', '-99', '-66','-44'))


#. Specify your criterion variable name ----

outcome <- "JODfr"


#. Specify your predictor variable names ----

predictors <- c("JOD1r",
                "JOD2r",
                "JOD3r")


#. Do you want to compare predictors: “Yes” or “No” ----

compare <- "Yes"


#. If “Yes” specify your focal variable to be compared; if “No” set to NULL ----

focal <- "JOD1r"


#. Specify number of bootstrapped replications (must be greater than N; recommend > 5k) ----

num.bootstraps <- 8000

#. Call the function to run the analysis -> Attention: takes up to 30 minutes ----

source("https://raw.githubusercontent.com/stonid/rwa/main/MultipleRegression.R")

#test

