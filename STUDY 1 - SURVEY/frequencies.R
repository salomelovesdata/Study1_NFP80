#***********************************************************************
#01 set up ----

# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("filtered_data_test.csv", sep = ",", na = c('NA','-77', '-99', '-66'))
view(mydata)  

nocom <- mydata %>%
 filter(v_261==0)


canWFH<- mydata %>%
  filter(mogWFH==2)

cannotWFH <- mydata %>%
  filter(mogWFH==1)


couldbutdont <- canWFH %>%
  filter(realWFH==1)


FREELANCE<- mydata %>%
  filter(freelanc==2)

noworkonsite <- mydata %>%
  filter(daySite==1)

alone <- mydata %>%
  filter(lifeSit6 == 1)


#. Check and delete duplicated cases ----
#library(dplyr)
duplicates <- mydata %>% 
  group_by(p_0001) %>% #here I used 'userID' instead of 'p0001' Correct? 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates)) # Number of duplicates (0)


#***********************************************************************
#*

names(mydata)

# Get a summary of your dataframe
summary_table <- lapply(mydata[, c('mogWFH', 'curWork', 'leadPos', 'freelanc', 'sectOrg', 'sizeOrg', 'comDay', 'auto', 'beifahre', 'bus', 'zug', 'fuss', 'zweirad', 'v_262', 
                                   'optWFH', 'wfh_Mon', 'wfh_Tue', 'wfh_Wed', 'wfh_Thu', 'wfh_Fri', 'wfh_sat', 'wfh_Sun', 'dayWFH', 'realWFH', 'daySite', 'decMaker', 'ort_Home',
                                  'ort_Cafe', 'ort_Share', 'advWFH1', 'advWFH2', 'advWFH3', 'advWFH4', 'advWFH5', 'advWFH6', 'advWFH7', 'advWFH8', 'advBuro1', 'advBuro2', 
                                  'advBuro3', 'advBuro4', 'advBuro5', 'nation', 'education', 'salary', 'region', 'lifeSit1', 'lifeSit2', 'lifeSit3',
                                  'lifeSit4', 'lifeSit5', 'lifeSit6')], tabyl)


#EXPORT FILE
combined_df <- bind_rows(summary_table, .id = "var_name")
write.csv(combined_df, "frequencies.csv", row.names = TRUE)

