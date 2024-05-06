# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, MPlusAutomation, vtable)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("clean_recoded.csv", sep = ",", na = c('NA','-77', '-99', '-66')) #DATA ALL CLEANED & READY
view(mydata)  


#It's easier to work with small datasets. Adapt for every construct needed to be tested for MI.

df_MI_SDT <- mydata %>% select(c(p_0001,
                               p_0004,
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
                               SDTa4
                              ))

df_MI_SDT <- df_MI_SDT %>% filter(!is.na(p_0004))

italian <- df_MI_SDT %>% 
  filter(p_0004==3)

french <- df_MI_SDT %>% 
  filter(p_0004==2)

german <- df_MI_SDT %>% 
  filter(p_0004==1)


descriptive.table_german <- sumtable(german, vars = c(                    
  'SDTr1',
  'SDTr2',
  'SDTr3',
  'SDTr4',
  'SDTc1',
  'SDTc2',
  'SDTc3',
  'SDTc4',
  'SDTa1',
  'SDTa2',
  'SDTa3',
  'SDTa4'
))



descriptive.table_italian <- sumtable(italian, vars = c(                    
  'SDTr1',
  'SDTr2',
  'SDTr3',
  'SDTr4',
  'SDTc1',
  'SDTc2',
  'SDTc3',
  'SDTc4',
  'SDTa1',
  'SDTa2',
  'SDTa3',
  'SDTa4'
))



descriptive.table_french <- sumtable(french, vars = c(                    
  'SDTr1',
  'SDTr2',
  'SDTr3',
  'SDTr4',
  'SDTc1',
  'SDTc2',
  'SDTc3',
  'SDTc4',
  'SDTa1',
  'SDTa2',
  'SDTa3',
  'SDTa4'
))


sjPlot::tab_corr(italian[, c("SDTr1", 
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
title = "SDT Item Correlations", show.p = TRUE, digits = 2, triangle = "lower", file = "Corr_SDT items_italian.htm") #correlation matrix
#If you want the actual p-values instead of asterisks, include ‘p.numeric = TRUE’ as an argument in the sjPlot::tab_corr command.



##########################################


# Prepare data for MPlus MI code
wide_df_SDT <- df_MI_SDT %>%
  pivot_wider(names_from = p_0004, values_from = c(SDTr1,
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
                                                   SDTa4), 
              names_glue = "{.value}_{p_0004}")








##################################################################################################################
#################################################################################################################


code <- mplusObject(
  VARIABLE =
    "USEVARIABLE =
SDTr1_1 SDTr1_2 SDTr1_3
SDTr2_1 SDTr2_2 SDTr2_3
SDTr3_1 SDTr3_2 SDTr3_3
SDTr4_1 SDTr4_2 SDTr4_3
SDTc1_1 SDTc1_2 SDTc1_3
SDTc2_1 SDTc2_2 SDTc2_3
SDTc3_1 SDTc3_2 SDTc3_3
SDTc4_1 SDTc4_2 SDTc4_3
SDTa1_1 SDTa1_2 SDTa1_3
SDTa2_1 SDTa2_2 SDTa2_3
SDTa3_1 SDTa3_2 SDTa3_3
SDTa4_1 SDTa4_2 SDTa4_3; ",
  
  ANALYSIS =
    "ESTIMATOR = MLR;
  MODEL= CONFIGURAL METRIC SCALAR;",
  
  MODEL = "
MODEL 1:
  REL_1 by SDTr1_1  SDTr2_1 SDTr3_1  SDTr4_1; 
  COM_1 by SDTc1_1  SDTc2_1  SDTc3_1  SDTc4_1; 
  AUT_1 by SDTa1_1  SDTa2_1 SDTa3_1  SDTa4_1;

 MODEL 2:
 REL_2 by SDTr1_2 SDTr2_2  SDTr3_2  SDTr4_2; 
  COM_2 by SDTc1_2  SDTc2_2  SDTc3_2  SDTc4_2; 
  AUT_2 by SDTa1_2  SDTa2_2  SDTa3_2  SDTa4_2 ;
  
  MODEL 3:
 REL_3 by SDTr1_3  SDTr2_3  SDTr3_3  SDTr4_3; 
  COM_3 by SDTc1_3  SDTc2_3  SDTc3_3  SDTc4_3; 
  AUT_3 by SDTa1_3  SDTa2_3  SDTa3_3  SDTa4_3 ;",
  
  OUTPUT = "sampstat;",
  rdata = wide_df_SDT
)
fit <- mplusModeler(code, modelout="MI_Language_MPlus.inp", run=TRUE)

