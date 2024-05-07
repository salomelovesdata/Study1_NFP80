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

########################################################################

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


#########################################################################


code <- mplusObject(
VARIABLE = 
"USEVARIABLE =
SDTr1 SDTr2 SDTr3 
SDTr4 SDTc1 SDTc2 SDTc3 SDTc4 SDTa1 SDTa2 SDTa3 SDTa4;",
    GROUPING = "p_0004 (1 = GERMAN 2 = FRENCH 3 = ITALIAN);",
MODEL = "
    REL by SDTr1 SDTr2 SDTr3 SDTr4; 
    COM by SDTc1 SDTc2 SDTc3 SDTc4; 
    AUT by SDTa1 SDTa2 SDTa3 SDTa4;",
OUTPUT = "sampstat;",
rdata = df_MI_SDT
)
fit <- mplusModeler(code, modelout="MI_Language_MPlus.inp", run=TRUE)



