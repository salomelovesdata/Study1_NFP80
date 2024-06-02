# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, MPlusAutomation, vtable)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("clean_recoded.csv", sep = ",", na = c('NA','-77', '-99', '-66')) #DATA ALL CLEANED & READY
view(mydata)  

mydata <- mydata %>% filter(!is.na(p_0004))


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


code <- mplusObject(
  TITLE = "Measurement invariance for region", 
  VARIABLE = "
      NAMES = SDTr1 SDTr2 SDTr3 SDTr4 SDTc1 SDTc2 SDTc3 SDTc4 SDTa1 SDTa2 SDTa3 SDTa4 p_0004;
      USEVARIABLES = SDTr1 SDTr2 SDTr3 SDTr4 SDTc1 SDTc2 SDTc3 SDTc4 SDTa1 SDTa2 SDTa3 SDTa4;
      GROUPING = p_0004 (1=GERMAN 2=FRENCH 3=ITALIAN);
  ", 
  ANALYSIS = "
  MODEL = CONFIGURAL METRIC SCALAR;",
  MODEL = "
      REL by SDTr1 SDTr2 SDTr3 SDTr4; 
      COM by SDTc1 SDTc2 SDTc3 SDTc4; 
      AUT by SDTa1 SDTa2 SDTa3 SDTa4;
  ",
  OUTPUT = "sampstat;",
  rdata = df_MI_SDT
)

fit <-mplusModeler(code, modelout="TEST_MI.inp", run=1)
summary(fit)

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
mydata$p_0004 <- as.factor(mydata$p_0004)

model <- aov(SDTa4 ~ p_0004, data = mydata)

# View the ANOVA results
summary(model)



#SDTr3 - difference between 1-2 and 2-3
#SDTr4 - difference between 1-2
#SDTc3 - difference between 1-2 and 1-3
#SDTa2 - difference between 1-2
#SDTa3 - difference between 1-2 and 1-3





# Bonferroni correction for multiple comparisons
pairwise.t.test(mydata$SDTa3, mydata$p_0004, p.adj="bonf")

