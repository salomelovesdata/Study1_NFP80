# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("clean_recoded.csv", sep = ",", na = c('NA','-77', '-99', '-66'))


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
mydata.scale[] <- lapply(mydata.scale, as.numeric)


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

#Omega SDT scales & SDT ----
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


#. CFAs ----

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
