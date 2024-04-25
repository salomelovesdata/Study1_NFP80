# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("clean_recoded.csv", sep = ",", na = c('NA','-77', '-99', '-66'))
view(mydata)  

#______________________________________________________________________________________


mydata.selected <- mydata %>% select(c(userID,
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
                                               crucial,
                                               WFHday_o,
                                               WFHday_r,
                                               AGday_r,
                                               WFHdec,
                                               WFHnorm,
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

names(mydata.selected)
view(mydata.selected)

#******************************************************************

#. Create latent constructs ----

mydata.scale <- mydata.selected %>%
  rowwise() %>%
  mutate(
    JOP = mean(c(JOP1r,JOP2r, JOP3r, JOP4r), na.rm = TRUE),
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


# Convert them to numeric variables
mydata.scale[] <- lapply(mydata.scale, as.numeric)

# If you get a warning about NAs introduced by coercion, 
# that means some values could not be converted to numeric.

names(mydata.scale)
describe(mydata.scale)

#_______________________________________________________________________________________


# Compute correlation matrix
cor_matrix <- cor(mydata.scale[, c("SDT", "SDTr", 
                                         "SDTa", "SDTc", "JODfr", 
                                         "JOD1r", "JOD2r", "JOD3r", "JOP", 
                                         "JSInt", "JO", "TRUST", "EXH", "SAT", 
                                         "COM", "WFHmog", "Wlead", "FLEXh", 
                                         "FLEXd", "WLB", "LONE", "WFHexp1",
                                         "WFHexp2", "PRESENT", "WFHculture1", 
                                         "WFHculture2", "WFHculture3", "WFHnorm")],
                  use = "pairwise.complete.obs", 
                  method = "pearson")

# Convert to data frame
cor_df <- as.data.frame(cor_matrix)

# Export to Excel
writexl::write_xlsx(cor_df, "2cor_matrix.xlsx")

#SDTc has 0 variance apparently ? cannot compute correlations for this subscale..


# Now, create correlation matrix with the cleaned data
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
                 show.p = TRUE, digits = 2, triangle = "lower", 
                 file = "Corr_SELECTED constructs_mydata.valid.htm")





