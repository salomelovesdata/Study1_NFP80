# SalomeDepraz
# 27 mars 2024


install.packages('pacman')
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan)

#* Set up WD

options(scipen=999, max.print=5000)


setwd(dirname(getActiveDocumentContext()$path))

df <- read.csv("15_4_2024.csv", sep = ";", na = c('NA','-77', '-99', '-66'))

#_______________________________________________________________________________________________
#* Prepping THE DATA *#
#* 0- Clean

df_clean <- df %>%
  filter(lfdn > 21) #Retrieve participants from before launch


#* 1-Prepare scales
#* 

#Define missing values as missing values

df_clean <- df_clean%>%
  mutate(
    sectOrg = ifelse(sectOrg == 15, NA, sectOrg),
    sizeOrg = ifelse(sizeOrg == 8, NA, sizeOrg),
    opiWFH = ifelse(opiWFH == 6, NA, opiWFH),
    decMaker = ifelse(decMaker == 5, NA, decMaker),
    normWFH = ifelse(normWFH == 6, NA, normWFH),
    JOD1 = ifelse(JOD1 == 1, NA, JOD1),
    JOD2 = ifelse(JOD2 == 1, NA, JOD2),
    JOD3 = ifelse(JOD3 == 7, NA, JOD3),
    JODf = ifelse(JODf == 1, NA, JODf),
    JODpref1 = ifelse(JODpref1 == 7, NA, JODpref1),
    JODpref2 = ifelse(JODpref2 == 7, NA, JODpref2),
    JODpref3 = ifelse(JODpref3 == 7, NA, JODpref3),
    dupl1_JOP1 = ifelse(dupl1_JOP1 == 7, NA, dupl1_JOP1),
    JOP2 = ifelse(JOP2 == 7, NA, JOP2),
    JOP3 = ifelse(JOP3 == 7, NA, JOP3),
    JOP4 = ifelse(JOP4 == 7, NA, JOP4),
    JOPf = ifelse(JOPf == 7, NA, JOPf),
    JSI1 = ifelse(JSI1 == 7, NA, JSI1),
    JSI2 = ifelse(JSI2 == 7, NA, JSI2),
    JSIf = ifelse(JSIf == 7, NA, JSIf),
    covid1 = ifelse(covid1 ==7, NA, covid1),
    education = ifelse(education ==7, NA, education),
    salary = ifelse(salary ==7, NA, salary),
    JOv1 = ifelse(JOv1 == 1, NA, JOv1),
    JOv2 = ifelse(JOv2 == 1, NA, JOv2),
    JOv3 = ifelse(JOv3 == 1, NA, JOv3)
    
 )

#Rename and recode scales
df_clean <- rename(JOP1 = dupl1_JOP1)


#_______________________________________________________________________________________________

#Create page_time (PT) data frame to do the analysis 

# Function to calculate page time for each participant - takes the page_history column as input 
# and returns a dataframe with the page_id and page_time columns.
calculate_page_time <- function(page_history, p_0001) {
  page_ids <- as.numeric(unlist(strsplit(page_history, ",")))
  page_time <- c(NA, diff(page_ids))
  data.frame(p_0001 = p_0001, page_id = page_ids, page_time = page_time)
}

# Create a new dataframe with the page time calculations
page_time_df <- df_clean %>%
  rowwise() %>%
  mutate(page_time_data = list(calculate_page_time(page_history, p_0001))) %>%
  unnest(page_time_data)



df <- df %>%
  mutate(
    PT_descriptive = rts267777 - rts267774,
    PT_JOO = rts267782 - rts267777,
    PT_JOD = rts268661 - rts267782,
    PT_prefJOD = rts268784 - rts268661,
    PT_newitems = rts268850 - rts268784,
    PT_newitems2 = rts268851 - rts267783,
    PT_JSP= rts268626 - rts268851,
    PT_JSI = rts268627 - rts268626,
    PT_SDT = rts267789 - rts268627,
    PT_SDT2 = rts267790 - rts267789,
    PT_Moods = rts267806 - rts267790,
    PT_Exhaustion = rts267792 - rts267806,
    PT_other = rts267793 - rts267792,
   
  )

#Compare PT to our minimum PT (2s/item rule)
df <- df  %>%
  mutate(
    bool_descriptive = ifelse(PT_descriptive >= 6, 1,0),
    bool_JOO = ifelse(PT_JOO >= 10, 1,0),
    bool_JOD = ifelse(PT_JOD >=10,1,0),
    bool_prefJOD = ifelse(PT_prefJOD >=10,1,0),
    bool_newitems = ifelse(PT_newitems >= 4,1,0),
    bool_newitems2 = ifelse(PT_newitems2 >= 4,1,0),
    bool_JSP = ifelse(PT_JSP >= 18,1,0),
    bool_JSI = ifelse(PT_JSI >= 4,1,0),
    bool_SDT = ifelse(PT_SDT >= 12,1,0),
    bool_SDT2 = ifelse(PT_SDT2 >= 12,1,0),
    bool_moods = ifelse(PT_Moods >= 4,1,0),
    bool_exhaustion = ifelse(PT_Exhaustion >= 8,1,0),
    bool_other = ifelse(PT_other >= 4,1,0),
    bool_JOP_passive = ifelse(PT_passive >= 10,1,0),
    bool_JSDp = ifelse(PT_JSDp >= 10,1,0),
    bool_JOP_active = ifelse(PT_JOP_active >= 10,1,0),
    bool_JSDa = ifelse(PT_JSDa >= 10,1,0)
  )


## GET THE ID OF PEOPLE WHO FAILED AT LEAST XX NUMBER OF TIMES
#automatisation ?

df$nb_failing_tests = 0
print(df$bool_JOO)

df[df$bool_JOO == "0", "nb_failing_tests"] = df[df$bool_JOO == "0", "nb_failing_tests"] + 1

df[df$bool_JOD == "0", "nb_failing_tests"] = df[df$bool_JOD == "0", "nb_failing_tests"] + 1

df[df$bool_prefJOD == "0", "nb_failing_tests"] = df[df$bool_prefJOD == "0", "nb_failing_tests"] + 1

df[df$bool_newitems == "0", "nb_failing_tests"] = df[df$bool_newitems == "0", "nb_failing_tests"] + 1

df[df$bool_newitems2 == "0", "nb_failing_tests"] = df[df$bool_newitems2 == "0", "nb_failing_tests"] + 1

df[df$bool_JSP == "0", "nb_failing_tests"] = df[df$bool_JSP == "0", "nb_failing_tests"] + 1

df[df$bool_JSI == "0", "nb_failing_tests"] = df[df$bool_JSI == "0", "nb_failing_tests"] + 1

df[df$bool_SDT == "0", "nb_failing_tests"] = df[df$bool_SDT == "0", "nb_failing_tests"] + 1

df[df$bool_SDT2 == "0", "nb_failing_tests"] = df[df$bool_SDT2 == "0", "nb_failing_tests"] + 1

df[df$bool_moods == "0", "nb_failing_tests"] = df[df$bool_moods == "0", "nb_failing_tests"] + 1

df[df$bool_other == "0", "nb_failing_tests"] = df[df$bool_other == "0", "nb_failing_tests"] + 1

df[df$bool_descriptive == "0", "nb_failing_tests"] = df[df$bool_descriptive == "0", "nb_failing_tests"] + 1

#df[df$bool_JOP_active == "0", "nb_failing_tests"] = df[df$bool_JOP_active == "0", "nb_failing_tests"] + 1

#df[df$bool_JOP_passive == "0", "nb_failing_tests"] = df[df$bool_JOP_passive == "0", "nb_failing_tests"] + 1

print(df[df$nb_failing_tests > 4, c("p_0001", "nb_failing_tests")], n=60)  #we have 33 people that failed our conditions at least 4 times.


#* SCREEN OUT PEOPLE THAT FAILED 3 TIMES OR MORE -aka failed 20% of the survey- OUR TIME-PAGE CONDITION


df <- df[(df$nb_failing_tests<=2), ]


#Optional : iterate through all rows and check if the condition is true. Return ID when it's true. 
#Stock it into new variable.

idJOO = df[
  df$bool_JOO == "0", "p_0001"
]

idJOD = df[
  df$bool_JOD == "0", "p_0001"
]

idprefJOD = df[
  df$bool_prefJOD == "0", "p_0001"
]


idNI = df[
  df$bool_newitems == "0", "p_0001"
]

idNI2 = df[
  df$bool_newitems2 == "0", "p_0001"
]

idJSP = df[
  df$bool_JSP == "0", "p_0001"
]

idJSI = df[
  df$bool_JSI == "0", "p_0001"
]

idSDT = df[
  df$bool_SDT == "0", "p_0001"
]

idSDT2 = df[
  df$bool_SDT2 == "0", "p_0001"
]

idmood = df[
  df$bool_moods == "0", "p_0001"
]

idexh = df[
  df$bool_exhaustion == "0", "p_0001"
]

idother = df[
  df$bool_other == "0", "p_0001"
]

idSDTextreme = df[
  df$bool_SDT_extreme == "1", "p_0001"
]

idSDTextreme2 = df[
  df$bool_SDT2_extreme == "1", "p_0001"
]



#Get percentage of successful screening

df_PT <- df %>% select(starts_with("PT"))
describe(df_PT)

#_______________________________________________________________________________________________
#Passive VERSION

df_passive <- df_passive %>%
  mutate(
    PT_JOP_passive = rts269177 - rts268850, #for the JOP passive version
    PT_JSDp = rts267783 - rts269177, #for the JOP passive version
  )

#Compare PT to our minimum PT (2s/item rule)
df_passive <- df_passive %>%
  mutate(
    bool_JOP_passive = ifelse(PT_JOP_passive >= 8,1,0),
    bool_JSDp = ifelse(PT_JSDp >= 10,1,0)
  )




#Get percentage of successful screening
mean(df_passive$bool_JOP_passive) #adjust for desired variable
mean(df_passive$bool_JSDp, na.rm=TRUE)

#_______________________________________________________________________________________________
# Active VERSION


df_active <- df_active%>%
  mutate(
    PT_JOP_active = rts267805 - rts268850, #for the JOP active version
    PT_JSDa = rts267783 - rts267805, #for the JOP active version
  )

#Compare PT to our minimum PT (2s/item rule)
df_active <- df_active %>%
  mutate(
    bool_JOP_active = ifelse(PT_JOP_active >= 8,1,0),
    bool_JSDa = ifelse(PT_JSDa >= 10,1,0)
  )


#Get percentage of successful screening
mean(df_active$bool_JOP_active, na.rm=TRUE) #adjust for desired variable
mean(df_active$bool_JSDa, na.rm=TRUE)
mean(df_active$PT_JOP_active, na.rm=TRUE)

#_______________________________________________________________________________________________
#PREPARING THE DATA FOR ANALYSIS

#Specify "Ich weiss es nicht" as missing value
#* 
#* J_S_P1 -> P4
#* J_O_P_A1 -> A4
#* J_O_P_P1 -> P4


df <- df%>%
  mutate(
    J_S_P1 = ifelse(J_S_P1 == 6, NA, J_S_P1),
    J_S_P2 = ifelse(J_S_P2 == 6, NA, J_S_P2),
    J_S_P3 = ifelse(J_S_P3 == 6, NA, J_S_P3),
    J_S_P4 = ifelse(J_S_P4 == 6, NA, J_S_P4)
  )



df$age <- 2023-df$age


#_________________________________________________

#Print mean & SD for all items 
describe(df)

#Answer frequency for an item
frequency_table <- table(df$gender)
print(frequency_table)


#Creation of latent constructs-

df <- df %>%
  rowwise() %>%
  mutate(
    autonomy_ = mean(c(SDT_A1, SDT_A2, SDT_A3, SDT_A4), na.rm = TRUE),
    competence_ = mean(c(SDT_C1, SDT_C2, SDT_C3, SDT_C4), na.rm = TRUE),
    relatedness_ = mean(c(SDT_R1, SDT_R2, SDT_R3, SDT_R4), na.rm = TRUE),
    exhaustion_ = mean(c(exh1, exh2, exh3, exh4), na.rm = TRUE),
    overall_justice_ = mean(c(J_O_Overall1, J_O_Overall2, J_O_Overall3, J_O_Overall4, J_O_Overall5), na.rm = TRUE),
    s_procedural_justice_ = mean(c(J_S_P1, J_S_P2, J_S_P3, J_S_P4), na.rm = TRUE),
    new_J_interperso = mean(c(J_S_IP, J_S_IF), na.rm = TRUE)
  ) %>%
  ungroup()

df_active <- df_active %>%
  rowwise() %>%
  mutate(
    JOP_active = mean(c(J_O_P_A1, J_O_P_A2, J_O_P_A3, J_O_P_A4), na.rm = TRUE),
    new_J_interperso = mean(c(J_S_IP, J_S_IF), na.rm = TRUE)
  ) %>%
  ungroup()

df_passive <- df_passive %>%
  rowwise() %>%
  mutate(
    JOP_passive = mean(c(J_O_P_P1, J_O_P_P2, J_O_P_P3, J_O_P_P4), na.rm = TRUE),
    new_J_interperso = mean(c(J_S_IP, J_S_IF), na.rm = TRUE)
  ) %>%
  ungroup()

describe(df$PT_SDT)

#___________________________________________________________________

#** FIT 

#Omega

alpha_A <- df[, c("SDT_A1", "SDT_A2", "SDT_A3", "SDT_A4")]
alpha_result <- omega(alpha_A)
print(alpha_result)

alpha_A <- df[, c("SDT_C1", "SDT_C2", "SDT_C3", "SDT_C4")]
alpha_result <- omega(alpha_A)
print(alpha_result)

alpha_A <- df[, c("SDT_R1", "SDT_R2", "SDT_R3", "SDT_R4")]
alpha_result <- omega(alpha_A)
print(alpha_result)


alpha_A <- df[, c("exh1", "exh2", "exh3", "exh4")]
alpha_result <- omega(alpha_A)
print(alpha_result)


omega <- df[, c("J_O_Overall1", "J_O_Overall2", "J_O_Overall3", "J_O_Overall4", "J_O_Overall5")]
alpha_result <- omega(alpha_A)
print(alpha_result)

omega <- df[, c("J_S_P1", "J_S_P2", "J_S_P3", "J_S_P4")]
alpha_result <- omega(alpha_A)
print(alpha_result)

alpha_A <- df_active[, c("J_O_P_A1", "J_O_P_A2", "J_O_P_A3", "J_O_P_A4")]
alpha_result <- omega(alpha_A)
print(alpha_result)

alpha_A <- df_passive[, c("J_O_P_P1", "J_O_P_P2", "J_O_P_P3", "J_O_P_P4")]
alpha_result <- omega(alpha_A)
print(alpha_result)



#_______________________________________________________________________


#_______________________________________________________________________
#Correlation matrix constructs

selected_columns <- df[, c( "J_O_Overall1", "J_O_Overall2", "J_O_Overall3", "J_O_Overall4", "J_O_Overall5" )]
selected_columns <- na.omit(selected_columns)
# Calculer la matrice de corrélation
correlation_matrix <- cor(selected_columns)

# Masquer la moitié inférieure de la matrice de corrélation
#correlation_matrix[upper.tri(correlation_matrix)]<-NA

# Afficher la matrice de corrélation
print(correlation_matrix)

# Afficher la matrice de corrélation avec des couleurs
cor.plot(correlation_matrix)

names(df)

#____________________________________________________________________


#CFA

# specifier le modele initial :
SDT_model <- '
autonomy =~ SDT_A1 + SDT_A2 + SDT_A3 + SDT_A4  # specifier les items pour le facteur  1
competence =~ SDT_C1 + SDT_C2 + SDT_C3 + SDT_C4 # specifier les items pour le facteur  2
relatedness =~ SDT_R1 + SDT_R2 + SDT_R3 + SDT_R4  # specifier les items pour le facteur  3
'

# executer le AFC
f1 <- cfa(SDT_model, missing = "fiml", estimator = 'MLR', data = df)
summary(f1, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

# see modification indices
summary(f1, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE, modindices=TRUE)




#Exhaustion
Exh <- '
exhaustion =~ exh1 + exh2 + exh3 + exh4

'
f4 <- cfa(Exh, missing = "fiml", estimator = 'MLR', data = df)
summary(f4, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

# see modification indices
summary(f4, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE, modindices=TRUE)




#Overall justice : items 4 & 5 work less good. MI: we should add the covariance between them
Overall_justice <- '
O_O_J =~ J_O_Overall1 +  J_O_Overall2 + J_O_Overall3 + J_O_Overall4 + J_O_Overall5
'
f6 <- cfa(Overall_justice, missing = "fiml", estimator = 'MLR', data = df)
summary(f6, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

# see modification indices
summary(f6, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE, modindices=TRUE)




#Procedural justice
Procedural_justice <- '
PJS =~ J_S_P1 + J_S_P2 + J_S_P3 + J_S_P4
'
f7 <- cfa(Procedural_justice, missing = "fiml", estimator = 'MLR', data = df)
summary(f7, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

# see modification indices
summary(f7, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE, modindices=TRUE)




#Organizational procedural justice active
JOP_active <- '
JOP_A =~ J_O_P_A1 + J_O_P_A2 + J_O_P_A3 + J_O_P_A4
'
f7 <- cfa(JOP_active, missing = "fiml", estimator = 'MLR', data = df_active)
summary(f7, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

# see modification indices
summary(f7, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE, modindices=TRUE)


names(df)


#export datafile
write.csv(df_passive, file = "Pdataset3110.csv", row.names = FALSE)


#_______________________________________________________________________



#Regressions with multiple predictors
RG <- lm(overall_justice_ ~ P_J_O_fairness + JOP_full  + new_J_interperso  , data = df)
RG <- lm(WLB ~ J_S_D1 + J_S_D2 + J_S_D3 + s_procedural_justice_ , data = df)
RG <- lm(O_Sat ~ J_S_D1 + J_S_D2 + J_S_D3 + s_procedural_justice_ , data = df)
RG <- lm(exhaustion_ ~ J_S_D1 + J_S_D2 + J_S_D3 + s_procedural_justice_ , data = df)
RG <- lm(negativity ~ J_S_D1 + J_S_D2 + J_S_D3 + s_procedural_justice_ , data = df)

# Afficher un résumé du modèle
summary(RG, standardized=TRUE)

#___________________________________________________________________
install.packages("boa")
