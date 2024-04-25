# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan, janitor, purr)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("clean_recoded.csv", sep = ",", na = c('NA','-77', '-99', '-66'))
view(mydata)  

names(mydata)

#######################################################################################

# Specify the model based on the structure of your CFA
# Here's an example model specification
model <- '
  # Specify your latent variables and indicators here
  SDTr =~ SDTr1 + SDTr2 + SDTr3 + SDTr4
  SDTa =~ SDTa1 + SDTa2 + SDTa3 + SDTa4
  SDTc =~ SDTc1 + SDTc2 + SDTc3 + SDTc4
'

# Fit the model to your data
fit <- cfa(model, data = mydata)

# Summary statistics of the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)
#########################################################

model <- '
  # Specify your latent variables and indicators here
  EXH =~ JO1r + JO2r + JO3r
 
'

# Fit the model to your data
fit <- cfa(model, data = mydata)

# Summary statistics of the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)
#######################################################3


model <- '
  # Specify your latent variables and indicators here
  JOP =~ JOP1r + JOP2r + JOP3r + JOP4r
 
'

# Fit the model to your data
fit <- cfa(model, data = mydata)

# Summary statistics of the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)


#######################

#Take out NA
mydata <- na.omit(mydata[, c("JOP1r", "JOP2r", "JOP3r", "JOP4r")]) #retrieve missing data

# Check for multicollinearity
cor(mydata[,c("JOP1r", "JOP2r", "JOP3r", "JOP4r")])

# Check for zero variance
apply(mydata[,c("JOP1r", "JOP2r", "JOP3r", "JOP4r")], 2, var)

# Check for missing values
apply(mydata[,c("JOP1r", "JOP2r", "JOP3r", "JOP4r")], 2, function(x) sum(is.na(x)))

