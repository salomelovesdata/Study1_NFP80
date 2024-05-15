#***********************************************************************
#01 set up ----

# Working dictionary
pacman::p_load(rstudioapi,dplyr,psych,haven,apaTables,readxl,tidyverse,MplusAutomation,semTools, lavaan)

options(scipen=999, max.print=5000)

setwd(dirname(getActiveDocumentContext()$path))

mydata <- read.csv("15_05_24.csv", sep = ";", na = c('NA','-77', '-99', '-66'))
view(mydata)  


mydata <- mydata %>% 
  filter(lfdn > 48) #Retrieve participants from before launch)

#Retrieve test participants from YouGov (email from Julia, 13.05.2024)

mydata <- mydata %>% 
  filter(!(p_0001 %in% c("2___a7843ea5f0f2ed64bd230ebbd26547d3_QNSBONZW", "2___c1a4b5c9b9b95fd0898e0930a86401cc_QNSBONZW", "2___b70ad35136b10673dcf7b25069d7ba15_QNSBONZW",
                         "2___1fd09331acd3ca72ed80d7f5cd25211a_QNSBONZW", "2___c670294a76158330b6ee172f1bdb379c_QNSBONZW", "2___c488179f9d36330f0b3d13d8ede4f4c4_QNSBONZW",
                         "2___04fd340fa20f64792ce3a46fec1954e3_QNSBONZW", "2___6d2d8aad175f801cf7fbe1113e2e63b8_QNSBONZW", "2___f104e472e4bffe7b98ec01c4a9ad0508_QNSBONZW",
                         "2___9ce61fbd32b2a71cbfa40c5c368b4d67_QNSBONZW", "2___515e1ed0df8de9beb9ffab16a18dd865_QNSBONZW", "2___cb15da1a300f8359e990421d3073af42_QNSBONZW",
                         "2___479206ceda7e6e8063a20c5d98544e15_QNSBONZW", "2___7c692f9a421e205f662e129699c74eb8_QNSBONZW", "2___cbbb1931fe5776885c36b0989a499e1f_QNSBONZW")))

#check data for non HO workers
mydata <- mydata %>% 
  filter(mogWFH == 1) 

#. Check and delete duplicated cases ----
#library(dplyr)
duplicates <- mydata %>% 
  group_by(p_0001) %>% 
  mutate(dupe = n()>1) %>%
  filter(dupe==T)
print(nrow(duplicates)) # Number of duplicates (0)





#***********************************************************************
#01 Page time screening (2secs/item) ----

#################### Salome


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
  #else if (page_number == "280455") {retval = 1 } #intro rest
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
  #"enough_time_on_280455",
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

#new_df <- as.data.frame(do.call(rbind, new_list))

new_df <- bind_rows(new_list)




###########################


calc_ratio <- function(x) {
  enough_time_cols <- startsWith(names(x), "enough_time_on_")
  page_ids_history = strsplit(x[["page_history"]], ",")
  num_pages = length(unlist(page_ids_history))
  num_enough_time = sum(x[enough_time_cols] == 1, na.rm = TRUE)
  return(num_enough_time / num_pages)
}

new_df$ratio = apply(new_df, 1, calc_ratio)

participants_with_high_ratio <- new_df$p_0001[new_df$ratio >= 0.20]  #adapt this to desired ratio. 
view(participants_with_high_ratio) #retrieve ID from careless responders (20 careless responders at 15.05.2023)

mydata <- mydata[!mydata$p_0001 %in% participants_with_high_ratio, ] #clean data (without careless responders)


#########################

write.csv(mydata, "filtered_data_test.csv", row.names = TRUE) #export the new clean data set :)


