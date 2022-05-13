library(tidyverse)
tidy_data <- function(file){
  data <- read_csv(file)
  data <- Filter(function(x)!all(is.na(x)), data) #remove empty columns
  colnames(data)[18] <- "OeFG%"
  colnames(data)[19] <- "OTOV%"
  colnames(data)[21] <- "OFT/FGA"
  colnames(data)[22] <- "DeFG%"
  colnames(data)[23] <- "DTOV"
  colnames(data)[25] <- "DFT/FGA"
  data <- data[-c(1,26,27)]
  
  asc_method <- c("Age","L","PL","OTOV%","DeFG%","DFT/FGA")
  desc_method <- c("W","PW","MOV","SOS","SRS","ORtg","DRtg","Pace","FTr",
                   "3PAr","TS%","OeFG%","ORB%","OFT/FGA","DTOV","Attend./G",
                   "NRtg","DRB%")
  for (i in asc_method){
    data[i] <- rank(data[i],ties.method = "min")
  }
  for (i in desc_method){
    data[i] <- rank(-data[i],ties.method = "min")
  }
  return (data)
}
data_rk <- tidy_data("basketball data.csv")

data_rk <- data_rk %>% filter(endsWith(Team,"*")) %>% #filters out nonplayoff 
  select(everything()) %>%                            #teams
  mutate(Team = str_replace(Team,"\\*",""))



#Rk is sorted by NRtg
#Help from https://stackoverflow.com/questions/22104962/how-to-remove-empty-columns-in-r/27098520

#linear regression
