library(tidyverse)
library(dplyr)
tidy_data <- function(file){
  data <- read_csv(file)
  #Help from https://stackoverflow.com/questions/22104962/how-to-remove-empty-columns-in-r/27098520
  
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

data_20 <- tidy_data("NBA20-21 data.csv")
data_19 <- tidy_data("NBA19-20 data.csv")
data_18 <- tidy_data("NBA18-19 data.csv")
data_17 <- tidy_data("NBA17-18 data.csv")
data_16 <- tidy_data("NBA16-17 data.csv")
data_15 <- tidy_data("NBA15-16 data.csv")
data_14 <- tidy_data("NBA14-15 data.csv")
data_13 <- tidy_data("NBA13-14 data.csv")
data_12 <- tidy_data("NBA12-13 data.csv")
data_11 <- tidy_data("NBA11-12 data.csv")

ten_year_data <- data_20 %>% filter(endsWith(Team,"!")) %>%
  select(everything())
find_winner <- function(data){
  year_data <- data %>% filter(endsWith(Team,"!")) %>%
    select(everything())
  print(year_data)
  ten_year_data <- rbind(ten_year_data,year_data)
  return(ten_year_data)
}
ten_frames <- list(data_19,data_18,data_17,data_16,data_15,data_14,
                        data_13,data_12,data_11)

for (i in ten_frames){
  ten_year_data <- find_winner(i)
}

ten_year_data <- ten_year_data %>% mutate(Team = str_replace(Team,"\\!",""))


