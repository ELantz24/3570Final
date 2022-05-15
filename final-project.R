library(tidyverse)
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
data_10 <- tidy_data("NBA10-11 data.csv")
data_09 <- tidy_data("NBA09-10 data.csv")
data_08 <- tidy_data("NBA08-09 data.csv")
data_07 <- tidy_data("NBA07-08 data.csv")
data_06 <- tidy_data("NBA06-07 data.csv")
data_05 <- tidy_data("NBA05-06 data.csv")
data_04 <- tidy_data("NBA04-05 data.csv")
data_03 <- tidy_data("NBA03-04 data.csv")
data_02 <- tidy_data("NBA02-03 data.csv")
data_01 <- tidy_data("NBA01-02 data.csv")
data_00 <- tidy_data("NBA00-01 data.csv")


prev_year_data <- data_20 %>% filter(endsWith(Team,"!")) %>%
  select(everything())
find_winner <- function(data){
  year_data <- data %>% filter(endsWith(Team,"!")) %>%
    select(everything())
  prev_year_data <- rbind(prev_year_data,year_data)
  return(prev_year_data)
}
prev_frames <- list(data_19,data_18,data_17,data_16,data_15,data_14,
                   data_13,data_12,data_11,data_10,data_09,
                   data_08,data_07,data_06,data_05,data_04,
                   data_03,data_02,data_01,data_00)

for (i in prev_frames){
  prev_year_data <- find_winner(i)
}

prev_year_data <- prev_year_data %>% mutate(Team = str_replace(Team,"\\!",""))
summarized <- summarize_all(prev_year_data,mean)

prev_year_data <- rbind(prev_year_data,summarized) %>%  
  mutate_if(is.numeric, ~round(., 0))
prev_year_data$Team[22] = "Average"
write.csv(prev_year_data,"prev_year_data.csv")
