library(dplyr)
library(docstring)



cleanPlayerSalaryYear <- function(playerSalary, year){
  #' @description This function cleans the salary CSV, dealing with
  #' player's who were traded
  #'
  #' 
  #' @param playerSalary dataframe. Read in salary_2017.csv file
  #' @return dataframe. A clean and complete salaries dataframe
  #' 
  
  # Consolidate Duplicate PlayerIDs by sum(Salary)
  playerSalary_subset <- playerSalary %>% 
    filter(Year==year) %>% 
    group_by(Player) %>% 
    summarise(Salary = sum(Salary),
              Year = mean(Year)
    )
  
  
  # Return Clean Dataframe
  return(data.frame(playerSalary_subset))
}

cleanPlayerSalaryTotal <- function(playerSalary, year_start, year_end){
  #' @description This function cleans the salary CSV, dealing with
  #' player's who were traded
  #'
  #' 
  #' @param playerSalary dataframe. Read in salary_2017.csv file
  #' @return dataframe. A clean and complete salaries dataframe
  #' 
  
  # Create Empty Dataframe
  playerSalarySubset <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(playerSalarySubset) <- c("Player", "Salary", "Year")
  for(year in year_start:year_end){
    playerSalarySubset_temp <- cleanPlayerSalaryYear(playerSalary, year)
    playerSalarySubset <- rbind(playerSalarySubset, playerSalarySubset_temp)
    
  }
  
  
  # Return Clean Dataframe
  return(playerSalarySubset)
}



# playerSalaies<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
# cleanPlayerSalary(playerSalaies,year=2017)
# 
# test2 <- cleanPlayerSalaryTotal(playerSalaies, year_start=2010, year_end=2017)
# 
# test <- playerSalay_2017 %>% group_by(Player) %>% summarise(Salary = sum(Salary))
