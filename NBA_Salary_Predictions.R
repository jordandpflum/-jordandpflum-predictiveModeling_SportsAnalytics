rm(list=ls())
player_salary_1985_2018<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
player_stats_1985_2018<-read.csv("Data/PlayerSalary_Season/players_career_metrics.csv", stringsAsFactors = FALSE)
player_metrics_season<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
##both datasets do have unique id but with different names
##Renaming it to player_id 
player_metrics_season_1985_2017<- player_metrics_season[player_metrics_season$Year >= 1985,]
names(player_stats_1985_2018)[names(player_stats_1985_2018) == 'X_id'] <- 'player_id'

##player_salary_stats_1985_2018 <- merge(player_salary_1985_2018,player_stats_1985_2018,by="player_id")

