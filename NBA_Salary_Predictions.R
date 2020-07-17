rm(list=ls())
player_salary_1985_2018<-read.csv("Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
player_stats_1985_2018<-read.csv("Data/PlayerSalary_Season/players_career_metrics.csv", stringsAsFactors = FALSE)
player_metrics_season<-read.csv("Data/PlayerMetrics_Season/player_metric_season.csv", stringsAsFactors = FALSE)
#both datasets do have unique id but with different names
#Renaming it to player_id 
#player_metrics_season2017<- subset(player_metrics_season, Year == 2017)


#names(player_stats_1985_2018)[names(player_stats_1985_2018) == 'X_id'] <- 'player_id'

#player_salary_stats_1985_2018 <- merge(player_salary_1985_2018,player_stats_1985_2018,by="player_id")

# Single out 2017
edit <- subset(player_metrics_season, Year == 2017)

# Remove NA columns
edit[,c("blank2","blanl")] <- list(NULL)

# Write file
#write.csv(edit,"C:/Users/timot/Documents/2017_player_metric.csv")

# The ID/Names of people have multiple observations because they were traded
ind <- which(duplicated(edit$Player))
unique(edit$COLID[ind]) #IDs of ppl who were traded
unique(edit$Player[ind]) #Names of ppl who were traded
length(unique(edit$Player[ind]))
edit <- edit[-c(ind),]
# The ID/Names of people with straght up missing stats AND weren't traded


full <- complete.cases(edit)       # find the players with all of their stats
ind2 <- which(full == FALSE)       # find which players dont have all their stats
x <- edit[ind2,]                   # make a subset table of those players
ind3 <- which(duplicated(x$Player)) # which ones have NA because of the trade
non <- x[ind3,]                     # subset those ones
repeats <- unique(non$Player) 
x <- x[!x$Player %in% repeats,]     # take out the traded players
edit <- edit[-c(ind2),]             # edit now has all unique players and players
                                    #with all the stats


x$COLID
x$Player
length(x$Player)
player<-edit$Player
edit$firstName <- sapply(strsplit(edit$Player, ' '), function(x) x[1]) #Get first name
edit$lastName <- sapply(strsplit(edit$Player, ' '), function(x) x[length(x)]) #Get last name
edit$unique <- apply(edit[,c('firstName', 'lastName')], 1, function(x) { paste(substring(tolower(x[2]),1,5),substring(tolower(x[1]),1,1),"01", sep = "", collapse = "") }) #Create unique id
