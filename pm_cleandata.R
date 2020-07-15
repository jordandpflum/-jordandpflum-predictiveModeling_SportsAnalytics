# Set file location
setwd("C:/Users/timot/Desktop")
x <- read.csv("player_metric_season.csv", header = TRUE)

# Single out 2017
edit <- subset(x, Year == 2017)

# Remove NA columns
edit[,c("blank2","blanl")] <- list(NULL)

# Write file
write.csv(edit,"C:/Users/timot/Documents/2017_player_metric.csv")

# The ID/Names of people have multiple observations because they were traded
ind <- which(duplicated(edit$Player))
unique(edit$COLID[ind]) #IDs of ppl who were traded
unique(edit$Player[ind]) #Names of ppl who were traded
length(unique(edit$Player[ind]))

# The ID/Names of people with straght up missing stats AND weren't traded


full <- complete.cases(edit)       # find the players with all of their stats
ind2 <- which(full == FALSE)       # find which players dont have all their stats
x <- edit[ind2,]                   # make a subset table of those players
ind3 <- which(duplicated(x$Player)) # which ones have NA because of the trade
non <- x[ind3,]                     # subset those ones
repeats <- unique(non$Player) 
x <- x[!x$Player %in% repeats,]     # take out the traded players

x$COLID
x$Player
length(x$Player)


# Which variables/columns have missing data

colnames(edit)[!complete.cases(t(edit))]

