DATA <- read.csv("2017_player_salary_and_metrics.csv")
library(ggplot2)
library(plyr)
#compare team salaries with each other
select_teams <- c(salarydf$Team)

sal <- salarydf[salarydf$Team %in% select_teams,] # keeping those in the set

sal$Team <- factor(sal$Team) #refactoring to eliminate empty levels.
boxplot(sal$Salary.in.. ~ sal$Team)

#summary of salaries
summary(salarydf$Salary.in..)

#summary of salaries just one team
justmiami <- c(which(salarydf$Team == 'MIA'))
miami <- salarydf[justmiami,]
summary(miami$Salary.in..)

##density plot with mean line and color by position ----------------------------
#prints a density plot with lines for each category 
#need to install ggplot2
#Parameters: 
  #data <- data frame you're using
  # x <- variable you want density of 
  #column <- name of categorical variable in data frame to separate lines
density.many <- function(dataframe,X,column){
  attach(dataframe)
  dplot <- ggplot(dataframe, aes(x=X, color=column)) +
    geom_density()+
    geom_vline(data=dataframe, aes(xintercept=grp.mean, color=column),
               linetype="dashed")
  dplot
}


density.many(DATA,Age,Pos)  
head(DATA)
attach(DATA)
#lildata <- ddply(DATA, "FGA", summarise, grp.mean=mean(FGA))
#lildata <- ddply(DATA, "Pos", summarise, grp.mean=mean(Age))

#ggplot(DATA, aes(x=Age, color=Pos)) +
#  geom_density()


#THIS WORKS-----------------DENSITY PLOT WITH MEAN LINES------------------
#calc mean for each group
lildata <- ddply(DATA, "Pos", summarise, grp.mean=mean(Age))
#make density plot 
ggplot(DATA, aes(x=Age, color=Pos)) +
  geom_density()
# Add mean lines
p<-ggplot(DATA, aes(x=Age, color=Pos)) +
  geom_density()+
  geom_vline(data=lildata, aes(xintercept=grp.mean, color=Pos),
             linetype="dashed")
p


#need to fix function
# http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
dplot <- function(df, catstring, xvariable, category){
  lildata <- ddply(df, catstring, summarise, grp.mean = mean(xvariable))
  #make density plot 
  ggplot(df, aes(x=xvariable, color=category)) +
    geom_density()
  # Add mean lines
  p<-ggplot(df, aes(x=xvariable, color=category)) +
    geom_density()+
    geom_vline(data=lildata, aes(xintercept=grp.mean, color=category),
               linetype="dashed")
  p
}
dplot(DATA, "Position", Age, Pos) #practice dplot fxn


#THIS WORKS-----------------DENSITY PLOT WITH MEAN LINES------------------
#Parameters 
  #df - dataframe
  #xaxis - column of data frame that is categories for x axis boxes
  #yaxis - column of data frame that is count for y axis
  # labelstr - string value for the y axis label 
barchart <- function(df, xaxis, yaxis){
  attach(df)
  labelstr <- max(df$yaxis)
  bar<-ggplot(data=df, aes(x=xaxis, y=yaxis)) +
    geom_bar(stat="identity", fill="steelblue")+
    theme_minimal()
  bar
}
barchart(DATA, Pos, FGA)


#practice <- ggplot(DATA, aes(x=factor(Pos)))+
 # geom_bar(stat="count", width=0.7, fill="steelblue")+
  #geom_text(aes(label=yaxis), vjust=-0.3, size=3.5)+
    #theme_minimal()
#practice


#SIDE BY SIDE BAR CHART
#subset for a two teams
#Parameters: 
  #team one - string of team value Ex: 'MIA'
  #team two - string of team value Ex: 'OKC'

sidebyside <- function(team1, team2){
  someteams <- DATA[which(DATA$Tm==team1),]
  otherteams <- DATA[which(DATA$Tm==team2),]  
  both <- rbind(someteams,otherteams)
  bothgroups<- ggplot(data=both, aes(x=Tm, y=salary, fill=Pos)) +
    geom_bar(stat="identity", position=position_dodge())
  bothgroups
}
sidebyside('MIA', 'HOU')#practice


#summary of values 
summary(DATA$FGA)
