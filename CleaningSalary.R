rm(list=ls())
player_salary_1985_2018<-read.csv("D:/rs/Data/PlayerSalary_Season/salaries_1985to2018.csv", stringsAsFactors = FALSE)
edit <- subset(player_salary_1985_2018,season_start==2017)
#write.csv(edit,"D:/rs/Data/PlayerSalary_Season/salaries_2017.csv")
## Find null value in the table
is.na(edit)
mydata[!complete.cases(mydata),]
## Find duplicates because they were traded
edit[duplicated(edit$player_id),]
## Find duplicate IDs
count=1
countd=1
duplist<-c()
while (count<551){
  if (dup$player_id[countd]==edit$player_id[count]){
    duplist<- c(duplist,count)
    countd=countd+1
  }
  count=count+1
}
## Find unduplicated duplicates IDs
dupn=dup$player_id[!duplicated(dup$player_id)]
## Find the amount of total salary of each ID
ama<-c()
count=1
countd=1
amount=0
while (countd<30){
  while (count<552){
    if (edit$player_id[count]==dupn[countd]){
      amount=amount+edit$salary[count]
    }
    count=count+1
  }
  ama<-c(ama,amount)
  countd=countd+1
  amount=0
  count=1
}
##Add the total salary to the IDs
count=1
countd=1
while (count<552){
  while (countd<30){
    if (edit$player_id[count]==dupn[countd]){
      edit$salary[count]=ama[countd]
    }
  countd=countd+1
  }
  count=count+1
}
##Delete all duplicates
editn=edit[!duplicated(edit$player_id),]