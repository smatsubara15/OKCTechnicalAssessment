library(dplyr)
shotsData<-data.frame(read.csv("/Users/scottsmacbook/OKCThunderAssessment/shots_data.csv"))

# use to count how many shots were taken in each zone
shotsData$shotsTaken<-1

# shot_distance is the distance of the shot from the basket, used to calculate if the shot is a three pointer.
shotsData$shotDistance<-sqrt(shotsData$x^2+shotsData$y^2)

# if the shot has a value of greater than 22 on the x axis, and a value of less than 7.8 on the y axis, 
#it is considered a corner three. If it does not meet this criteria, but the distance from the basket is greater than 23.75, then it is considered a non-corner three. Any other shot is considered a 2-pointer.
shotsData$shotType<-ifelse(abs(shotsData$x)>22 & shotsData$y<=7.8,"C3",
                             ifelse(shotsData$shotDistance>23.75,"NC3","2PT"))
#group the data by team and shot type so that we can find the shot distribution and eFG% from each spot
teamData<-shotsData %>% group_by(team,shotType) %>%
  dplyr::summarize(shotsMade = sum(fgmade),shotsTaken=sum(shotsTaken)) %>% 
  as.data.frame()

TotalShots<-length(shotsData$team)
TeamAShots<-sum(teamData[which(teamData$team=="Team A"),4])
TeamBShots<-TotalShots-TeamAShots

# used in calculation of eFG% where we add 0.5*3PM to the numberator
teamData$isThree<-ifelse(teamData$shotType=="C3" | teamData$shotType=="NC3",1,0)

teamData$shotDistribution<-round(ifelse(teamData$team=="Team A",
                                teamData$shotsTaken/TeamAShots,teamData$shotsTaken/TeamBShots),4)

teamData$eFGpercentage<-round((teamData$shotsMade+(teamData$isThree*0.5*teamData$shotsMade))/teamData$shotsTaken,4)

finalTeamData<-teamData[c("team","shotType","shotDistribution","eFGpercentage")]

print(finalTeamData)