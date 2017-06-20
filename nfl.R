library(dplyr)

#Download the NFL data in zipped format
location<-"http://www.repole.com/sun4cast/stats/nflstats.zip"
zip_file <- "nfltemp/nflstats"

#Uncomment these next 3 lines to 
#dir.create("nfltemp")
#download.file(location,zip_file)
#unzip(zip_file)

unzip(zip_file, exdir="nfltemp")

#Need to read 2012 and 2013
nfl_game_2013_data_frame<-read.csv(file="nfltemp/nfl2013stats.csv")
nfl_game_2012_data_frame<-read.csv(file="nfltemp/nfl2012stats.csv")

#Combine into one dataframe
nfl_game_data_frame<-rbind(nfl_game_2012_data_frame, nfl_game_2013_data_frame)

nfl_data_frame_by_team<-group_by(nfl_game_data_frame, "Team Name")

#However, instead of using the current stats (which would be useless), 
#we can use average of the last 16 games for each team

#Best one so far
allrows = colnames(nfl_game_data_frame)[c(-1, -2, -18, -33, -34, -35)]

#Convert percentage to numbers
nfl_game_data_frame$ThirdDownPctOff <- as.numeric(sub("%","", nfl_game_data_frame$ThirdDownPctOff))
nfl_game_data_frame$ThirdDownPctDef <- as.numeric(sub("%","", nfl_game_data_frame$ThirdDownPctDef))
nfl_game_data_frame$TimePossOff <- as.numeric(sub(":",".", nfl_game_data_frame$TimePossOff))
nfl_game_data_frame$TimePossDef <- as.numeric(sub(":",".", nfl_game_data_frame$TimePossDef))

#Mark the winner or loser
nfl_game_data_frame[nfl_game_data_frame$ScoreOff - nfl_game_data_frame$ScoreDef < 0, "WinOrLose"] = "WIN"
nfl_game_data_frame[nfl_game_data_frame$ScoreOff - nfl_game_data_frame$ScoreDef > 0, "WinOrLose"] = "LOSE"

#See if the game was "Fun"
nfl_game_data_frame["Fun"] = "OKAY"
nfl_game_data_frame[(abs(nfl_game_data_frame$ScoreOff - nfl_game_data_frame$ScoreDef) < 15) & ((nfl_game_data_frame$PassYdsOff + nfl_game_data_frame$PassYdsOff) > 550), "Fun"] = "FUN"
nfl_game_data_frame[(abs(nfl_game_data_frame$ScoreOff - nfl_game_data_frame$ScoreDef) > 20) | ((nfl_game_data_frame$PassYdsOff + nfl_game_data_frame$PassYdsOff) < 450), "Fun"] = "BORING"

#From http://stackoverflow.com/questions/32855107/how-to-sum-last-n-rows-conditionaly-in-r
#Add the game number for each
nfl_game_data_frame %>%
    group_by(TeamName) %>%
    mutate(game_num = rank(Date)) %>%
    as.data.frame -> z

#Add the last 16 and average
last16 <- function(x) {
    z[x, "TeamName"] -> team
    z[x, "game_num"] -> game
    game <- game - 1
    game - 15 -> last_three
    if(last_three < 1) last_three <- 0
    z[z$game_num %in% last_three:game &
          z$TeamName == team, allrows] -> pnts
    
    colSums(pnts)/16
}

#Get the totals
totals <- sapply(1:nrow(z), FUN = last16)

#Make it a data frame
nfl_stats_total<-data.frame(z$Site, z$Line, z$TotalLine, z$WinOrLose, z$Fun, t(totals))

names(nfl_stats_total)<-sub("z.","", names(nfl_stats_total))

#Write to file
nfl_stats_total_2013 <- nfl_stats_total[(nrow(nfl_stats_total)/2 + 1):nrow(nfl_stats_total) ,]
write.csv(nfl_stats_total_2013, file="nfl_game_2013.csv", row.names = FALSE)

##################################
# Seperate into testing/training
##################################

nfl_stats_total_2013<-subset(nfl_stats_total_2013, select = -c(PuntAvgOff))
trainingAll <- nfl_stats_total_2013
library(caret)
set.seed(62433)

inTrain<-seq(0, 411, 1)
nfl_training <- trainingAll[ inTrain,]
nfl_validation <- trainingAll[-inTrain,]

nfl_fun_training = subset(nfl_training, select = -c(WinOrLose))
nfl_fun_validation = subset(nfl_validation, select = -c(WinOrLose))

nfl_training<-subset(nfl_training, select = -c(Fun))
nfl_validation<-subset(nfl_validation, select = -c(Fun))