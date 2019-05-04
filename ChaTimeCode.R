library(tidyverse)
library(ggplot2)
library(dbplyr)
library(MASS)

#Function
##Bhattacharyya using MASS package
bhatta <- function(df1, df2, bns = 512, xmin = -15, xmax =15, ymin = -15, ymax =15){
  dfprob <-kde2d(df1$X,df1$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob$z <- dfprob$z /sum(dfprob$z)
  dfprob2 <-kde2d(df2$X,df2$Y,n=bns, lims=c(xmin, xmax, ymin, ymax))
  dfprob2$z <-dfprob2$z /sum(dfprob2$z)
  bhat <- sum(sqrt(dfprob2$z * dfprob$z))
  return(bhat)
}

TIZ <- function(movementdf, rewarddf){
  #caculates the zone
  h<-mean(rewarddf$X)
  k<-mean(rewarddf$Y)
  r <- 6
  #Calculates the percent time in zone
  zonemv<- subset(movementdf, (X-h)^2+(Y-k)^2 <= r^2)
  timespent <- length(zonemv$X)/length(movementdf$X)
  return(timespent)
}



df <- CleanedParticipantData

#Clean the data
df1<-df[!(df$Session=="Test" & df$RewardFound==1),]

#Remove Participant28
df1 <- subset(df1, !Participant == 28)

#MasterML <- df1
#MasterML <- rename(MasterML,
#                   PickupType = RewardType,
#                   TargetFound = RewardFound,
#                   BlockID = Block)


RType = c('F','M','W')

BhattasTIZ <- data.frame()

for (p in unique(df1$Participant)){
  for (i in RType){
    ##Create reward subset from Training
    Reward <- subset(df1, Participant == p & RewardType == i & Session == 'Train' & RewardFound == 1)
    #Create Movement from Test
    Movement <- subset(df1, Participant == p & Session == 'Test' & Movement == 1 & TrialTime > 5 & RewardType == i)
    #Calculate Performance - targets found in last block of session 1
    ##Calculates the number of rewards they found in the last block
    LastBlock <- subset(Reward, Block == max(Reward$Block))
    trials <- unique(LastBlock$Trial)
    found<-length(LastBlock$RewardFound)
    perfound <- found/length(trials)
    #Calulate Bhattacharyaa
    bhat <- round(bhatta(Movement, Reward), digits = 4)
    #Calculate TIZ
    timein <- round(TIZ(Movement, Reward), digits = 4)
    dt <- data.frame("Participant" = p,"Environment"= unique(Reward$Environment), "Delay" = unique(Reward$Delay), "Item" = i, "Bhattacharyya" = bhat, "TIZ" = timein,  "%Found" = perfound)
    BhattasTIZ <- rbind(BhattasTIZ, dt)
  }
}

