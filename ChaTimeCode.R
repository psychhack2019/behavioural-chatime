library(tidyverse)
library(ggplot2)
library(dbplyr)
library(MASS)
library(ez)

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


#ANOVA comparing natural vs artifact rewards in natural vs urban env
NvsA <- aov(Bhattacharyya ~ Environment*Item, data=BhattasTIZ)
summary(NvsA)

#T-test comparing Environment 
t.test(BhattasTIZ$Bhattacharyya~BhattasTIZ$Environment)

#### Graphing the natural vs artifact in rural vs urban ####
plotDat <- BhattasTIZ %>%
  group_by(Environment,Item) %>%
  summarise(Bmean=mean(Bhattacharyya),SE= sd(Bhattacharyya)/sqrt(length(Bhattacharyya)))

plotDat$Environment <- as.numeric(plotDat$Environment)
#as.numeric changes into 1 == Rural, 2 == urban because they were factors
plotDat$Environment[plotDat$Environment==1] <- "Rural"
plotDat$Environment[plotDat$Environment==2] <- "Urban"

plotDat$Item <-as.character(plotDat$Item)
plotDat$Item[plotDat$Item=="F"] <- "Food"
plotDat$Item[plotDat$Item=="M"] <- "Money"
plotDat$Item[plotDat$Item=="W"] <- "Water"

ggplot(plotDat, aes(x=Environment, y=Bmean, fill=Item)) + 
  geom_bar(position=position_dodge(),stat = "identity") +
  scale_fill_manual(values=c("gold1","darkseagreen1","lightskyblue2")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  geom_errorbar(aes(ymin=Bmean-SE, ymax=Bmean+SE),  position=position_dodge(.9),width=0.2) +
  theme_classic() +
  labs(x ="Environment", y = "Mean Bhattacharyya", fill = "Reward Type")  #change label

#### Graphing Rural vs. Urban with RewardType combined ####
plotDat2 <- BhattasTIZ %>%
  group_by(Environment) %>%
  summarise(Bmean=mean(Bhattacharyya),SE= sd(Bhattacharyya)/sqrt(length(Bhattacharyya)))

plotDat2$Environment <- as.numeric(plotDat2$Environment)
#as.numeric changes into 1 == Rural, 2 == urban because they were factors
plotDat2$Environment[plotDat2$Environment==1] <- "Rural"
plotDat2$Environment[plotDat2$Environment==2] <- "Urban"

ggplot(plotDat2, aes(x=Environment, y=Bmean, fill=Environment)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("mistyrose1","paleturquoise2")) +
#  scale_fill_brewer(palette = "Dark2") + #change colour
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) + 
  geom_errorbar(aes(ymin=Bmean-SE, ymax=Bmean+SE),width=0.2) +
  theme_classic() +
  labs(x ="Environment", y = "Mean Bhattacharyya", fill="")

#df of Ps with lowest/ highest B coefficient
L45train <- filter(df1, Participant == 45 & Session == "Train" & RewardType == "W")
L45test <- filter(df1, Participant == 45 & Session == "Test" & RewardType == "W")

H4train <- filter(df1, Participant == 4 & Session == "Train" & RewardType == "W")
H4test <- filter(df1, Participant == 4 & Session == "Test" & RewardType == "W")

write.csv(L45train, "/behavioural-chatime/L45train.csv")
write.csv(L45test, "/behavioural-chatime/L45test.csv")
write.csv(H4train, "/behavioural-chatime/H4train.csv")
write.csv(H4test, "/behavioural-chatime/H4test.csv")
