library(RSQLite)
library(tidyverse)
library(feather)
library(DT)
source("r/functions.r")

# Set path of the database
dbpath<-"C:/Users/Administrador.000/Desktop/Sebastian/Code/Whoscored/CSV/Total/test2.sqlite"
# Set parameter value to "sql" or "file"
parameter <- "file"


# Get all shot data in database

con<-dbConnect(drv=SQLite(),dbname=dbpath)
shots<-update_shots(parameter)

# Get matches data and number of matches per season
matches<-dbGetQuery(con,"SELECT * FROM matches")

nmatches<-matches %>%
    group_by(league,season) %>%
    summarize(nmatches=n())


# Data for general analysis

general_summ<-shots %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),goalavg=round(goals/shots,3),
              penalties=sum(Penalty==1),
              penaltygoals=sum(ifelse(Penalty==1 & Goal==1,1,0)),
              penaltyavg=round(penaltygoals/penalties,3),
              owngoals=sum(OwnGoal==1)) %>%
    left_join(nmatches,by=c("league","season")) %>%
    select(1:2,10,3:length(.)) %>%
    mutate(shotsavg=round(shots/nmatches,3),goalavgmatch=round(goals/nmatches,3))



dbDisconnect(con)