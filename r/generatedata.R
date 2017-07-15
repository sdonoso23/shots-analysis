library(RSQLite)
library(tidyverse)
library(feather)
library(DT)
source("r/functions.r")

# Set path of the database
dbpath<-"C:/Users/Administrador.000/Desktop/Sebastian/Code/Whoscored/CSV/Total/test2.sqlite"
con<-dbConnect(drv=SQLite(),dbname=dbpath)


# Set parameter value to "sql" or "update"
parameter <- "update"

# Get all shot data in database
shots<-update_shots(parameter)

# Get matches data and number of matches per season
matches<-dbGetQuery(con,"SELECT * FROM matches")

nmatches<-matches %>%
    group_by(league,season) %>%
    summarize(nmatches=n())

dbDisconnect(con)