library(tidyverse)
library(ggplot2)
library(feather)
library(RSQLite)

####GGPLOT####

#Draws pitch via ggplot, can set pitch and lines colors 

ggpitch<-function(pitchcolor,linecolor){
    return(ggplot()+
         geom_segment(aes(x=0,y=0,xend=0,yend=100),color=linecolor)+
         geom_segment(aes(x=100,y=0,xend=0,yend=0),color=linecolor)+
         geom_segment(aes(x=0,y=100,xend=100,yend=100),color=linecolor)+
         geom_segment(aes(x=100,y=0,xend=100,yend=100),color=linecolor)+
         geom_segment(aes(x=50,y=0,xend=50,yend=100),color=linecolor)+
         geom_segment(aes(x=100,y=62.5,xend=93.75,yend=62.5),color=linecolor)+
         geom_segment(aes(x=100,y=37.5,xend=93.75,yend=37.5),color=linecolor)+
         geom_segment(aes(x=93.75,y=62.5,xend=93.75,yend=37.5),color=linecolor)+
         geom_segment(aes(x=81.25,y=18.75,xend=81.25,yend=81.25),color=linecolor)+
         geom_segment(aes(x=81.25,y=18.75,xend=100,yend=18.75),color=linecolor)+
         geom_segment(aes(x=81.25,y=81.25,xend=100,yend=81.25),color=linecolor)+
         geom_segment(aes(x=0,y=62.5,xend=6.25,yend=62.5),color=linecolor)+
         geom_segment(aes(x=0,y=37.5,xend=6.25,yend=37.5),color=linecolor)+
         geom_segment(aes(x=6.25,y=62.5,xend=6.25,yend=37.5),color=linecolor)+
         geom_segment(aes(x=18.75,y=18.75,xend=18.75,yend=81.25),color=linecolor)+
         geom_segment(aes(x=18.75,y=18.75,xend=0,yend=18.75),color=linecolor)+
         geom_segment(aes(x=18.75,y=81.25,xend=0,yend=81.25),color=linecolor)+
         geom_curve(aes(x=50,y=35,xend=50,yend=65),curvature=1,ncp=400,color=linecolor)+
         geom_curve(aes(x=50,y=35,xend=50,yend=65),curvature=-1,ncp=400,color=linecolor)+
         geom_curve(aes(x=18.75,y=35,xend=18.75,yend=65),curvature=0.5,ncp=400,color=linecolor)+
         geom_curve(aes(x=81.25,y=35,xend=81.25,yend=65),curvature=-0.5,ncp=400,color=linecolor)+
         coord_cartesian(xlim=c(0,100),ylim=c(0,100))+
         theme(legend.position="right",panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               panel.background = element_rect(pitchcolor),
               axis.line=element_blank(),axis.text.x=element_blank(),
               axis.text.y=element_blank(),axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank()))}
                                 
######DATA FUNCTIONS######

# Gets all shots with its qualifiers in database
get_allshots<-function(con){
    system.time(events<-dbGetQuery(con,
                                   "SELECT * FROM events where type IN 
                                   ('SavedShot','Goal','MissedShots','ShotOnPost')"))
    
    system.time(qualifiers<-dbGetQuery(con,
                                       "SELECT keyid, qualname, qualvalue FROM qualifiers 
                                       WHERE keyid IN 
                                       (SELECT keyid FROM events where type IN 
                                       ('SavedShot','Goal','MissedShots','ShotOnPost'))"))
    
    matches<-dbGetQuery(con,"SELECT wsmatchid,season,league,hometeamname,
                        awayteamname FROM matches")
    players<-dbGetQuery(con,"SELECT * FROM players")
    teams<-dbGetQuery(con,"SELECT * FROM teams")
    
    shots<-events %>%
        left_join(matches,by="wsmatchid") %>%
        left_join(players,by="playerid") %>%
        left_join(teams,by="teamid") %>%
        left_join(qualifiers,by="keyid") %>%
        spread(key=qualname,value=qualvalue,convert=TRUE,fill=0) %>%
        mutate(Goal=if_else(type=="Goal",1,0),Angle=anglexy(x,y),Distance=distxy(x,y))
    
    shots$Goal<-as.factor(shots$Goal)
    
    return(shots)
    
}


# Function to update or load data. 
# Parameter value: "sql" to load from sql  (takes around 10 min)
# Parameter value: "file" to load from file (faster if available) 

update_shots<-function(parameter){
    
    if (parameter=="sql"){
    #Eliminate UCL penalty shootouts and own goals
        shots<-get_allshots(con) %>%
            filter(period != "PenaltyShootout") %>%
            filter(OwnGoal == 0) 
        
        write_feather(shots,"data/shots.feather")
        
    }else{
        shots<-read_feather("data/shots.feather")
    } 
    return(shots)
}    




######MUTATE FUNCTIONS#####

# Distance from shot location to center of goal
distxy<-function(x,y){
    distance<-sqrt(((100-x)^2+(50-y)^2))
    return(distance)                   
}

# Angle formed between shot location and both posts
anglexy<-function(x,y){
    side1<-sqrt(((100-x)^2+(45-y)^2))
    side2<-sqrt(((100-x)^2+(55-y)^2))
    side3<-sqrt(((100-100)^2+(55-45)^2))
    a<-side1^2+side2^2-side3^2
    b<-2*side1*side2
    x<-if_else(is.na(acos(a/b)),3.14,acos(a/b))
    return(x)
    }


