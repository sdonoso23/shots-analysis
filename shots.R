library(RSQLite)
library(tidyverse)
library(caret)
library(DT)
source("functions.r")

con<-dbConnect(drv=SQLite(), 
               dbname="C:/Users/Administrador.000/Desktop/Sebastian/Code/Whoscored/CSV/Total/test2.sqlite")



####Variable Analysis####
#General:

nmatches<-matches %>%
    group_by(league,season) %>%
    summarize(nmatches=n())

shots %>%
    filter(period != "PenaltyShootout") %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),goalavg=round(goals/shots,3),
              penalties=sum(Penalty==1),
              penaltygoals=sum(ifelse(Penalty==1 & Goal==1,1,0)),
              penaltyavg=round(penaltygoals/penalties,3),
              owngoals=sum(OwnGoal==1)) %>%
    left_join(nmatches,by=c("league","season")) %>%
    select(1:2,10,3:length(.)) %>%
    mutate(shotsavg=round(shots/nmatches,3),goalavgmatch=round(goals/nmatches,3)) %>%
    datatable()


#Location: X & Y Angle & Distance

shots %>%
    filter(period != "PenaltyShootout" | OwnGoal==0) %>%
    select(keyid,league,season,Goal,Angle,Distance) %>%
    gather(Angle:Distance,key=var,value=value) %>%
    ggplot()+geom_density(aes(x=value,color=Goal))+
    facet_wrap(~var,scales = "free")+theme_classic()
    
shots %>%
    filter(period != "PenaltyShootout") %>%
    filter(OwnGoal==0) %>%
    select(keyid,league,season,x,y,Goal,Angle,Distance) %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),shotmeanx=mean(x),
              x50=sum(x<50))


shots %>%
    filter(x<25 & OwnGoal==0) %>%
    #{ggpitch("white","black")+geom_point(data=.,aes(x=x,y=y))}
    datatable()
    

    
#Assist: Assisted
shots %>%
    filter(period != "PenaltyShootout") %>%
    filter(OwnGoal==0) %>%
    select(keyid,league,season,x,y,Assisted,Goal) %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),
              assistedshots=sum(Assisted==1),
              assistedgoals=sum(Assisted==1 & Goal==1),
              assistedpct=round(assistedshots/shots,3),
              assistedpctgoals=round(assistedgoals/goals,3)) %>%
    datatable()



#Game situation: DirectFreekick, FastBreak, FromCorner, RegularPlay, SetPiece, ThrowinSetPiece

shots %>%
    mutate(suma=DirectFreekick+Penalty+FastBreak+FromCorner+RegularPlay+SetPiece+ThrowinSetPiece) %>%
    filter(suma==0)
    
shots %>%
    select(keyid,league,season,Penalty,DirectFreekick,FastBreak,FromCorner,RegularPlay,SetPiece,ThrowinSetPiece) %>%
    mutate(suma=DirectFreekick+Penalty+FastBreak+FromCorner+RegularPlay+SetPiece+ThrowinSetPiece) %>%
    filter(suma==0) %>%
    group_by(league,season) %>%
    summarize(n())

names(shots)

#Bodypart: Head, OtherBodyPart, Left Foot, Right Foot
shots %>%
    mutate(suma=Head+OtherBodyPart+LeftFoot+RightFoot) %>%
    group_by(league,suma) %>%
    summarize(n())

shots %>%
    filter(period != "PenaltyShootout" | OwnGoal==0) %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),
              headshots=sum(Head==1),
              headgoals=sum(Head==1 & Goal==1),
              headshotsavg=round(headshots/shots,3),
              headgoalsavg=round(headgoals/goals,3),
              obpshots=sum(OtherBodyPart==1),
              obpgoals=sum(OtherBodyPart==1 & Goal==1),
              obpshotsavg=round(obpshots/shots,3),
              obpgoalsavg=round(obpgoals/goals,3),
              rfshots=sum(RightFoot==1),
              rfgoals=sum(RightFoot==1 & Goal==1),
              rfshotsavg=round(rfshots/shots,3),
              rfgoalsavg=round(rfgoals/goals,3),
              lfshots=sum(LeftFoot==1),
              lfgoals=sum(LeftFoot==1 & Goal==1),
              lfshotsavg=round(lfshots/shots,3),
              lfgoalsavg=round(lfgoals/goals,3),
              feetshots=lfshots+rfshots,
              feetgoals=lfgoals+rfgoals,
              feetshotsavg=round(feetshots/shots,3),
              feetgoalsavg=round(feetgoals/goals,3)) %>%
    datatable()

#Extra: Big Chance
shots %>%
    filter(period != "PenaltyShootout" | OwnGoal==0) %>%
    group_by(league,season) %>%
    summarize(shots=n(),goals=sum(Goal==1),
              bigchance=sum(BigChance==1),
              bigchancegoals=sum(BigChance==1 & Goal==1),
              bigchanceshotsavg=round(bigchance/shots,3),
              bigchancegoalavg=round(bigchancegoals/goals,3)
              ) %>%
    datatable()

shots %>%
    filter(BigChance==1) %>%
    group_by(league,season) %>%
    summarize(bigchance=sum(BigChance==1),minx=min(x)) %>%
    datatable()
    
shots %>%
    filter(BigChance==1 & x<60)
