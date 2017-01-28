Team_Years_Facet_Plot<-ggplot(Tm_rnd_Outcome_Summary,aes(Rnd,Avg_Years_Played,colour=Rnd,label=round(Avg_Years_Played,digits = 0)))+geom_point()+geom_text(hjust = 0, nudge_x = -.5,size=2.5)+facet_wrap(~Tm)+xlab("Draft Round")+ylab("Avg. # Years Played")+theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+scale_colour_tableau()+scale_shape_tableau()

0nfl<-filter(nfl,Rnd %in% c(1,2,3,4,5,6,7))
levels(nfl$Tm)[levels(nfl$Tm)=="beta"] <- "two"
Tm_group<-group_by(nfl,Tm)
Tm_rnd_group<-group_by(nfl,Tm,Rnd)
Tm_pos_group<-group_by(nfl,Tm,Position.Standard)
Tm_rnd_pos_group<-group_by(nfl,Tm,Rnd,Position.Standard)
rnd_pos_group<-group_by(nfl,Rnd,Position.Standard)
age_group<-group_by(nfl,Age)
sch_group<-group_by(nfl,College.Univ)
pos_group<-group_by(nfl,Position.Standard)
rd_group<-group_by(nfl,Rnd)
tm_group<-group_by(nfl,Tm)

Tm_Outcome_Summary<-summarise(Tm_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
Tm_rnd_Outcome_Summary<-summarise(Tm_rnd_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
Tm_pos_Outcome_Summary<-summarise(Tm_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),Avg_Rnd=mode(Rnd))
Tm_rnd_pos_Outcome_Summary<-summarise(Tm_rnd_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
Rnd_Pos_Outcome_Summary<-summarise(rnd_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2))
Age_Outcome_Summary<-summarise(age_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
College_Outcome_Summary<-summarise(sch_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))

install.packages("ggthemes")
library(ggthemes)
install.packages("ggcolor")
Team_Years_Facet_Plot
