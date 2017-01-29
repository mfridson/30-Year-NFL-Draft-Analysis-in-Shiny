Team_Years_Facet_Plot<-ggplot(Tm_rnd_Outcome_Summary,aes(Rnd,Avg_Years_Played,colour=Rnd,label=round(Avg_Years_Played,digits = 0)))+geom_point()+geom_text(hjust = 0, nudge_x = -.5,size=2.5)+facet_wrap(~Tm)+xlab("Draft Round")+ylab("Avg. # Years Played")+theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))+scale_colour_tableau()+scale_shape_tableau()

Age_Years_Plot<-ggplot(Age_Outcome_Summary,aes(Position.Standard,Avg_Years_Played,fill=Age))+geom_bar(stat="identity",position="dodge")+xlab("Draft Age")+ylab("Avg. # Years Played")+theme_fivethirtyeight()+scale_colour_tableau()+scale_shape_tableau()

Rnd_Years_Plot<-ggplot(Rnd_Pos_Outcome_Summary,aes(Rnd,Pro_Bowl_App,fill=Position))+geom_bar(stat="identity",position="dodge")+xlab("Draft Age")+ylab("Avg. # Years Played")+theme_fivethirtyeight()+scale_colour_tableau()+scale_shape_tableau()

nfl<-filter(nfl,Rnd %in% c(1,2,3,4,5,6,7))

Tm_rnd_group<-group_by(nfl,Tm,Rnd)
Tm_pos_group<-group_by(nfl,Tm,Position.Standard)
Tm_rnd_pos_group<-group_by(nfl,Tm,Rnd,Position.Standard)
rnd_pos_group<-group_by(nfl,Rnd,Position=Position.Standard)
age_group<-group_by(nfl,Age,Position.Standard)
sch_group<-group_by(nfl,College.Univ)
pos_group<-group_by(nfl,Position.Standard)
rd_group<-group_by(nfl,Rnd)
tm_group<-group_by(nfl,Tm)


Tm_rnd_Outcome_Summary<-summarise(Tm_rnd_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
Tm_pos_Outcome_Summary<-summarise(Tm_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),Avg_Rnd=mode(Rnd))
Tm_rnd_pos_Outcome_Summary<-summarise(Tm_rnd_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))
Rnd_Pos_Outcome_Summary<-summarise(rnd_pos_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2))
Age_Outcome_Summary<-filter(summarise(age_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),Avg_PBs_PerYear=mean(PB_Per_Year_Played,2),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS")),Age!="Not Applicable" & Position.Standard!="LS")
College_Outcome_Summary<-summarise(sch_group,Drafted_Players=n(),Avg_Years_Played=mean(Years_Played,2),Pro_Bowl_App=sum(PB),Avg_PBs=mean(PB,3),QB = sum(Position.Standard=="QB"),RB = sum(Position.Standard=="RB"),WR = sum(Position.Standard=="WR"),TE = sum(Position.Standard=="TE"),T = sum(Position.Standard=="T"),DL = sum(Position.Standard=="DL"),DT = sum(Position.Standard=="WR"),LB = sum(Position.Standard=="LB"),DB = sum(Position.Standard=="DB"),ST = sum(Position.Standard=="K" | Position.Standard=="P" | Position.Standard=="LS"))

install.packages("ggthemes")
library(ggthemes)
install.packages("ggcolor")
Team_Years_Facet_Plot
Age_Years_Plot
Rnd_Years_Plot
latex
install.packages("latex")
install.packages("tables")
install.packages("pander")
latex(College_Outcome_Summary)
View(pandoc.table(College_Outcome_Summary))
View(print(xtable(College_Outcome_Summary),type="html"))
View(College_Outcome_Summary)
View(Rnd_Overall_Outcome_Summary)
trans