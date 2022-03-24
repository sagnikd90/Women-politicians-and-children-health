library(dplyr)
library(ggplot2)
library(Hmisc)
library(broom)
library(jtools)
library(tidyverse)
library(stargazer)
library(hrbrthemes)
library(rdrobust)
library(rddensity)
library(waffle)
library(sf)
library(viridis)

memory.limit(size = 40000)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\R Results\\R Graphs")

df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Final_election_data_for_analysis.csv")

df_2<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Constituency_female_male.csv")

df_3<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\First_second_constituency_elections.csv")

df_4<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_all_final_analysis.csv")

df_5<-read_sf("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Paper_with_Shilpi\\Political Competetion\\Datasets\\Shape File Constituency\\maps-master\\maps-master\\assembly-constituencies\\India_AC.shp")


################## Relation between women share of seats and child survival probability ##################

###################### Neonatal: Across birth order #########################

png("Correlation_neonatal_female_seat_share_birth_order.png")

df_4%>%group_by(birth_order,election_year)%>%
  mutate(neonat=mean(neonat,na.rm = TRUE),
         female_winner=mean(female_winner,na.rm = TRUE))%>%
  ggplot(aes(x=female_winner,y=neonat,color=factor(birth_order)))+
  geom_point(size=5)+
  geom_smooth(aes(linetype=factor(birth_order)),method = "lm",se=FALSE,show.legend = FALSE)+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Share of female winners")+
  ylab("Survival probability of neonatal")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))


dev.off()

######################## Infant:Across birth order #####################################

png("Correlation_infant_female_seat_share_birth_order.png")

df_4%>%group_by(birth_order,election_year)%>%
  mutate(infant=mean(infant,na.rm = TRUE),
         female_winner=mean(female_winner,na.rm = TRUE))%>%
  ggplot(aes(x=female_winner,y=infant,color=factor(birth_order)))+
  geom_point(size=5)+
  geom_smooth(aes(linetype=factor(birth_order)),method = "lm",se=FALSE,show.legend = FALSE)+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Share of female winners")+
  ylab("Survival probability of infants")+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))

dev.off()

###################### Neonatal: Across sex #########################

png("Correlation_neonatal_female_seat_share_sex.png")

df_4%>%group_by(sex,election_year)%>%
  mutate(neonat=mean(neonat,na.rm = TRUE),
         female_winner=mean(female_winner,na.rm = TRUE))%>%
  ggplot(aes(x=female_winner,y=neonat,color=factor(sex)))+
  geom_point(size=5)+
  geom_smooth(aes(linetype=factor(sex)),method = "lm",se=FALSE,show.legend = FALSE)+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Share of female winners")+
  ylab("Survival probability of neonatal")+
  scale_color_discrete(name="Sex",
                       labels=c("Male","Female"))


dev.off()

######################## Infant:Across sex #####################################

png("Correlation_infant_female_seat_share_sex.png")

df_4%>%group_by(sex,election_year)%>%
  mutate(infant=mean(infant,na.rm = TRUE),
         female_winner=mean(female_winner,na.rm = TRUE))%>%
  ggplot(aes(x=female_winner,y=infant,color=factor(sex)))+
  geom_point(size=5)+
  geom_smooth(aes(linetype=factor(sex)),method = "lm",se=FALSE,show.legend = FALSE)+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Share of female winners")+
  ylab("Survival probability of infants")+
  scale_color_discrete(name="Sex",
                       labels=c("Male","Female"))

dev.off()

######################################### Map ###################################################

df_5$ST_NAME<- tolower(df_5$ST_NAME)
df_5$DIST_NAME<- tolower(df_5$DIST_NAME)

df_5$area<- st_area(df_5)

df_map<- df_5

df_map$ST_NAME[df_map$ST_NAME=="orissa"]<- "odisha"
df_map$ST_NAME[df_map$ST_NAME=="uttarkhand"]<- "uttarakhand"

######################## District name matching #########################

df_map$DIST_NAME<- str_remove(df_map$DIST_NAME,"[*]")

df_map$DIST_NAME[df_map$DIST_NAME=="marigaon" & df_map$ST_NAME=="assam"]<- "morigaon"
df_map$DIST_NAME[df_map$DIST_NAME=="sibsagar" & df_map$ST_NAME=="assam"]<- "sivsagar"
df_map$DIST_NAME[df_map$DIST_NAME=="buxar*" & df_map$ST_NAME=="bihar"]<- "buxar"
df_map$DIST_NAME[df_map$DIST_NAME=="lakhisarai*" & df_map$ST_NAME=="bihar"]<- "lakhisarai"
df_map$DIST_NAME[df_map$DIST_NAME=="sheohar*" & df_map$ST_NAME=="bihar"]<- "sheohar"
df_map$DIST_NAME[df_map$DIST_NAME=="janjgir - champa" & df_map$ST_NAME=="chhattisgarh"]<- "janjgir-champa"
df_map$DIST_NAME[df_map$DIST_NAME=="supaul*" & df_map$ST_NAME=="bihar"]<- "supaul"
df_map$DIST_NAME[df_map$DIST_NAME=="anand*" & df_map$ST_NAME=="gujarat"]<- "anand"
df_map$DIST_NAME[df_map$DIST_NAME=="banas kantha" & df_map$ST_NAME=="gujarat"]<- "banaskantha"
df_map$DIST_NAME[df_map$DIST_NAME=="dohad*" & df_map$ST_NAME=="gujarat"]<- "dohad"
df_map$DIST_NAME[df_map$DIST_NAME=="navsari*" & df_map$ST_NAME=="gujarat"]<- "navsari"
df_map$DIST_NAME[df_map$DIST_NAME=="panch mahals" & df_map$ST_NAME=="gujarat"]<- "panchmahal"
df_map$DIST_NAME[df_map$DIST_NAME=="patan*" & df_map$ST_NAME=="gujarat"]<- "patan"
df_map$DIST_NAME[df_map$DIST_NAME=="porbandar*" & df_map$ST_NAME=="gujarat"]<- "porbandar"
df_map$DIST_NAME[df_map$DIST_NAME=="sabar kantha" & df_map$ST_NAME=="gujarat"]<- "sabarkantha"
df_map$DIST_NAME[df_map$DIST_NAME=="fatehabad*" & df_map$ST_NAME=="gujarat"]<- "fatehabad"
df_map$DIST_NAME[df_map$DIST_NAME=="jhajjar*" & df_map$ST_NAME=="gujarat"]<- "jhajjar"
df_map$DIST_NAME[df_map$DIST_NAME=="panchkula*" & df_map$ST_NAME=="gujarat"]<- "panchkula"
df_map$DIST_NAME[df_map$DIST_NAME=="hazaribag" & df_map$ST_NAME=="jharkhand"]<- "hazaribagh"
df_map$DIST_NAME[df_map$DIST_NAME=="bagalkot*" & df_map$ST_NAME=="karnataka"]<- "bagalkot"
df_map$DIST_NAME[df_map$DIST_NAME=="chamrajnagar" & df_map$ST_NAME=="karnataka"]<- "chamarajanagar"
df_map$DIST_NAME[df_map$DIST_NAME=="davangere" & df_map$ST_NAME=="karnataka"]<- "davanagere"
df_map$DIST_NAME[df_map$DIST_NAME=="kasaragod" & df_map$ST_NAME=="kerala"]<- "kasargod"
df_map$DIST_NAME[df_map$DIST_NAME=="barwani*" & df_map$ST_NAME=="madhya pradesh"]<- "barwani"
df_map$DIST_NAME[df_map$DIST_NAME=="dindori*" & df_map$ST_NAME=="madhya pradesh"]<- "dindori"
df_map$DIST_NAME[df_map$DIST_NAME=="katni*" & df_map$ST_NAME=="madhya pradesh"]<- "katni"
df_map$DIST_NAME[df_map$DIST_NAME=="umaria*" & df_map$ST_NAME=="madhya pradesh"]<- "umaria"
df_map$DIST_NAME[df_map$DIST_NAME=="gondiya*" & df_map$ST_NAME=="maharashtra"]<- "gondiya"
df_map$DIST_NAME[df_map$DIST_NAME=="mumbai (suburban)" & df_map$ST_NAME=="maharashtra"]<- "mumbai suburban"
df_map$DIST_NAME[df_map$DIST_NAME=="nandurbar*" & df_map$ST_NAME=="maharashtra"]<- "nandurbar"
df_map$DIST_NAME[df_map$DIST_NAME=="washim*" & df_map$ST_NAME=="maharashtra"]<- "washim"
df_map$DIST_NAME[df_map$DIST_NAME=="imphal east*" & df_map$ST_NAME=="manipur"]<- "imphal east"
df_map$DIST_NAME[df_map$DIST_NAME=="ri bhoi" & df_map$ST_NAME=="meghalaya"]<- "ribhoi"
df_map$DIST_NAME[df_map$DIST_NAME=="anugul*" & df_map$ST_NAME=="odisha"]<- "anugul"
df_map$DIST_NAME[df_map$DIST_NAME=="bargarh*" & df_map$ST_NAME=="odisha"]<- "baragarh"
df_map$DIST_NAME[df_map$DIST_NAME=="debagarh*" & df_map$ST_NAME=="odisha"]<- "debagarh"
df_map$DIST_NAME[df_map$DIST_NAME=="gajapati*" & df_map$ST_NAME=="odisha"]<- "gajapati"
df_map$DIST_NAME[df_map$DIST_NAME=="jagatsinghapur*" & df_map$ST_NAME=="odisha"]<- "jagatsinghapur"
df_map$DIST_NAME[df_map$DIST_NAME=="jajapur*" & df_map$ST_NAME=="odisha"]<- "jajapur"
df_map$DIST_NAME[df_map$DIST_NAME=="jharsuguda*" & df_map$ST_NAME=="odisha"]<- "jharsuguda"
df_map$DIST_NAME[df_map$DIST_NAME=="kendrapara*" & df_map$ST_NAME=="odisha"]<- "kendrapara"
df_map$DIST_NAME[df_map$DIST_NAME=="malkangiri*" & df_map$ST_NAME=="odisha"]<- "malkangiri"
df_map$DIST_NAME[df_map$DIST_NAME=="nayagarh*" & df_map$ST_NAME=="odisha"]<- "nayagarh"
df_map$DIST_NAME[df_map$DIST_NAME=="fatehgarh sahib*" & df_map$ST_NAME=="punjab"]<- "fatehgarh sahib"
df_map$DIST_NAME[df_map$DIST_NAME=="mansa*" & df_map$ST_NAME=="punjab"]<- "mansa"
df_map$DIST_NAME[df_map$DIST_NAME=="moga*" & df_map$ST_NAME=="punjab"]<- "moga"
df_map$DIST_NAME[df_map$DIST_NAME=="muktsar*" & df_map$ST_NAME=="punjab"]<- "muktsar"
df_map$DIST_NAME[df_map$DIST_NAME=="baran*" & df_map$ST_NAME=="rajasthan"]<- "baran"
df_map$DIST_NAME[df_map$DIST_NAME=="dausa*" & df_map$ST_NAME=="rajasthan"]<- "dausa"
df_map$DIST_NAME[df_map$DIST_NAME=="hanumangarh*" & df_map$ST_NAME=="rajasthan"]<- "hanumangarh"
df_map$DIST_NAME[df_map$DIST_NAME=="karauli*" & df_map$ST_NAME=="rajasthan"]<- "karauli"
df_map$DIST_NAME[df_map$DIST_NAME=="rajsamand*" & df_map$ST_NAME=="rajasthan"]<- "rajsamand"
df_map$DIST_NAME[df_map$DIST_NAME=="karur*" & df_map$ST_NAME=="tamil nadu"]<- "karur"
df_map$DIST_NAME[df_map$DIST_NAME=="nagapattinam*" & df_map$ST_NAME=="tamil nadu"]<- "nagapattinam"
df_map$DIST_NAME[df_map$DIST_NAME=="namakkal*" & df_map$ST_NAME=="tamil nadu"]<- "namakkal"
df_map$DIST_NAME[df_map$DIST_NAME=="dhalai*" & df_map$ST_NAME=="tripura"]<- "namakkal"
df_map$DIST_NAME[df_map$DIST_NAME=="ambedkar nagar*" & df_map$ST_NAME=="uttar pradesh"]<- "ambedkar nagar"
df_map$DIST_NAME[df_map$DIST_NAME=="barabanki" & df_map$ST_NAME=="uttar pradesh"]<- "bara banki"
df_map$DIST_NAME[df_map$DIST_NAME=="chandauli*" & df_map$ST_NAME=="uttar pradesh"]<- "chandauli"
df_map$DIST_NAME[df_map$DIST_NAME=="gautam buddha nagar*" & df_map$ST_NAME=="uttar pradesh"]<- "gautam buddha nagar"
df_map$DIST_NAME[df_map$DIST_NAME=="kushinagar *" & df_map$ST_NAME=="uttar pradesh"]<- "kushinagar"
df_map$DIST_NAME[df_map$DIST_NAME=="sant kabir nagar*" & df_map$ST_NAME=="uttar pradesh"]<- "sant kabir nagar"
df_map$DIST_NAME[df_map$DIST_NAME=="siddharthnagar" & df_map$ST_NAME=="uttar pradesh"]<- "siddharth nagar"
df_map$DIST_NAME[df_map$DIST_NAME=="north 24 parganas" & df_map$ST_NAME=="west bengal"]<- "north twenty four parganas"
df_map$DIST_NAME[df_map$DIST_NAME=="paschim medinapur" & df_map$ST_NAME=="west bengal"]<- "paschim mednipur"
df_map$DIST_NAME[df_map$DIST_NAME=="purba medinapur" & df_map$ST_NAME=="west bengal"]<- "purba mednipur"
df_map$DIST_NAME[df_map$DIST_NAME=="south 24 parganas" & df_map$ST_NAME=="west bengal"]<- "south twenty four parganas"


df_map<- df_map %>% 
  group_by(ST_NAME, DIST_NAME) %>% 
  summarise(area=sum(area))%>%
  mutate(id=paste(ST_NAME,DIST_NAME,sep = "-"))

##########################################################################################################

df_survival<- df_4%>%
  select(state_name,
         district_name,
         cohort,
         infant)%>%
  subset(cohort>=2011)%>%
  group_by(state_name,district_name)%>%
  summarise(survival=mean(infant,na.rm = TRUE))%>%
  mutate(id=paste(state_name,district_name,sep="-"))


df_elections<- df_3%>%
  subset(Position==1)%>%
  subset(Sex=="F"|Sex=="M" &
           Year>=2011)%>%
  mutate(female_winner=ifelse(Sex=="F",1,0))%>%
  group_by(State_Name,District_Name)%>%
  summarise(female_winner=mean(female_winner,na.rm = TRUE))%>%
  mutate(id=paste(State_Name,District_Name,sep="-"))

df_analysis<- merge(df_elections,df_survival[, c("id", 
                                                 setdiff(colnames(df_survival),
                                                         colnames(df_elections)))], by="id")

df_final_map<- merge(df_analysis,df_map[, c("id", 
                                            setdiff(colnames(df_map),
                                                    colnames(df_analysis)))], by="id",all = TRUE)

########################### Election map ################################

df_final_map<- st_as_sf(df_final_map)


png("Election_map.png")

df_final_map%>%
  ggplot()+
  geom_sf(aes(fill=female_winner))+
  theme_ipsum()+
  scale_fill_viridis("% Female\nwinners",
                     direction = -1,
                     alpha = 0.3)+
  theme_bw()+
  theme(text=element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

png("Infant_map.png")

df_final_map%>%
  ggplot()+
  geom_sf(aes(fill=survival))+
  theme_ipsum()+
  scale_fill_viridis("% Infant\nsurvivors",
                     direction = -1,
                     alpha = 0.3)+
  theme_bw()+
  theme(text=element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


############################ Waffle plot of seat share of women ##############################

df_waffle<- df_3%>%
  subset(Position==1 &
              Sex!="")

df_waffle$Sex<- as.factor(df_waffle$Sex)

df_waffle_1<- df_waffle%>%
  subset(Year<=1994)

df_waffle_2<- df_waffle%>%
  subset(Year>=2010)

df_waffle_graph_1<- df_waffle_1%>%
  group_by(Sex)%>%
  summarise(total=n())

df_waffle_graph_2<- df_waffle_2%>%
  group_by(Sex)%>%
  summarise(total=n())

######## Waffle graph 1 ###############

parts_1<- c("Male"=(4782/4995)*100,"Female"=(213/4995)*100)

p_1<- waffle(parts_1,
                 rows=6, 
                 size=4,
                 legend_pos = "", 
                 xlab = "1 square = 1 % of seats won (For period 1989-1994)",
                 colors = c("blue2", "red1"))

######## Waffle graph 2 ###############
  
parts_2<- c("Male"=(4548/5006)*100,"Female"=(458/5006)*100)
 
p_2<- waffle(parts_2,
             rows=6,
             size=4,
             legend_pos = "bottom", 
             xlab = "1 square = 1 % of seats won (For period 2010-2015)",
             colors = c("blue2", "red1"))

png("Waffle_plot_female_seat_share.png")

iron(p_1,p_2)

dev.off()

######################## Trends in neonatal and infant survival ###########################

df_survival_trends<- df_4%>%
  select(cohort,
         sex,
         birth_order,
         neonat,
         infant)

###################### Trend in neonatal survival probability across birth orders ######################

png("trend_neonatal_survival.png")

df_survival_trends%>%
  group_by(cohort,birth_order)%>%
  summarise(mean_neonat=mean(neonat,na.rm = TRUE))%>%
  ggplot(aes(x=cohort,y=mean_neonat,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_smooth(aes(linetype=factor(birth_order)),method = "lm",se=FALSE,show.legend = FALSE)+
  theme_bw()+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  xlab("Birth cohorts")+
  ylab("Neonatal survival probability")+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

###################### Trend in infant survival probability across birth orders ######################

png("trend_infant_survival.png")

df_survival_trends%>%
  group_by(cohort,birth_order)%>%
  summarise(mean_infant=mean(infant,na.rm = TRUE))%>%
  ggplot(aes(x=cohort,y=mean_infant,color=factor(birth_order)))+
  geom_point(size=5,alpha=0.5)+
  geom_smooth(aes(linetype=factor(birth_order)),method = "lm",se=FALSE,show.legend = FALSE)+
  theme_bw()+
  scale_color_discrete(name="Birth order",
                       labels=c("1","2",">=3"))+
  xlab("Birth cohorts")+
  ylab("Infant survival probability")+ 
  theme(text = element_text(size = 20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

########################## Trend in difference in neonatal survival probability between male and female across birth orders ############################

png("Boxplot_neonatal.png")

df_survival_trends%>%
  group_by(sex,birth_order,cohort)%>%
  summarise(mean_neonat=mean(neonat,na.rm = TRUE))%>%
  ggplot(aes(x=factor(birth_order),y=mean_neonat,fill=factor(sex)))+
  geom_boxplot(notch = TRUE,alpha=0.3)+
  theme_bw()+
  scale_fill_discrete(name="Sex",
                      labels=c("Male","Female"))+
  xlab("Birth orders")+
  ylab("Neonatal survival probability")+
  theme(text=element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

########################## Trend in difference in infant survival probability between male and female across birth orders ############################

png("Boxplot_infant.png")

df_survival_trends%>%
  group_by(sex,birth_order,cohort)%>%
  summarise(mean_infant=mean(infant,na.rm = TRUE))%>%
  ggplot(aes(x=factor(birth_order),y=mean_infant,fill=factor(sex)))+
  geom_boxplot(notch = TRUE,alpha=0.3)+
  theme_bw()+
  scale_fill_discrete(name="Sex",
                      labels=c("Male","Female"))+
  xlab("Birth orders")+
  ylab("Inafnt survival probability")+
  theme(text=element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


dev.off()

########################## Trend in difference in neonatal survival probability between male and female across years from election ############################

df_4$year_gap_fac[df_4$year_gap==1]<-"1"
df_4$year_gap_fac[df_4$year_gap==2]<-"2"
df_4$year_gap_fac[df_4$year_gap==3]<-"3"
df_4$year_gap_fac[df_4$year_gap>3]<-"Greater than 3"

png("Boxplot_neonatal_year_gap.png")

df_4%>%
  group_by(sex,year_gap_fac,cohort)%>%
  summarise(mean_neonat=mean(neonat,na.rm = TRUE))%>%
  ggplot(aes(x=year_gap_fac,y=mean_neonat,fill=factor(sex)))+
  geom_boxplot(notch = TRUE,alpha=0.3)+
  theme_bw()+
  scale_fill_discrete(name="Sex",
                      labels=c("Male","Female"))+
  xlab("Years after election")+
  ylab("Neonatal survival probability")+
  theme(text=element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

########################## Trend in difference in infant survival probability between male and female across birth orders ############################

png("Boxplot_infant_year_gap.png")

df_4%>%
  group_by(sex,year_gap_fac,cohort)%>%
  summarise(mean_infant=mean(infant,na.rm = TRUE))%>%
  ggplot(aes(x=year_gap_fac,y=mean_infant,fill=factor(sex)))+
  geom_boxplot(notch = TRUE,alpha=0.3)+
  theme_bw()+
  scale_fill_discrete(name="Sex",
                      labels=c("Male","Female"))+
  xlab("Years after election")+
  ylab("Infant survival probability")+
  theme(text=element_text(size=20),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

dev.off()


############### RDD plot showing share of female winners determined by vote share of females in a district #########################

df_rdd<- df_2%>%
  mutate(win_margin=vote_percent_female-vote_percent_male,
         female_winner=ifelse(position_female==1,1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  mutate(mean_female_winner=mean(female_winner))

png("rdd_plot_district_seat_share.png")

rdplot(df_rdd$mean_female_winner,df_rdd$win_margin,
       c=0,p=2,
       kernel = "triangular",ci=TRUE,
       x.label = "Margin of victory",
       y.label = "Average of district level share of female winners",
       title = "",
       col.dots = "red",
       col.lines = "blue")

dev.off()

####################### Manipulation test based on density discontinuity ##############################

rd_2<- rddensity(df_rdd$win_margin,
                 c=0,
                 p=3)

png("rdd_manipulation_test.png")

rdplotdensity(rd_2,
              df_rdd$win_margin,
              xlabel = "Victory margin",
              ylabel = "Density")

dev.off()



