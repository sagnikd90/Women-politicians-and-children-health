library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(reshape2)
library(stargazer)
library(knitr)

memory.limit(size = 40000)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\R Results\\Regression results")

df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_all_final_analysis.csv")

sum_df<- df_1%>%
  mutate(win_30=ifelse(female_winner<=0.30,1,0))

############################### Neonatal ####################################

df_summary_1<- sum_df%>%
  group_by(sex,birth_order)%>%
  summarise(neonat_below30=round(mean(neonat[win_30==1],na.rm=TRUE),digits = 3),
            neonat_above30=round(mean(neonat[win_30==0],na.rm=TRUE),digits = 3))%>%
  mutate(diff=neonat_below30-neonat_above30)

df_neonat_1_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==2674)%>%
  select(sex,birth_order,p_val)

df_neonat_2_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==9684)%>%
  select(sex,birth_order,p_val)

df_neonat_3_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==2690)%>%
  select(sex,birth_order,p_val)


df_neonat_1_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==8307)%>%
  select(sex,birth_order,p_val)

df_neonat_2_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==2476)%>%
  select(sex,birth_order,p_val)

df_neonat_3_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(neonat~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==7708)%>%
  select(sex,birth_order,p_val)

df_neonat_pval<- rbind(df_neonat_1_pval_m,
                       df_neonat_2_pval_m,
                       df_neonat_3_pval_m,
                       df_neonat_1_pval_f,
                       df_neonat_2_pval_f,
                       df_neonat_3_pval_f)


df_neonat_summ<- cbind(df_summary_1,
                       df_neonat_pval)

df_neonat_summ<- df_neonat_summ%>%
  select(sex...1,
         birth_order...2,
         neonat_below30,
         neonat_above30,
         diff,
         p_val)

########################### Infant #####################################

df_summary_2<- sum_df%>%
  group_by(sex,birth_order)%>%
  summarise(infant_below30=round(mean(infant[win_30==1],na.rm=TRUE),digits = 3),
            infant_above30=round(mean(infant[win_30==0],na.rm=TRUE),digits = 3))%>%
  mutate(diff=infant_below30-infant_above30)

df_infant_1_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==1)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==4972)%>%
  select(sex,birth_order,p_val)

df_infant_2_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==2)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==5365)%>%
  select(sex,birth_order,p_val)

df_infant_3_pval_m<- sum_df%>%
  subset(sex==0 & birth_order==3)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==2690)%>%
  select(sex,birth_order,p_val)

df_infant_1_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==1)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==8307)%>%
  select(sex,birth_order,p_val)

df_infant_2_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==2)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==2476)%>%
  select(sex,birth_order,p_val)

df_infant_3_pval_f<- sum_df%>%
  subset(sex==1 & birth_order==3)%>%
  mutate(p_val=t.test(infant~win_30,var.equal=TRUE)$p.value)%>%
  subset(X==7708)%>%
  select(sex,birth_order,p_val)

df_infant_pval<- rbind(df_infant_1_pval_m,
                       df_infant_2_pval_m,
                       df_infant_3_pval_m,
                       df_infant_1_pval_f,
                       df_infant_2_pval_f,
                       df_infant_3_pval_f)

df_infant_summ<- cbind(df_summary_2,
                       df_infant_pval)

df_infant_summ<- df_infant_summ%>%
  select(sex...1,
         birth_order...2,
         infant_below30,
         infant_above30,
         diff,
         p_val)

kable(df_neonat_summ,"latex")

kable(df_infant_summ,"latex")


#################################### Mother level characteristics ################################

df_election_data<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Final_election_data_for_analysis.csv")

df_election_data<- df_election_data%>%
  select(id,
         State_Name,
         District_Name,
         Year)

df_nfhs_all_births<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS_all_birth_orders_summary_stats.csv")

df_all_births<- merge(df_nfhs_all_births,df_election_data[, c("id", setdiff(colnames(df_election_data),colnames(df_nfhs_all_births)))], by="id")

df_all_births<- df_all_births%>%
  mutate(year_gap=cohort-election_year)%>%
  subset(year_gap>0 & year_gap<=5)

df_all_births<- df_all_births%>%
  select(CASEID,
         neonat,
         infant,
         sex,
         mothers_age_at_birth,
         wait_time,
         birth_order,
         rural,
         religion,
         caste,
         econ,
         EDYRTOTAL,
         AGE,
         AGEFRSTMAR,
         AGEAT1STBIRTH)

df_all_births<- df_all_births[order(df_all_births$CASEID),]

df_all_births<- df_all_births%>%
  mutate(rural_hh=ifelse(rural=="rural",1,0),
         hindu=ifelse(religion=="hindu",1,0),
         muslim=ifelse(religion=="muslim",1,0),
         christian=ifelse(religion=="christian",1,0),
         obc=ifelse(caste=="OBC",1,0),
         SC=ifelse(caste=="SC",1,0),
         ST=ifelse(caste=="ST",1,0),
         poor=ifelse(econ=="poorer"|econ=="poorest",1,0),
         rich=ifelse(econ=="richer"|econ=="richest",1,0),
         middle=ifelse(econ=="middle",1,0))


df_nfhs_mean<- df_all_births%>%
  summarise(neonat_mean=round(mean(neonat,na.rm = TRUE),digits = 3),
            infant_mean=round(mean(infant,na.rm = TRUE),digits = 3),
            girls_mean=round(mean(sex,na.rm = TRUE),digits = 3),
            birth_order_mean=round(mean(birth_order,na.rm = TRUE),digits = 3),
            edyr_mean=round(mean(EDYRTOTAL,na.rm = TRUE),digits = 3),
            agemr_mean=round(mean(AGEFRSTMAR,na.rm = TRUE),digits = 3),
            agefb_mean=round(mean(AGEAT1STBIRTH,na.rm = TRUE),digits = 3),
            rural_mean=round(mean(rural_hh,na.rm = TRUE),digits = 3),
            hindu_mean=round(mean(hindu,na.rm = TRUE),digits = 3),
            muslim_mean=round(mean(muslim,na.rm = TRUE),digits = 3),
            christian_mean=round(mean(christian,na.rm = TRUE),digits = 3),
            obc_mean=round(mean(obc,na.rm = TRUE),digits = 3),
            sc_mean=round(mean(SC,na.rm = TRUE),digits = 3),
            st_mean=round(mean(ST,na.rm = TRUE),digits = 3),
            poor_mean=round(mean(poor,na.rm = TRUE),digits = 3),
            middle_mean=round(mean(middle,na.rm = TRUE),digits = 3),
            rich_mean=round(mean(rich,na.rm = TRUE),digits = 3))


df_nfhs_mean<- as.data.frame(t(df_nfhs_mean))

df_nfhs_mean<- tibble::rownames_to_column(df_nfhs_mean,"Variables1")

colnames(df_nfhs_mean)[2]<- "Mean"

df_nfhs_sd<- df_all_births%>%
  summarise(neonat_sd=round(sd(neonat,na.rm = TRUE),digits = 3),
            infant_sd=round(sd(infant,na.rm = TRUE),digits = 3),
            girls_sd=round(sd(sex,na.rm = TRUE),digits = 3),
            birth_order_sd=round(sd(birth_order,na.rm = TRUE),digits = 3),
            edyr_sd=round(sd(EDYRTOTAL,na.rm = TRUE),digits = 3),
            agemr_sd=round(sd(AGEFRSTMAR,na.rm = TRUE),digits = 3),
            agefb_sd=round(sd(AGEAT1STBIRTH,na.rm = TRUE),digits = 3),
            rural_sd=round(sd(rural_hh,na.rm = TRUE),digits = 3),
            hindu_sd=round(sd(hindu,na.rm = TRUE),digits = 3),
            muslim_sd=round(sd(muslim,na.rm = TRUE),digits = 3),
            christian_sd=round(sd(christian,na.rm = TRUE),digits = 3),
            obc_sd=round(sd(obc,na.rm = TRUE),digits = 3),
            sc_sd=round(sd(SC,na.rm = TRUE),digits = 3),
            st_sd=round(sd(ST,na.rm = TRUE),digits = 3),
            poor_sd=round(sd(poor,na.rm = TRUE),digits = 3),
            middle_sd=round(sd(middle,na.rm = TRUE),digits = 3),
            rich_sd=round(sd(rich,na.rm = TRUE),digits = 3))


df_nfhs_sd<- as.data.frame(t(df_nfhs_sd))

df_nfhs_sd<- tibble::rownames_to_column(df_nfhs_sd,"Variables2")

colnames(df_nfhs_sd)[2]<- "SD"

df_nfhs_summ<- cbind(df_nfhs_mean,df_nfhs_sd)

df_nfhs_summ<- df_nfhs_summ%>%
  select(-Variables2)

kable(df_nfhs_summ,"latex")

######################### Election data summary stats ###########################

df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Final_election_data_for_analysis.csv")

df_2<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\All_constituencies.csv")

df_3<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Constituency_female_male.csv")


########################## Constituency level summary stats #############################

#################### District-years share of women candidates ##########################

df_const<- df_2%>%
  subset(Sex=="F"|Sex=="M")%>%
  mutate(women_cand=ifelse(Sex=="F",1,0))

df_dist<- df_const%>%
  group_by(State_Name,District_Name,Constituency_Name,Year)%>%
  summarise(tot_wom=sum(Sex=="F"),
            tot_man=sum(Sex=="M"))%>%
  mutate(tot_cand=tot_wom+tot_man,
         wom_share=tot_wom/tot_cand)%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(wom_cand_share=round(mean(wom_share,na.rm = TRUE),digits = 3))

df_dist<- as.data.frame(df_dist)

df_dist_cand_mean<- df_dist%>%
  summarise(wom_cand_mean=round(mean(wom_cand_share),digits = 3))

df_dist_cand_sd<- df_dist%>%
  summarise(wom_cand_sd=round(sd(wom_cand_share),digits = 3))

####################### District year share of vote percentage received by women ##########################

df_wom_vote<- df_const%>%
  subset(Sex=="F")%>%
  group_by(State_Name,District_Name,Constituency_Name,Year)%>%
  summarise(vote_wom=mean(Vote_Share_Percentage/100,na.rm = TRUE))

df_wom_vote<- as.data.frame(df_wom_vote)

df_wom_vote<- df_wom_vote%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(vote_wom=mean(vote_wom))

df_wom_vote<- as.data.frame(df_wom_vote)

df_wom_vote_mean<- df_wom_vote%>%
  summarise(mean_vote_share=round(mean(vote_wom),digits = 3))

df_wom_vote_sd<- df_wom_vote%>%
  summarise(sd_vote_share=round(sd(vote_wom),digits = 3))

######################### Share of district-years with at least one woman candidate ########################

df_dist_wom_cand<- df_const%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(wom_cand=mean(women_cand,na.rm = TRUE))%>%
  mutate(wom_dist_cand=ifelse(wom_cand>0,1,0))%>%
  ungroup()%>%
  summarise(dist_wom=mean(wom_dist_cand))

df_dist_wom_cand<- as.data.frame(df_dist_wom_cand)

df_dist_wom_cand_mean<- df_dist_wom_cand%>%
  summarise(wom_dist_cand_mean=round(mean(wom_dist_cand),digits = 3))

df_dist_wom_cand_sd<- df_dist_wom_cand%>%
  summarise(wom_dist_cand_sd=round(sd(wom_dist_cand),digits = 3))

######################### Share of district-years with at least one woman candidate won ########################

df_first_wom<- df_const%>%
  subset(Position==1)%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(first_wom=sum(Sex=="F"))%>%
  mutate(one_wom_won=ifelse(first_wom>0,1,0))

df_first_wom<- as.data.frame(df_first_wom)

df_first_wom_mean<- df_first_wom%>%
  summarise(first_wom_mean=round(mean(one_wom_won),digits = 3))

df_first_wom_sd<- df_first_wom%>%
  summarise(first_wom_sd=round(sd(one_wom_won),digits = 3))

######################### Share of districts with at least one man woman election ##############################

df_man_wom_F<- df_const%>%
  subset(Position==1|Position==2)%>%
  subset(Sex=="F")%>%
  mutate(id=paste(State_Name,District_Name,Constituency_Name,Year,sep = "-"))%>%
  select(State_Name,
         District_Name,
         Constituency_Name,
         Year,
         month,
         Position,
         Party,
         Votes,
         Valid_Votes,
         Electors,
         N_Cand,
         Turnout_Percentage,
         Vote_Share_Percentage,
         Margin,
         Margin_Percentage,
         id)

colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Position")] <- "position_female"
colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Party")] <- "party_female"
colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Votes")] <- "votes_female"
colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Vote_Share_Percentage")] <- "vote_percent_female"
colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Margin")] <- "margin_female"
colnames(df_man_wom_F)[which(names(df_man_wom_F) == "Margin_Percentage")] <- "margin_percent_female"

df_man_wom_M<- df_const%>%
  subset(Position==1|Position==2)%>%
  subset(Sex=="M")%>%
  mutate(id=paste(State_Name,District_Name,Constituency_Name,Year,sep = "-"))%>%
  select(State_Name,
         District_Name,
         Constituency_Name,
         Year,
         month,
         Position,
         Party,
         Votes,
         Valid_Votes,
         Electors,
         N_Cand,
         Turnout_Percentage,
         Vote_Share_Percentage,
         Margin,
         Margin_Percentage,
         id)

colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Position")] <- "position_male"
colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Party")] <- "party_male"
colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Votes")] <- "votes_male"
colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Vote_Share_Percentage")] <- "vote_percent_male"
colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Margin")] <- "margin_male"
colnames(df_man_wom_M)[which(names(df_man_wom_M) == "Margin_Percentage")] <- "margin_percent_male"

df_man_wom<- merge(df_man_wom_M, 
                   df_man_wom_F[, 
                           c("id", 
                             setdiff(colnames(df_man_wom_F),
                                     colnames(df_man_wom_M)))], 
                   by="id",all = TRUE)

df_man_wom_dist<- df_man_wom%>%
  mutate(manwom=ifelse(!is.na(position_female),1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(manwom=mean(manwom,na.rm = TRUE))%>%
  mutate(manwom_elec=ifelse(manwom>0,1,0))%>%
  group_by(State_Name)%>%
  summarise(manwom_elec=mean(manwom_elec))%>%
  subset(!is.na(State_Name))

df_man_wom_dist<- as.data.frame(df_man_wom_dist)

df_man_wom_dist_mean<- df_man_wom_dist%>%
  summarise(manwom_elec_mean=round(mean(manwom_elec),digits = 3))

df_man_wom_dist_sd<- df_man_wom_dist%>%
  summarise(manwom_elec_sd=round(sd(manwom_elec),digits = 3))

############################ Share of district-years with at least one woman winner in man-woman elections ###########################

df_manwom_win<- df_3%>%
  mutate(win_marg=vote_percent_female-vote_percent_male)%>%
  mutate(win_female=ifelse(win_marg>0,1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(wom_win=mean(win_female))%>%
  mutate(wom_dist=ifelse(wom_win>0,1,0))

df_manwom_win<- as.data.frame(df_manwom_win)

df_manwom_win_mean<- df_manwom_win%>%
  summarise(women_win_mean=round(mean(wom_dist,na.rm = TRUE),digits = 3))

df_manwom_win_sd<- df_manwom_win%>%
  summarise(women_win_sd=round(sd(wom_dist,na.rm = TRUE),digits = 3))


########################### Share of district years with close man woman elections ###################################

df_manwom_close_win<- df_3%>%
  mutate(win_marg=vote_percent_female-vote_percent_male)%>%
  mutate(win_female_close=ifelse(win_marg>=-3.5 & win_marg<=3.5,1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(close_elec=mean(win_female_close))%>%
  mutate(close_dist=ifelse(close_elec>0,1,0))

df_manwom_close_win<- as.data.frame(df_manwom_close_win)

df_manwom_close_win_mean<- df_manwom_close_win%>%
  summarise(close_elec_mean=round(mean(close_dist,na.rm = TRUE),digits = 3))

df_manwom_close_win_sd<- df_manwom_close_win%>%
  summarise(close_elec_sd=round(sd(close_dist,na.rm = TRUE),digits = 3))


###################### Share of districts years with close man woman elections with at least one woman winner ######################

df_manwom_close_wom_win<- df_3%>%
  mutate(win_marg=vote_percent_female-vote_percent_male)%>%
  mutate(win_female_close=ifelse(win_marg>=-3.5 & win_marg<=3.5,1,0))%>%
  subset(win_female_close==1)%>%
  mutate(fwin=ifelse(win_marg>0,1,0))%>%
  group_by(State_Name,District_Name,Year)%>%
  summarise(fwin=mean(fwin))%>%
  mutate(fwin_dist=ifelse(fwin>0,1,0))

df_manwom_close_wom_win<- as.data.frame(df_manwom_close_wom_win)

df_manwom_close_wom_win_mean<- df_manwom_close_wom_win%>%
  summarise(fwin_mean=round(mean(fwin_dist,na.rm = TRUE),digits = 3))

df_manwom_close_wom_win_sd<- df_manwom_close_wom_win%>%
  summarise(fwin_sd=round(sd(fwin_dist,na.rm = TRUE),digits = 3))


################### District-year mean of female winners ##########################

df_fshare_mean<- df_1%>%
  summarise(fwin_mean=round(mean(female_winner,na.rm = TRUE),digits = 3))

df_fshare_sd<- df_1%>%
  summarise(fwin_sd=round(sd(female_winner,na.rm = TRUE),digits = 3))


df_mean<- cbind(df_dist_cand_mean,
                df_wom_vote_mean,
                df_dist_cand_mean,
                df_first_wom_mean,
                df_man_wom_dist_mean,
                df_manwom_win_mean,
                df_manwom_close_win_mean,
                df_manwom_close_wom_win_mean,
                df_fshare_mean)


df_sd<- cbind(df_dist_cand_sd,
              df_wom_vote_sd,
              df_dist_cand_sd,
              df_first_wom_sd,
              df_man_wom_dist_sd,
              df_manwom_win_sd,
              df_manwom_close_win_sd,
              df_manwom_close_wom_win_sd,
              df_fshare_sd)

df_mean<- as.data.frame(t(df_mean))

df_mean<- tibble::rownames_to_column(df_mean,"Variables1")

colnames(df_mean)[2]<- "Mean"

df_sd<- as.data.frame(t(df_sd))

df_sd<- tibble::rownames_to_column(df_sd,"Variables2")

colnames(df_sd)[2]<- "SD"

df_summ<- cbind(df_mean,df_sd)

df_summ<- df_summ%>%
  select(-Variables2)

kable(df_summ,"latex")