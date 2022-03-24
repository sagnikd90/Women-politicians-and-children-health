library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(lmtest)
library(reshape2)
library(stargazer)
library(hrbrthemes)
library(miceadds)
library(fixest)
library(clusterSEs)
library(rsq)
library(AER)
library(ivpack)

memory.limit(size = 40000)

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\R Results\\Regression results")

df_1<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_all_final_analysis.csv")

df_2<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_1_2_final_analysis.csv")

df_3<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_1_3_final_analysis.csv")

df_4<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Births_2_3_final_analysis.csv")

############### Releveling the number of years after election birth ####################

df_1$year_gap_fac[df_1$year_gap==1]<-1
df_1$year_gap_fac[df_1$year_gap==2]<-2
df_1$year_gap_fac[df_1$year_gap==3]<-3
df_1$year_gap_fac[df_1$year_gap>3]<-9

df_1$year_gap_fac<- relevel(as.factor(df_1$year_gap_fac),ref="9")

df_2$year_gap_fac[df_2$year_gap==1]<-1
df_2$year_gap_fac[df_2$year_gap==2]<-2
df_2$year_gap_fac[df_2$year_gap==3]<-3
df_2$year_gap_fac[df_2$year_gap>3]<-9

df_2$year_gap_fac<- relevel(as.factor(df_2$year_gap_fac),ref="9")

df_3$year_gap_fac[df_3$year_gap==1]<-1
df_3$year_gap_fac[df_3$year_gap==2]<-2
df_3$year_gap_fac[df_3$year_gap==3]<-3
df_3$year_gap_fac[df_3$year_gap>3]<-9

df_3$year_gap_fac<- relevel(as.factor(df_3$year_gap_fac),ref="9")

df_4$year_gap_fac[df_4$year_gap==1]<-1
df_4$year_gap_fac[df_4$year_gap==2]<-2
df_4$year_gap_fac[df_4$year_gap==3]<-3
df_4$year_gap_fac[df_4$year_gap>3]<-9

df_4$year_gap_fac<- relevel(as.factor(df_4$year_gap_fac),ref="9")

################################ For different year gaps ################################

df_1_1<- df_1%>%
  subset(year_gap_fac=="1")


df_1_1_1<-df_1_1%>%
  subset(!is.na(female_winner_3_5))

df_1_2<- df_1%>%
  subset(year_gap_fac=="2")


df_1_2_1<-df_1_2%>%
  subset(!is.na(female_winner_3_5))

df_1_3<- df_1%>%
  subset(year_gap_fac=="3")

df_1_3_1<-df_1_3%>%
  subset(!is.na(female_winner_3_5))

df_1_4<- df_1%>%
  subset(year_gap_fac=="9")

df_1_4_1<-df_1_4%>%
  subset(!is.na(female_winner_3_5))

df_2_1<- df_2%>%
  subset(year_gap_fac=="1")

df_2_1_1<-df_2_1%>%
  subset(!is.na(female_winner_3_5))

df_2_2<- df_2%>%
  subset(year_gap_fac=="2")

df_2_2_1<-df_2_2%>%
  subset(!is.na(female_winner_3_5))

df_2_3<- df_2%>%
  subset(year_gap_fac=="3")

df_2_3_1<-df_2_3%>%
  subset(!is.na(female_winner_3_5))

df_2_4<- df_2%>%
  subset(year_gap_fac=="9")

df_2_4_1<-df_2_4%>%
  subset(!is.na(female_winner_3_5))

df_3_1<- df_3%>%
  subset(year_gap_fac=="1")

df_3_1_1<-df_3_1%>%
  subset(!is.na(female_winner_3_5))

df_3_2<- df_3%>%
  subset(year_gap_fac=="2")

df_3_2_1<-df_3_2%>%
  subset(!is.na(female_winner_3_5))

df_3_3<- df_3%>%
  subset(year_gap_fac=="3")

df_3_3_1<-df_3_3%>%
  subset(!is.na(female_winner_3_5))

df_3_4<- df_3%>%
  subset(year_gap_fac=="9")

df_3_4_1<-df_3_4%>%
  subset(!is.na(female_winner_3_5))

df_4_1<- df_4%>%
  subset(year_gap_fac=="1")

df_4_1_1<-df_4_1%>%
  subset(!is.na(female_winner_3_5))

df_4_2<- df_4%>%
  subset(year_gap_fac=="2")

df_4_2_1<-df_4_2%>%
  subset(!is.na(female_winner_3_5))

df_4_3<- df_4%>%
  subset(year_gap_fac=="3")

df_4_3_1<-df_4_3%>%
  subset(!is.na(female_winner_3_5))

df_4_4<- df_4%>%
  subset(year_gap_fac=="9")

df_4_4_1<-df_4_4%>%
  subset(!is.na(female_winner_3_5))

######################## Differential impact by sex for different year gaps #########################

########################### Neonatal survival probability ################################

##################### Year gap = 1 #############################

ivreg_1<- ivreg(neonat~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_1)

seiv_1<- cluster.robust.se(ivreg_1,factor(df_1_1_1$district_name))

##################### Year gap = 2 #############################

ivreg_2<- ivreg(neonat~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_2)

seiv_2<- cluster.robust.se(ivreg_2,factor(df_1_2_1$district_name))

##################### Year gap = 3 #############################

ivreg_3<- ivreg(neonat~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_3)

seiv_3<- cluster.robust.se(ivreg_3,factor(df_1_3_1$district_name))

##################### Year gap > 3 #############################

ivreg_4<- ivreg(neonat~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_4)

seiv_4<- cluster.robust.se(ivreg_4,factor(df_1_4_1$district_name))

########################### Infantal survival probability ################################

##################### Year gap = 1 #############################

ivreg_5<- ivreg(infant~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_1)

seiv_5<- cluster.robust.se(ivreg_5,factor(df_1_1_1$district_name))

##################### Year gap = 2 #############################

ivreg_6<- ivreg(infant~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_2)

seiv_6<- cluster.robust.se(ivreg_6,factor(df_1_2_1$district_name))

##################### Year gap = 3 #############################

ivreg_7<- ivreg(infant~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_3)

seiv_7<- cluster.robust.se(ivreg_7,factor(df_1_3_1$district_name))

##################### Year gap > 3 #############################

ivreg_8<- ivreg(infant~female_winner*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*sex+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1_4)

seiv_8<- cluster.robust.se(ivreg_8,factor(df_1_4_1$district_name))

stargazer(seiv_1,seiv_2,seiv_3,seiv_4,seiv_5,seiv_6,seiv_7,seiv_8,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "margin",
                   "birth_order",
                   "mothers_age_at_birth",
                   "wait_time",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Share of female MLA's*Child is girl"),
          out = "Regression_results_year_gap_across_sex.tex")

stargazer(ivreg_1,ivreg_2,ivreg_3,ivreg_4,ivreg_5,ivreg_6,ivreg_7,ivreg_8,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "margin",
                   "birth_order",
                   "mothers_age_at_birth",
                   "wait_time",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Share of female MLA's*Child is girl"),
          out = "Rsq_results_year_gap_across_sex.tex")

######################## Differential impact by birth order for different year gaps #########################

########################### Neonatal survival probability ################################

##################### Year gap = 1 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_1<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_1)

seiv_1<- cluster.robust.se(ivreg_1,factor(df_2_1_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_2<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_1)

seiv_2<- cluster.robust.se(ivreg_2,factor(df_3_1_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_3<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_1)

seiv_3<- cluster.robust.se(ivreg_3,factor(df_4_1_1$district_name))

##################### Year gap = 2 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_4<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_2)

seiv_4<- cluster.robust.se(ivreg_4,factor(df_2_2_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_5<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_2)

seiv_5<- cluster.robust.se(ivreg_5,factor(df_3_2_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_6<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_2)

seiv_6<- cluster.robust.se(ivreg_6,factor(df_4_2_1$district_name))

##################### Year gap = 3 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_7<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_3)

seiv_7<- cluster.robust.se(ivreg_7,factor(df_2_3_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_8<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_3)

seiv_8<- cluster.robust.se(ivreg_8,factor(df_3_3_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_9<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_3)

seiv_9<- cluster.robust.se(ivreg_9,factor(df_4_3_1$district_name))

##################### Year gap > 3 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_10<- ivreg(neonat~female_winner*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time|
                   female_winner_3_5*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time,
                 data = df_2_4)

seiv_10<- cluster.robust.se(ivreg_10,factor(df_2_4_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_11<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_4)

seiv_11<- cluster.robust.se(ivreg_11,factor(df_3_4_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_12<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_4)

seiv_12<- cluster.robust.se(ivreg_12,factor(df_4_4_1$district_name))

stargazer(seiv_1,seiv_4,seiv_7,seiv_10,seiv_2,seiv_5,seiv_8,seiv_11,seiv_3,seiv_6,seiv_9,seiv_12,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "sex",
                   "margin",
                   "mothers_age_at_birth",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Share of female MLA's*(Birth order=2 vs 1)",
                               "Share of female MLA's*(Birth order=3 vs 1)"),
          out = "Regression_results_year_gap_neonat_across_birth_order.tex")

stargazer(ivreg_1,ivreg_4,ivreg_7,ivreg_10,ivreg_2,ivreg_5,ivreg_8,ivreg_11,ivreg_3,ivreg_6,ivreg_9,ivreg_12,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "sex",
                   "margin",
                   "mothers_age_at_birth",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Share of female MLA's*(Birth order=2 vs 1)",
                               "Share of female MLA's*(Birth order=3 vs 1)"),
          out = "Rsq_results_year_gap_neonat_across_birth_order.tex")

###################### Infant survival probability #################################

##################### Year gap = 1 #############################

ivreg_1<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_1)

seiv_1<- cluster.robust.se(ivreg_1,factor(df_2_1_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_2<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_1)

seiv_2<- cluster.robust.se(ivreg_2,factor(df_3_1_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_3<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_1)

seiv_3<- cluster.robust.se(ivreg_3,factor(df_4_1_1$district_name))

##################### Year gap = 2 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_4<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_2)

seiv_4<- cluster.robust.se(ivreg_4,factor(df_2_2_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_5<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_2)

seiv_5<- cluster.robust.se(ivreg_5,factor(df_3_2_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_6<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_2)

seiv_6<- cluster.robust.se(ivreg_6,factor(df_4_2_1$district_name))

##################### Year gap = 3 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_7<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_2_3)

seiv_7<- cluster.robust.se(ivreg_7,factor(df_2_3_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_8<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_3_3)

seiv_8<- cluster.robust.se(ivreg_8,factor(df_3_3_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_9<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_4_3)

seiv_9<- cluster.robust.se(ivreg_9,factor(df_4_3_1$district_name))

##################### Year gap > 3 #############################

############################# Birth order = 2 vs 1 ################################

ivreg_10<- ivreg(infant~female_winner*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time|
                   female_winner_3_5*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time,
                 data = df_2_4)

seiv_10<- cluster.robust.se(ivreg_10,factor(df_2_4_1$district_name))

############################# Birth order = 3 vs 1 ################################

ivreg_11<- ivreg(infant~female_winner*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time|
                   female_winner_3_5*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time,
                 data = df_3_4)

seiv_11<- cluster.robust.se(ivreg_11,factor(df_3_4_1$district_name))

############################# Birth order = 3 vs 2 ################################

ivreg_12<- ivreg(infant~female_winner*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time|
                   female_winner_3_5*factor(birth_order)+
                   (margin)^2+
                   factor(district_name)+
                   factor(cohort)+
                   sex+
                   EDYRTOTAL+
                   rural+
                   religion+
                   caste+
                   mothers_age_at_birth+
                   factor(econ)+
                   wait_time,
                 data = df_4_4)

seiv_12<- cluster.robust.se(ivreg_12,factor(df_4_4_1$district_name))

stargazer(seiv_1,seiv_4,seiv_7,seiv_10,seiv_2,seiv_5,seiv_8,seiv_11,seiv_3,seiv_6,seiv_9,seiv_12,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "sex",
                   "margin",
                   "mothers_age_at_birth",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Share of female MLA's*(Birth order=2 vs 1)",
                               "Share of female MLA's*(Birth order=3 vs 1)"),
          out = "Regression_results_year_gap_infant_across_birth_order.tex")

stargazer(ivreg_1,ivreg_4,ivreg_7,ivreg_10,ivreg_2,ivreg_5,ivreg_8,ivreg_11,ivreg_3,ivreg_6,ivreg_9,ivreg_12,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "sex",
                   "margin",
                   "mothers_age_at_birth",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Share of female MLA's*(Birth order=2 vs 1)",
                               "Share of female MLA's*(Birth order=3 vs 1)"),
          out = "Rsq_results_year_gap_infant_across_birth_order.tex")

