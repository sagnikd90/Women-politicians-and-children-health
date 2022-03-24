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


########################## Ordering the variables #########################

df_1<- df_1[order(df_1$CASEID),]
df_2<- df_2[order(df_2$CASEID),]
df_3<- df_3[order(df_3$CASEID),]
df_4<- df_4[order(df_4$CASEID),]

########################### For all birth orders ###################################

################################## Neonatal survival probability #######################

########################### OLS regressions #######################

reg_1<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ))

se_1 <- coeftest(reg_1,
                 vcov = vcovHC(reg_1,
                               type = "HC0",
                               cluster = "district_name"))


reg_2<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ)+
             df_1$wait_time)

se_2 <- coeftest(reg_2,
                 vcov = vcovHC(reg_2,
                               type = "HC0",
                               cluster = "district_name"))


reg_3<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ)+
             df_1$wait_time+
             df_1$year_gap_fac)

se_3 <- coeftest(reg_3,
                 vcov = vcovHC(reg_3,
                               type = "HC0",
                               cluster = "district_name"))


################## IV regressions #######################

ivreg_1<- ivreg(neonat~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ),
                data = df_1)

seiv_1<- coeftest(ivreg_1,
                vcov = vcovHC(ivreg_1,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_2<- ivreg(neonat~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1)

seiv_2<- coeftest(ivreg_2,
                  vcov = vcovHC(ivreg_2,
                                type = "HC0",
                                cluster = "district_name"))


ivreg_3<- ivreg(neonat~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_1)

seiv_3<- coeftest(ivreg_3,
                  vcov = vcovHC(ivreg_3,
                                type = "HC0",
                                cluster = "district_name"))

stargazer(se_1,se_2,se_3,seiv_1,seiv_2,seiv_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "birth_order",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births"),
          out = "Regression_results_neonatal_all_birth_orders.tex")

stargazer(reg_1,reg_2,reg_3,ivreg_1,ivreg_2,ivreg_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "birth_order",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births"),
          out = "Rsq_results_neonatal_all_birth_orders.tex")


################################## Infant survival probability #######################

########################### OLS regressions #######################

reg_1<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ))

se_1 <- coeftest(reg_1,
                 vcov = vcovHC(reg_1,
                               type = "HC0",
                               cluster = "district_name"))


reg_2<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ)+
             df_1$wait_time)

se_2 <- coeftest(reg_2,
                 vcov = vcovHC(reg_2,
                               type = "HC0",
                               cluster = "district_name"))

reg_3<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             df_1$close_election+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             df_1$mothers_age_at_birth+
             factor(df_1$econ)+
             df_1$wait_time+
             df_1$year_gap_fac)

se_3 <- coeftest(reg_3,
                 vcov = vcovHC(reg_3,
                               type = "HC0",
                               cluster = "district_name"))


################## IV regressions #######################

ivreg_1<- ivreg(infant~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ),
                data = df_1)

seiv_1<- coeftest(ivreg_1,
                  vcov = vcovHC(ivreg_1,
                                type = "HC0",
                                cluster = "district_name"))

ivreg_2<- ivreg(infant~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time,
                data = df_1)

seiv_2<- coeftest(ivreg_2,
                  vcov = vcovHC(ivreg_2,
                                type = "HC0",
                                cluster = "district_name"))


ivreg_3<- ivreg(infant~female_winner+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_1)

seiv_3<- coeftest(ivreg_3,
                  vcov = vcovHC(ivreg_3,
                                type = "HC0",
                                cluster = "district_name"))

stargazer(se_1,se_2,se_3,seiv_1,seiv_2,seiv_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "birth_order",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births"),
          out = "Regression_results_infant_all_birth_orders.tex")

stargazer(reg_1,reg_2,reg_3,ivreg_1,ivreg_2,ivreg_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "birth_order"),
          covariate.labels = c("Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "Share of female MLA's",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births"),
          out = "Rsq_results_infant_all_birth_orders.tex")


################## Analysis to see if there are any birth order effects #############################

df_2_1<- df_2%>%
  subset(!is.na(female_winner_3_5))

df_3_1<- df_3%>%
  subset(!is.na(female_winner_3_5))

df_4_1<- df_4%>%
  subset(!is.na(female_winner_3_5))

########################## Neonatal survibal probability #########################

###################### Birth order=2 vs 1 ############################

reg_1<- lm(df_2$neonat~df_2$female_winner*factor(df_2$birth_order)+
             (df_2$margin)^2+
             df_2$close_election+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$sex+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             df_2$mothers_age_at_birth+
             factor(df_2$econ)+
             df_2$wait_time+
             df_2$year_gap_fac)

se_1 <- coeftest(reg_1,
                 vcov = vcovHC(reg_1,
                               type = "HC0",
                               cluster = "district_name"))

ivreg_1<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_2)


seiv_1<- cluster.robust.se(ivreg_1,factor(df_2_1$district_name))

###################### Birth order=3 vs 1 ############################

reg_2<- lm(df_3$neonat~df_3$female_winner*factor(df_3$birth_order)+
             (df_3$margin)^2+
             df_3$close_election+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$sex+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             df_3$mothers_age_at_birth+
             factor(df_3$econ)+
             df_3$wait_time+
             df_3$year_gap_fac)

se_2<- coeftest(reg_2,
                 vcov = vcovHC(reg_2,
                               type = "HC0",
                               cluster = "district_name"))

ivreg_2<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_3)

seiv_2<- cluster.robust.se(ivreg_2,factor(df_3_1$district_name))

###################### Birth order=3 vs 2 ############################

reg_3<- lm(df_4$neonat~df_4$female_winner*factor(df_4$birth_order)+
             (df_4$margin)^2+
             df_4$close_election+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$sex+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             df_4$mothers_age_at_birth+
             factor(df_4$econ)+
             df_4$wait_time+
             df_4$year_gap_fac)

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_3<- ivreg(neonat~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_4)

seiv_3<- cluster.robust.se(ivreg_3,factor(df_4_1$district_name))

stargazer(se_1,se_2,se_3,seiv_1,seiv_2,seiv_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 2)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 2)",
                               "Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "(Share of female MLA's)*(Birth order=3 vs 1)"),
          out = "Regression_results_neonat_birth_order_2vs1.tex")


stargazer(reg_1,reg_2,reg_3,ivreg_1,ivreg_2,ivreg_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 2)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 2)",
                               "Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "(Share of female MLA's)*(Birth order=3 vs 1)"),
          out = "Rsq_results_neonat_birth_orders_2vs1.tex")

########################## Infantal survibal probability #########################

###################### Birth order=2 vs 1 ############################

reg_1<- lm(df_2$infant~df_2$female_winner*factor(df_2$birth_order)+
             (df_2$margin)^2+
             df_2$close_election+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$sex+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             df_2$mothers_age_at_birth+
             factor(df_2$econ)+
             df_2$wait_time+
             df_2$year_gap_fac)

se_1 <- coeftest(reg_1,
                 vcov = vcovHC(reg_1,
                               type = "HC0",
                               cluster = "district_name"))

ivreg_1<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_2)

seiv_1<- cluster.robust.se(ivreg_1,factor(df_2_1$district_name))

###################### Birth order=3 vs 1 ############################

reg_2<- lm(df_3$infant~df_3$female_winner*factor(df_3$birth_order)+
             (df_3$margin)^2+
             df_3$close_election+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$sex+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             df_3$mothers_age_at_birth+
             factor(df_3$econ)+
             df_3$wait_time+
             df_3$year_gap_fac)

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_2<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_3)

seiv_2<- cluster.robust.se(ivreg_2,factor(df_3_1$district_name))

###################### Birth order=3 vs 2 ############################

reg_3<- lm(df_4$infant~df_4$female_winner*factor(df_4$birth_order)+
             (df_4$margin)^2+
             df_4$close_election+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$sex+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             df_4$mothers_age_at_birth+
             factor(df_4$econ)+
             df_4$wait_time+
             df_4$year_gap_fac)

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_3<- ivreg(infant~female_winner*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac|
                  female_winner_3_5*factor(birth_order)+
                  (margin)^2+
                  close_election+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  mothers_age_at_birth+
                  factor(econ)+
                  wait_time+
                  year_gap_fac,
                data = df_4)

seiv_3<- cluster.robust.se(ivreg_3,factor(df_4_1$district_name))

stargazer(se_1,se_2,se_3,seiv_1,seiv_2,seiv_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 2)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 2)",
                               "Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "(Share of female MLA's)*(Birth order=3 vs 1)"),
          out = "Regression_results_infant_birth_order_2vs1.tex")


stargazer(reg_1,reg_2,reg_3,ivreg_1,ivreg_2,ivreg_3,
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap_fac",
                   "margin",
                   "close_election"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 1)",
                               "Share of female MLA's",
                               "(Birth order=3 vs 2)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=3 vs 2)",
                               "Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "(Birth order=3 vs 1)",
                               "Child is girl",
                               "Mother's age at birth",
                               "Gap between births",
                               "(Share of female MLA's)*(Birth order=2 vs 1)",
                               "(Share of female MLA's)*(Birth order=3 vs 1)"),
          out = "Rsq_results_infant_birth_orders_2vs1.tex")
