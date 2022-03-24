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

###################################### For all birth orders ################################

#################################### OLS regressions ###################################

################################## Neonatal survival probability #######################

reg_1<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")


se_1 <- sqrt(diag(se_1))


reg_2<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")


se_2 <- sqrt(diag(se_2))


reg_3<- lm(df_1$neonat~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")


se_3 <- sqrt(diag(se_3))


################################## Infant survival probability #######################

reg_4<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")


se_4 <- sqrt(diag(se_4))


reg_5<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "district_name")


se_5 <- sqrt(diag(se_5))


reg_6<- lm(df_1$infant~df_1$female_winner+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "district_name")


se_6 <- sqrt(diag(se_6))

stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          order = c("^female_winner$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Share of female MLA's",
                               "Margin of victory",
                               "Child is female",
                               "Birth order=2",
                               "Birth order=3",
                               "Wait time"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################### OLS across birth orders ####################################

df_1_1<- df_1%>%
  subset(birth_order==1)

df_1_2<- df_1%>%
  subset(birth_order==2)

df_1_3<- df_1%>%
  subset(birth_order==3)

###################### Birth order=1######################

reg_1<- lm(df_1_1$neonat~df_1_1$female_winner+
       (df_1_1$margin)^2+
       factor(df_1_1$district_name)+
       factor(df_1_1$cohort)+
       df_1_1$sex+
       df_1_1$EDYRTOTAL+
       df_1_1$rural+
       df_1_1$religion+
       df_1_1$caste+
       factor(df_1_1$econ)+
       df_1_1$wait_time+
       factor(df_1_1$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")


se_1 <- sqrt(diag(se_1))


reg_2<- lm(df_1_1$infant~df_1_1$female_winner+
       (df_1_1$margin)^2+
       factor(df_1_1$district_name)+
       factor(df_1_1$cohort)+
         df_1_1$sex+
         df_1_1$EDYRTOTAL+
         df_1_1$rural+
         df_1_1$religion+
         df_1_1$caste+
       factor(df_1_1$econ)+
         df_1_1$wait_time+
         factor(df_1_1$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")


se_2 <- sqrt(diag(se_2))

###################### Birth order=2 #######################

reg_3<- lm(df_1_2$neonat~df_1_2$female_winner+
       (df_1_2$margin)^2+
       factor(df_1_2$district_name)+
       factor(df_1_2$cohort)+
         df_1_2$sex+
         df_1_2$EDYRTOTAL+
         df_1_2$rural+
         df_1_2$religion+
         df_1_2$caste+
       factor(df_1_2$econ)+
         df_1_2$wait_time+
         factor(df_1_2$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")


se_3 <- sqrt(diag(se_3))


reg_4<- lm(df_1_2$infant~df_1_2$female_winner+
       (df_1_2$margin)^2+
       factor(df_1_2$district_name)+
       factor(df_1_2$cohort)+
         df_1_2$sex+
         df_1_2$EDYRTOTAL+
         df_1_2$rural+
         df_1_2$religion+
       df_1_2$caste+
       factor(df_1_2$econ)+
         df_1_2$wait_time+
         factor(df_1_2$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")


se_4 <- sqrt(diag(se_4))

###################### Birth order=3 #######################

reg_5<- lm(df_1_3$neonat~df_1_3$female_winner+
       (df_1_3$margin)^2+
       factor(df_1_3$district_name)+
       factor(df_1_3$cohort)+
         df_1_3$sex+
         df_1_3$EDYRTOTAL+
         df_1_3$rural+
         df_1_3$religion+
         df_1_3$caste+
       factor(df_1_3$econ)+
         df_1_3$wait_time+
         factor(df_1_3$year_gap))

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "district_name")


se_5 <- sqrt(diag(se_5))


reg_6<- lm(df_1_3$infant~df_1_3$female_winner+
       (df_1_3$margin)^2+
       factor(df_1_3$district_name)+
       factor(df_1_3$cohort)+
         df_1_3$sex+
         df_1_3$EDYRTOTAL+
         df_1_3$rural+
         df_1_3$religion+
         df_1_3$caste+
       factor(df_1_3$econ)+
         df_1_3$wait_time+
         factor(df_1_3$year_gap))

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "district_name")


se_6 <- sqrt(diag(se_6))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Margin of victory",
                               "Child is female",
                               "Wait time",
                               "Share of female MLA's",
                               "Margin of victory",
                               "Child is female",
                               "Wait time",
                               "Share of female MLA's",
                               "Margin of victory",
                               "Child is female",
                               "Wait time"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))

#################### Differential effect across birth orders #######################

###################### Birth order 1 vs 2 #######################

reg_1<- lm(df_2$neonat~df_2$female_winner*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$sex+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")


se_1 <- sqrt(diag(se_1))


reg_2<- lm(df_2$infant~df_2$female_winner*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$sex+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")


se_2 <- sqrt(diag(se_2))

###################### Birth order 1 vs 3 #######################

reg_3<- lm(df_3$neonat~df_3$female_winner*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$sex+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")


se_3 <- sqrt(diag(se_3))


reg_4<- lm(df_3$infant~df_3$female_winner*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$sex+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")


se_4 <- sqrt(diag(se_4))

###################### Birth order 2 vs 3 #######################

reg_5<- lm(df_4$neonat~df_4$female_winner*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$sex+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "district_name")


se_5 <- sqrt(diag(se_5))


reg_6<- lm(df_4$infant~df_4$female_winner*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$sex+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "district_name")


se_6 <- sqrt(diag(se_6))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "(Birth order=2 vs 1)",
                               "Margin of victory",
                               "Child is female",
                               "Wait time",
                               "(Birth order=2 vs 1)xShare of female MLA's",
                               "Share of female MLA's",
                               "(Birth order=3 vs 1)",
                               "Margin of victory",
                               "Child is female",
                               "Wait time",
                               "(Birth order=3 vs 1)xShare of female MLA's",
                               "Share of female MLA's",
                               "(Birth order=3 vs 2)",
                               "Margin of victory",
                               "Child is female",
                               "Wait time",
                               "(Birth order=3 vs 2)xShare of female MLA's"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## Differential impact across sex ###########################

######################## For all birth orders ###########################

########################### For girls ##########################

df_1<- df_1%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_1$neonat~df_1$female_winner*df_1$sex+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_1$infant~df_1$female_winner*df_1$sex+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_1$neonat~df_1$female_winner*df_1$sex_1+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_1$infant~df_1$female_winner*df_1$sex_1+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time+
             factor(df_1$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap",
                   "birth_order"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## For birth order=1 ###########################

########################### For girls ##########################

df_1_1<- df_1_1%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_1_1$neonat~df_1_1$female_winner*df_1_1$sex+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time+
             factor(df_1_1$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_1_1$infant~df_1_1$female_winner*df_1_1$sex+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time+
             factor(df_1_1$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_1_1$neonat~df_1_1$female_winner*df_1_1$sex_1+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time+
             factor(df_1_1$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_1_1$infant~df_1_1$female_winner*df_1_1$sex_1+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time+
             factor(df_1_1$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## For birth order=2 ###########################

########################### For girls ##########################

df_1_2<- df_1_2%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_1_2$neonat~df_1_2$female_winner*df_1_2$sex+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time+
             factor(df_1_2$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_1_2$infant~df_1_2$female_winner*df_1_2$sex+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time+
             factor(df_1_2$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_1_2$neonat~df_1_2$female_winner*df_1_2$sex_1+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time+
             factor(df_1_2$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_1_2$infant~df_1_2$female_winner*df_1_2$sex_1+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time+
             factor(df_1_2$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## For birth order=3 ###########################

########################### For girls ##########################

df_1_3<- df_1_3%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_1_3$neonat~df_1_3$female_winner*df_1_3$sex+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time+
             factor(df_1_3$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_1_3$infant~df_1_3$female_winner*df_1_3$sex+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time+
             factor(df_1_3$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_1_3$neonat~df_1_3$female_winner*df_1_3$sex_1+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time+
             factor(df_1_3$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_1_3$infant~df_1_3$female_winner*df_1_3$sex_1+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time+
             factor(df_1_3$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## Differential impact across birth order and sex ###########################

######################## For birth order=2 vs 1 ###########################

########################### For girls ##########################

df_2<- df_2%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_2$neonat~df_2$female_winner*df_2$sex*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_2$infant~df_2$female_winner*df_2$sex*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_2$neonat~df_2$female_winner*df_2$sex_1*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_2$infant~df_2$female_winner*df_2$sex_1*factor(df_2$birth_order)+
             (df_2$margin)^2+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             factor(df_2$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "(Birth order=2 vs 1)",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy",
                               "Share of female MLA's * (Birth order=2 vs 1)",
                               "(Birth order=2 vs 1)*Child is a girl",
                               "Share of female MLA's*(Birth order=2 vs 1)*Child is a girl",
                               "(Birth order=2 vs 1)*Child is a boy",
                               "Share of female MLA's*(Birth order=2 vs 1)*Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))

######################## For birth order=3 vs 1 ###########################

########################### For girls ##########################

df_3<- df_3%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_3$neonat~df_3$female_winner*df_3$sex*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_3$infant~df_3$female_winner*df_3$sex*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_3$neonat~df_3$female_winner*df_3$sex_1*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_3$infant~df_3$female_winner*df_3$sex_1*factor(df_3$birth_order)+
             (df_3$margin)^2+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             factor(df_3$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "(Birth order=3 vs 1)",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy",
                               "Share of female MLA's * (Birth order=3 vs 1)",
                               "(Birth order=3 vs 1)*Child is a girl",
                               "Share of female MLA's*(Birth order=3 vs 1)*Child is a girl",
                               "(Birth order=3 vs 1)*Child is a boy",
                               "Share of female MLA's*(Birth order=3 vs 1)*Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


######################## For birth order=3 vs 2 ###########################

########################### For girls ##########################

df_4<- df_4%>%
  mutate(sex_1=ifelse(sex==0,1,0))

reg_1<- lm(df_4$neonat~df_4$female_winner*df_4$sex*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_4$infant~df_4$female_winner*df_4$sex*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

############################ For boys ##########################

reg_3<- lm(df_4$neonat~df_4$female_winner*df_4$sex_1*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_4$infant~df_4$female_winner*df_4$sex_1*factor(df_4$birth_order)+
             (df_4$margin)^2+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             factor(df_4$year_gap))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


stargazer(reg_1,reg_2,reg_3,reg_4,
          se=list(se_1,se_2,se_3,se_4),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "year_gap"),
          covariate.labels = c("Share of female MLA's",
                               "Child is a girl",
                               "Child is a boy",
                               "(Birth order=3 vs 2)",
                               "Margin of victory",
                               "Wait time",
                               "Share of female MLA's * Child is a girl",
                               "Share of female MLA's * Child is a boy",
                               "Share of female MLA's * (Birth order=3 vs 2)",
                               "(Birth order=3 vs 2)*Child is a girl",
                               "Share of female MLA's*(Birth order=3 vs 2)*Child is a girl",
                               "(Birth order=3 vs 2)*Child is a boy",
                               "Share of female MLA's*(Birth order=3 vs 2)*Child is a boy"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))


###################### Differential impact according to gap from year of election #########################

df_1$year_gap_fac[df_1$year_gap==1]<-1
df_1$year_gap_fac[df_1$year_gap==2]<-2
df_1$year_gap_fac[df_1$year_gap==3]<-3
df_1$year_gap_fac[df_1$year_gap>3]<-9

df_1$year_gap_fac<- as.factor(df_1$year_gap_fac)

df_1_1$year_gap_fac[df_1_1$year_gap==1]<-1
df_1_1$year_gap_fac[df_1_1$year_gap==2]<-2
df_1_1$year_gap_fac[df_1_1$year_gap==3]<-3
df_1_1$year_gap_fac[df_1_1$year_gap>3]<-9

df_1_1$year_gap_fac<- as.factor(df_1_1$year_gap_fac)

df_1_2$year_gap_fac[df_1_2$year_gap==1]<-1
df_1_2$year_gap_fac[df_1_2$year_gap==2]<-2
df_1_2$year_gap_fac[df_1_2$year_gap==3]<-3
df_1_2$year_gap_fac[df_1_2$year_gap>3]<-9

df_1_2$year_gap_fac<- as.factor(df_1_2$year_gap_fac)

df_1_3$year_gap_fac[df_1_3$year_gap==1]<-1
df_1_3$year_gap_fac[df_1_3$year_gap==2]<-2
df_1_3$year_gap_fac[df_1_3$year_gap==3]<-3
df_1_3$year_gap_fac[df_1_3$year_gap>3]<-9

df_1_3$year_gap_fac<- as.factor(df_1_3$year_gap_fac)

df_2$year_gap_fac[df_2$year_gap==1]<-1
df_2$year_gap_fac[df_2$year_gap==2]<-2
df_2$year_gap_fac[df_2$year_gap==3]<-3
df_2$year_gap_fac[df_2$year_gap>3]<-9

df_2$year_gap_fac<- as.factor(df_2$year_gap_fac)


df_3$year_gap_fac[df_3$year_gap==1]<-1
df_3$year_gap_fac[df_3$year_gap==2]<-2
df_3$year_gap_fac[df_3$year_gap==3]<-3
df_3$year_gap_fac[df_3$year_gap>3]<-9

df_3$year_gap_fac<- as.factor(df_3$year_gap_fac)


df_4$year_gap_fac[df_4$year_gap==1]<-1
df_4$year_gap_fac[df_4$year_gap==2]<-2
df_4$year_gap_fac[df_4$year_gap==3]<-3
df_4$year_gap_fac[df_4$year_gap>3]<-9

df_4$year_gap_fac<- as.factor(df_4$year_gap_fac)


#################### Differential effect by year after election ########################

###################### For all birth orders ##############################

reg_1<- lm(df_1$neonat~df_1$female_winner*relevel(df_1$year_gap_fac,ref="9")+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))

reg_2<- lm(df_1$infant~df_1$female_winner*relevel(df_1$year_gap_fac,ref="9")+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$sex+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))

###################### For birth order=1 ##############################

reg_3<- lm(df_1_1$neonat~df_1_1$female_winner*relevel(df_1_1$year_gap_fac,ref="9")+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$sex+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time)

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))

reg_4<- lm(df_1_1$infant~df_1_1$female_winner*relevel(df_1_1$year_gap_fac,ref="9")+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$sex+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time)

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


###################### For birth order=2 ##############################

reg_5<- lm(df_1_2$neonat~df_1_2$female_winner*relevel(df_1_2$year_gap_fac,ref="9")+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$sex+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time)

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "district_name")

se_5 <- sqrt(diag(se_5))

reg_6<- lm(df_1_2$infant~df_1_2$female_winner*relevel(df_1_2$year_gap_fac,ref="9")+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$sex+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time)

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "district_name")

se_6 <- sqrt(diag(se_6))


###################### For birth order=3 ##############################

reg_7<- lm(df_1_3$neonat~df_1_3$female_winner*relevel(df_1_3$year_gap_fac,ref="9")+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$sex+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time)

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "district_name")

se_7 <- sqrt(diag(se_7))

reg_8<- lm(df_1_3$infant~df_1_3$female_winner*relevel(df_1_3$year_gap_fac,ref="9")+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$sex+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time)

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "district_name")

se_8 <- sqrt(diag(se_8))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order",
                   "sex",
                   "margin",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3",
                               "Share of female MLA's",
                               "Year gap=1",
                               "Year gap=2",
                               "Year gap=3",
                               "Share of female MLA's * Born years after election=1",
                               "Share of female MLA's * Born years after election=2",
                               "Share of female MLA's * Born years after election=3"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))

###################### Differential effect by year after election and sex ####################################

######################## For all birth orders ############################

reg_1<- lm(df_1$neonat~df_1$female_winner*relevel(df_1$year_gap_fac,ref="9")*df_1$sex+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")

se_1 <- sqrt(diag(se_1))


reg_2<- lm(df_1$infant~df_1$female_winner*relevel(df_1$year_gap_fac,ref="9")*df_1$sex+
             (df_1$margin)^2+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             factor(df_1$birth_order)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             df_1$wait_time)

se_2 <- vcovHC(reg_2, 
               type = "HC0", 
               cluster = "district_name")

se_2 <- sqrt(diag(se_2))


######################## For birth order=1 ############################

reg_3<- lm(df_1_1$neonat~df_1_1$female_winner*relevel(df_1_1$year_gap_fac,ref="9")*df_1_1$sex+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time)

se_3 <- vcovHC(reg_3, 
               type = "HC0", 
               cluster = "district_name")

se_3 <- sqrt(diag(se_3))


reg_4<- lm(df_1_1$infant~df_1_1$female_winner*relevel(df_1_1$year_gap_fac,ref="9")*df_1_1$sex+
             (df_1_1$margin)^2+
             factor(df_1_1$district_name)+
             factor(df_1_1$cohort)+
             df_1_1$EDYRTOTAL+
             df_1_1$rural+
             df_1_1$religion+
             df_1_1$caste+
             factor(df_1_1$econ)+
             df_1_1$wait_time)

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")

se_4 <- sqrt(diag(se_4))


######################## For birth order=2 ############################

reg_5<- lm(df_1_2$neonat~df_1_2$female_winner*relevel(df_1_2$year_gap_fac,ref="9")*df_1_2$sex+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time)

se_5 <- vcovHC(reg_5, 
               type = "HC0", 
               cluster = "district_name")

se_5 <- sqrt(diag(se_5))


reg_6<- lm(df_1_2$infant~df_1_2$female_winner*relevel(df_1_2$year_gap_fac,ref="9")*df_1_2$sex+
             (df_1_2$margin)^2+
             factor(df_1_2$district_name)+
             factor(df_1_2$cohort)+
             df_1_2$EDYRTOTAL+
             df_1_2$rural+
             df_1_2$religion+
             df_1_2$caste+
             factor(df_1_2$econ)+
             df_1_2$wait_time)

se_6 <- vcovHC(reg_6, 
               type = "HC0", 
               cluster = "district_name")

se_6 <- sqrt(diag(se_6))


######################## For birth order=3 ############################

reg_7<- lm(df_1_3$neonat~df_1_3$female_winner*relevel(df_1_3$year_gap_fac,ref="9")*df_1_3$sex+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time)

se_7 <- vcovHC(reg_7, 
               type = "HC0", 
               cluster = "district_name")

se_7 <- sqrt(diag(se_7))


reg_8<- lm(df_1_3$infant~df_1_3$female_winner*relevel(df_1_3$year_gap_fac,ref="9")*df_1_3$sex+
             (df_1_3$margin)^2+
             factor(df_1_3$district_name)+
             factor(df_1_3$cohort)+
             df_1_3$EDYRTOTAL+
             df_1_3$rural+
             df_1_3$religion+
             df_1_3$caste+
             factor(df_1_3$econ)+
             df_1_3$wait_time)

se_8 <- vcovHC(reg_8, 
               type = "HC0", 
               cluster = "district_name")

se_8 <- sqrt(diag(se_8))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,reg_7,reg_8,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6,se_7,se_8),
          omit = c("district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order",
                   "margin",
                   "wait_time"),
          covariate.labels = c("Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl",
                               "Share of female MLA's",
                               "Years born after election=1",
                               "Years born after election=2",
                               "Years born after election=3",
                               "Child is a girl",
                               "Female winner year gap=1",
                               "Female winner year gap=2",
                               "Female winner year gap=3",
                               "Female winner sex",
                               "Year gap=1 * sex",
                               "Year gap=2 * sex",
                               "Year gap=3 * sex",
                               "Share of female MLA's * (Year-gap=1) * Child is a girl",
                               "Share of female MLA's * (Year-gap=2) * Child is a girl",
                               "Share of female MLA's * (Year-gap=3) * Child is a girl"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))

############################# IV regressions ###############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ),
                data=df_1)


se_1<- coeftest(ivreg_1,
                vcov = vcovHC(ivreg_1,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_2<- ivreg(neonat~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order),
                data=df_1)

se_2<- coeftest(ivreg_2,
                vcov = vcovHC(ivreg_2,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_3<- ivreg(neonat~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_1)


se_3<- coeftest(ivreg_3,
                vcov = vcovHC(ivreg_3,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_4<- ivreg(infant~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ),
                data=df_1)

se_4<- coeftest(ivreg_4,
                vcov = vcovHC(ivreg_4,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_5<- ivreg(infant~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order),
                data=df_1)


se_5<- coeftest(ivreg_5,
                vcov = vcovHC(ivreg_5,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_6<- ivreg(infant~female_winner+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_1)


se_6<- coeftest(ivreg_6,
                vcov = vcovHC(ivreg_6,
                              type = "HC0",
                              cluster = "district_name"))

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order"),
          order = c("^female_winner$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Share of female MLA's",
                               "Child is female",
                               "Mother's age at birth",
                               "Birth spacing"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"),
          out = "IV_overall.tex")

###################################### Across sex of the child for all birth orders ################################

#################################### OLS regressions ###################################

################################## Neonatal survival probability #######################

reg_1<- lm(df_1$neonat~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ))

se_1 <- vcovHC(reg_1, 
               type = "HC0", 
               cluster = "district_name")


se_1 <- sqrt(diag(se_1))


reg_2<- lm(df_1$neonat~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$mothers_age_at_birth+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             factor(df_1$birth_order))

se_2<- vcovHC(reg_2, 
              type = "HC0", 
              cluster = "district_name")


se_2<- sqrt(diag(se_2))


reg_3<- lm(df_1$neonat~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$mothers_age_at_birth+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             factor(df_1$birth_order)+
             df_1$wait_time+
             df_1$district_name:(seq_along(df_1$cohort))^2)

se_3<- vcovHC(reg_3, 
              type = "HC0", 
              cluster = "district_name")


se_3<- sqrt(diag(se_3))


################################## Infant survival probability #######################

reg_4<- lm(df_1$infant~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ))

se_4 <- vcovHC(reg_4, 
               type = "HC0", 
               cluster = "district_name")


se_4 <- sqrt(diag(se_4))


reg_5<- lm(df_1$infant~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$mothers_age_at_birth+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             factor(df_1$birth_order))

se_5<- vcovHC(reg_5, 
              type = "HC0", 
              cluster = "district_name")


se_5<- sqrt(diag(se_5))


reg_6<- lm(df_1$infant~df_1$female_winner*df_1$sex+
             factor(df_1$state_name)+
             factor(df_1$district_name)+
             factor(df_1$cohort)+
             df_1$mothers_age_at_birth+
             df_1$EDYRTOTAL+
             df_1$rural+
             df_1$religion+
             df_1$caste+
             factor(df_1$econ)+
             factor(df_1$birth_order)+
             df_1$wait_time+
             df_1$district_name:(seq_along(df_1$cohort))^2)

se_6<- vcovHC(reg_6, 
              type = "HC0", 
              cluster = "district_name")


se_6<- sqrt(diag(se_6))


stargazer(reg_1,reg_2,reg_3,reg_4,reg_5,reg_6,
          se=list(se_1,se_2,se_3,se_4,se_5,se_6),
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order"),
          order = c("^female_winner$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Share of female MLA's",
                               "Child is female",
                               "Mother's age at birth",
                               "Birth spacing",
                               "Share of female MLA's * girl child"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"),
          out = "Baseline_all_sex.tex")


############################# IV regressions ###############################

ivreg_1<- ivreg(neonat~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ),
                data=df_1)


se_1<- coeftest(ivreg_1,
                vcov = vcovHC(ivreg_1,
                              type = "HC0",
                              cluster = "district_name"))

ivreg_2<- ivreg(neonat~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order),
                data=df_1)


se_2<- coeftest(ivreg_2,
                vcov = vcovHC(ivreg_2,
                              type = "HC0",
                              cluster = "district_name"))


ivreg_3<- ivreg(neonat~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_1)


se_3<- coeftest(ivreg_3,
                vcov = vcovHC(ivreg_3,
                              type = "HC0",
                              cluster = "district_name"))


ivreg_4<- ivreg(infant~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ),
                data=df_1)


se_4<- coeftest(ivreg_4,
                vcov = vcovHC(ivreg_4,
                              type = "HC0",
                              cluster = "district_name"))


ivreg_5<- ivreg(infant~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order),
                data=df_1)


se_5<- coeftest(ivreg_5,
                vcov = vcovHC(ivreg_5,
                              type = "HC0",
                              cluster = "district_name"))


ivreg_6<- ivreg(infant~female_winner*sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*sex+
                  female_winner_3_5+
                  sex+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  factor(birth_order)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_1)


se_6<- coeftest(ivreg_6,
                vcov = vcovHC(ivreg_6,
                              type = "HC0",
                              cluster = "district_name"))

stargazer(se_1,se_2,se_3,se_4,se_5,se_6,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order"),
          order = c("^female_winner$",
                    "^sex$",
                    "^mothers_age_at_birth$",
                    "^wait_time$"),
          covariate.labels = c("Share of female MLA's",
                               "Child is female",
                               "Mother's age at birth",
                               "Birth spacing",
                               "Share of female MLA's * girl child"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"),
          out = "IV_all_sex.tex")



############################# Neonatal ################################

####################### For birth order 1 vs 2 ###########################

########################### OLS ###############################

reg_1<- lm(df_2$neonat~df_2$female_winner*factor(df_2$birth_order)+
             factor(df_2$state_name)+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$mothers_age_at_birth+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             df_2$district_name:(seq_along(df_2$cohort))^2)

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsq_1<- round(rsq(reg_1),digits = 3)

########################## IV ####################################


ivreg_1<- ivreg(neonat~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_2)

seiv_1<- coeftest(ivreg_1,
                vcov = vcovHC(ivreg_1,
                              type = "HC0",
                              cluster = "district_name"))

rsqiv_1<- round(rsq(ivreg_1),digits = 3)

####################### For birth order 1 vs 3 ###########################

########################### OLS ###############################

reg_2<- lm(df_3$neonat~df_3$female_winner*factor(df_3$birth_order)+
             factor(df_3$state_name)+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$mothers_age_at_birth+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             df_3$district_name:(seq_along(df_3$cohort))^2)

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)


########################## IV ####################################


ivreg_2<- ivreg(neonat~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_3)

seiv_2<- coeftest(ivreg_2,
                  vcov = vcovHC(ivreg_2,
                                type = "HC0",
                                cluster = "district_name"))

rsqiv_2<- round(rsq(ivreg_2),digits = 3)

####################### For birth order 2 vs 3 ###########################

########################### OLS ###############################

reg_3<- lm(df_4$neonat~df_4$female_winner*factor(df_4$birth_order)+
             factor(df_4$state_name)+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$mothers_age_at_birth+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             df_4$district_name:(seq_along(df_4$cohort))^2)

se_3<- coeftest(reg_3,
                  vcov = vcovHC(reg_3,
                                type = "HC0",
                                cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

########################## IV ####################################


ivreg_3<- ivreg(neonat~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_4)

seiv_3<- coeftest(ivreg_3,
                  vcov = vcovHC(ivreg_3,
                                type = "HC0",
                                cluster = "district_name"))

rsqiv_3<- round(rsq(ivreg_3),digits = 3)


stargazer(se_1,seiv_1,se_2,seiv_2,se_3,seiv_3,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))

############################# Infant ################################

####################### For birth order 1 vs 2 ###########################

########################### OLS ###############################

reg_1<- lm(df_2$infant~df_2$female_winner*factor(df_2$birth_order)+
             factor(df_2$state_name)+
             factor(df_2$district_name)+
             factor(df_2$cohort)+
             df_2$mothers_age_at_birth+
             df_2$EDYRTOTAL+
             df_2$rural+
             df_2$religion+
             df_2$caste+
             factor(df_2$econ)+
             df_2$wait_time+
             df_2$district_name:(seq_along(df_2$cohort))^2)

se_1<- coeftest(reg_1,
                vcov = vcovHC(reg_1,
                              type = "HC0",
                              cluster = "district_name"))


rsq_1<- round(rsq(reg_1),digits = 3)

########################## IV ####################################


ivreg_1<- ivreg(infant~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_2)

seiv_1<- coeftest(ivreg_1,
                  vcov = vcovHC(ivreg_1,
                                type = "HC0",
                                cluster = "district_name"))

rsqiv_1<- round(rsq(ivreg_1),digits = 3)


####################### For birth order 1 vs 3 ###########################

########################### OLS ###############################

reg_2<- lm(df_3$infant~df_3$female_winner*factor(df_3$birth_order)+
             factor(df_3$state_name)+
             factor(df_3$district_name)+
             factor(df_3$cohort)+
             df_3$mothers_age_at_birth+
             df_3$EDYRTOTAL+
             df_3$rural+
             df_3$religion+
             df_3$caste+
             factor(df_3$econ)+
             df_3$wait_time+
             df_3$district_name:(seq_along(df_3$cohort))^2)

se_2<- coeftest(reg_2,
                vcov = vcovHC(reg_2,
                              type = "HC0",
                              cluster = "district_name"))

rsq_2<- round(rsq(reg_2),digits = 3)

########################## IV ####################################

ivreg_2<- ivreg(infant~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_3)

seiv_2<- coeftest(ivreg_2,
                  vcov = vcovHC(ivreg_2,
                                type = "HC0",
                                cluster = "district_name"))

rsqiv_2<- round(rsq(ivreg_2),digits = 3)

####################### For birth order 2 vs 3 ###########################

########################### OLS ###############################

reg_3<- lm(df_4$infant~df_4$female_winner*factor(df_4$birth_order)+
             factor(df_4$state_name)+
             factor(df_4$district_name)+
             factor(df_4$cohort)+
             df_4$mothers_age_at_birth+
             df_4$EDYRTOTAL+
             df_4$rural+
             df_4$religion+
             df_4$caste+
             factor(df_4$econ)+
             df_4$wait_time+
             df_4$district_name:(seq_along(df_4$cohort))^2)

se_3<- coeftest(reg_3,
                vcov = vcovHC(reg_3,
                              type = "HC0",
                              cluster = "district_name"))

rsq_3<- round(rsq(reg_3),digits = 3)

########################## IV ####################################

ivreg_3<- ivreg(infant~female_winner*factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2|
                  female_winner_3_5*factor(birth_order)+
                  female_winner_3_5+
                  factor(birth_order)+
                  factor(state_name)+
                  factor(district_name)+
                  factor(cohort)+
                  mothers_age_at_birth+
                  EDYRTOTAL+
                  rural+
                  religion+
                  caste+
                  factor(econ)+
                  wait_time+
                  district_name:(seq_along(cohort))^2,
                data=df_4)

seiv_3<- coeftest(ivreg_3,
                  vcov = vcovHC(ivreg_3,
                                type = "HC0",
                                cluster = "district_name"))

rsqiv_3<- round(rsq(ivreg_3),digits = 3)

stargazer(se_1,seiv_1,se_2,seiv_2,se_3,seiv_3,
          omit = c("state_name",
                   "district_name",
                   "cohort",
                   "EDYRTOTAL",
                   "econ",
                   "Constant",
                   "caste",
                   "religion",
                   "rural",
                   "birth_order"),
          no.space = TRUE,
          omit.stat = c("ser","f","adj.rsq"))
