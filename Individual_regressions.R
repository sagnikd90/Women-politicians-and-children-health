library(dplyr)
library(foreign)
library(ggplot2)
library(Hmisc)
library(plm)
library(broom)
library(jtools)
library(tidyverse)
library(lmtest)
library(AER)

memory.limit(size = 40000)

df<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Final_data_for_analysis.csv")

############################################## Regressions #######################################

reg_1<- lm(data = df,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df)


################################# Separately for male and female #################################

df_female<- df%>%
  subset(sex==1)

df_male<- df%>%
  subset(sex==0)

######################################## For males ####################################

reg_1<- lm(data = df_male,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_male,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_male)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_male)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  sex+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_male)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_male)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_male)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_male)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_male)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_male)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_male)


######################################## For females ####################################


reg_1<- lm(data = df_female,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_female,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             factor(birth_order)+
             mothers_age_at_birth+
             total_child+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_female)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_female)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_female)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_female)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_female)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_female)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_female)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_female)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  factor(birth_order)+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_female)

################################################## For different birth orders #######################################

############################# Birth order=1 #######################

df_1<- df%>%
  subset(birth_order==1)

reg_1<- lm(data = df_1,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_1,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_1)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_1)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_1)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_1)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_1)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_1)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_1)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_1)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_1)

################################ First order born males ##########################

df_1_males<- df_1%>%
  subset(sex==0)

reg_1<- lm(data = df_1_males,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_1_males,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ First order born females ##########################

df_1_females<- df_1%>%
  subset(sex==1)

reg_1<- lm(data = df_1_females,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_1_females,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 


############################# Birth order=2 #######################

df_2<- df%>%
  subset(birth_order==2)

reg_1<- lm(data = df_2,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_2,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_2)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_2)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_2)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_2)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_2)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_2)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_2)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_2)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_2)


################################ Second order born males ##########################

df_2_males<- df_2%>%
  subset(sex==0)

reg_1<- lm(data = df_2_males,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_2_males,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ Second order born females ##########################

df_2_females<- df_2%>%
  subset(sex==1)

reg_1<- lm(data = df_2_females,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_2_females,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

############################# Birth order=3 #######################

df_3<- df%>%
  subset(birth_order==3)

reg_1<- lm(data = df_3,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_3,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ Third order born males ##########################

df_3_males<- df_3%>%
  subset(sex==0)

reg_1<- lm(data = df_3_males,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_3_males,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ Third order born females ##########################

df_3_females<- df_3%>%
  subset(sex==1)

reg_1<- lm(data = df_3_females,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_3_females,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_3)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_3)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_3)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_3)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_3)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_3)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_3)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_3)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_3)

############################# Birth order=4 #######################

df_4<- df%>%
  subset(birth_order==4)

reg_1<- lm(data = df_4,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_4,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ Fourth order born males ##########################

df_4_males<- df_4%>%
  subset(sex==0)

reg_1<- lm(data = df_4_males,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_4_males,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

################################ Fourth order born females ##########################

df_4_females<- df_4%>%
  subset(sex==1)

reg_1<- lm(data = df_4_females,
           formula = neonat~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 

reg_2<- lm(data = df_4_females,
           formula = infant~female_winner+
             factor(district_name)+
             factor(cohort)+
             mothers_age_at_birth+
             total_child+
             sex+
             read_write_woman+
             religion+
             caste+
             type_of_house) 


######################################## IV regressions ######################################################

######################################## Constituencies with margin 1.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_1_5,
                data = df_4)

######################################## Constituencies with margin 2% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2,
                data = df_4)

######################################## Constituencies with margin 2.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_2_5,
                data = df_4)

######################################## Constituencies with margin 3% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3,
                data = df_4)

######################################## Constituencies with margin 3.5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_3_5,
                data = df_4)

######################################## Constituencies with margin 4% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4,
                data = df_4)

######################################## Constituencies with margin 4_5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_4_5,
                data = df_4)

######################################## Constituencies with margin 5% #############################

ivreg_1<- ivreg(neonat~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_4)

ivreg_2<- ivreg(infant~female_winner+
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house|
                  factor(district_name)+
                  factor(cohort)+
                  sex+
                  mothers_age_at_birth+
                  total_child+
                  read_write_woman+
                  religion+
                  caste+
                  type_of_house+
                  female_winner_5,
                data = df_4)
