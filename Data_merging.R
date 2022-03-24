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

setwd("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets")

memory.limit(size = 40000)

######################################### Merging the NFHS datasets with the election data ###########################################

df_election_data<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\Final_election_data_for_analysis.csv")

df_nfhs_all_births<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS_all_birth_orders.csv")

df_nfhs_1_2_births<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS_1_2_birth_orders.csv")

df_nfhs_1_3_births<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS_1_3_birth_orders.csv")

df_nfhs_2_3_births<- read.csv("C:\\Users\\das90\\OneDrive - Cuny GradCenter\\Research\\Women_politicians_and_child_survivability\\Datasets\\NFHS_2_3_birth_orders.csv")

################################### All birth order merging #################################

df_all_births<- merge(df_nfhs_all_births,df_election_data[, c("id", setdiff(colnames(df_election_data),colnames(df_nfhs_all_births)))], by="id")

df_all_births<- df_all_births%>%
  mutate(year_gap=cohort-election_year)%>%
  subset(year_gap>0 & year_gap<=5)

df_1_2_births<- merge(df_nfhs_1_2_births,df_election_data[, c("id", setdiff(colnames(df_election_data),colnames(df_nfhs_1_2_births)))], by="id")

df_1_3_births<- merge(df_nfhs_1_3_births,df_election_data[, c("id", setdiff(colnames(df_election_data),colnames(df_nfhs_1_3_births)))], by="id")

df_2_3_births<- merge(df_nfhs_2_3_births,df_election_data[, c("id", setdiff(colnames(df_election_data),colnames(df_nfhs_2_3_births)))], by="id")

write.csv(df_all_births,"Births_all_final_analysis.csv")

write.csv(df_1_2_births,"Births_1_2_final_analysis.csv")

write.csv(df_1_3_births,"Births_1_3_final_analysis.csv")

write.csv(df_2_3_births,"Births_2_3_final_analysis.csv")