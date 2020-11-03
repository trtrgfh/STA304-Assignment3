#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml

# Author: Yehao Zheng, Kaicheng Huang, Yujing Hua, Tingyi Li
# Data: 31 October 2020
# Contact: yehao.zheng@mail.utoronto.ca


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/yehao/Desktop/STA304 PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables
reduced_data_survey <- 
  raw_data_survey %>% 
  select(registration,
         vote_intention,
         vote_2020,
         employment,
         foreign_born,
         gender,
         race_ethnicity,
         household_income,
         education,
         state,
         age)


# Use filter to find the data only for people who will vote
filter_survey<-reduced_data_survey %>% 
  filter(vote_intention=="Yes, I will vote"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))

#take out all the rows containing N/A
filter_survey<-na.omit(filter_survey)

#Make age groups in both survey nad census data
filter_survey<-filter_survey %>% 
  mutate(agegroup = case_when(age <=40 ~ '40 or less',
                              age >40  & age <= 60 ~ '40 to 60',
                              age >60  & age <= 80 ~ '60 to 80',
                              age >80 ~ 'above 80'
  )) 
unique(filter_survey$agegroup)

#Make both census and survey data have the same expressions for gender
unique(filter_survey$gender)
filter_survey<-rename(filter_survey,sex=gender)
unique(filter_survey$sex)

#Make both census and survey data have the same expressions for the voter's income
#Make both census and survey data have the same expressions for the voter's race
unique(filter_survey$race_ethnicity)
others<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)","Asian (Filipino)",
          "Pacific Islander (Samoan)","Pacific Islander (Guamanian)",
          "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)")
filter_survey<-filter_survey %>% 
  mutate(race = case_when(race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% others ~"Other Asians and Pacific Islander",
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity=="Other race "~"Other race"
  )) 
filter_survey$race_ethnicity<-NULL
unique(filter_survey$race)


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(filter_survey, "outputs/survey_data.csv")

