#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml

# Author: Yehao Zheng, Kaicheng Huang, Yujing Hua, Tingyi Li
# Data: 31 October 2020
# Contact: yehao.zheng@mail.utoronto.ca

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data_census <- read_dta("/Users/yehao/Desktop/STA304 PS3/inputs/usa_00005.dta")

# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_census <- 
  raw_data_census %>% 
  select(statefip,
         sex, 
         age,
         perwt,
         race, 
         citizen,
         hhincome)

#Change data types
reduced_data_census$age<-as.numeric(reduced_data_census$age)

#Assume all people who are older than 18 will vote
filter_census<-reduced_data_census %>%
  filter(age>=18& (citizen!="not a citizen" & citizen!="n/a"))

#Filter out NA
filter_census$hhincome<-ifelse(filter_census$hhincome==9999999,
                                      NaN,filter_census$hhincome)

filter_census<-na.omit(filter_census)

#Make both census and survey data have the same expressions for the different States of America
filter_census<-filter_census %>% 
  mutate(state = case_when(statefip=="alabama"~"AL",
                           statefip=="alaska"~"AK",
                           statefip=="arizona"~"AZ",
                           statefip=="arkansas"~"AR",
                           statefip=="california"~"CA",
                           statefip=="colorado"~"CO",
                           statefip=="connecticut"~"CT",
                           statefip=="delaware"~"DE",
                           statefip=="florida"~"FL",
                           statefip=="georgia"~"GA",
                           statefip=="hawaii"~"HI",
                           statefip=="idaho"~"ID",
                           statefip=="illinois"~"IL",
                           statefip=="indiana"~"IN",
                           statefip=="iowa"~"IA",
                           statefip=="kansas"~"KS",
                           statefip=="kentucky"~"KY",
                           statefip=="louisiana"~"LA",
                           statefip=="maine"~"ME",
                           statefip=="maryland"~"MD",
                           statefip=="massachusetts"~"MA",
                           statefip=="michigan"~"MI",
                           statefip=="minnesota"~"MN",
                           statefip=="mississippi"~"MS",
                           statefip=="missouri"~"MO",
                           statefip=="montana"~"MT",
                           statefip=="nebraska"~"NE",
                           statefip=="nevada"~"NV",
                           statefip=="new hampshire"~"NH",
                           statefip=="new jersey"~"NJ",
                           statefip=="new mexico"~"NM",
                           statefip=="new york"~"NY",
                           statefip=="north carolina"~"NC",
                           statefip=="north dakota"~"ND",
                           statefip=="ohio"~"OH",
                           statefip=="oklahoma"~"OK",
                           statefip=="oregon"~"OR",
                           statefip=="pennsylvania"~"PA",
                           statefip=="rhode island"~"RI",
                           statefip=="south carolina"~"SC",
                           statefip=="south dakota"~"SD",
                           statefip=="tennessee"~"TN",
                           statefip=="texas"~"TX",
                           statefip=="utah"~"UT",
                           statefip=="vermont"~"VT",
                           statefip=="virginia"~"VA",
                           statefip=="washington"~"WA",
                           statefip=="west virginia"~"WV",
                           statefip=="wisconsin"~"WI",
                           statefip=="wyoming"~"WY",
                           statefip=="district of columbia"~"DC")) 
filter_census$statefip<-NULL

unique(filter_census$state)
unique(filter_survey$state)

#Make age groups in both survey nad census data
filter_census<-filter_census %>% 
  mutate(agegroup = case_when(age <=40 ~ '40 or less',
                              age >40  & age <= 60 ~ '40 to 60',
                              age >60  & age <= 80 ~ '60 to 80',
                              age >80 ~ 'above 80'
  )) 

unique(filter_census$agegroup)

#Make both census and survey data have the same expressions for gender
unique(filter_census$sex)
filter_census$sex<-ifelse(filter_census$sex=="female","Female","Male")
unique(filter_census$sex)

#Make both census and survey data have the same expressions for the voter's income
unique(filter_survey$household_income)
min(filter_census$hhincome)
max(filter_census$hhincome)
filter_census<-filter_census %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 

filter_census$hhincome<-NULL

unique(filter_census$household_income)
unique(filter_survey$household_income)

#Make both census and survey data have the same expressions for the voter's race
unique(filter_census$race)
filter_census<-filter_census %>% 
  mutate(new_race = case_when(race=="white"~"White",
                           race=="black/african american/negro"~"Black, or African American",
                           race=="chinese"~"Chinese",
                           race=="japanese"~"Japanese",
                           race=="american indian or alaska native"~"American Indian or Alaska Native",
                           race=="two major races"~"Other race",
                           race=="other race, nec"~"Other race",
                           race=="three or more major races"~"Other race",
                           race=="other asian or pacific islander"~"Other Asians and Pacific Islander"
  )) 
unique(filter_census$new_race)

filter_census$race<-filter_census$new_race
filter_census$new_race<-NULL

unique(filter_census$race)
unique(filter_survey$race)


# Saving the census data as a csv file in my
# working directory
write_csv(filter_census, "outputs/census_data.csv")
