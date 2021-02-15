library("RODBC")
library(dplyr)
library(stringr)
library(lubridate)
library(anytime)
library(tidyverse)
library(data.table)

#Connecting to SQL Server and Loading Dataset

myconn<-odbcConnect("dartmouthqbs181","username","password")

#~~~~Question 1~~~~~~~

#Data tables marked Q1_X (with X being the question section) represent my final answer for that part.

#Parts A and B

#Load in IC_BP_v2 Dataset, fix typo, create BPStatus and set discrete values

Q1_AB <- read.csv("//filepath/IC_BP_v2.csv") %>% 
  rename(DiastolicValue = Diastolicvalue) %>%
  mutate(BPStatus = BPAlerts) %>%
  mutate(BPStatus = str_replace_all(
    BPStatus, c("Hypo1" = "1", "Normal" = "1",
                "Hypo2" = "0", "HTN1" = "0",
                "HTN2" = "0", "HTN3" = "0"))) %>%
  mutate(BPStatus = as.factor(BPStatus))

#Part C

#Load Demographics Table

Q1_rawdemographics <- sqlQuery(myconn,"select * from Demographics")

#Merge Demographics and IC_BP_v2 using ID = contactid, convert Enrollment Date to proper format
#Then calculated the WeekNumber of observations using difftime() and created index using floor function

Q1_C <- Q1_AB %>%
  inner_join(Q1_rawdemographics, by = c("ID" = "contactid")) %>%
  group_by(ID) %>%
  mutate(ObservedTime = as.Date(ObservedTime, origin = "1899-12-30")) %>%
  mutate(tri_enrollmentcompletedate = as.Date(tri_enrollmentcompletedate,format="%m/%d/%Y")) %>%
  drop_na(tri_enrollmentcompletedate) %>%
  mutate(DaysAfterEnrollment = as.numeric(difftime(ObservedTime,tri_enrollmentcompletedate,units=c("weeks")))) %>%
  mutate(WeekNumber = floor(DaysAfterEnrollment)+1)


#Part D

#Create summary table for the first 12 weeks of every patient visit

Q1_initialaverages <- Q1_C %>%
  select(ID, WeekNumber,SystolicValue, DiastolicValue) %>%
  filter(between(WeekNumber, 1, 12)) %>%
  group_by(ID, WeekNumber) %>%
  summarise_each(funs(mean))

#Extreme missingness in data. 
#Frequency Table of only weeks 1-12 shows that only 6 patients have data for all first 12 weeks 
#Frequency Table of all weeks shows 68 patients overall with over 12 weeks of data

Q1_frequency1 <- Q1_initialaverages %>%
  group_by(ID) %>%
  count()


Q1_frequency2 <- Q1_C %>%
  group_by(ID) %>%
  count()

#Evidently, many patients only began being observed several weeks after initial enrollment
#To fix this problem, create separate column that dynamically creates the "first week" 
#This way, patients with a delayed observation start can be better reflected in data.

Q1_RevisedDates <- Q1_C %>%
  select(-DaysAfterEnrollment, -WeekNumber) %>%
  group_by(ID) %>%
  mutate(FirstObservation = min(ObservedTime)) %>%
  mutate(DaysAfterObservation = as.numeric(difftime(ObservedTime,FirstObservation,units=c("weeks")))) %>%
  mutate(WeekNumber = floor(DaysAfterObservation)+1)
  
Q1_D <- Q1_RevisedDates %>%
  select(ID, WeekNumber,SystolicValue, DiastolicValue) %>%
  filter(between(WeekNumber, 1, 13)) %>%
  group_by(ID, WeekNumber) %>%
  summarise_each(funs(mean))

#While still imperfect (someone could have gotten observed once very early then had string of observations much later),
#This is the most accurate accounting I could think to do, and it caught an additional 61 observations.

#Part E

#To provide a bit of wiggle room in case observation for week 12 does not exist, I included weeks 11 and 13 as well.
#If a patient has a week 12, then that is used in the calculations, if not then week 11 or 13 are used instead.

Q1_Weekly <- Q1_D %>%
  filter(WeekNumber %in% c(1,11,12,13)) %>%
  group_by(ID) %>% 
  mutate(ObservationFrequency = n()) %>%
  filter(ObservationFrequency != 1) %>%
  filter(!(ObservationFrequency == 4 & WeekNumber %in% c(11,13))) %>%
  mutate(ObservationFrequency = ifelse(
    (ObservationFrequency %in% c(2,4) | (ObservationFrequency %in% 3 & WeekNumber %in% c(1,12))), 1, 0)
    ) %>%
  group_by(ID, ObservationFrequency) %>% 
  mutate(O2= n()) %>%
  filter(!(ObservationFrequency == 0 & O2 == 1)) %>%
  filter(!(ObservationFrequency == 0 & O2 == 2 & WeekNumber == 13)) %>%
  group_by(ID) %>%
  select(-ObservationFrequency, -O2)

#Reshaping table to subtract Week 1 and Week 12 values for Systolic and Diastolic Values

Q1_filteredevens <- Q1_Weekly %>%
  filter(row_number() %% 2 == 0) %>%
  rename(SystolicAfter = SystolicValue) %>%
  rename(DiastolicAfter = DiastolicValue)
Q1_filteredodds <- Q1_Weekly %>%
  filter(row_number() %% 2 == 1) %>%
  rename(SystolicBefore = SystolicValue) %>%
  rename(DiastolicBefore = DiastolicValue) %>%
  group_by() %>%
  select(DiastolicBefore,SystolicBefore)
  
Q1_finalDiffs <- cbind(Q1_filteredevens, Q1_filteredodds)

Q1_finalDiffs <- Q1_finalDiffs %>%
  mutate(SystolicDiff = SystolicAfter - SystolicBefore) %>%
  mutate(DiastolicDiff = DiastolicAfter - DiastolicBefore) %>%
  group_by()

#Compute and Assemble Summary Statistics Table

Q1_SummarySys <- Q1_finalDiffs %>% 
  summarise(Mean=mean(SystolicDiff), Max=max(SystolicDiff), Min=min(SystolicDiff), 
            Median=median(SystolicDiff), Std=sd(SystolicDiff))

Q1_SummaryDia <- Q1_finalDiffs %>% 
  summarise(Mean=mean(DiastolicDiff), Max=max(DiastolicDiff), Min=min(DiastolicDiff), 
            Median=median(DiastolicDiff), Std=sd(DiastolicDiff))

Q1_Summary <- rbind(Q1_SummarySys, Q1_SummaryDia)

Q1_percentiles_sys <- data.table(t(quantile(Q1_finalDiffs$SystolicDiff, probs = c(0.25, 0.75))))
Q1_percentiles_dia <- data.table(t(quantile(Q1_finalDiffs$DiastolicDiff, probs = c(0.25, 0.75))))
Q1_percentiles <- rbind(Q1_percentiles_sys, Q1_percentiles_dia)

Q1_titles <- c("Difference in Systolic Values From Week 1", "Difference in Diastolics Value From Week 1")

Q1_E <- cbind(Q1_titles, Q1_Summary, Q1_percentiles)

Q1_E <- Q1_E %>%
  select(Q1_titles, Mean, Median, everything())


#Part F

#Create table for very first and very last measure

#If both Systolic and Diastolic values are lower than 130 and 80 respectively, then falls into "controlled" category.

Q1_timecontrolled <- Q1_Weekly %>%
  mutate(Controlled = ifelse((SystolicValue <130 & DiastolicValue < 80) == TRUE, 1, 0))

#Create vector of alternating 0 and 1 and bind it to compare columns. 
#If a patient goes from 0 to 1 then their blood pressure, then their blood pressure was controlled.

f <- function(n){
  i <- 1:(n)
  ifelse(i %% 2, 0, 1)
}

Q1_pressurecheck <- c(f(70))

Q1_roughcheck <- cbind(Q1_timecontrolled, Q1_pressurecheck) %>%
  rename(Check = "...6")

Q1_F <- Q1_roughcheck %>%
  group_by(ID) %>%
  filter(Controlled == Check) %>%
  mutate(ObservationFrequency = n()) %>%
  filter(ObservationFrequency == 2)

nrow(Q1_F)/2

#Number of patients brought from uncontrolled to controlled = 6



#~~~~Question 2~~~~~~~


Q2_merged <- sqlQuery(myconn,"
WITH dem as (
select
*
FROM
Demographics

),
cond as (
select
tri_patientid,
STRING_AGG(tri_name,', ') as tri_name
FROM
Conditions
group by tri_patientid
),
texts as (
select
tri_contactId, max(TextSentDate) as TextSentDate
FROM
Text 
group by
tri_contactId
)
select
c.*,
d.*
from
(
select
a.*,
b.*
from cond a
LEFT OUTER JOIN
dem b ON
a.tri_patientid = b.contactid
) c
INNER JOIN
texts d ON
c.contactid = d.tri_contactId
", as.is = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE))


#~~~~Question 3~~~~~~~

#Port Demographics, Conditions and Text tables

Q3_demographics <- sqlQuery(myconn,"select * from Demographics")

Q3_conditions <- sqlQuery(myconn,"select * from Conditions") %>%
  group_by(tri_patientid) %>%
  summarise(tri_name = paste(tri_name, collapse = ", "))

Q3_text <- sqlQuery(myconn,"select tri_contactId, SenderName, TextSentDate from Text", as.is = c(TRUE,TRUE,TRUE)) %>%
  mutate(TextSentDate = anytime(TextSentDate)) %>%
  group_by(tri_contactId) %>%
  filter(TextSentDate==max(TextSentDate))

#Merge tables

Q3_merged <- Q3_demographics %>% 
  inner_join(Q3_conditions, by = c("contactid" = "tri_patientid")) %>%
  inner_join(Q3_text, by = c("contactid" = "tri_contactId"))



                      
