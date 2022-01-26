library(tidyverse)
library(readxl)
library(sqldf)

med_2020 = read_xlsx("Medical 2020.xlsx")
rx_2020 = read_xlsx("Rx 2020.xlsx")
hris = read_xlsx("HRIS.xlsx")


## Aggregate cost by type of visit
aggregate(med_2020$cost, by = list(Category = med_2020$tos), FUN = sum)


## Give count of ER visit by person
ER_visits = med_2020 %>% filter(ER == 1) %>% count(Person, From) %>% 
  select(-n) %>% group_by(Person) %>%
  summarize(
    total_ER_visits = n()
  )  

sqldf("SELECT SUM(total_ER_visits) 
      FROM ER_visits")
  
## Patients with > $0 costs
ER_visit_cost = med_2020 %>% filter(ER == 1) %>% distinct(Person, From, cost) %>% 
  group_by(Person, From) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0) 

sqldf("SELECT COUNT(Person) 
      FROM ER_visit_cost")

sqldf("SELECT COUNT(DISTINCT(Person))
      FROM ER_visit_cost")


## How many ER visits results in a COVID positive


## How many total ER visits occurred where people got tested for COVID (positive or negative)


## Positive/Negative cases by CPT codes:



## ER visits by relation
a = med_2020 %>%
  filter(ER == 1) %>% distinct(Person, From, cost, Relcode) %>% 
  group_by(Person, From, Relcode) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0)

sqldf("SELECT COUNT(Person), Relcode 
      FROM a
      GROUP BY Relcode")


## ER visits by age group
b = med_2020 %>%
  filter(ER == 1) %>% distinct(Person, From, cost, `Age Range`) %>% 
  group_by(Person, From, `Age Range`) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0)

sqldf("SELECT COUNT(Person), `Age Range` 
      FROM b
      GROUP BY `Age Range`")

## merging
merged = med_2020 %>% filter(ER == 1) %>% count(Person, From, cost, EE) %>% 
  select(-n) %>% group_by(Person, From, EE) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0) %>%
  left_join(hris, by = "EE") %>%
  janitor::clean_names()

sqldf("SELECT COUNT(person_x) AS persons, Nurse 
      FROM merged
      GROUP BY Nurse")

## Repeat ER visits
repeated = med_2020 %>% filter(ER == 1) %>% count(Person, cost, EE) %>%
  filter(n > 1) %>% group_by(Person, EE) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0) %>%
  left_join(hris, by = "EE") %>%
  janitor::clean_names()

sqldf("SELECT COUNT(person_x) AS persons, Nurse 
      FROM repeated
      GROUP BY Nurse")


## CPT4 that start with J by type of service
query = med_2020 %>% filter(str_detect(CPT4, "^J")) %>%
  janitor::clean_names()

sqldf("SELECT COUNT(person) AS persons, tos 
      FROM query
      GROUP BY tos")



