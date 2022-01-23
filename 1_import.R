library(tidyverse)
library(readxl)
library(sqldf)

med_2020 = read_xlsx("Medical 2020.xlsx")
rx_2020 = read_xlsx("Rx 2020.xlsx")


## Aggregate cost by type of visit
aggregate(med_2020$cost, by = list(Category = med_2020$tos), FUN = sum)


## Give count of ER visit by person
ER_visits = med_2020 %>% filter(ER == 1) %>% count(Person, From, ER) %>% 
  select(-n) %>% group_by(Person, ER) %>%
  summarize(
    total_ER_visits = n()
  )  %>% select(-ER)

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
sqldf("SELECT COUNT(ICD10)
      FROM med_2020
      WHERE ICD10 = 'U07.1'")

## How many total ER visits occurred where people got tested for COVID (positive or negative)
sqldf("SELECT COUNT(ICD10)
      FROM med_2020
      WHERE ICD10 IN ('Z03.818', 'Z20.828')")

## Positive/Negative cases by CPT codes:
cpt = med_2020 %>%
  select(ICD10, CPT4, Person) %>%
  filter(CPT4 == 86328 | CPT4 == 86769 | CPT4 == 87635) %>%
  mutate(test = if_else(ICD10 == "U07.1", "positive", "negative")) %>%
  distinct(ICD10, CPT4, Person, test)




