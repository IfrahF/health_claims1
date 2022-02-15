library(tidyverse)
library(readxl)
library(sqldf)

med_2020 = read_xlsx("Medical 2020.xlsx")
rx_2020 = read_xlsx("Rx 2020.xlsx")
hris = read_xlsx("HRIS.xlsx")
risk = read_xlsx("Risk 2020.xlsx") %>%
  janitor::clean_names()
hosp = read_xlsx("hospital 2020.xlsx") %>%
  janitor::clean_names()
bio = read_xlsx("Biometricsx 2020.xlsx") %>%
  janitor::clean_names()

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
a = med_2020 %>% filter(ER == 1) %>%
  filter(ICD10 == "U07.1" | ICD10 == "J12.89" | ICD10 == "J20.8" | ICD10 == "J22" | 
           ICD10 == "J98.8" | ICD10 == "J80" | ICD10 == "Z86.19" | ICD10 == "Z86.16" |
           ICD10 == "U09.9") %>% 
  distinct(Person, From, cost) %>% 
  group_by(Person, From) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0) 

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

## How many ER visits were by diabetics? (merge Risk database by Person) 
diab_risk = med_2020 %>%
  left_join(risk, by = c("Person", "EE")) %>% filter(ER == 1) %>% 
  count(Person, From, EE, cost, Diabetes) %>% 
  select(-n) %>% group_by(Person, From, EE, Diabetes) %>%
  summarize(
    total_cost = sum(cost)
  ) %>% filter(total_cost > 0) 

sqldf("SELECT COUNT(Person) AS visits 
      FROM diab_risk
      WHERE Diabetes = 'Y'")


sqldf("SELECT COUNT(DISTINCT(Person)) AS diabetics 
      FROM diab_risk
      WHERE Diabetes = 'Y'")


correct_er = med_2020 %>%
   group_by(Person, From) %>%
   mutate(total_cost = sum(cost)) %>% 
   filter(total_cost > 0) %>%
   filter(CPT4 %in% c(99281, 99282, 99283, 99284, 99285)) %>%
   distinct(Person, From, .keep_all = TRUE) 


# number of employees and number of members for nurses and non
sqldf("SELECT COUNT(Person) AS total_employees_t20
       FROM hris
       GROUP BY T2020")

sqldf("SELECT COUNT(Person) AS total_employees_t19
       FROM hris
       GROUP BY T2019")

sqldf("SELECT COUNT(Person) AS total_employees_t18
       FROM hris
       GROUP BY T2018")
  
hris = hris %>%
  janitor::clean_names() %>%
  mutate(employee = if_else(t2020 %in% c(1, 2, 3), 1, 0)) %>%
  mutate(members = if_else(t2020 == 1, 1, 
                           if_else(t2020 %in% c(2, 3), 2, 
                                   if_else(t2020 == 4, 4, 0))))

sqldf("SELECT SUM(employee) AS total_employees_t20, nurse
       FROM hris
       GROUP BY nurse")

sqldf("SELECT SUM(members) AS total_members_t20
       FROM hris")

sqldf("SELECT SUM(members) AS total_members_t20, nurse
       FROM hris
       GROUP BY nurse")

#number of nurse and non-nurse ER visits. divide by membership to see who had the higher rate of ER visits
repeated = med_2020 %>% filter(ER == 1) %>% 
  count(Person, cost, From, EE) %>%
  group_by(Person, From) %>%
  mutate(total_cost = sum(cost)) %>% 
  filter(total_cost > 0) %>%
  distinct(Person, From, .keep_all = TRUE)  %>%
  janitor::clean_names() %>%
  left_join(hris, by = "ee") %>%
  janitor::clean_names()

sqldf("SELECT COUNT(person_x) AS total_people, nurse
       FROM repeated
       GROUP BY nurse")

sqldf("SELECT COUNT(DISTINCT(person_x)) AS total_people, nurse
       FROM repeated
       GROUP BY nurse")


# Opioid
## 364 prescriptions
## 136 people

sqldf("SELECT COUNT(DISTINCT(Person)) AS total_people, Class
       FROM rx_2020
       GROUP BY Class")

opioid = rx_2020 %>%
  janitor::clean_names() %>%
  mutate(supply = if_else(days < 30, 2,
                          if_else(days < 10, 1, 0)))

supply = sqldf("SELECT DISTINCT Person, SUM(Days) AS supply
       FROM rx_2020
       WHERE Class == 'ANALGESICS - OPIOID'
       GROUP BY Person")

supply = supply %>%
  janitor::clean_names() %>%
  mutate(supply_range = if_else(supply > 180, "More than 180 days",
                          if_else(supply >= 90, "90-180 days", 
                                  if_else(supply >= 30, "30-90 days", 
                                          if_else(supply >= 10, "10-30 days", "Less than 10 days")))))

sqldf("SELECT supply_range, COUNT(Person) AS total_people
       FROM supply
       GROUP BY supply_range")

## 123-01 has almost 2 years supply of opioids (2 types - OXYCODONE HCL and MORPHINE SULFATE ER)
## 55+ year old male
## Has diabetes and hypertension with a risk score of 7
## Also has PSYCHOTHERAPEUTIC AND NEUROLOGICAL AGENTS prescriptions
## Has Chronic pain syndrome (G89.4) and Polyneuropathy (G62.9) so he is in a lot of pain


## People with risk score over 10
sqldf("SELECT COUNT(DISTINCT(person)) AS risky_people 
      FROM risk
      WHERE risk_score > 10")

## People with BMI over 26
sqldf("SELECT COUNT(DISTINCT(person)) AS high_bmi
      FROM bio
      WHERE bmi > 26")

pain_med = sqldf("SELECT COUNT(DISTINCT(Person)) AS people_in_pain
       FROM rx_2020
       WHERE Class IN ('ANALGESICS - OPIOID', 'ANALGESICS - ANTI-INFLAMMATORY', 'ANALGESICS - NONNARCOTIC')")

tot_adm = hosp %>%
  distinct(person, from, .keep_all = T)

sqldf("SELECT SUM(los) AS total_los
      FROM hosp")

nurses = hosp %>% 
  inner_join(hris, by = "person") %>%
  filter(nurse == "Y") %>%
  summarise(
    los = sum(los)
  )

# Nurse admission rate = 5/240 = 2.08%
# Non-nurse admission rate = 8/783 = 1.02%

riskxhosp = hosp %>% 
  inner_join(risk, by = "person") %>%
  filter(risk_score > 10) %>%
  summarise(
    los = sum(los)
  )




