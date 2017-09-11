library(readxl)
library(rJava)
library(e1071)
library(resample)
library(magrittr)
library(dplyr)
library(GGally)
library(gender)
library(readr)
library(ggplot2)
setwd("F:/patee/spring 2017/525 - data mining/labs/project/sf salary/sal/")
data1 <- read.csv("Salaries.csv", na = c("Not Provided"), header = TRUE)
str(data1)
summary(data1)
head(data1)

# cleaning data

names(data1)

# id

table(duplicated(data1$Id))
data1 <- data1 %>%
  select(-Id)

# notes and agency

data1 %>%
  select(c(Notes,Agency)) %>%
  summarize_each(funs(unique_vars = length(unique(.))))

# remove notes and agency

data1 <- data1 %>%
  select(-Notes, -Agency)

View(data1)

data1 %>%
  select(one_of(names(data1)[sapply(data1, is.numeric)])) %>%
  summary


data1 %>%
  filter(TotalPay < 0)

data1 %>%
  filter(TotalPay == 0)

data1 %>%
  filter(TotalPay > 0)

# JobTitle
str(data1$JobTitle)

data1 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head

data1 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

data1 %>%
  filter(!duplicated(JobTitle)) %>%
  head()

options(scipen = 999)

data1 %>%
  mutate(SharesTitle = duplicated(JobTitle)) %>%
  ggplot(aes(x=TotalPay)) +
  geom_density(aes(fill = SharesTitle), alpha = 0.6)

# We now look at numeric variables
data1 %>%
  select(one_of(names(data1)[sapply(data1, is.numeric)])) %>%
  summary

data1 %>%
  filter(TotalPay < 0) # just 1, maybe an error?

# who has a pay of 0?
data1 %>%
  filter(TotalPay == 0) # 368 employees, some with benefits -- perhaps retied or injured?

# who has a pay between 0 and 100.00
data1 %>%
  filter(TotalPay > 0, TotalPay < 100)

# perhaps Status will reveal something about interesting about that

data1 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.5)

# status ocunts
data1 %>%
  group_by(Status) %>%
  summarise(Frequency = n())

# OvertimePay
str(data1$OvertimePay)

# OtherPay
str(data1$OtherPay)

# Benefits
str(data1$Benefits)

# Year
str(data1$Year)

#Clean Data
data1 <- filter(data1,JobTitle!="NA",BasePay!="NA",Benefits!="NA",BasePay!=0,OvertimePay!=0)
data2 <- filter(data1,JobTitle %in% c("Transit Operator","Registered Nurse","Special Nurse","Public Svc Aide-Public Works","Police Officer 3","Custodian","Firefighter","Recreation Leader","Patient Care Assistant"))
write.csv(data1,"F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/newsal.csv")
write.csv(data2,"F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/newsal2.csv")

