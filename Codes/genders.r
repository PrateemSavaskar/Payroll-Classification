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
library(tidyr)
setwd("F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs")
data1 <- read.csv("newsal.csv", na = c("Not Provided"), header = TRUE)

View(data1)

names <- extract(data1, EmployeeName, c("FirstName", "LastName"), "([^ ]+) (.*)")
View(names)

names$Gender_year <- 2011

genders <- gender_df(names, name_col = "FirstName", year_col = "Gender_year", method = c("ssa", "ipums", "napp", "demo"))

View(genders)

newsalwgender <- merge(names, genders[, c("name", "gender")], by.x = "FirstName", by.y = "name", all.x = TRUE)

View(newsalwgender)

write.csv(newsalwgender,"F:/patee/spring 2017/525 - data mining/labs/project/sf salary/final/salary docs/newsalwgender.csv")
