setwd("/Users/christopherwenz/Code/State_Demo")
library(tidyverse)
library(readr)
school_districts <- read_csv("data/school_districts.csv")

library(readxl)
ussd20 <- read_excel("data/ussd20.xls", skip = 2) %>%
  janitor::clean_names()%>%
  filter(state_postal_code == "MA")%>%
  rename(Zip=district_id)

merge <- left_join(school_districts, ussd20)


school_distrcit
  