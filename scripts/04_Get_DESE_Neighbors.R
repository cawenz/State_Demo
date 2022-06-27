# Define the normalization function
library(tidyverse)
library(FNN)
nor <-function(x) { (x -min(x))/(max(x)-min(x))} 

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Get the separate groups
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
allsimp <- all6 %>%
  filter(district_name != "Gosnold")%>%
  select(
    typegroup,
    district_name,
    district_code,
    enrollpergrade,
    first_language_not_english,
    english_language_learner, 
    economically_disadvantaged, 
    students_with_disabilities,
    hispanic, black,white,multi,asian
  )%>%
  arrange(typegroup)%>%
  group_by(typegroup)%>%
  mutate(
    row=row_number()
  )%>%
  relocate(row)

#
#*****************************************************************************************
#
# Get the DART merge 
#
#*****************************************************************************************
#
dartmatches <- read_csv("output/dartmatches.csv")


dartmerge <- dartmatches %>%
  filter(targetrow!=1)%>%
  mutate(neighbor=row_number())%>%
  relocate(neighbor)%>%
  rename(
    district_code=MAdisID, 
    target_DART=target)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Unified 5 neighbors
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

unified <- allsimp %>%
  filter(typegroup=="Unified")%>%
  mutate(across(enrollpergrade:asian, ~scale_this(.x)))%>%
  ungroup()

uniknn <- unified %>%
  select(enrollpergrade:asian)%>%
  get.knn(k=10, algorithm = "kd_tree")%>%
  as.data.frame()%>%
  select(contains("nn.index"))%>%
  bind_cols(unified %>% select(district_code, district_name))%>%
  rename(target=district_name)%>%
  pivot_longer(cols=contains("nn"), names_to="neighbor", values_to="row")%>%
  left_join(unified %>% select(row, district_code, district_name), by="row")%>%
  rename(
    district_code=district_code.x,
    neighnor_code=district_code.y,
    neighbor_name=district_name
  )%>%
  mutate(neighbor=parse_number(str_remove(neighbor, "nn.index.")))%>%
  left_join(dartmerge)



secondary <- allsimp %>%
  filter(typegroup=="Secondary")

elementary <- allsimp %>%
  filter(typegroup=="Elementary")

