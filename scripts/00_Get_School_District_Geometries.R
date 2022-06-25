library(tidyverse)
library(tidycensus)
library(viridis)
key = "feff854f778d95f02fa512b65a8b3cb44fe04782"
# census_api_key(key, overwrite = FALSE, install = T)
readRenviron("~/.Renviron")

censusvars <- list(
  #Percent Below the poverty level totals
  "B06012_001",
  "B06012_002",
  "B06012_003",
  # Educational attainment totals
  "B06009_001",
  "B06009_002",
  "B06009_003",
  "B06009_004",
  "B06009_005",
  "B06009_006",
  "B06009_007",
  # Median income in past 12 months (inflation adjusted)
  "B06011_001",
  # Per-capita income in 2020 inflation adjusted dollars
  "B19301_001"
)

maunified <- get_acs(geography ="school district (unified)", 
                     variables = c("B06012_001",
                                   "B06012_002",
                                   "B06012_003",
                                   "B06009_001",
                                   "B06009_002",
                                   "B06009_003",
                                   "B06009_004",
                                   "B06009_005",
                                   "B06009_006",
                                   "B19013_001",
                                   "B19301_001"
                                   ),
                     state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Unified")


masecondary <- get_acs(geography ="school district (secondary)", 
                       variables = c("B06012_001",
                                     "B06012_002",
                                     "B06012_003",
                                     "B06009_001",
                                     "B06009_002",
                                     "B06009_003",
                                     "B06009_004",
                                     "B06009_005",
                                     "B06009_006",
                                     "B19013_001",
                                     "B19301_001"),
                       state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Secondary")

maelem <- get_acs(geography ="school district (elementary)", 
                  variables = c("B06012_001",
                                "B06012_002",
                                "B06012_003",
                                "B06009_001",
                                "B06009_002",
                                "B06009_003",
                                "B06009_004",
                                "B06009_005",
                                "B06009_006",
                                "B19013_001",
                                "B19301_001"),
                  state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Elementary")

madist <- bind_rows(maunified, masecondary, maelem)%>%
  mutate(name=
    case_when(
    variable == "B06012_001" ~ "Poverty_Total",
    variable == "B06012_002" ~ "Poverty_Below100per",
    variable == "B06012_003" ~ "Poverty_100to149per",
    variable == "B06009_001" ~ "Education_Total",
    variable == "B06009_002" ~ "Education_LessThanHS",
    variable == "B06009_003" ~ "Education_HSGrad",
    variable == "B06009_004" ~ "Education_SomeCollege",
    variable == "B06009_005" ~ "Education_Bachelors",
    variable == "B06009_006" ~ "Education_GradorProfessional",
    variable == "B19013_001" ~ "Median_Income", 
    variable == "B19301_001" ~ "PerCapita_Income"
     ))%>%
  pivot_wider(names_from=name, values_from=c("estimate", "moe"), 
              names_glue= "{name}_{.value}")%>%
  group_by(GEOID)%>%
  fill(Education_Total_estimate:PerCapita_Income_moe, .direction="up")%>%
  group_by(GEOID) %>% 
  slice(1)%>%
  select(GEOID:geometry, starts_with("Education"), starts_with("Poverty"),
         starts_with("Median"), starts_with("PerCapita"))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Make simpler variables to be used in analysis
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

madistcensus <- madist %>%
  ungroup()%>%
  mutate(
    belowHS=round(Education_LessThanHS_estimate/Education_Total_estimate,3)*100,
    belowHS_ci=round(Education_LessThanHS_moe/Education_Total_estimate,3)*100,
    college=round(Education_Bachelors_estimate/Education_Total_estimate,3)*100,
    colege_ci=round(Education_Bachelors_moe/Education_Total_estimate,3)*100,
    povbelow100=round(Poverty_Below100per_estimate/Poverty_Total_estimate,3)*100,
    povbelow100_ci=round(Poverty_Below100per_moe/Poverty_Total_estimate,3)*100,
    povbetween150=round(Poverty_100to149per_estimate/Poverty_Total_estimate,3)*100,
    povbetween150_ci=round(Poverty_100to149per_moe/Poverty_Total_estimate,3)*100
    # medincomerank=round(percent_rank(Median_Income_estimate), digits=2)*100,
    # percapincomerank=round(percent_rank(PerCapita_Income_estimate), digits=2)*100
  )%>%
  group_by(type)%>%
  mutate(
    medincomerank=round(percent_rank(Median_Income_estimate), digits=2)*100,
    percapincomerank=round(percent_rank(PerCapita_Income_estimate), digits=2)*100
  )%>%
  select(GEOID:geometry, belowHS:percapincomerank, contains("Median"), contains("PerCapita"))%>%
  drop_na(PerCapita_Income_moe)%>%
  janitor::clean_names()%>%
  relocate(geometry, .after=per_capita_income_moe)
