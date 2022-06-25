# Define Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#*****************************************************************************************
#
# Enroll by Grade
#
#*****************************************************************************************
#
enrollbygradefolder <- list.files("./data/DESEdata/enrollbygrade", full.names = T)

enrollbygradefiles <- lapply(enrollbygradefolder, function(i){
  x <- read_excel(i, col_types="text")
  x$file <- i
  x
}
)

gradesoffered <-  read_excel("data/DESEdata/GradesByDistricts.xlsx", 
                             col_types = c("skip", "text", "text"))%>%
  janitor::clean_names()

enrollbygrade <- bind_rows(enrollbygradefiles)%>%
  mutate(year=str_sub(file,start=-9L, end=-6L ))%>%
  select(-file)%>%
  janitor::clean_names()%>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020", "2021"))%>%
  relocate(year)%>%
  mutate(across(pk:total, ~ as.numeric(.x)))%>%
  # Remove PK enrollment 
  mutate(enrollNoPK=rowSums(.[5:18]))%>%
  # Add the grades offered
  left_join(gradesoffered)%>%
  mutate(numgrades=
           str_count(grade_list, ",")+1) %>% 
  mutate(across(pk:sp, ~ ifelse(.x == 0, -999, .x)))%>%
  unite(.,col=fix_grade_count, pk:x12, sep=",", remove=F)%>%
  mutate(numzero=14-str_count(fix_grade_count, "-999"), 
         numgrades=ifelse(is.na(numgrades), numzero, numgrades)
  )%>%
  mutate(across(pk:sp, ~ ifelse(.x == -999, 0, .x)), 
         enrollpergrade=trunc(total/numgrades)
  )

names(enrollbygrade) <- str_replace(names(enrollbygrade), "x", "grade")

enrolltotal <- enrollbygrade %>%
  select(year,  district_code, total, numgrades, enrollpergrade)

enroll6 <- enrolltotal %>%
  group_by(district_code)%>%
  summarize(total=round(mean(total), digits=0),
            enrollpergrade=trunc(weighted.mean(enrollpergrade, numgrades)),
            numgrades=Mode(numgrades)
  )

#*****************************************************************************************
#
# Section Description
#
#*****************************************************************************************

racegenderfolder <- list.files("./data/DESEdata/EnrollmentbyRaceGender_District", full.names=T)

racegenderfiles <- lapply(racegenderfolder, function(i){
  x <- read_excel(i, col_types="text", skip=1)
  x$file <- i
  x
})

racegender <- bind_rows(racegenderfiles) %>% 
  mutate(year=str_sub(file,start=-9L, end=-6L ))%>%
  select(-file)%>%
  janitor::clean_names()%>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020", "2021"))%>%
  relocate(year)%>%
  mutate(across(african_american:non_binary, ~ as.numeric(.x)))%>%
  # tack on total enrollment
  full_join(enrolltotal)

# Create a five-year average
rg6 <- racegender %>% 
  # turn the percentages to n
  mutate(across(african_american:non_binary, ~ trunc((.x*total)/100)))%>%
  group_by(district_name, district_code)%>%
  summarize(across(african_american:total, ~ round(weighted.mean(.x/total), digits=3)*100))%>%
  select(-non_binary, -total)

names(rg6) <- c("district_name","district_code", 
                "black", "asian", "hispanic", 
                "white", "native", "nativeHPI", 
                "multi", "male", "female"
)

#*****************************************************************************************
#
# Selected Populations
#
#*****************************************************************************************
selectpopfolder <- list.files("./data/DESEdata/select_pop", full.names = T)

selectpopfiles <- lapply(selectpopfolder, function(i){
  x <- read_excel(i, col_types = "text", skip=1)
  x$file=i
  x
}
)

selectpop <- bind_rows(selectpopfiles)%>%
  mutate(year=str_sub(file,start=-9L, end=-6L ))%>%
  select(-file)%>%
  janitor::clean_names()%>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020", "2021"))%>%
  select(-contains("lunch"), -contains("income"))%>%
  relocate(year)%>%
  mutate(across(
    first_language_not_english_number:economically_disadvantaged_percent, ~ as.numeric(.x)
  )
  )%>%
  rename(
    high_needs_number=high_needs_number_15, 
    high_needs_percent=high_needs_number_16
  )%>%
  left_join(enrolltotal)

# Get 6 year average

selectpop6 <- selectpop %>%
  ungroup()%>%
  select(year:district_code, contains("number"), total) %>%
  mutate(all=rowSums(.[4:8]))%>%
  ungroup()%>%
  group_by(district_code)%>%
  summarize(across(contains("number"), ~ round(weighted.mean(.x/total), digits=3)*100))

names(selectpop6) <- str_remove_all(names(selectpop6), "_number")

#*****************************************************************************************
#
# Bring all these indicators together
#
#*****************************************************************************************
all6frames <- list(enroll6, rg6, selectpop6)

all6 <- reduce(all6frames, full_join)%>%
  left_join(fullGEOS, by=c("district_code"="MAdisID"))%>%
  mutate(drop=
           ifelse((is.na(GSLO) & district_code!= "00000000"), T,F)
  )%>%
  filter(drop==F)%>%
  # left_join(madist, by=c("LEAID"="GEOID"))%>%
  select(-per_capita_income_moe, -drop, -nomatch, -variable, -CHARTER_LEA_TEXT, -longitude,-latitude)%>%
  mutate(
    typegroup=
      ifelse(type2=="Charter" & LEVEL %in% c("Elementary", "Middle"), "Elementary",
       ifelse(type2=="Charter" & LEVEL == "High", "Secondary", 
        ifelse(type2=="Charter" & LEVEL == "Other", "Unified",
         ifelse(type2=="Tech-Voc-Ag", "Secondary",
          ifelse(type2=="Virtual", "Unified", type2
          ))))))


