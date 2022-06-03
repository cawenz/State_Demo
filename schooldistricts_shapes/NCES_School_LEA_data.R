library(readr)
library(tidyverse)
#*****************************************************************************************
#
# Clean the NCES LEA data for 21-22
#
#*****************************************************************************************
ccd_lea_029_2122_w_0a_022522<- read_csv("~/Code/State_Demo/data/NCESdata/ccd_lea_029_2122_w_0a_022522.csv", 
                                         col_types = cols(ST_LEAID = col_character(), 
                                                          LZIP=col_character(),
                                                          MZIP=col_character(),
                                                          LEAID = col_character())) %>% 
  filter(ST=="MA")%>%
  mutate(LZIP=paste0("0", LZIP),
         MZIP=paste0("0", MZIP), 
         MAdisID=paste0(str_sub(ST_LEAID, start=4), "0000")
  )%>%
  relocate(MAdisID, .after=ST_LEAID)%>%
  select(
    LEA_NAME,LZIP, MAdisID, LEAID,SY_STATUS, SY_STATUS_TEXT, LEA_TYPE, 
    LEA_TYPE_TEXT, LEVEL, CHARTER_LEA, CHARTER_LEA_TEXT, 
    OPERATIONAL_SCHOOLS, 
    NOGRADES, starts_with("G_"), GSLO, GSHI)

ccdlea2122 <- map2_df(ccd_lea_029_2122_w_0a_022522, names(ccd_lea_029_2122_w_0a_022522),
                      ~  replace(.x, .x=="Yes", .y))%>%
  mutate(across(G_1_OFFERED:G_12_OFFERED, ~ parse_number(.x)))%>%
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ str_remove_all(str_remove_all(.x, "_OFFERED"),"G_"))) %>% 
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ ifelse(.x=="No", NA, .x)))%>%
  unite(col="gradesoff", G_PK_OFFERED:G_12_OFFERED, sep=",", na.rm=T)%>%
  mutate(gradesoff=str_replace(gradesoff, "KG", "K"))%>%
  select(LEA_NAME:LEAID, contains("LEA_TYPE"),LEVEL:OPERATIONAL_SCHOOLS, gradesoff, GSLO, GSHI)

#*****************************************************************************************
#
# Clean the NCES School data for 21-22
#
#*****************************************************************************************
options(scipen=999)
library(readxl)
library(readr)
ccd_sch_029_2122_w_0a_022522 <- read_csv("~/Code/State_Demo/data/NCESdata/ccd_sch_029_2122_w_0a_022522.csv")%>%
  filter(ST=="MA")%>%
  mutate(LZIP=paste0("0", LZIP),
         MZIP=paste0("0", MZIP), 
         MAdisID=paste0(str_sub(ST_LEAID, start=4), "0000")
  ) %>% 
  select(
    SCH_NAME, LEA_NAME,LZIP, ST_LEAID,MAdisID, LEAID, ST_SCHID, NCESSCH, SCHID, 
    SY_STATUS, SY_STATUS_TEXT, 
    SCH_TYPE, SCH_TYPE_TEXT,
    CHARTER_TEXT,
    LEVEL, starts_with("G_"), GSLO, GSHI)
  
ccdsch2122 <- 
  map2_df(ccd_sch_029_2122_w_0a_022522, names(ccd_sch_029_2122_w_0a_022522),
          ~  replace(.x, .x=="Yes", .y))%>%
  mutate(across(G_1_OFFERED:G_12_OFFERED, ~ parse_number(.x)))%>%
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ str_remove_all(str_remove_all(.x, "_OFFERED"),"G_"))) %>% 
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ ifelse(.x=="No", NA, .x)))%>%
  unite(col="gradesoff", G_PK_OFFERED:G_12_OFFERED, sep=",", na.rm=T)%>%
  mutate(gradesoff=str_replace(gradesoff, "KG", "K"))%>%
  select(SCH_NAME:gradesoff, GSLO:GSHI)%>%
  mutate(CHARTER_TEXT=ifelse(CHARTER_TEXT=="CHARTER_TEXT","Yes", CHARTER_TEXT))


testjoin <- left_join(ccdlea2122, maunified, by=c("LEAID"="GEOID"))%>%
  mutate(nomatch = is.na(type))%>%
  filter(nomatch == T)
