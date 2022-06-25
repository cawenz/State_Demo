library(readr)
library(tidyverse)
library(tidygeocoder)
#*****************************************************************************************
#
# Clean the NCES LEA data for 21-22
#
#*****************************************************************************************
ccd_lea_029_2122_w_0a_022522<- read_csv("data/NCESdata/ccd_lea_029_2122_w_0a_022522.csv", 
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
  mutate(addrGEOID=
           paste0(LSTREET1,", ",LCITY,", ",LSTATE,", ",LZIP)
  )%>%
  relocate(addrGEOID)%>%
  select(
    LEA_NAME,LZIP, MAdisID, LEAID,SY_STATUS, SY_STATUS_TEXT, LEA_TYPE, 
    LEA_TYPE_TEXT, LEVEL, CHARTER_LEA, CHARTER_LEA_TEXT, addrGEOID,
    OPERATIONAL_SCHOOLS, 
    NOGRADES, starts_with("G_"), GSLO, GSHI)

ccdlea2122 <- map2_df(ccd_lea_029_2122_w_0a_022522, names(ccd_lea_029_2122_w_0a_022522),
                      ~  replace(.x, .x=="Yes", .y))%>%
  mutate(across(G_1_OFFERED:G_12_OFFERED, ~ parse_number(.x)))%>%
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ str_remove_all(str_remove_all(.x, "_OFFERED"),"G_"))) %>% 
  mutate(across(G_PK_OFFERED:G_KG_OFFERED, ~ ifelse(.x=="No", NA, .x)))%>%
  unite(col="gradesoff", G_PK_OFFERED:G_12_OFFERED, sep=",", na.rm=T)%>%
  mutate(gradesoff=str_replace(gradesoff, "KG", "K"))%>%
  select(LEA_NAME:LEAID, contains("LEA_TYPE"),addrGEOID, LEVEL:OPERATIONAL_SCHOOLS, gradesoff, GSLO, GSHI)

#*****************************************************************************************
#
# Clean the NCES School data for 21-22
#
#*****************************************************************************************
options(scipen=999)
library(readxl)
library(readr)
ccd_sch_029_2122_w_0a_022522 <- read_csv("data/NCESdata/ccd_sch_029_2122_w_0a_022522.csv")%>%
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


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Merge the census district data with the Federal list of LEAs
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


MAgeoDIS <- left_join(ccdlea2122, madistcensus, by=c("LEAID"="geoid"))%>%
  mutate(nomatch=is.na(type),
         addrGEOID=
                  ifelse(MAdisID=="04140000", "1 Commercial St, Adams, MA 01220",
                         ifelse(MAdisID=="04310000", "190 Hampshire St, Lawrence, MA 01840",
                                ifelse(MAdisID=="05220000",  "2201 Grand Army of the Republic Hwy, Swansea, MA 02777",addrGEOID
                                ))))%>%
  tidygeocoder::geocode(addrGEOID, method="osm", lat=latitude, long=longitude)
  

fullGEOS <- MAgeoDIS %>%
  mutate(latitude=
           ifelse(MAdisID=="05120000", 41.64379, latitude),
         longitude=
           ifelse(MAdisID=="05120000", -70.38097, longitude)
  )%>%
  mutate(type2=
           ifelse(is.na(type) & CHARTER_LEA=="CHRTIDEAESEA", "Charter",
            ifelse(is.na(type) & grepl("Vocational|Technical|Agricult", LEA_NAME), "Tech-Voc-Ag",
             ifelse(is.na(type) & grepl("Collaborative|Consortium|Cooperative", LEA_NAME), "Collaborative",
              ifelse(is.na(type) & grepl("Virtual", LEA_NAME), "Virtual", 
               ifelse(MAdisID=="04680000", "Tech-Voc-Ag", type
               ))))))%>%
  drop_na(type2)
                     

# Store the geography for the districts in geoJson format

maDistPolygons <- fullGEOS %>%
  filter(type2 %in% c("Unified","Elementary","Secondary")) %>% 
  select(MAdisID, type, geometry)

sf::st_write(fullGEOS, "output/fullGEOSmass.geojson")
