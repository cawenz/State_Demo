library(tidyverse)

MAdistricts <- read_csv("data/school_districts.csv")%>%
  janitor::clean_names()%>%
  unite(.,col="addrGEO",c("address_1", "town", "state","zip"),sep=", ",remove=F)%>%
  tidygeocoder::geocode(addrGEO, method="osm", lat=latitude, long=longitude)

nogeocode <- MAdistricts %>%
  filter(is.na(latitude)|is.na(longitude))%>%
  filter(!grepl("non-op", org_name))

library(googlesheets4)

gs4_create(sheets=nogeocode)

library(tidygeocoder)

disGeo <- tidygeocoder::geocode(MAdistricts$addrGEO)