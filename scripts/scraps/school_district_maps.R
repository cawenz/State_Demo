
setwd("./schooldistricts_shapes")
getwd()

library(rgdal)
library(precrec)

shape <- readOGR(dsn=".", layer="SCHOOLDISTRICTS_POLY")

library(tidyverse)
library(tidycensus)
library(viridis)
key = "feff854f778d95f02fa512b65a8b3cb44fe04782"
# census_api_key(key, overwrite = FALSE, install = T)
readRenviron("~/.Renviron")
maunified <- get_acs(geography ="school district (unified)", variables = "B19013_001",
        state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Unified")
masecondary <- get_acs(geography ="school district (secondary)", variables = "B19013_001",
                     state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Secondary")

maelem <- get_acs(geography ="school district (elementary)", variables = "B19013_001",
                       state = "MA",geometry = TRUE, year = 2020)%>%
  mutate(type="Elementary")

madist <- bind_rows(maunified, masecondary, maelem)

library(ggpattern)
ggplot()+
  geom_sf(data=maunified, fill="yellow",alpha=0.5, color="white")+
  geom_sf(data=maelem, fill="red", alpha=0.5, color="red")+
  geom_sf(data=masecondary, fill="blue", alpha=0.5, color="blue")
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")
  
  ggplot()+
    geom_sf(data=maunified[14,6])
  

  
acs2020 <- load_variables(2020, "acs5", cache = TRUE)
