library(rvest)
library(tidyverse)
library(geojsonio)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Define the url parts for rvest
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
url <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode=04450000&orgtypecode=5&"
urlpt1 <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode="
urlpt2 <- "&orgtypecode=5&"


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Load data
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
disGEOS <- geojson_sf("output/fullGEOSmass.geojson")
#
desedirectory <- read_csv("data/school_districts.csv")%>%
  mutate(disname=
           ifelse(grepl("Match", `Org Name`), "MATCH Charter Public School (District)",
            ifelse(grepl("Southern Worcester County", `Org Name`), "Southern Worcester County Regional Vocational Technical",
             ifelse(grepl("City on a Hill", `Org Name`),
              "City on a Hill Charter Public School Circuit Street (District)",
              ifelse(grepl("Science II", `Org Name`), "Pioneer Charter School of Science II (PCSS-II) (District)",
              `Org Name`
             )))))%>%
  filter(!grepl("(non-op)", `Org Name`))

# desecodes <- desedirectory %>%
#   select(`Org Code`, `Org Name`)%>%
#   rename(district_code=`Org Code`)%>%
#   full_join(all6)
#   full_join(all6)

# Create a simple frame with the urls to use in the rvest loop
darturls <- 
  disGEOS %>%
  filter(type2!="Collaborative")%>%
  select(LEA_NAME, MAdisID, -geometry)%>%
  mutate(url= paste0(urlpt1,MAdisID,urlpt2))%>%
  left_join(desedirectory, by=c("MAdisID"="Org Code"))%>%
  #Remove the districts that do not have DART page 
  filter(!LEA_NAME %in% c("Dover", "Gosnold", "Halifax", 
                          "Marion", "Pelham", "Plympton",
                          "Rochester", "Sherborn", "Shutesbury",
                          "Southampton", "Sturbridge"
                          ))


noDART <- disGEOS %>%
  filter(LEA_NAME %in%c("Dover", "Gosnold", "Halifax", 
                        "Marion", "Pelham", "Plympton",
                        "Rochester", "Sherborn", "Shutesbury",
                        "Southampton", "Sturbridge"
  )) %>% 
  rename(target=LEA_NAME,
         `Org Code`=MAdisID,
  )%>%
  select(target, `Org Code`)

noDART$geometry <- NULL
darturls$geometry <- NULL


dartlist <- list()

for(row in 1:nrow(darturls)){
  drt1 <- paste(darturls[row,3])%>%
    read_html()%>%
    html_elements("table")%>%
    html_table(na.strings = "")
  
  drt2 <- drt1[[4]]%>%
    drop_na(X1)%>%
    mutate(X1=str_trim(str_remove(X1, "[//*]"), side="both"),
           target=paste(darturls[row,1]))%>%
    relocate(target)
  
  dartlist[[row]] <- drt2
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Clean up the DART data frame
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Collapse all of the list items into a single data frame
darttable <- reduce(dartlist, full_join)


dartclean <- darttable %>%
  # eliminate the header rows (which were carried over from the scrape of each table)
  mutate(keep=ifelse(X1=="District Name", F, T))%>%
  filter(keep==T)%>%
  left_join(desedirectory, by=c("X1"="disname"))%>%
  rename(matchCode=`Org Code`)%>%
  relocate(matchCode, .after=X1)%>%
  mutate(matchCode=
           ifelse(X1=="Greenfield Commonwealth Virtual District","39010000",
            ifelse(X1 == "Phoenix Charter Academy (District)", "04931001",
             matchCode
            )))%>%
  drop_na(matchCode)%>%
  full_join(noDART)%>%
  select(target, X1, matchCode, X1:X14)%>%
  left_join(desedirectory, by=c("target"="disname"))%>%
  rename(targetCode=`Org Code`)%>%
  relocate(targetCode)%>%
  mutate(targetCode=
           ifelse(grepl("Phoenix Academy Public Charter High School Springfield", target),
                  "35080000",
           ifelse(grepl("Phoenix Academy Public Charter High School Lawrence", target),
                  "35180000",
           ifelse(target=="Phoenix Charter Academy (District)", 
                  "04931001",
           ifelse(grepl("Martin Luther King",target), 
                  "04920000",
           ifelse(grepl("Edward M. Kennedy", target),
                  "04520000",
           ifelse(grepl("Learning First Charter", target),
                  "04860000",
           ifelse(grepl("City on a Hill", target),
                  "04370000", 
           ifelse(grepl("Community Day Charter Public School", target),
                  "04310000",
                  targetCode
           )))))))))%>%
  group_by(target)%>%
  mutate(nmatch=n())%>%
  relocate(nmatch, .after=targetCode)%>%
  ungroup()%>%
  mutate(
    targetrow=
      ifelse(targetCode==matchCode, 1,0))%>%
  relocate(targetrow, .after=target)
  
dart <- dartclean %>%
  select(targetCode, nmatch, target, targetrow,X1,matchCode)%>%
  rename(matchdis=X1,
         MAdisID=targetCode, 
         MAdisIDmatch=matchCode
  )%>%
  group_by(target)%>%
  arrange(target, desc(targetrow))%>%
  left_join(ccdlea2122, by="MAdisID")%>%
  select(MAdisID:MAdisIDmatch,LEAID,GSLO:GSHI, `OPERATIONAL_SCHOOLS`)%>%
  rename(GSLOtarget=GSLO, 
         GSHItarget=GSHI,
         LEAIDtarget=LEAID,
         numSchoolstarget=`OPERATIONAL_SCHOOLS`
         )%>%
  left_join(ccdlea2122, by=c("MAdisIDmatch"="MAdisID"))%>%
  select(MAdisID, LEAIDtarget,target,targetrow, nmatch,numSchoolstarget,GSLOtarget, GSHItarget,
         MAdisIDmatch, LEAID, matchdis, `OPERATIONAL_SCHOOLS`, GSLO,GSHI)

write.csv(dart, "output/dartmatches.csv", row.names=F)

# 
# 
# gradeconfig <- unique(dart[c("GSLO","GSHI")])
# 
# dartcount <- dartclean %>%
#   group_by(target)%>%
#   count(targetrow, .drop=F)
