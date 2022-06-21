library(rvest)
url <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode=04450000&orgtypecode=5&"
urlpt1 <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode="
urlpt2 <- "&orgtypecode=5&"



darturls <- 
  lea2122 %>%
  select(LEA_NAME, MAdisID)%>%
  mutate(url= paste0(urlpt1,MAdisID,urlpt2))




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
    # stringdist_join(darturls, by=c("X1"="LEA_NAME"), max_dist=2)%>%
    # group_by(X1)%>%
    # slice(1)%>%
    relocate(target)
  
  dartlist[[row]] <- drt2
}

dartlist2 <- list()
darturls2 <- darturls[213:426,]


for(row in 1:nrow(darturls2)){
  drt3 <- paste(darturls2[row,3])%>%
    read_html()%>%
    html_elements("table")%>%
    html_table(na.strings = "")
  
  drt4 <- drt3[[4]]%>%
    drop_na(X1)%>%
    mutate(X1=str_trim(str_remove(X1, "[//*]"), side="both"),
           target=paste(darturls2[row,1]))%>%
    stringdist_join(darturls, by=c("X1"="LEA_NAME"), max_dist=2)%>%
    group_by(X1)%>%
    slice(1)%>%
    relocate(target)%>%
    select(-url)
  
  dartlist2[[row]] <- drt4
}



darttable1 <- reduce(dartlist, full_join)
darttable2 <- reduce(dartlist2, full_join)

darttablecomplete <- full_join(darttable1, darttable2)

dart <- paste(darturls[1,3]) %>%
  read_html()%>%
  html_elements("table")%>%
  html_table(na.strings = "")


library(fuzzyjoin)


  
  
  
  fuzzyjoin::stringdist_join(darturls, by=c("X1"="MAdisID"), max_dist = 2)






darturls[,3]