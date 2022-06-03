library(rvest)
url <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode=04450000&orgtypecode=5&"
urlpt1 <- "https://profiles.doe.mass.edu/analysis/default.aspx?orgcode="
urlpt2 <- "&orgtypecode=5&"



darturls <- 
  districts %>%
  select(1:2)%>%
  mutate(url= paste0(urlpt1,DisCode,urlpt2))

dart <- paste(darturls[1,3]) %>%
  read_html()%>%
  html_elements("table")%>%
  html_table(na.strings = "")

  hpsdart<- dart[[4]]%>%
  drop_na(X1)%>%
  mutate(X1=str_trim(str_remove(X1, "[//*]"), side="both"))%>%
  fuzzyjoin::stringdist_join(districts[1:3], by=c("X1"="District"), max_dist = 2)






