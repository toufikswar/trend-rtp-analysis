library("tidyverse")
library("lubridate")


ret <- read_delim(paste0("./teams/zebra__2020-08-27_10h00.csv"), delim = "\t")

empty <- tibble(
  a = character(),
  b = character(),
  c = character(),
  d = double(),
  e = double(),
  f = character(),
  g = character(),
  h = character(),
  i = double(),
)

myFiles <- list.files("./teams/")


for(filename in myFiles){
  
  temp <- read_delim(paste0("./teams/",filename), delim = "\t")
  
  
  temp$day <- filename %>%
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}") %>%
    str_replace("_", " ") %>%
    str_replace("h", ":") %>%
    paste("00", sep = ":") %>%
    ymd_hms()
  
  
  empty <- rbind(empty, temp)
  
  
}

empty %>%
write_excel_csv2("teams.csv")  
