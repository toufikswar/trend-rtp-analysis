library("tidyverse")
library("lubridate")


df.all <- read_delim("./all.csv", delim = "\t")

View(df.all)

df.all <- df.all %>%
  select(Name, `Anti Virus Status (Get Windows Security Center Health Status)`)


empty <- tibble(
  a = character(),
  b = character(),
  c = double()
)

myFiles <- list.files("./data/")


for(filename in myFiles){

  temp <- read_delim(paste0("./data/",filename), delim = "\t")

  
  temp$day <- filename %>%
    str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}") %>%
    str_replace("_", " ") %>%
    str_replace("h", ":") %>%
    paste("00", sep = ":") %>%
    ymd_hms()
  
  
  empty <- rbind(empty, temp)

  
}


loaded.data <- empty
write_excel_csv(loaded.data, "./loaded_csv_data.csv")


loaded.data <- read_delim("./loaded_csv_data.csv", delim = "\t")

### Number of appearences of the different antiviruses
appearing <- empty %>%
  na.omit() %>%
  mutate(trend_rtp_off = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: off", all_antiviruses, fixed = T) & !grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T) , TRUE, FALSE),
         trend_rtp_on = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         cylance_rtp_off = ifelse(grepl("CylancePROTECT (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         cylance_rtp_on = ifelse(grepl("CylancePROTECT (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         avast_rtp_off = ifelse(grepl("Avast Antivirus (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         avast_rtp_on = ifelse(grepl("Avast Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         malwarebyte_rtp_off = ifelse(grepl("Avast Antivirus (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         malwarebyte_rtp_on = ifelse(grepl("Avast Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         win_def_rtp_off = ifelse(grepl("Windows Defender Antivirus (RTP: off", all_antiviruses, fixed = T) | grepl("Windows Defender (RTP: off", all_antiviruses, fixed = T) , TRUE, FALSE),
         win_def_rtp_on = ifelse(grepl("Windows Defender Antivirus (RTP: on", all_antiviruses, fixed = T) | grepl("Windows Defender (RTP: on", all_antiviruses, fixed = T) , TRUE, FALSE),
         last_seen_from_execution = round(difftime(day,last_seen, units = "days"),0)) %>%
  arrange(-last_seen_from_execution) %>%
  filter(last_seen_from_execution <= 15) %>%
  select(name, day, trend_rtp_off, trend_rtp_on, cylance_rtp_off, cylance_rtp_on, avast_rtp_off, avast_rtp_on, malwarebyte_rtp_off, malwarebyte_rtp_on, win_def_rtp_off ,win_def_rtp_on) %>%
  group_by(name) %>%
  summarise(appear = n())

##### Part with different antiviruses
empty %>%
  na.omit() %>%
  mutate(trend_rtp_off = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: off", all_antiviruses, fixed = T) & !grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T) , TRUE, FALSE),
         trend_rtp_on = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         cylance_rtp_off = ifelse(grepl("CylancePROTECT (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         cylance_rtp_on = ifelse(grepl("CylancePROTECT (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         avast_rtp_off = ifelse(grepl("Avast Antivirus (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         avast_rtp_on = ifelse(grepl("Avast Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         malwarebyte_rtp_off = ifelse(grepl("Avast Antivirus (RTP: off", all_antiviruses, fixed = T), TRUE, FALSE),
         malwarebyte_rtp_on = ifelse(grepl("Avast Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         win_def_rtp_off = ifelse(grepl("Windows Defender Antivirus (RTP: off", all_antiviruses, fixed = T) | grepl("Windows Defender (RTP: off", all_antiviruses, fixed = T) , TRUE, FALSE),
         win_def_rtp_on = ifelse(grepl("Windows Defender Antivirus (RTP: on", all_antiviruses, fixed = T) | grepl("Windows Defender (RTP: on", all_antiviruses, fixed = T) , TRUE, FALSE),
         last_seen_from_execution = round(difftime(day,last_seen, units = "days"),0)) %>%
  arrange(-last_seen_from_execution) %>%
  filter(last_seen_from_execution <= 15) %>%
  select(name, day, trend_rtp_off, trend_rtp_on, cylance_rtp_off, cylance_rtp_on, avast_rtp_off, avast_rtp_on, malwarebyte_rtp_off, malwarebyte_rtp_on, win_def_rtp_off ,win_def_rtp_on) %>%
  inner_join(appearing, by = c("name" = "name")) %>% 
  group_by(name, appear) %>%
  mutate(trend_on = sum(trend_rtp_on),
         trend_off = sum(trend_rtp_off),
         cylance_on = sum(cylance_rtp_on),
         cylance_off = sum(cylance_rtp_off),
         avast_on = sum(avast_rtp_on),
         avast_off = sum(avast_rtp_off),
         malwarebyte_on = sum(malwarebyte_rtp_on),
         malwarebyte_off = sum(malwarebyte_rtp_off),
         win_def_on = sum(win_def_rtp_on),
         win_def_off = sum(win_def_rtp_off)) %>%
  ungroup() %>%
  group_by(name, appear) %>%
  summarise(pct_trend_on = round(max(trend_on/appear)*100,0),
            pct_trend_off = round(max(trend_off/appear)*100,0),
            pct_cylance_on = round(max(cylance_on/appear)*100,0),
            pct_cylance_off = round(max(cylance_off/appear)*100,0),
            pct_avast_on = round(max(avast_on/appear)*100,0),
            pct_avast_off = round(max(avast_off/appear)*100,0),
            pct_malwarebyte_on = round(max(malwarebyte_on/appear)*100,0),
            pct_malwarebyte_off = round(max(malwarebyte_off/appear)*100,0),
            pct_win_def_on = round(max(win_def_on/appear)*100,0),
            pct_win_def_off = round(max(win_def_off/appear)*100,0)) %>% View()
  write_excel_csv("all_antiviruses_ON_OFF.csv")
            








# Intervals with number of times RTP shows ON or OFF, along with number of devices
empty %>%
  na.omit() %>%
  mutate(active_trend_rtp = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         last_seen_from_execution = round(difftime(day,last_seen, units = "days"),0)) %>%
  arrange(-last_seen_from_execution) %>%
  filter(last_seen_from_execution <= 15) %>%
  group_by(`Intervals` = cut_width(last_seen_from_execution,3, boundary = 3)) %>%
  summarize(countn = n_distinct(name),
            time_showed_on = sum(active_trend_rtp, na.rm = T),
            time_showed_off = n() - time_showed_on)
  




  

### Percentage of the time trend appears On along with RTP RA status
empty %>%
  na.omit() %>%
  mutate(active_trend_rtp = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         last_seen_from_execution = round(difftime(day,last_seen, units = "days"),0)) %>%
  arrange(last_seen_from_execution) %>%
  filter(last_seen_from_execution <= 15) %>%
  select(name, active_trend_rtp, last_seen_from_execution) %>%
  group_by(name, last_seen_from_execution) %>%
  summarise(number_on = sum(active_trend_rtp, na.rm = T),
            number_off = n() - number_on) %>% 
  arrange(name,last_seen_from_execution) %>%
  ungroup() %>%
  group_by(name) %>%
  summarize(pct_time_on = (sum(number_on)/sum(number_on + number_off))*100 ) %>% 
  arrange(pct_time_on) %>%
  inner_join(df.all, by = c("name" = "Name")) %>% 
  View()

  
  



### Number of times Trend appears on and off, by intervals
empty %>%
  na.omit() %>%
  mutate(active_trend_rtp = ifelse(grepl("Trend Micro OfficeScan Antivirus (RTP: on", all_antiviruses, fixed = T), TRUE, FALSE),
         last_seen_from_execution = round(difftime(day,last_seen, units = "days"),0)) %>%
  arrange(last_seen_from_execution) %>%
  filter(last_seen_from_execution <= 15) %>%
  select(name, active_trend_rtp, last_seen_from_execution) %>%
  group_by(name, last_seen_from_execution) %>%
  summarise(number_on = sum(active_trend_rtp, na.rm = T),
            number_off = n() - number_on) %>% 
  ungroup() %>%
  group_by(`Intervals` = cut_width(last_seen_from_execution,3, boundary = 3)) %>%
  summarise(off = round(mean(number_off),0),
            on = round(mean(number_on),0)) %>%
  View()
  
  





  

  