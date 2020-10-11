library(tidyverse)
library(formattable)

# We get data saved from script.R (less caluclation time)
df <- read_csv("./all_antiviruses_ON_OFF.csv")


# List of all devices and their RA RTP status
df.all <- read_delim("./all.csv", delim = "\t")




## Devices with trend ON and OFF
df %>%
  inner_join(df.all, by = c("name" = "Name")) %>%
  filter( pct_trend_off > 0) %>% View()
  write_excel_csv("trend_on_AND_off.csv")

## Devices with RTP ON <90% of time
df %>%
  inner_join(df.all, by = c("name" = "Name")) %>%
  select(name, pct_trend_on) %>%
  filter(pct_trend_on < 90) %>%
  rename(`% of time Trend RTP is ON` = pct_trend_on) %>%
  write_excel_csv("NRG_Trend_RTP_OFF_not100pct.csv")



## test with add_count
df %>%
  add_count(interv=cut_width(pct_trend_on, 10, boundary = 0)) %>%
  group_by(interv) %>%
  mutate(count_trend = max(n))
  
  
## Summary table (the one sent to NRG)
df %>%
  mutate(trend = sapply(.$pct_trend_on, funk),
         cylance = sapply(.$pct_cylance_on, funk),
         avast = sapply(.$pct_avast_on, funk),
         malware = sapply(.$pct_malwarebyte_on, funk),
         windef = sapply(.$pct_win_def_on, funk)) %>%
  select(name, trend, cylance, malware, windef) %>%
  gather(trend, cylance, malware, windef, key=antivirus, value=number) %>%
  group_by(number, antivirus) %>%
  summarise(countn = n()) %>%
  spread(antivirus, countn) %>%
  replace(is.na(.), 0) %>%
  rename(`% of time RTP is ON` = number, `# of devices Cylance` = cylance, `# of devices MalwareByte` = malware,
         `# of devices Trend` = trend, `# of devices Windows Defender` = windef) %>%
  formattable()
  

## Function needed to create the above table
funk <- function(x){
  if( x >= 0 & x <10){ return("0-10") }
  if( x >= 10 & x <20){ return("10-20") }
  if( x >= 20 & x <30){ return("20-30") }
  if( x >= 30 & x <40){ return("30-40") }
  if( x >= 40 & x <50){ return("40-50") }
  if( x >= 50 & x <60){ return("50-60") }
  if( x >= 60 & x <70){ return("60-70") }
  if( x >= 70 & x <80){ return("70-80") }
  if( x >= 80 & x <90){ return("80-90") }
  if( x >= 90){ return("90-100") }
}


  
## Same as summary table but case by case
df %>%
  add_count(cylance_ON=cut_width(pct_cylance_on, 10, boundary = 0)) %>%
  group_by(cylance_ON) %>%
  summarise(n = max(n))


df %>%
  add_count(avast_ON=cut_width(pct_avast_on, 10, boundary = 0)) %>%
  group_by(avast_ON) %>%
  summarise(n = max(n))


df %>%
  add_count(malw_ON=cut_width(pct_malwarebyte_on, 10, boundary = 0)) %>%
  group_by(malw_ON) %>%
  summarise(n = max(n))


df %>%
  add_count(win_def=cut_width(pct_win_def_on, 10, boundary = 0)) %>%
  group_by(win_def) %>%
  summarise(n = max(n))




  
