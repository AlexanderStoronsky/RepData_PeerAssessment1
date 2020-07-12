
library(dplyr)
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = temp)
data<-read.csv(temp,header = TRUE,stringsAsFactors = FALSE)
unlink(temp)
df<-tbl_df(data)
sum <- df %>% filter(FATALITIES!="0") %>% group_by(EVTYPE) %>%
  summarise(total_fatalities = sum(FATALITIES, na.rm = F)) %>%
  arrange(desc(total_fatalities))
sum <- sum %>% mutate(EVTYPE = gsub(".*HEAT.*", "HEAT", EVTYPE)) %>%
  mutate(EVTYPE = gsub(".*FLOOD.*", "FLOOD", EVTYPE)) %>%
  mutate(EVTYPE = gsub(".*WIND.*|.*STORM.*", "WIND", EVTYPE)) %>%
  mutate(EVTYPE = gsub(".*RIP.*", "RIP", EVTYPE))  %>%
  mutate(EVTYPE = gsub(".*COLD.*", "COLD", EVTYPE))  %>%
sum <- sum %>% group_by(EVTYPE) %>% filter(total_fatalities > 100) %>% 
  summarise(total_fatalities = sum(total_fatalities, na.rm = F)) %>%
  arrange(desc(total_fatalities))


#data <- data[,apply(data,2,function(x){sum(!is.na(x))>0})]
#data<-data[!apply(data == "", 1, all),]
data <- data[!apply(is.na(data) | data == "", 1, all),]
data <- data[,!apply(is.na(data) | data == "", 2, all)]
df<-tbl_df(data)
df<- (df %>% filter(V1!=""))
df<- df %>% rename(country_code = V1, rank = V2, country = V4,US_dollars=V5, comments = V6)
df<-df %>% mutate(US_dollars = gsub('\w*HEAT\w*', "", US_dollars))
