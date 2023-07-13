library(jsonlite)
library(tidyverse)
library(data.table)
library(lubridate)

master_dt <- tibble()

# difftime(now(), lubridate::as_datetime("2021-04-26 12:42:00 EDT", tz = "US/Eastern"), units = "secs")

#wait until race time
#Sys.sleep(7260)

#two hours
for(i in c(1:480)) {
  message(i)
tmp <- "C:/Users/Kyle/Documents/Kyle/Personal/Data/Indycar/temp_test_indy500"
url <- "http://racecontrol.indycar.com/xml/timingscoring.json"
download.file(url, destfile = tmp,quiet = FALSE, mode = "wb")

line = readLines(tmp)
line = line[2]

write(line, "C:/Users/Kyle/Documents/Kyle/Personal/Data/Indycar/test_indy500.json")

wb <- jsonlite::fromJSON("C:/Users/Kyle/Documents/Kyle/Personal/Data/Indycar/test_indy500.json")

new_dt <- wb[["timing_results"]][["Item"]]
current_time <- Sys.time()
new_dt$time_stamp <- current_time

master_dt <- rbindlist(list(master_dt, new_dt), use.names = TRUE, fill = TRUE)
fwrite(master_dt, "C:/Users/Kyle/Documents/Kyle/Personal/Data/Indycar/master_dt_practice_indy500.csv")
Sys.sleep(30)
}



#analysis
num_laps_at_each_point <- master_dt %>% group_by(lastName, laps) %>% 
  count()

master_dt %>% unique() %>% filter(lastName %in% c("Palou", "Power")) %>% 
  mutate(gap = as.numeric(gap),
         laps = as.numeric(laps)) %>% 
  filter(gap > 0) %>% 
  select(laps, gap) %>% unique() %>% 
  ggplot(aes(x=laps, y=gap)) + geom_line() +
  labs(x="Lap", y="Gap to leader", title = "Will Power seconds behind Alex Palou") +
  ylim(0,2.5) +
  theme_bw()
