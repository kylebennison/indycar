library(indycar)
library(ggplot2)
library(dplyr)
library(statRdaysCFB)

df <- load_data("https://racetools.com/logfiles/IndyCar/2023/Gallagher%20Grand%20Prix(Indianapolis%20Motor%20Speedway%20RC)-Qualifications%20-%20Segment%203%20_%20Firestone%20Fast%206_(Q4.I)_2023-08-11.zip")
df <- preprocess_laps(df)
df %>% plot_laps_boxplot()
df %>% plot_sector_quickest_heatmap()

