rm(list = ls())
library(data.table)
library(tidyverse)
library(lubridate)

# Latest Elo ratings per Drew
data <- fread("https://raw.githubusercontent.com/drewbennison/thesingleseater/master/datasets/elo_ratings/elo_tracker.csv")

# Latest driver costs from Indycar
driver_costs_current <- tribble(
  ~driver, ~cost,
  "Scott Dixon", 32,
  "Josef Newgarden", 31,
  "Will Power", 30,
  "Patricio O'Ward", 29,
  "Colton Herta", 28,
  "Alex Palou", 28,
  "Simon Pagenaud", 27,
  "Graham Rahal", 26,
  "Alexander Rossi", 26,
  "Takuma Sato", 26,
  "Marcus Ericsson", 25,
  "Felix Rosenqvist", 25,
  "Sebastien Bourdais", 24,
  "Jack Harvey", 24,
  "James Hinchcliffe", 24,
  "Ryan Hunter-Reay", 24,
  "Rinus VeeKay", 24,
  "Conor Daly", 22,
  "Max Chilton", 20,
  "Romain Grosjean", 20,
  "Ed Jones", 20,
  "Charlie Kimball", 20,
  "Jimmie Johnson", 18,
  "Dalton Kellett", 18
)

# Most recent elo rating for each driver
elo_max <- data %>% group_by(driver) %>% filter(date == max(date))

# Costs of active drivers this weekend
race_weekend <- elo_max %>% left_join(driver_costs_current, by = "driver") %>% 
  filter(is.na(cost) == F) %>% 
  ungroup()

# Check if any drivers are missing
anti_join(driver_costs_current, race_weekend)

# Optimize Lineup ---------------------------------------------------------
dollars <- 100
team_master <- tibble(driver1 = "name", elo1 = 1500, cost1 = 25, driver_slot = 1, team = 0)

for (run_num in 1:10000){

  if(run_num %% 10 == 0){
    message("run ", run_num)}
  
  for(driver_slot in 1:4){
    dollars_remaining <- 100
    driver1 <- race_weekend %>% arrange(desc(EloRating)) %>% slice_sample() %>% pull(driver)
    elo1 <- race_weekend %>% arrange(desc(EloRating)) %>% filter(driver == driver1) %>% pull(EloRating)
    cost1 <- race_weekend %>% arrange(desc(EloRating)) %>% filter(driver == driver1) %>% pull(cost)
    driver_slot <- driver_slot
    team <- run_num
    
    team_tbl <- tibble(driver1 = driver1,
                        elo1 = elo1,
                        cost1 = cost1,
                        driver_slot = driver_slot,
                        team = team)
    
    team_master <- rbind(team_master, team_tbl)
  
}
}

# Filter out teams that got a driver multiple times
rows_to_use <- team_master %>% count(team, driver1) %>% filter(n == 1) %>% group_by(team) %>% 
  summarise(sum_drivers = sum(n)) %>% 
  filter(sum_drivers == 4)

# Find top teams
team_master %>% filter(team %in% rows_to_use$team) %>%  group_by(team) %>% 
  summarise(team_total_cost = sum(cost1), team_total_elo = sum(elo1)) %>% 
  filter(team_total_cost <= 100) %>% 
  arrange(desc(team_total_elo))

# Check team out
team_master %>% filter(team == 6444)

# Total combinations
23*22*21*20
