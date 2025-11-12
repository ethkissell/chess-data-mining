library(readr)
library(tidyverse)
library(dplyr)

# Create player-games dataframe
x2013 <- read_csv("RStudio/Chess/jan_2013-jan_2014.csv")

unix2013 <- x2013 %>% transform(date_time = paste(Date, " ", Time, " UTC"))
unix2013 <- unix2013 %>% transform(unix = as.numeric(as.POSIXct(date_time, format = "%Y.%m.%d %H:%M:%S")))

player_games_white <- unix2013 %>% 
    transmute(Player = White, Win = if_else(Result == "0-1",1,0), Time = unix, GameID = GameID)
player_games_black <- unix2013 %>% 
  transmute(Player = Black, Win = if_else(Result == "0-1",1,0), Time = unix, GameID = GameID)
player_games <- rbind(player_games_white, player_games_black)

player_games <- player_games[order(player_games$Time),]

# get a list of players
players <- data.frame(Player = unique(player_games$Player)) %>% transform(winstreak = 0)
players$ID <- seq.int(nrow(players))

player_games <- inner_join(player_games, players, by = join_by(Player))

# count the winstreaks

for(i in 1:nrow(player_games)) {
  index <- player_games[i,"ID"]
  player_games[i,"winstreak"] <- players[index, "winstreak"]
  if (player_games[i,"Win"] == 1){
    players[index, "winstreak"] <- players[index, "winstreak"]+1
  } else
  {
    players[index, "winstreak"] <- 0
  }
}

player_games <- player_games %>% group_by(Player)%>%
  summarise(num_games = n()) %>% filter(num_games > 30)

player_games <- player_games[1:i,]
player_games <- player_games %>% transform(wins = if_else(Win==1, "Win", "Loss"))
ggplot(data = player_games, aes(x = winstreak, fill = wins)) + geom_bar(position = "fill", binwidth = 1, alpha = 0.5) + xlim(-1,20)


