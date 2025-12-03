#Code by Peyton Barger
library(readr)
library(tidyverse)
library(dplyr)

# Create player-games dataframe
x2013 <- read_csv("jan_2013-jan_2014.csv")

unix2013 <- x2013 %>% transform(date_time = paste(Date, " ", Time, " UTC"))
unix2013 <- unix2013 %>% transform(unix = as.numeric(as.POSIXct(date_time, format = "%Y.%m.%d %H:%M:%S")))

player_games_white <- unix2013 %>% 
    transmute(Player = White, Win = if_else(Result == "0-1",1,0), Time = unix, GameID = GameID, color = "White", eloDiff = WhiteElo-BlackElo)
player_games_black <- unix2013 %>% 
  transmute(Player = Black, Win = if_else(Result == "0-1",1,0), Time = unix, GameID = GameID, color = "Black", eloDiff = BlackElo-WhiteElo)
player_games <- rbind(player_games_white, player_games_black)

player_games <- player_games[order(player_games$Time),]

# get a list of players
player_summaries <- read_csv("player_summaries_clustering.csv")
players <- player_summaries %>% dplyr::select(player)
players <- players %>% transform(winstreak = 0)

player_games <- inner_join(player_games, players, by = join_by(Player == player))
player_games <- player_games %>% transform(winstreak = 0)
# count the winstreaks

game_winstreaks <- data.frame(matrix(ncol = ncol(player_games), nrow = 0))
colnames(game_winstreaks) <- colnames(player_games)

for(i in 1:nrow(players)) {
  individual_player_games <- player_games %>% filter(Player == players[i,"player"])
  winstreak <- 0
  for(j in 1:nrow(individual_player_games)){
    individual_player_games[j, "winstreak"] <- winstreak
    winstreak <- individual_player_games[j, "Win"]*(winstreak + 1)
  }
  game_winstreaks <- rbind(game_winstreaks,individual_player_games)
}


