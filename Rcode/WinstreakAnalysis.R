#Code by Peyton Barger
library(readr)
library(tidyverse)
library(dplyr)

# Create player-games dataframe
x2013 <- read_csv("jan_2013-jan_2014.csv")

unix2013 <- x2013 %>% transform(date_time = paste(Date, " ", Time, " UTC"))
unix2013 <- unix2013 %>% transform(unix = as.numeric(as.POSIXct(date_time, format = "%Y.%m.%d %H:%M:%S")))

player_games_white <- unix2013 %>% 
    transmute(Player = White, Win = if_else(Result == "1-0",1,0), Time = unix, GameID = GameID, color = "White", eloDiff = WhiteElo-BlackElo, elo = WhiteElo)
player_games_black <- unix2013 %>% 
  transmute(Player = Black, Win = if_else(Result == "0-1",1,0), Time = unix, GameID = GameID, color = "Black", eloDiff = BlackElo-WhiteElo, elo = BlackElo)
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

model <- glm(Win~eloDiff, data = game_winstreaks, family = "binomial")
summary(model)

game_winstreaks <- game_winstreaks %>% transform(win_prob = predict(model, newdata = game_winstreaks, type = "response"))
game_winstreaks <- na.omit(game_winstreaks)

# winstreak_summaries <- data.frame(matrix(nrow = 0, ncol = 14))
# colnames(winstreak_summaries) <- c("player", "exp_win", "actual_win", "exp_win_0", "actual_win_0", "exp_win_1", "actual_win_1", "exp_win_2", "actual_win_2", "exp_win_3", "actual_win_3", "exp_win_4", "actual_win_4", "elo")
# for(i in 1:nrow(players)){
#   player_winstreaks <- game_winstreaks %>% filter(Player == players$player[i])
#   winstreak_summary <- player_winstreaks %>% group_by(winstreak) %>% summarise(exp_win = sum(win_prob), actual_win = sum(Win))
#   winstreak_summaries_row <- data.frame(matrix(nrow = 1, ncol = 14))
#   colnames(winstreak_summaries_row) <- c("player", "exp_win", "actual_win", "exp_win_0", "actual_win_0", "exp_win_1", "actual_win_1", "exp_win_2", "actual_win_2", "exp_win_3", "actual_win_3", "exp_win_4", "actual_win_4", "elo")
#   winstreak_summaries_row[1,"player"] <- players$player[i]
#   winstreak_summaries_row[1,"exp_win"] <- sum(player_winstreaks$win_prob)
#   winstreak_summaries_row[1,"actual_win"] <- sum(player_winstreaks$Win)
#   winstreak_summaries_row[1,"exp_win_0"] <- (winstreak_summary %>% filter(winstreak == 0) %>% summarise(sum(exp_win)))
#   winstreak_summaries_row[1,"actual_win_0"] <- (winstreak_summary %>% filter(winstreak == 0) %>% summarise(sum(actual_win)))
#   winstreak_summaries_row[1,"exp_win_1"] <- (winstreak_summary %>% filter(winstreak == 1) %>% summarise(sum(exp_win)))
#   winstreak_summaries_row[1,"actual_win_1"] <- (winstreak_summary %>% filter(winstreak == 1) %>% summarise(sum(actual_win)))
#   winstreak_summaries_row[1,"exp_win_2"] <- (winstreak_summary %>% filter(winstreak == 2) %>% summarise(sum(exp_win)))
#   winstreak_summaries_row[1,"actual_win_2"] <- (winstreak_summary %>% filter(winstreak == 2) %>% summarise(sum(actual_win)))
#   winstreak_summaries_row[1,"exp_win_3"] <- (winstreak_summary %>% filter(winstreak == 3) %>% summarise(sum(exp_win)))
#   winstreak_summaries_row[1,"actual_win_3"] <- (winstreak_summary %>% filter(winstreak == 3) %>% summarise(sum(actual_win)))
#   winstreak_summaries_row[1,"exp_win_4"] <- (winstreak_summary %>% filter(winstreak == 4) %>% summarise(sum(exp_win)))
#   winstreak_summaries_row[1,"actual_win_4"] <- (winstreak_summary %>% filter(winstreak == 4) %>% summarise(sum(actual_win)))
#   winstreak_summaries_row[1,"elo"] <- mean(player_winstreaks$elo)
#   winstreak_summaries <- rbind(winstreak_summaries,winstreak_summaries_row)
# }

winstreak_summaries <- game_winstreaks %>% group_by(winstreak) %>% summarise(exp_win = sum(win_prob), actual_win = sum(Win)) %>% filter(winstreak<6)

ggplot(data = winstreak_summaries) + geom_point(aes(x = winstreak, y = exp_win), color = "blue", alpha = 0.5) + geom_point(aes(x = winstreak, y = actual_win), color = "red", alpha = 0.5)
ggplot(data = winstreak_summaries) + geom_point(aes(x = winstreak, y = actual_win/exp_win), color = "blue", alpha = 1)

winstreak_summaries2 <- game_winstreaks %>% filter(elo<1300) %>% group_by(winstreak) %>% summarise(exp_win = sum(win_prob), actual_win = sum(Win)) %>% filter(winstreak<6)

winstreak_summaries3 <- game_winstreaks %>% filter(elo>1700) %>% group_by(winstreak) %>% summarise(exp_win = sum(win_prob), actual_win = sum(Win)) %>% filter(winstreak<6)
colors <- c("All games" = "blue", "Low level games (<1300 Elo)" = "red", "High level games (>1700 Elo)" = "orange")

ggplot() + geom_point(data = winstreak_summaries, aes(x = winstreak, y = (actual_win/exp_win)*100-100, color = "All games"), alpha = 1)  + 
  geom_point(data = winstreak_summaries2, aes(x = winstreak, y = (actual_win/exp_win)*100-100, , color = "Low level games (<1300 Elo)"), alpha = 1) + 
  ylim(-5,30) + 
  geom_point(data = winstreak_summaries3, aes(x = winstreak, y = (actual_win/exp_win)*100-100 , color = "High level games (>1700 Elo)"), alpha = 1)   + 
  labs(x = "Winstreak", 
       y = "% Difference In Expected Win Rate", 
       color = "Legend",
       title = "Winstreak impact on win rates",
       subtitle = "and how it affects different ratings") + 
  scale_color_manual(values = colors)
                                                                                             
