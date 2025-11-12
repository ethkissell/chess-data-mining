library(readr)
library(tidyverse)
library(dplyr)
players <- read_csv("RStudio/Chess/players.csv")
player_games <- read_csv("RStudio/Chess/player_games.csv")
Jan_2013 <- read_csv("RStudio/Chess/2013_Jan.csv")
x2013 <- read_csv("RStudio/Chess/jan_2013-jan_2014.csv")

#count the number of games each player has played
games <- player_games %>% 
  group_by(`Player ID`) %>%
  summarise(num_games = n())

ggplot(data = games, aes(x = num_games)) + geom_histogram() +labs(x = "games played")

# Attach player names
games <- inner_join(x = games, y = players)

#Filter players that play more than 5 games
regulars <- games %>% filter(num_games > 5)

#Average Elo ratings
eloWhite <- Jan_2013 %>% 
  select(White, WhiteElo)
eloWhite[, 2] <- apply(eloWhite[, 2], 2, function(x) as.numeric(as.character(x)))

eloWhite <-eloWhite%>% 
  group_by(White) %>%
  summarise(num_games_white = n(), avg_elo = mean(WhiteElo))

eloBlack <- Jan_2013 %>% 
  select(Black, BlackElo)
eloBlack[, 2] <- apply(eloBlack[, 2], 2, function(x) as.numeric(as.character(x)))

eloBlack <-eloBlack%>% 
  group_by(Black) %>%
  summarise(num_games_black = n(), avg_elo = mean(BlackElo))

elo <- full_join(eloBlack,eloWhite, join_by(Black == White))
elo[is.na(elo)] <- 0

elo <- elo %>% transmute(player = Black, avg = ((num_games_black*avg_elo.x)+(num_games_white*avg_elo.y))/(num_games_black+num_games_white))


ggplot(data = elo, aes(x = avg)) + geom_histogram() +labs(x = "Average elo")

#combine Elo and games played into one table
elo_games <- full_join(elo, games, join_by(player == 'Player name'))

ggplot(data = elo_games, aes(x = num_games, y = avg)) + geom_point(alpha = 0.2) +labs(x = "Games Played", y = "Elo") + geom_smooth(method = 'lm', se = FALSE) + geom_hline(yintercept = mean(elo$avg), color = 'red')

elo_games_regulars <- elo_games %>% filter(player %in% regulars$'Player name')
ggplot(data = elo_games_regulars, aes(x = num_games, y = avg)) + geom_point(alpha = 0.2) +labs(x = "Games Played", y = "Elo") + geom_smooth(method = 'lm', se = FALSE) + geom_hline(yintercept = mean(elo_games_regulars$avg), color = 'red')

#get elo difference
Jan_2013[,8] <- apply(Jan_2013[, 8], 2, function(x) as.numeric(as.character(x)))
Jan_2013[,9] <- apply(Jan_2013[, 9], 2, function(x) as.numeric(as.character(x)))

elo_diffs <- Jan_2013 %>% 
  transform(elo_diff = WhiteElo-BlackElo) %>% 
  filter(Result != "1/2-1/2") %>%
  transmute(Result = ifelse(Result == "1-0", 1, 0), elo_diff = elo_diff)

ggplot(data = elo_diffs, aes(x = elo_diff, y = Result)) + geom_jitter(alpha = 0.02, height = 0.2) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + geom_hline(yintercept = .5)
# glm(result ~ elo_diffs, )
#get the number of seconds a player has stuck with playing
x2013_jan_players <- x2013 %>% filter(White %in% players$`Player name` | Black %in% players$`Player name`)
x2013_jan_players <- x2013_jan_players %>% transform(date_time = paste(Date, " ", Time, " UTC"))
x2013_jan_players <- x2013_jan_players %>% transform(unix = as.numeric(as.POSIXct(date_time, format = "%Y.%m.%d %H:%M:%S")))



first_last_white <- x2013_jan_players %>% group_by(White) %>% summarise(latest = max(unix), earliest = min(unix), num_games = n(),diff = latest-earliest) %>% transform(player = White) %>% select(player, latest, earliest, diff, num_games)
first_last_black <- x2013_jan_players %>% group_by(Black) %>% summarise(latest = max(unix), earliest = min(unix), num_games = n(),diff = latest-earliest)  %>% transform(player = Black) %>% select(player, latest, earliest, diff, num_games)
play_length <- rbind(first_last_white, first_last_black) %>%group_by(player) %>% summarise(latest = max(latest), earliest = min(earliest), diff = latest-earliest, tot_num_games = sum(num_games))
play_length <- play_length%>% filter(player %in% players$`Player name`) %>% filter(diff > 0)
ggplot(data = play_length, aes(x = diff)) + geom_histogram()

play_length <- left_join(play_length, games, by = join_by('player'=='Player name'))
play_length <- play_length %>% transform(days_played = diff/86400)
ggplot(data = play_length, aes(x = days_played, y = tot_num_games)) + geom_point(alpha = 0.3) + geom_hline(yintercept = 1060.9)
quantile(play_length$tot_num_games, prob = .5)
year <- play_length %>% filter(days_played > 330 & tot_num_games > 1060.9) %>% summarise(n())
stoppers <- play_length %>% filter(days_played <= 330 & tot_num_games > 1060.9) %>% summarise(n())
year/(year + stoppers)
#over 81.6% of players that played throughout the duration of a year were in the top 10% of players in games played
