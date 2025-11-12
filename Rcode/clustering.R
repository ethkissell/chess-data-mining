library(fpc)
library(readr)
library(tidyverse)
library(dplyr)


#data preprocessing
x2013 <- read_csv("jan_2013-jan_2014.csv")
x2013_white <- x2013 %>% transmute(player = White, Date = Date, Time = Time, GameID = GameID, color = "white", Timer = as.numeric(sub("([0,9]+).*$", replacement = "\\1",TimeControl)), increment = as.numeric(sub(".*\\+([0-9]+)", replacement = "\\1",TimeControl)), Event, Elo = WhiteElo)
x2013_black <- x2013 %>% transmute(player = Black, Date = Date, Time = Time, GameID = GameID, color = "black", Timer = as.numeric(sub("([0,9]+).*$", replacement = "\\1",TimeControl)), increment = as.numeric(sub(".*\\+([0-9]+)", replacement = "\\1",TimeControl)), Event, Elo = BlackElo)
x2013_player_games <- rbind(x2013_white,x2013_black)
x2013_player_games$Timer[is.na(x2013_player_games$Timer)] <- 120
x2013_player_games$increment[is.na(x2013_player_games$increment)] <- 0
x2013_player_games <- x2013_player_games %>% filter(Event %in% c("Rated Classical game", "Rated Bullet game", "Rated Blitz game"))

#sample players
players <- read_csv("players.csv")
set.seed(303)
rows <- sample(nrow(players), 2000)
player_sample <- players[rows,]
x2013_sample_players <- x2013_player_games %>% filter(player %in% player_sample$`Player name`)
x2013_sample_players <- x2013_sample_players %>% transform(date_time = paste(Date, " ", Time, " UTC"))
x2013_sample_players <- x2013_sample_players %>% transform(unix = as.numeric(as.POSIXct(date_time, format = "%Y.%m.%d %H:%M:%S"))) %>% transform(est_end = unix + as.numeric(2*Timer + (20* increment)))
x2013_sample_players <- na.omit(x2013_sample_players)
player_sessions <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(player_sessions) <- c("session_no", "start", "end", "diff", "count", "player")

for(i in 1:nrow(player_sample)){
  games_played <- x2013_sample_players %>% filter(player == player_sample$`Player name`[i])
  if(nrow(games_played) == 0){
    next
    }
  Dbscan_cl<- dbscan(games_played$unix, eps = 7200, MinPts = 1)
  categories <- as.list(Dbscan_cl[["cluster"]])
  games_played$session_no <- categories
  
  games_played <- games_played %>% 
    group_by(session_no) %>% 
    summarise(start = min(unix), end = max(est_end), diff = end-start, count = n(), 
      nBullet = sum(Event == "Rated Bullet game"), nBlitz = sum(Event == "Rated Blitz game"), nClassical = sum(Event == "Rated Classical game"),
      bulletElo = sum((Event == "Rated Bullet game")*Elo)/nBullet, blitzElo = sum((Event == "Rated Blitz game")*Elo)/nBlitz, classicalElo = sum((Event == "Rated Classical game")*Elo)/nClassical)
  games_played$player = player_sample$`Player name`[i]
  player_sessions <- rbind(player_sessions, games_played)
}

#Time series of ratings
blitzElos <- player_sessions %>% filter(!is.na(blitzElo))
top_elo_sessions <- blitzElos %>% group_by(player) %>% summarise(count = n()) %>% top_n(5, count)
top_player_sessions <- blitzElos %>% filter(player %in% top_elo_sessions$player)
ggplot(data = top_player_sessions, aes(x = (start-1357023600)/86400, y = blitzElo, color = player)) + geom_line() + theme(legend.position = "none") + labs(x = "Days since Jan 1, 2013" , y = "Blitz Rating")

#session summaries
blitzSummaries <- blitzElos %>% group_by(player) %>%
  summarise(sessions = n(), avg_length = mean(diff), avg_game_count = mean(nBlitz))
session_summaries <- player_sessions %>% group_by(player) %>%
  summarise(sessions = n(), avg_length = mean(diff), avg_game_count = mean(count))

blitzSummaries2 <- data.frame(matrix(nrow = nrow(blitzSummaries), ncol = 2))
colnames(blitzSummaries2) <- c("startElo", "endElo")
for(i in 1:nrow(blitzSummaries)){
  player_data <- blitzElos %>% filter(player == blitzSummaries$player[i])
  blitzSummaries2[i,1] <- (player_data %>% filter(session_no == min(unlist(player_data$session_no))))[["blitzElo"]]
  blitzSummaries2[i,2] <- (player_data %>% filter(session_no == max(unlist(player_data$session_no))))[["blitzElo"]]
}

blitzSummaries <- cbind(blitzSummaries, blitzSummaries2)

ggplot(data = blitzSummaries, aes(x = sessions, y = endElo)) + geom_point() + geom_hline(yintercept = 1500) + geom_smooth(method = "lm")
ggplot(data = blitzSummaries, aes(x = sessions*avg_game_count, y = endElo)) + geom_point() + geom_hline(yintercept = 1500) + geom_smooth(method = "lm")

#Change in Elo plots
ggplot(data = blitzSummaries, aes(x = sessions, y = endElo-startElo)) + geom_point() + geom_hline(yintercept = 0)
ggplot(data = blitzSummaries, aes(x = sessions*avg_game_count, y = endElo-startElo)) + geom_point() + geom_hline(yintercept = 0)

ggplot(data = blitzSummaries, aes(x = sessions, y = sessions* avg_game_count)) + geom_point()

player_frequencies <- session_summaries %>% transform(frequency = if_else(sessions >= 300,"frequent",if_else(sessions<=100,"infrequent","moderate")))
frequent_players <- player_frequencies%>% filter(frequency =="frequent")
moderately_frequent_players <- player_frequencies%>% filter(frequency =="moderate")
infrequent_players <- player_frequencies%>% filter(frequency =="infrequent")

ggplot(data = blitzSummaries, aes(x = sessions, y = avg_game_count)) + geom_point() + geom_hline(yintercept = 0)

#plot how many games per session groups of players play
ggplot(data = infrequent_players, aes(x = avg_game_count)) + geom_histogram() +xlim(0,30)
quantile(infrequent_players$avg_game_count, .5)
mean(infrequent_players$avg_game_count)


ggplot(data = moderately_frequent_players, aes(x = avg_game_count)) + geom_histogram() +xlim(0,30)
quantile(moderately_frequent_players$avg_game_count, .5)
mean(moderately_frequent_players$avg_game_count)


ggplot(data = frequent_players, aes(x = avg_game_count)) + geom_histogram() +xlim(0,30) + geom_vline(xintercept = quantile(frequent_players$avg_game_count, .5))
quantile(frequent_players$avg_game_count, .5)
mean(frequent_players$avg_game_count)


ggplot(data = blitzSummaries, aes(x = sessions, y = sessions*avg_game_count)) + geom_point() + geom_hline(yintercept = 0)

#plot how long each session is for different groups

ggplot(data = infrequent_players, aes(x = avg_length/60)) + geom_histogram(binwidth = 20) +xlim(0,250)  + labs(x = "Session Length (minutes)", title = "Play Times of Infrequent Players", subtitle = "Players with less than 100 sessions over the course of a year")
ggplot(data = moderately_frequent_players, aes(x = avg_length/60)) + geom_histogram(binwidth = 20) +xlim(0,250)  + labs(x = "Session Length (minutes)", title = "Play Times of Moderately Frequent Players", subtitle = "Players with 100-300 sessions over the course of a year")
ggplot(data = frequent_players, aes(x = avg_length/60)) + geom_histogram(binwidth = 20) +xlim(0,250) + labs(x = "Session Length (minutes)", title = "Play Times of Frequent Players", subtitle = "Players with over 300 sessions over the course of a year")

for(i in 1:60){
  filtered <- session_summaries %>% filter(sessions>= as.numeric(i*10))
  plot <- ggplot(data = filtered, aes(x = avg_length/60)) + geom_histogram(binwidth = 20) +xlim(0,250)  + labs(x = "Session Length (minutes)", title = "Length of Player sessions", subtitle = paste0("Of players with at least ", i*10, " sessions"))
  ggsave(filename = paste0("plot", i, ".png"), width = 1600, height = 1200, units = "px", path = ("animation"))
}

for(i in 1:10){
  filtered <- session_summaries %>% filter(sessions>= as.numeric(i))
  plot <- ggplot(data = filtered, aes(x = avg_length/60)) + geom_histogram(binwidth = 20) +xlim(0,250)  + labs(x = "Session Length (minutes)", title = "Length of Player sessions", subtitle = paste0("Of players with at least ", i, " sessions"))
  ggsave(filename = paste0("plot", i, ".png"), width = 1600, height = 1200, units = "px", path = ("animation2"))
}

ggplot(data = frequent_players, aes(x = avg_length/60, y=endElo)) + geom_point() + geom_hline(yintercept = mean(frequent_players$endElo)) + ylim(1250,2000)
ggplot(data = moderately_frequent_players, aes(x = avg_length/60, y=endElo)) + geom_point() + geom_hline(yintercept = mean(moderately_frequent_players$endElo)) + ylim(1250,2000)
ggplot(data = infrequent_players, aes(x = avg_length/60, y=endElo)) + geom_point() + geom_hline(yintercept = mean(infrequent_players$endElo)) + ylim(1250,2000)

#How tight are play schedules 
library(anytime)
frequent_player_sessions <- player_sessions %>% filter(player %in% frequent_players$player) %>%  transform(time = hour(anytime(start)))

cluster_rates <- data.frame(matrix(nrow = nrow(frequent_players), ncol = 1))
colnames(cluster_rates) <- c("cluster_rate")
for(i in 1:nrow(frequent_players)){
  single_player_sessions <- frequent_player_sessions %>% filter(player == frequent_players$player[i])
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates[i,1] <- mean(clusters!=0)
}
ggplot(data = cluster_rates, aes(x = cluster_rate)) + geom_histogram()


moderate_player_sessions <- player_sessions %>% filter(player %in% moderately_frequent_players$player) %>%  transform(time = hour(anytime(start)))

cluster_rates2 <- data.frame(matrix(nrow = nrow(moderately_frequent_players), ncol = 1))
colnames(cluster_rates2) <- c("cluster_rate")
for(i in 1:nrow(moderately_frequent_players)){
  single_player_sessions <- moderate_player_sessions %>% filter(player == moderately_frequent_players$player[i])
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates2[i,1] <- mean(clusters!=0)
}
ggplot(data = cluster_rates2, aes(x = cluster_rate)) + geom_histogram()
