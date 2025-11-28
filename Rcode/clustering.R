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

#cluster players' play sessions
  #Initialize data frame
player_sessions <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(player_sessions) <- c("session_no", "start", "end", "diff", "count", "player")

#Iterate through each player
for(i in 1:nrow(player_sample)){
  #Get df with one player
  games_played <- x2013_sample_players %>% filter(player == player_sample$`Player name`[i])
  if(nrow(games_played) == 0){
    next
  }
  
  #cluster with epsilon of 2 hours
  Dbscan_cl<- dbscan(games_played$unix, eps = 7200, MinPts = 1)
  categories <- as.list(Dbscan_cl[["cluster"]])
  games_played$session_no <- categories
  
  #Organize by game type
  games_played <- games_played %>% 
    group_by(session_no) %>% 
    summarise(start = min(unix), end = max(est_end), diff = end-start, count = n(), 
      nBullet = sum(Event == "Rated Bullet game"), nBlitz = sum(Event == "Rated Blitz game"), nClassical = sum(Event == "Rated Classical game"),
      bulletElo = sum((Event == "Rated Bullet game")*Elo)/nBullet, blitzElo = sum((Event == "Rated Blitz game")*Elo)/nBlitz, classicalElo = sum((Event == "Rated Classical game")*Elo)/nClassical)
  games_played$player = player_sample$`Player name`[i]
  
  #Add player data to dataframe
  player_sessions <- rbind(player_sessions, games_played)
}

#Time series of ratings
blitzElos <- player_sessions %>% filter(!is.na(blitzElo))
bulletElos <- player_sessions %>% filter(!is.na(bulletElo))
classicalElos <- player_sessions %>% filter(!is.na(classicalElo))

top_elo_sessions <- blitzElos %>% group_by(player) %>% summarise(count = n()) %>% top_n(5, count)
top_player_sessions <- blitzElos %>% filter(player %in% top_elo_sessions$player)
ggplot(data = top_player_sessions, aes(x = (start-1357023600)/86400, y = blitzElo, color = player)) + geom_line() + theme(legend.position = "none") + labs(x = "Days since Jan 1, 2013" , y = "Blitz Rating")

#session summaries
blitzSummaries <- blitzElos %>% group_by(player) %>%
  summarise(blitz_sessions = n(), avg_blitz_count = mean(nBlitz))
bulletSummaries <- bulletElos %>% group_by(player) %>%
  summarise(bullet_sessions = n(), avg_bullet_count = mean(nBullet))
classicalSummaries <- classicalElos %>% group_by(player) %>%
  summarise(classical_sessions = n(), avg_classical_count = mean(nClassical))

session_summaries <- player_sessions %>% group_by(player) %>%
  summarise(sessions = n(), avg_length = mean(diff), avg_game_count = mean(count))

blitzSummaries2 <- data.frame(matrix(nrow = nrow(blitzSummaries), ncol = 2))
colnames(blitzSummaries2) <- c("start_Elo_blitz", "end_Elo_blitz")
bulletSummaries2 <- data.frame(matrix(nrow = nrow(bulletSummaries), ncol = 2))
colnames(bulletSummaries2) <- c("start_Elo_bullet", "end_Elo_bullet")
classicalSummaries2 <- data.frame(matrix(nrow = nrow(classicalSummaries), ncol = 2))
colnames(classicalSummaries2) <- c("start_Elo_classical", "end_Elo_classical")

for(i in 1:nrow(blitzSummaries)){
  #get first and last session data
  player_data <- blitzElos %>% filter(player == blitzSummaries$player[i])
  blitzSummaries2[i,1] <- (player_data %>% filter(session_no == min(unlist(player_data$session_no))))[["blitzElo"]]
  blitzSummaries2[i,2] <- (player_data %>% filter(session_no == max(unlist(player_data$session_no))))[["blitzElo"]]
}
for(i in 1:nrow(bulletSummaries)){
  player_data <- bulletElos %>% filter(player == bulletSummaries$player[i])
  bulletSummaries2[i,1] <- (player_data %>% filter(session_no == min(unlist(player_data$session_no))))[["bulletElo"]]
  bulletSummaries2[i,2] <- (player_data %>% filter(session_no == max(unlist(player_data$session_no))))[["bulletElo"]]
}
for(i in 1:nrow(classicalSummaries)){
  player_data <- classicalElos %>% filter(player == classicalSummaries$player[i])
  classicalSummaries2[i,1] <- (player_data %>% filter(session_no == min(unlist(player_data$session_no))))[["classicalElo"]]
  classicalSummaries2[i,2] <- (player_data %>% filter(session_no == max(unlist(player_data$session_no))))[["classicalElo"]]
}
blitzSummaries <- cbind(blitzSummaries, blitzSummaries2) %>% transform(blitz_games = blitz_sessions*avg_blitz_count)
classicalSummaries <- cbind(classicalSummaries, classicalSummaries2) %>% transform(classical_games = classical_sessions*avg_classical_count)
bulletSummaries <- cbind(bulletSummaries, bulletSummaries2) %>% transform(bullet_games = bullet_sessions*avg_bullet_count)




ggplot(data = blitzSummaries, aes(x = blitz_sessions, y = end_Elo_blitz)) + geom_point() + geom_hline(yintercept = 1500) + geom_smooth(method = "lm") + labs(title = "Elo Rating vs Session count", x = "Sessions", y = "Elo")
ggplot(data = blitzSummaries, aes(x = blitz_sessions*avg_blitz_count, y = end_Elo_blitz)) + geom_point() + geom_hline(yintercept = 1500) + geom_smooth(method = "lm") + labs(title = "Elo Rating vs Game count", x = "Games", y = "Elo")
ggplot(data = blitzSummaries, aes(x = avg_blitz_count, y = end_Elo_blitz)) + geom_point() + geom_hline(yintercept = 1500) + geom_smooth(method = "lm")
lm(end_Elo_blitz ~ blitz_sessions, data = blitzSummaries)
lm(end_Elo_blitz ~ (blitz_games), data = blitzSummaries)
lm(end_Elo_blitz ~ blitz_sessions*avg_blitz_count, data = blitzSummaries)

blitz_in_range <- blitzSummaries %>% filter(blitz_sessions <= 100 & blitz_sessions >= 50)
blitz_in_range2 <- blitzSummaries %>% filter(blitz_sessions <= 350 & blitz_sessions >= 300)

ggplot(data = blitz_in_range2, aes(x = avg_blitz_count, y = end_Elo_blitz-start_Elo_blitz)) + geom_point() + geom_smooth(method = "lm") +xlim(0,10) + labs(x = "Games Played", y = "Elo")
ggplot(data = blitz_in_range, aes(x = avg_blitz_count, y = end_Elo_blitz-start_Elo_blitz)) + geom_point() + geom_smooth(method = "lm") +xlim(0,10) + labs(x = "Games Played", y = "Elo")


#Change in Elo plots
ggplot(data = blitzSummaries, aes(x = blitz_sessions, y = end_Elo_blitz-start_Elo_blitz)) + geom_point() + geom_hline(yintercept = 0)
ggplot(data = blitzSummaries, aes(x = blitz_sessions*avg_blitz_count, y = end_Elo_blitz-start_Elo_blitz)) + geom_point() + geom_hline(yintercept = 0)
ggplot(data = blitzSummaries, aes(x = blitz_sessions, y = blitz_sessions* avg_blitz_count)) + geom_point()

player_frequencies <- session_summaries %>% transform(frequency = if_else(sessions >= 300,"frequent",if_else(sessions<=100,"infrequent","moderate")))
frequent_players <- player_frequencies%>% filter(frequency =="frequent")
moderately_frequent_players <- player_frequencies%>% filter(frequency =="moderate")
infrequent_players <- player_frequencies%>% filter(frequency =="infrequent")

ggplot(data = blitzSummaries, aes(x = blitz_sessions, y = avg_blitz_count)) + geom_point() + geom_hline(yintercept = 0)

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

cluster_rates <- data.frame(matrix(nrow = nrow(frequent_players), ncol = 2))
colnames(cluster_rates) <- c("cluster_rate", "player")
for(i in 1:nrow(frequent_players)){
  single_player_sessions <- frequent_player_sessions %>% filter(player == frequent_players$player[i])
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates[i,1] <- mean(clusters!=0)
  cluster_rates[i,2] <- frequent_players$player[i]
}
ggplot(data = cluster_rates, aes(x = cluster_rate)) + geom_histogram() + labs(title = "Cluster rates of highly active players")

cluster_rates <- data.frame(matrix(nrow = nrow(frequent_players), ncol = 2))
colnames(cluster_rates) <- c("cluster_rate", "player")
for(i in 1:nrow(frequent_players)){
  single_player_sessions <- frequent_player_sessions %>% filter(player == frequent_players$player[i])
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates[i,1] <- mean(clusters!=0)
  cluster_rates[i,2] <- frequent_players$player[i]
}

moderate_player_sessions <- player_sessions %>% filter(player %in% moderately_frequent_players$player) %>%  transform(time = hour(anytime(start)))

cluster_rates2 <- data.frame(matrix(nrow = nrow(moderately_frequent_players), ncol = 2))
colnames(cluster_rates2) <- c("cluster_rate", "player")
for(i in 1:nrow(moderately_frequent_players)){
  single_player_sessions <- moderate_player_sessions %>% filter(player == moderately_frequent_players$player[i])
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates2[i,2] <- moderately_frequent_players$player[i]
  cluster_rates2[i,1] <- mean(clusters!=0)
}
ggplot(data = cluster_rates2, aes(x = cluster_rate)) + geom_histogram()+ labs(title = "Cluster rates of moderately active players")

all_player_sessions <- player_sessions %>%  transform(time = hour(anytime(start)))
cluster_rates3 <- data.frame(matrix(nrow = nrow(player_sample), ncol = 2))
colnames(cluster_rates3) <- c("cluster_rate", "player")
for(i in 1:nrow(player_sample)){
  single_player_sessions <- all_player_sessions %>% filter(player == player_sample$`Player name`[i])
  if(nrow(single_player_sessions) == 0){
    next
  }
  Dbscan_cl<- dbscan(single_player_sessions$time, eps = 1, MinPts = floor(nrow(single_player_sessions)/5))
  clusters <- as.list(Dbscan_cl[["cluster"]])
  single_player_sessions$cluster_no <- clusters
  cluster_rates3[i,2] <- player_sample$`Player name`[i]
  cluster_rates3[i,1] <- mean(clusters!=0)
}
cluster_rates3 <- na.omit(cluster_rates3)

#Relationship to elo
x2013_frequent_players <- x2013_sample_players %>% filter(player %in% frequent_players$player)
player_elos <- x2013_frequent_players %>% group_by(player) %>% summarise(elo = max(Elo))
player_elos <- inner_join(player_elos, cluster_rates)
ggplot(data = player_elos, aes(x = cluster_rate, y = elo)) + geom_point() + geom_smooth(method = "lm")

x2013_moderate_players <- x2013_sample_players %>% filter(player %in% moderately_frequent_players$player)
player_elos2 <- x2013_moderate_players %>% group_by(player) %>% summarise(elo = max(Elo))
player_elos2 <- inner_join(player_elos2, cluster_rates2)
ggplot(data = player_elos2, aes(x = cluster_rate, y = elo)) + geom_point() + geom_smooth(method = "lm")

#Patterns regarding hiatus (>2 months without play)
player_hiatus <- data.frame(matrix(nrow = 0, ncol = 12))
colnames(player_hiatus) <- c("cluster_no", "start", "end", "diff", "count", "nBullet", "nBlitz", "nClassical", "bulletElo", "blitzElo", "classicalElo", "player")

for(i in 1:nrow(player_sample)){
  single_player_sessions <- player_sessions %>% filter(player == player_sample$`Player name`[i])
  if(nrow(single_player_sessions) == 0){
    next
  }
  Dbscan_cl<- dbscan(single_player_sessions$start, eps = 86400*60, MinPts = 1)
  clusters <- as.list(Dbscan_cl[["cluster"]])
  if(max(unlist(clusters))<=1){next}
  single_player_sessions$cluster_no <- clusters
  single_player_sessions <- single_player_sessions %>% 
    group_by(cluster_no) %>% 
    summarise(start = min(start), end = max(end), diff = end-start, count = n(), 
              nBullet = sum(nBullet), nBlitz = sum(nBlitz), nClassical = sum(nClassical),
              bulletElo = sum(bulletElo, na.rm = TRUE)/nBullet, blitzElo = sum(blitzElo, na.rm = TRUE)/nBlitz, classicalElo = sum(classicalElo, na.rm = TRUE)/nClassical)
  single_player_sessions$player = player_sample$`Player name`[i]
  player_hiatus <- rbind(player_hiatus, single_player_sessions)
}
hiatus_counts <- player_hiatus %>% group_by(player) %>% summarise(hiatus_count = max(as.numeric(cluster_no))-1)
hiatus_players <- unique(player_hiatus$player)

hiatus_diffs <- data.frame(matrix(nrow = 0, ncol = 3))
for(i in 1:length(hiatus_players)){
  single_player_group <- player_hiatus %>% filter(player == hiatus_players[i])
  single_player_group <- single_player_group %>% 
    transform(lag = lag(blitzElo))
  player_blitz <-(blitzSummaries %>% filter(player == hiatus_players[i]) %>% select(end_Elo_blitz))[1,1]
  single_player_group <- single_player_group %>% transform( diff = blitzElo - lag) %>% transmute(diff, player, blitzElo = as.numeric(player_blitz))
  single_player_group <- na.omit(single_player_group)
  hiatus_diffs <- rbind(hiatus_diffs, single_player_group)
}

ggplot(data = hiatus_diffs, aes(x = diff)) + geom_histogram(binwidth = 50)  + xlim(-500,500) + geom_vline(xintercept = quantile(hiatus_diffs$diff, probs = .5), color = "red")
ggplot(data = hiatus_diffs, aes(x = diff, y = blitzElo)) + geom_point() 

ggplot(data = blitzSummaries, aes(x = end_Elo_blitz)) + geom_histogram()+ geom_vline(xintercept = quantile(blitzSummaries$end_Elo_blitz, probs = .5), color = "red")
blitzSummaries3 <- blitzSummaries %>% filter(player %in% hiatus_players)
ggplot(data = blitzSummaries3, aes(x = end_Elo_blitz)) + geom_histogram()+ geom_vline(xintercept = quantile(blitzSummaries3$end_Elo_blitz, probs = .5), color = "red")


#combine dataframes
player_summaries <- session_summaries %>% 
  full_join(blitzSummaries, by = join_by(player)) %>% 
  full_join(bulletSummaries, by = join_by(player)) %>% 
  full_join(classicalSummaries, by = join_by(player)) %>%
  full_join(hiatus_counts, by = join_by(player)) %>%
  full_join(cluster_rates3, by = join_by(player))

#fill NAs
player_summaries$blitz_sessions[is.na(player_summaries$blitz_sessions)] <- 0
player_summaries$bullet_sessions[is.na(player_summaries$bullet_sessions)] <- 0
player_summaries$classical_sessions[is.na(player_summaries$classical_sessions)] <- 0

player_summaries$blitz_games[is.na(player_summaries$blitz_games)] <- 0
player_summaries$bullet_games[is.na(player_summaries$bullet_games)] <- 0
player_summaries$classical_games[is.na(player_summaries$classical_games)] <- 0

player_summaries$avg_blitz_count[is.na(player_summaries$avg_blitz_count)] <- 0
player_summaries$avg_bullet_count[is.na(player_summaries$avg_bullet_count)] <- 0
player_summaries$avg_classical_count[is.na(player_summaries$avg_classical_count)] <- 0

player_summaries$start_Elo_blitz[is.na(player_summaries$start_Elo_blitz)] <- 1500
player_summaries$start_Elo_bullet[is.na(player_summaries$start_Elo_bullet)] <- 1500
player_summaries$start_Elo_classical[is.na(player_summaries$start_Elo_classical)] <- 1500

player_summaries$end_Elo_blitz[is.na(player_summaries$end_Elo_blitz)] <- 1500
player_summaries$end_Elo_bullet[is.na(player_summaries$end_Elo_bullet)] <- 1500
player_summaries$end_Elo_classical[is.na(player_summaries$end_Elo_classical)] <- 1500

player_summaries$hiatus_count[is.na(player_summaries$hiatus_count)] <- 0

player_summaries$cluster_rate[is.na(player_summaries$cluster_rate)] <- 0

write.csv(player_summaries, "player_summaries_clustering.csv")

