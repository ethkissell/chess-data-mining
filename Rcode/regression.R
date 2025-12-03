#Load data
library(readr)
library(fpc)
library(readr)
library(tidyverse)
library(dplyr)
  #Data from clustering.R
player_summaries <- read_csv("player_summaries_clustering.csv")
players <- unique(player_summaries$player)
games <- read_csv("jan_2013-jan_2014.csv")

#Wrangle more data
games <- games %>% filter(White %in% players | Black %in% players)
player_game_length <- data.frame(matrix(nrow = length(players), ncol = 2))
colnames(player_game_length) <- c("player", "avg_half_move_count")
for(i in 1:length(players)){
  individual_player_games <- games %>% filter(White %in% players[i] | Black %in% players[i])
  player_game_length[i,2] <- mean(individual_player_games$`Half Move Count`)
  player_game_length[i,1] <- players[i]
}

player_summaries <- full_join(player_summaries, player_game_length, by = join_by("player"))
player_summaries <- player_summaries %>% transform(avg_game_length = avg_length/avg_game_count)

#remove elo columns so we dont use elo to predict elo
useable_cols <- player_summaries %>% dplyr::select(-start_Elo_blitz, -start_Elo_bullet, -end_Elo_bullet, -start_Elo_classical, -end_Elo_classical, -player, -1)

model <- glm(formula = end_Elo_blitz ~ . , data = useable_cols)
summary(model)

null_model <- glm(formula = end_Elo_blitz ~ 1 , data = useable_cols)


library(boot)
set.seed(303)
five_cv <- cv.glm(data = useable_cols, glmfit = model, K = 5)
sqrt(five_cv$delta[1])
set.seed(303)
five_cv_null <- cv.glm(data = useable_cols, glmfit = null_model, K = 5)
sqrt(five_cv_null$delta[1])

library(olsrr)
forward_model <- ols_step_forward_adj_r2(model)
forward_model$metrics

new_model <-  glm(formula = end_Elo_blitz ~ 
                    avg_blitz_count + 
                    avg_half_move_count + 
                    avg_classical_count + 
                    bullet_sessions + 
                    classical_sessions + 
                    classical_games + 
                    sessions + 
                    avg_game_length + 
                    avg_length + 
                    avg_game_count   , 
                  data = useable_cols)
set.seed(303)
summary(new_model)
five_cv <- cv.glm(data = useable_cols, glmfit = new_model, K = 5)
sqrt(five_cv$delta[1])


useable_cols <- player_summaries %>% 
  transform(improvement = if_else(end_Elo_blitz-start_Elo_blitz > 100, 1, 0))%>%
  dplyr::select(-end_Elo_bullet, -end_Elo_blitz, -end_Elo_classical, -player, -1)
rows <- sample(nrow(useable_cols), floor(nrow(useable_cols)*.8))
training <- useable_cols[rows,]
testing <- useable_cols[-rows,]

logistic_model <- glm(formula = improvement ~ . , data = training, family = "binomial")
library(MASS)
step_model <- stepAIC(logistic_model, direction = "both", trace = FALSE)

summary(step_model)

predictions <- testing %>% transform(improvement_prob = predict(logistic_model, newdata = testing, type='response')) %>%
                           transform(improvement_pred = if_else(improvement_prob>0.11,1,0)) %>%
                           transform(correct = improvement_pred == improvement)
CER <- 1-mean(predictions$correct)

#create confusion matrix
conf <- table(predictions$improvement,predictions$improvement_pred)
conf
conf[2,2]/(conf[2,1]+conf[2,2])
conf[1,2]/(conf[1,2]+conf[1,1])

library(ROCR)

pred <- prediction(predictions=predictions$improvement_prob,
                   labels=predictions$improvement)
#plot TPR vs. FPR curve
perf3 <- performance(pred,measure='tpr',x.measure='fpr')
plot(perf3,colorize=TRUE,lwd=2, main = "Results of Logistic Model predicting Improvement In Players")
abline(a=0,b=1)

#plot CER curve
perf2 <- performance(pred,measure='err')
plot(perf2,lwd=2)
