all_pitches <- read.csv("all_pitches.csv", header = T)
all_stats <- read.csv("all_stats.csv", header = T)
all_players <- read.csv("all_players.csv", header = T)

str(all_pitches)
View(all_pitches)

library("tidyverse")
col_drop <- c("notes")


all_pitches <- all_pitches[,!(names(all_pitches) %in% col_drop)]

all_pitches$pitcher <- str_replace(all_pitches$pitcher, "Povse", "Player A")
all_pitches$pitcher <- as.factor(all_pitches$pitcher)

all_pitches$game_date <- as.character(all_pitches$game_date)

all_pitches$game_date <- strptime(as.character(all_pitches$game_date), "%d/%m/%Y")

all_pitches$inning <- as.factor(all_pitches$inning)

unique(all_pitches$tagged_pitch_type)
unique(all_pitches$auto_pitch_type)

#Decided to use tagged pitch type and would use the auto pitch type to fill in undefined
cols.factor <- c("tagged_pitch_type", "auto_pitch_type")

all_pitches[cols.factor] <- sapply(all_pitches[cols.factor], as.character)

change_pitches <- c("Undefined", "Other")

all_pitches$tagged_pitch_type <- ifelse(all_pitches$tagged_pitch_type %in% change_pitches, 
                                        all_pitches$auto_pitch_type, all_pitches$tagged_pitch_type)

all_pitches[cols.factor] <- lapply(all_pitches[cols.factor], as.factor)


all_pitches <- all_pitches[!(all_pitches$tagged_pitch_type %in% change_pitches),]

#1. Which of the two pitchers will likely get injured first based on previous research?

quick_summaries <- all_pitches %>%
  group_by(pitcher, tagged_pitch_type) %>%
  summarize(mean_speed = mean(rel_speed, na.rm = T))

times_thrown <- group_by(all_pitches,pitcher, tagged_pitch_type) %>%
  count()
  
quick_summaries <- merge(quick_summaries, times_thrown, by = c("pitcher", "tagged_pitch_type"))

quick_summaries <- quick_summaries %>%
  group_by(pitcher) %>%
  mutate(pct_pitched = n / sum(n))

quick_summaries$pct_pitched <- quick_summaries$pct_pitched * 100
quick_summaries$pct_pitched <- round(quick_summaries$pct_pitched, 2)
quick_summaries$pct_pitched <- paste(quick_summaries$pct_pitched, sep = "", "%")

library("ggrepel")

ggplot(quick_summaries, aes(tagged_pitch_type, mean_speed, color = pitcher)) +
  geom_point() +
  scale_color_manual(breaks = c("Player A", "Player B"),
                     values = c("purple", "red")) +
  labs(title = "Pitch type average speed and %thrown", x = "Pitch type", y = "Speed") +
  geom_label_repel(aes(label = pct_pitched),
                   box.padding = 0.25,
                   point.padding = 0.5)
                  
speed_cors <- cor(all_pitches[, c(23:27, 29:ncol(all_pitches))], use = "pairwise.complete.obs")

pitcher_dif <- aggregate(all_pitches[, c(23:27, 29:ncol(all_pitches))], by = list(all_pitches$tagged_pitch_type, all_pitches$pitcher), 
          FUN = mean, na.rm = T)

pitcher_dif <- pitcher_dif %>%
  arrange(Group.1)

pitcher_dif <- pitcher_dif[-5, ]

ggplot(all_pitches, aes(x = rel_side, y = rel_height, color = tagged_pitch_type)) +
  geom_jitter(alpha = .2) +
  facet_grid(~ pitcher) +
  labs(title = "Pitcher Release Point", x = "Distance from center of rubber (ft)", 
       y = "Height above home plate (ft)",
       color = "Pitch Type") +
  theme_bw()

ggplot(all_pitches, aes(x = tagged_pitch_type, y = rel_height)) +
  geom_boxplot() +
  facet_grid(~ pitcher) +
  labs(title = "Pitcher release height", x = "Pitch type", y = "Height above home plate (ft)") +
  theme(legend.position = "None") +
  theme_bw()

#2. During which pitch of the plate appearance will the pitcher most likely throw a specific pitch? 

all_pitches$Pitch_count <- paste(all_pitches$balls, all_pitches$strikes, sep = "-")

all_pitches <- all_pitches[,c(1:14, 52, 15:ncol(all_pitches))]
all_pitches <- all_pitches[, -53]

all_pitches$count <- as.factor(all_pitches$count)

all_pitches$tagged_pitch_type <- droplevels(all_pitches)$tagged_pitch_type


library("caret")
library("nnet")
library("e1071")


training.samples <- all_pitches$tagged_pitch_type %>%
  createDataPartition(p = 0.8, list = F)

train.data <- all_pitches[training.samples,]
test.data <- all_pitches[-training.samples,]  
  
myCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model <- train(tagged_pitch_type ~ strikes + balls + outs + pa_of_inning + pitcher + batter_side,
               data = train.data,
               method = "multinom",
               trControl = myCtrl,
               na.action = na.omit)

model

predicted.pitch <- model %>% predict(test.data)

confusionMatrix(predicted.pitch, test.data$tagged_pitch_type)

#Simplify whether it will be a fastball or not

all_pitches$fball_or_not <- ifelse(all_pitches$tagged_pitch_type == "Fastball", "Yes", "No")
all_pitches$fball_or_not <- as.factor(all_pitches$fball_or_not)
fball_or_not <- all_pitches$fball_or_not

set.seed(1)

training.samples2 <- all_pitches$tagged_pitch_type %>%
  createDataPartition(p = 0.8, list = F)

train.data2 <- all_pitches[training.samples2,]
test.data2 <- all_pitches[-training.samples2,]  

myCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

log_model <- train(fball_or_not ~ strikes + balls + outs + inning + pitch_of_pa + 
                     pa_of_inning + batter_side + pitcher,
                   data = train.data,
                   method = "glm",
                   trControl = myCtrl,
                   na.action = na.omit)


log_model
  

predicted.pitch2 <- log_model %>% predict(test.data2)
confusionMatrix(predicted.pitch2, test.data2$fball_or_not)



#There doesn't appear to be good predictive power with this dataset to determine when a pitcher
#will throw a fastball or on what specific count. Based on the amount of times the pitchers throw a fastball, it is
#best to simply guess they will throw a fastball.



ggplot(all_pitches, aes(x = tagged_pitch_type, fill = tagged_pitch_type)) +
  geom_bar() +
  facet_grid(~Pitch_count)

ggplot(all_pitches, aes(x = tagged_pitch_type, fill = tagged_pitch_type)) +
  geom_bar() +
  facet_grid(~pitch_of_pa)

pitch_by_count <- aggregate(all_pitches$tagged_pitch_type, 
                            by = list(all_pitches$pitcher, 
                                      all_pitches$Pitch_count),
                            FUN = count)

pitch_by_count <- all_pitches %>%
  group_by(pitcher, Pitch_count) %>%
  count(tagged_pitch_type)
  

pitch_by_pct <- pitch_by_count %>%
  group_by(pitcher, Pitch_count) %>%
  mutate(pct_pitched = n / sum(n))

pitch_by_pct$pct_pitched <- as.numeric(pitch_by_pct$pct_pitched)

pitch_by_pct$pct_pitched <- round(pitch_by_pct$pct_pitched * 100, 2)


pitch_by_pct <- pitch_by_pct[, -4]

pitch_by_pct <- pitch_by_pct %>%
  spread(key = Pitch_count, value = pct_pitched) %>%
  arrange(pitcher, desc(`0-0`))

ggplot(data = all_pitches, aes(x = plate_loc_side, 
                               y = plate_loc_height, 
                               color = tagged_pitch_type, 
                               shape = pitch_call)) +
  geom_jitter() +
  facet_grid(~pitcher) +
  labs(title = "Pitch location by player", 
       x = "Distance from center of home plate (ft)", 
       y = "Height above home plate (ft)") +
  scale_shape_manual(values = c(1, 2, 5, 6, 4, 7))

ggplot(all_pitches, aes(x = plate_loc_side, 
                       y = plate_loc_height)) +
  geom_hex() +
  facet_grid(~pitcher + batter_side)

ggplot(all_pitches, aes(x = plate_loc_side, 
                        y = plate_loc_height, 
                        fill = tagged_pitch_type)) +
  geom_bin2d() +
  facet_grid(~pitcher + batter_side)


all_pitches$game_date <- as.POSIXct(all_pitches$game_date)



#5. Effectiveness of stretch vs. windup in pitching


pitcher_sets <- c("Stretch","Windup")

set_effectiveness <- all_pitches %>%
  filter(pitcher_set %in% pitcher_sets) %>%
  group_by(pitcher, pitcher_set) %>%
  count(pitch_call, play_result)

set_effectiveness <- spread(set_effectiveness, key = pitcher_set, value = n) %>%
  arrange(pitcher, desc(Stretch))

ks_bb <- all_pitches %>%
  filter(pitcher_set %in% pitcher_sets) %>%
  group_by(pitcher, pitcher_set) %>%
  count(k_or_bb) %>%
  spread(key = pitcher_set, value = n) %>%
  arrange(pitcher, desc(`Stretch`)) %>%
  filter(k_or_bb != "Undefined") %>%
  ungroup()


#7. Predictive modeling for stolen bases based on factors

str(all_players)
str(all_stats)

cols.numeric <- c("avg_exit_speed", "hard_hit_perc")

all_stats[cols.numeric] <- sapply(all_stats[cols.numeric], as.character)
all_stats[cols.numeric] <- lapply(all_stats[cols.numeric], as.numeric)

dummy.vars <- dummyVars(~ ., data = all_stats[, -2])
train.dummy <- predict(dummy.vars, all_stats[, -2])
View(train.dummy)

pre.process <- preProcess(train.dummy, method = "bagImpute")
imputed.data <- predict(pre.process, train.dummy)
View(imputed.data)


all_stats$avg_exit_speed <- imputed.data[, 6]
all_stats$hard_hit_perc <- imputed.data[, 7]



all_players_stats <- merge(all_stats, all_players, by = c("player_code", "year"))

cols.numeric <- c("avg_exit_speed", "hard_hit_perc")

all_players_stats[cols.numeric] <- sapply(all_players_stats[cols.numeric], as.character)
all_players_stats[cols.numeric] <- lapply(all_players_stats[cols.numeric], as.numeric)

summary(all_players_stats[,7:12])

train.samples <- createDataPartition(all_players_stats$sb, p = 0.8, list = F)

myTrain <- all_players_stats[train.samples,]
myTest <- all_players_stats[-train.samples,]

myCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

lmModel <- train(sb ~ tpa + avg + slg,
                 data = myTrain,
                 preProcess = c("center", "scale"),
                 method = "glmnet",
                 trControl = myCtrl)

sb_predict <- predict(lmModel, myTest)

plot(sb_predict, myTest$sb,
     xlab = "predicted", ylab = "actual")
abline(a=0,b=1)

RMSE(sb_predict, myTest$sb)

train_predict <- predict(lmModel, myTrain)

RMSE(train_predict, myTrain$sb)

arrange(lmModel$results, RMSE) %>% head


#__________________________________________________
#GM Boosted
#__________________________________________________

tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))

gbm_model <- train(sb ~ tpa + avg + slg,
                   data = myTrain, 
                   method = "gbm", 
                   trControl = myCtrl, 
                   tuneGrid =tg, 
                   verbose = FALSE)

xgpredict <- predict(gbm_model, myTest)
plot(xgpredict, myTest$sb,
     xlab = "predicted", ylab = "actual")
abline(a=0,b=1)

RMSE(xgpredict, myTest$sb)

#Most of the models are not particularly good at predicting sb.
#To improve on the model I would want their speed, the count of the pitch,
#the score of the game, basically as much situational information as possible,
#and this dataset doesn't have enough of that. 

#8. Correlations between physiological metrics and on-field performance

pc <- cor(all_players_stats[,9:12], all_players_stats[,5:8], use = "pairwise.complete.obs")

#Very little to no correlations between physiological metrics and on field
#performance metrics



