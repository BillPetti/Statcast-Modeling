## Modeling with Statcast data
## Type of hit and predicted wOBA
## Bill Petti
## https://github.com/BillPetti
## Created June 15, 2016

# Load packages and functions

require(pacman)
p_load(RMySQL, dplyr, reshape2, ggplot2, grid, gridExtra, ROCR, randomForest, gam)

sessionInfo()
 
# load tableau color palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/tableau_colorblind_palette")

# load custom ggplot2 theme
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

# load custom theme for plotting batted ball data
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_battedball_grey.R")

# load custom confusion matrix and classification model evaluation function
source("/Users/williampetti/General-Code-Reference/confusionMatrix.R")

# Load connection information for the FanGraphs database

fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")

lapply(fg_database_values, load, .GlobalEnv)

# acquire a list of MLBAM ids for hitters that have put at least one ball in play between 2015-2016

# connect to the FanGraph's database

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

mlbamid.df.2016 <- dbGetQuery(con, "select batter from gd_pitch where substr(gamedate, 1, 4) = 2016 and des2 like 'in play%' group by batter") %>% mutate(year = 2016)

dbDisconnect(con)

# mlbamid.df.2016$batter <- as.character(mlbamid.df.2016$batter)

# function to scrape and format data from BaseballSavant.com

scrape_statcast <- function(batter, year) {
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?hfPT=&hfZ=&hfGT=R%7C&hfPR=11|12|13|&hfAB=&stadium=&hfBBT=&hfBBL=&hfC=&season=",year,"&player_type=batter&hfOuts=&pitcher_throws=&batter_stands=&start_speed_gt=&start_speed_lt=&perceived_speed_gt=&perceived_speed_lt=&spin_rate_gt=&spin_rate_lt=&exit_velocity_gt=&exit_velocity_lt=&launch_angle_gt=&launch_angle_lt=&distance_gt=&distance_lt=&batted_ball_angle_gt=&batted_ball_angle_lt=&game_date_gt=&game_date_lt=&team=&position=&hfRO=&home_road=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&sort_order=desc&min_abs=0&xba_gt=&xba_lt=&px1=&px2=&pz1=&pz2=&type=details&player_id=")
  x <- read.csv(paste0(url,batter))
  x[x=="null"] <- NA
  x$game_pk <- as.character(x$game_pk) %>% as.numeric()
  x$on_1b <- as.character(x$on_1b) %>% as.numeric()
  x$on_2b <- as.character(x$on_2b) %>% as.numeric()
  x$on_3b <- as.character(x$on_3b) %>% as.numeric()
  x$hit_distance_sc <- as.character(x$hit_distance_sc) %>% as.numeric()
  x$hit_speed <- as.character(x$hit_speed) %>% as.numeric()
  x$hit_angle <- as.character(x$hit_angle) %>% as.numeric()
  x
}

# pull separate data for the 2015 and 2016 seasons

statcast <- mlbamid.df.2016 %>% group_by(batter, year) %>% do(scrape_statcast(.$batter,.$year))

statcast.2016 <- statcast

# code the type of batted ball outcome

statcast.2016$hit_type <- with(statcast.2016, ifelse(grepl("Single", statcast.2016$events), 1,
                                                ifelse(grepl("Double", statcast.2016$events), 2,
                                                       ifelse(grepl("Triple", statcast.2016$events), 3, 
                                                              ifelse(grepl("Home Run", statcast.2016$events), 4, 0))))) %>%
  as.factor()

# code the run values for each outcome

statcast.2016$run_value <- with(statcast.2016, ifelse(hit_type == 1, .7,
                                                     ifelse(hit_type == 2, 1.1,
                                                            ifelse(hit_type == 3, 1.5,  
                                                                   ifelse(hit_type == 4, 1.7, 0))))) %>%
  as.numeric()

# calculate the horizontal angle of the batted ball using a custom function

horiz_angle <- function(df) {
  angle <- with(df, round(tan((hc_x-128)/(208-hc_y))*180/pi*.75,1))
  angle
  }

statcast.2016$hor_angle <- horiz_angle(statcast.2016)

# create a variable for the fielding team

statcast.2016$fieldingTeam <- with(statcast.2016, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
  as.factor()

# include row names for unique record identification

statcast.2016$row <- row.names(statcast.2016) %>% as.numeric()

# recode stand and home_team as factors

statcast.2016$stand <- as.factor(statcast.2016$stand)
statcast.2016$home_team <- as.factor(statcast.2016$home_team)

# subset 

working_data <- ungroup(statcast.2016) %>%
  filter(game_date <= "2016-05-28") %>%
  select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team, row) %>%
  filter(!is.na(hit_distance_sc)) %>%
  filter(!is.na(hit_angle)) %>% 
  filter(!is.na(hit_speed)) %>%
  arrange(desc(hit))

table(working_data$hit)

# remove apparently miscoded balls with x,y of 1,1

working_data <- filter(working_data, hc_x != 1, hc_y != 1)

# create training and test sets
# scaled data

set.seed(42)
train <- sample_frac(working_data, .15, replace = FALSE)
split <- setdiff(working_data, train)
test <- sample_frac(split, .50, replace = FALSE)
validate <- setdiff(split, test)

nrow(train) + nrow(test) + nrow(validate) == nrow(working_data)

with(train, table(hit)) %>% prop.table()
with(test, table(hit)) %>% prop.table()
with(validate, table(hit)) %>% prop.table()

# normalize exit velocity, launch angle and distance
# scaled features

scaled_data <- scale(train[,c(1:5)])
scale_values <- attr(scaled_data, 'scaled:scale')
scale_values

center_values <- attr(scaled_data, 'scaled:center')
center_values

train <- cbind(scaled_data, select(train, hit:row))

# save levels for factor variables

levels_home_team <- levels(train$home_team)
levels_stand <- levels(train$stand)
levels_fieldingTeam <- levels(train$fieldingTeam)

# apply scaling to test data

test$hit_distance_sc <- (test$hit_distance_sc - center_values[1]) / scale_values[1]

test$hit_speed <- (test$hit_speed - center_values[2]) / scale_values[2]

test$hit_angle <- (test$hit_angle - center_values[3]) / scale_values[3]

test$hc_x <- (test$hc_x - center_values[4]) / scale_values[4]

test$hc_y <- (test$hc_y - center_values[5]) / scale_values[5]

# apply scaling to validation data

validate$hit_distance_sc <- (validate$hit_distance_sc - center_values[1]) / scale_values[1]

validate$hit_speed <- (validate$hit_speed - center_values[2]) / scale_values[2]

validate$hit_angle <- (validate$hit_angle - center_values[3]) / scale_values[3]

validate$hc_x <- (validate$hc_x - center_values[4]) / scale_values[4]

validate$hc_y <- (validate$hc_y - center_values[5]) / scale_values[5]

# build, test, validate models

# model hit/no-hit random forest

# with all variables

set.seed(42)
rf.1 <- randomForest(as.factor(hit) ~ ., data = select(train, -row), ntree = 501, importance = TRUE)

print(rf.1)

plot(rf.1)

varImpPlot(rf.1)

# predict values and tune for optimal out of sample accuracy

predict_fit.rf.1 <- data.frame(fits = predict(rf.1, test, type = "prob")[,2], actuals = test$hit)
pred.rf.1 <- prediction(predict_fit.rf.1$fits, predict_fit.rf.1$actuals)
roc.pred.rf.1 <- performance(pred.rf.1, measure = "tpr", x.measure = "fpr")
plot(roc.pred.rf.1)
abline(a = 0, b = 1)
rf.1.opt <- opt.cut(roc.pred.rf.1, pred.rf.1)
rf.1.opt
rf.1.opt <- rf.1.opt[3]
predict_fit.rf.1$fits <- with(predict_fit.rf.1, ifelse(fits > rf.1.opt, 1, 0)) 

rf.1_confusion_test <- confusionMatrix(model = rf.1, x = test, y = test$hit)
rf.1_confusion_validate <- confusionMatrix(model = rf.1, x = validate, y = validate$hit)
confusionMatrix(df = predict_fit.rf.1)

# without home_field, stand, or fieldingTeam

set.seed(42)
rf.2 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -home_team, -stand, -fieldingTeam), ntree = 501, importance = TRUE)

print(rf.2) # accuracy does not improve past the selected ntree

plot(rf.2)

varImpPlot(rf.2)

predict_fit.rf.2 <- data.frame(fits = predict(rf.2, test, type = "prob")[,2], actuals = test$hit)

pred.rf.2 <- prediction(predict_fit.rf.2$fits, predict_fit.rf.2$actuals)

roc.pred.rf.2 <- performance(pred.rf.2, measure = "tpr", x.measure = "fpr")

plot(roc.pred.rf.2, colorize = T)

abline(a = 0, b = 1)

rf.2.opt <- opt.cut(roc.pred.rf.2, pred.rf.2)

rf.2.opt

rf.2.opt <- rf.2.opt[3]

predict_fit.rf.2$fits <- with(predict_fit.rf.2, ifelse(fits > rf.2.opt, 1, 0)) 

rf.2_confusion_test <- confusionMatrix(model = rf.2, x = test, y = test$hit)
rf.2_confusion_validate <- confusionMatrix(model = rf.2, x = validate, y = validate$hit)

# rf.2.cv <- rfcv(train[,c(1:5)], as.factor(train[,6]), cv.fold = 5)

# can we improve the model by altering the number of variables randomly tried at each branch?

tune.rf.2 <- tuneRF(train[,c(1:5)], as.factor(train[,6]), stepFactor = .5, plot = TRUE)

# with just speed, angle, and distance

set.seed(42)
rf.3 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -hc_x, -hc_y, -home_team, -stand, -fieldingTeam), ntree = 501, importance = TRUE)

print(rf.3) 

plot(rf.3) # accuracy does not improve past the selected ntree

varImpPlot(rf.3)

predict_fit.rf.3 <- data.frame(fits = predict(rf.3, test, type = "prob")[,2], actuals = test$hit)

pred.rf.3 <- prediction(predict_fit.rf.3$fits, predict_fit.rf.3$actuals)

roc.pred.rf.3 <- performance(pred.rf.3, measure = "tpr", x.measure = "fpr")

plot(roc.pred.rf.3)

abline(a = 0, b = 1)

rf.3.opt <- opt.cut(roc.pred.rf.3, pred.rf.3)

rf.3.opt

rf.3.opt <- rf.3.opt[3]

predict_fit.rf.3$fits <- with(predict_fit.rf.3, ifelse(fits > rf.3.opt, 1, 0)) 

rf.3_confusion_test <- confusionMatrix(model = rf.3, x = test, y = test$hit)
rf.3_confusion_validate <- confusionMatrix(model = rf.3, x = validate, y = validate$hit)

# only hit_distance_sc

set.seed(42)
rf.4 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -home_team, -fieldingTeam, -stand, -hit_speed, -hit_angle, -hc_x, -hc_y), importance = TRUE)

print(rf.4)

plot(rf.4)

varImpPlot(rf.4)

predict_fit.rf.4 <- data.frame(fits = predict(rf.4, test, type = "prob")[,2], actuals = test$hit)

pred.rf.4 <- prediction(predict_fit.rf.4$fits, predict_fit.rf.4$actuals)
roc.pred.rf.4 <- performance(pred.rf.4, measure = "tpr", x.measure = "fpr")
plot(roc.pred.rf.4)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.rf.4, pred.rf.4)
opt
opt <- opt[3]
predict_fit.rf.4$fits <- with(predict_fit.rf.4, ifelse(fits > opt, 1, 0)) 

rf.4_confusion_test <- confusionMatrix(model = rf.4, x = test, y = test$hit)
rf.4_confusion_validate <- confusionMatrix(model = rf.4, x = validate, y = validate$hit)

# only speed, angle, stand, fielding team and park

set.seed(42)
rf.5 <- randomForest(as.factor(hit) ~ ., mtry = 2, ntrees = 501, data = select(train, -row, -hit_distance_sc, -hc_y, -hc_x), importance = TRUE)

print(rf.5)

plot(rf.5)

varImpPlot(rf.5)

predict_fit.rf.5 <- data.frame(fits = predict(rf.5, test, type = "prob")[,2], actuals = test$hit)

pred.rf.5 <- prediction(predict_fit.rf.5$fits, predict_fit.rf.5$actuals)
roc.pred.rf.5 <- performance(pred.rf.5, measure = "tpr", x.measure = "fpr")
plot(roc.pred.rf.5)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.rf.5, pred.rf.5)
opt
opt <- opt[3]
predict_fit.rf.5$fits <- with(predict_fit.rf.5, ifelse(fits > opt, 1, 0)) 

rf.5_confusion_test <- confusionMatrix(model = rf.5, x = test, y = test$hit)
rf.5_confusion_validate <- confusionMatrix(model = rf.5, x = validate, y = validate$hit)

# can we improve the model by altering the number of variables randomly tried at each branch?

tune.rf.2 <- tuneRF(train[,c(2:3, 7:9)], train[,6], stepFactor = .5, plot = TRUE)

# mtry = 2 is the optimal number

# the optimal model appears to be rf.5

# plot the performance of the model based on the three features included

# examine whether the model is missing systematically for certain records

test_rows <- test$row
validate_rows <- validate$row
pred_data_emp_1 <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% test_rows)

pred_data_emp <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% validate_rows) %>%
  rbind(pred_data_emp_1)

out_of_training_rows <- pred_data_emp$row

rf.5_full_out_of_sample_data <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% out_of_training_rows) %>%
  filter(!is.na(hit_speed)) %>%
  filter(!is.na(hit_angle))

rf.5_full_out_of_sample_data_scaled <- rf.5_full_out_of_sample_data

rf.5_full_out_of_sample_data_scaled$hit_speed <- (rf.5_full_out_of_sample_data_scaled$hit_speed - center_values[2]) / scale_values[2]

rf.5_full_out_of_sample_data_scaled$hit_angle <- (rf.5_full_out_of_sample_data_scaled$hit_angle - center_values[3]) / scale_values[3]

rf.5.prob <- predict(rf.5, rf.5_full_out_of_sample_data_scaled, type = "response")

rf.5_full_out_of_sample_data <- cbind(filter(rf.5_full_out_of_sample_data, row %in% out_of_training_rows), rf.5.prob)

names(rf.5_full_out_of_sample_data)[65] <- "fits"

rf.5_full_out_of_sample_data_reduced <- rf.5_full_out_of_sample_data %>%
  select(hit_distance_sc, hit_angle, hit_speed, hit, fits)

rf.5_mean_table <- rf.5_full_out_of_sample_data_reduced %>% 
  group_by(hit, fits) %>%
  summarise_each(funs(mean(., na.rm = TRUE),n()))

rf.5_full_out_of_sample_data$type <- with(rf.5_full_out_of_sample_data, ifelse(hit == fits, 1, 0))

rf.5_full_out_of_sample_data$hit_label <- with(rf.5_full_out_of_sample_data, ifelse(hit == 1, "Hit", "Out"))

# condensed tab palette

tab_condensed <- c("#006BA4", "#C85200")

rf.5_plot1 <- ggplot(rf.5_full_out_of_sample_data, aes(hit_angle, hit_distance_sc)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nLaunch Angle") +
  ylab("Batted Ball Distance (ft.)\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

rf.5_plot1

rf.5_plot2 <- ggplot(rf.5_full_out_of_sample_data, aes(hit_speed, hit_distance_sc)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nSpeed off the Bat") +
  ylab("Batted Ball Distance (ft.)\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

rf.5_plot2

rf.5_plot3 <- ggplot(rf.5_full_out_of_sample_data, aes(hit_angle, hit_speed)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nLaunch Angle") +
  ylab("Speed off the Bat\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

rf.5_plot3

grid_plot_rf.5 <- grid.arrange(rf.5_plot1, rf.5_plot2, rf.5_plot3, ncol = 3)
grid_plot_rf.5

ggsave("grid_plot_rf.5.png", grid_plot_rf.5, scale = 1.2, width = 11, height = 8.5, units = "in")

## predicted probabilities using rf.5

# create a data set that combines the test and validation sets

test_rows <- test$row
validate_rows <- validate$row
pred_data_emp_1 <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% test_rows)

pred_data_emp <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% validate_rows) %>%
  rbind(pred_data_emp_1)

out_of_training_rows <- pred_data_emp$row

pred_data_emp <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>% 
  filter(row %in% out_of_training_rows) %>%
  select(hit_angle, hit_speed, stand, fieldingTeam, home_team) %>%
  filter(complete.cases(.))

# empirical data

pred_data_emp_scaled <- pred_data_emp

pred_data_emp_scaled$hit_speed <- (pred_data_emp_scaled$hit_speed - center_values[2]) / scale_values[2]

pred_data_emp_scaled$hit_angle <- (pred_data_emp_scaled$hit_angle - center_values[3]) / scale_values[3]

pred_prob_hit <- predict(rf.5, pred_data_emp_scaled, type = "prob")[,2]
pred_prob_hit_fits <- cbind(pred_data_emp, pred_prob_hit)

pred_prob_hit_fits[,c(1:2)] <- pred_prob_hit_fits[,c(1:2)] %>%
  round(.)
  
angle_speed_pred <- pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

angle_speed_pred

ggsave("angle_speed_pred.png", angle_speed_pred, scale = 1.2, width = 11, height = 8.5, units = "in")

# calculate average probabilities by bucketed angles and speed, and compare to actual 

pred_prob_hit_fits$angle_buckets <- cut(pred_prob_hit_fits$hit_angle, seq(from = -90, to = 90, by = 10), include.lowest = TRUE)

pred_prob_hit_fits$speed_buckets <- cut(pred_prob_hit_fits$hit_speed, seq(from = 0, to = 140, by = 10), include.lowest = TRUE)

pred_prob_by_buckets <- pred_prob_hit_fits %>% 
  group_by(angle_buckets, speed_buckets) %>% 
  summarise(ave_prob = round(mean(pred_prob_hit),3)) %>%
  ungroup(.) %>%
  arrange(desc(speed_buckets), desc(angle_buckets))

empirical_prob_hit <- select(working_data_original, hit_distance_sc:hit)

empirical_prob_hit$angle_buckets <- cut(empirical_prob_hit$hit_angle, seq(from = -90, to = 90, by = 10), include.lowest = TRUE)

empirical_prob_hit$speed_buckets <- cut(empirical_prob_hit$hit_speed, seq(from = 0, to = 140, by = 10), include.lowest = TRUE)

empirical_prob_hit <- empirical_prob_hit %>% 
  group_by(angle_buckets, speed_buckets) %>% 
  summarise(count = n(), empirical_ave = round(mean(hit),3)) %>%
  ungroup(.) %>%
  arrange(desc(speed_buckets), desc(angle_buckets))

compare_prob <- left_join(pred_prob_by_buckets, empirical_prob_hit, by = c("angle_buckets", "speed_buckets"))

# regress the empirical probabilties by the predicted probabilities

compare_lm <- lm(empirical_ave ~ ave_prob, data = compare_prob)

summary(compare_lm)$sigma 

require(scales)

compare_prob_plot <- compare_prob %>% ggplot(aes(ave_prob, empirical_ave)) + geom_point(aes(size = count), alpha = .5, color = "#006BA4") + stat_smooth(method = "lm", color = "#C85200") + scale_x_continuous(labels = percent) + scale_y_continuous(limits = c(0,1), labels = percent)+ xlab("\nPredicted Batting Average") + ylab("Empirical Batting Average\n") + ggtitle("\nPredicted vs. Empirical Batting Average: Statcast Data (2016)\n") + annotate(geom = "text", x = .94, y = .80, label = paste0("RMSE = ",round(summary(compare_lm)$sigma,2)), fontface = "bold") + theme_bp_grey()

compare_prob_plot

ggsave("compare_prob_plot.png", compare_prob_plot, scale = 1.2, width = 11, height = 8.5, units = "in")

# apply predictive model to all out of sample data in 2016

statcast.2016.scaled_original <- ungroup(statcast.2016) %>% 
  filter(hc_x != 1, hc_y != 1) %>%
  filter(row %in% out_of_training_rows)

statcast.2016.scaled <- statcast.2016.scaled_original

statcast.2016.scaled$hit_speed <- (statcast.2016.scaled$hit_speed - center_values[2]) / scale_values[2]

statcast.2016.scaled$hit_angle <- (statcast.2016.scaled$hit_angle - center_values[3]) / scale_values[3]

statcast.2016.fit <- predict(rf.5, statcast.2016.scaled, type = "response") %>%
  as.data.frame()
statcast.2016.scaled <- cbind(statcast.2016.scaled, statcast.2016.fit)
names(statcast.2016.scaled)[65] <- "fit" 
statcast.2016.predicted <- statcast.2016.scaled %>%
  mutate(fit_dummy = fit)

# statcast.2016.predicted <- left_join(
#   select(statcast.2016.scaled, row, fit),
#   statcast.2016, by = "row") %>%
#   mutate(fit_dummy = fit) %>%
#   filter(fit != NA)

statcast.2016.predicted$fit_dummy <- as.character(statcast.2016.predicted$fit_dummy) %>% as.numeric()           
statcast.2016.predicted <- filter(statcast.2016.predicted, !is.na(fit_dummy))

statcast.2016.predicted$hit <- as.character(statcast.2016.predicted$hit) %>% as.numeric()  

with(statcast.2016.predicted, table(hit, fit_dummy))
with(statcast.2016.predicted, table(fit_dummy))

statcast.2016.predicted$expected_outs <- with(statcast.2016.predicted, ifelse(fit_dummy == 0, 1, 0))

statcast.2016.predicted$outs <- with(statcast.2016.predicted, ifelse(hit == 0, 1, 0))

statcast.2016.predicted$expected_outs_made <- with(statcast.2016.predicted, ifelse(expected_outs == 1 & outs == 1, 1, 0))

statcast.2016.predicted$unexpected_outs_made <- with(statcast.2016.predicted, ifelse(expected_outs == 0 & hit == 0, 1, 0))

conversion_by_team <- statcast.2016.predicted %>% 
  group_by(fieldingTeam) %>% 
  summarise(expected_outs_total = sum(expected_outs), expected_outs_converted = sum(expected_outs_made), unexpected_outs_converted = sum(unexpected_outs_made), expected_hits_total = sum(fit_dummy)) %>%
  mutate(expected_outs_perc = expected_outs_converted/expected_outs_total, unexpected_outs_perc = unexpected_outs_converted/expected_hits_total)

conversion_by_team_plot <- ungroup(conversion_by_team) %>% ggplot(aes(expected_outs_perc, unexpected_outs_perc)) + geom_text(aes(label = fieldingTeam), color = "#006BA4", fontface = "bold") + stat_smooth(color = "#C85200") + scale_x_continuous(limits = c(.70,.90), labels = percent) + scale_y_continuous(labels = percent)+ xlab("\n% of Expected Outs Made") + ylab("% of Unexpected Outs Converted\n") + ggtitle("\nExpected Outs vs. Unexpected Outs: Statcast Data (2016)\n") + theme_bp_grey()

conversion_by_team_plot

ggsave("conversion_by_team_plot.png", conversion_by_team_plot, scale = 1.2, width = 11, height = 8.5, units = "in")

write.csv(conversion_by_team, "conversion_by_team.csv", row.names = FALSE)      

expected_ave_by_team <- statcast.2016.predicted %>% 
  filter(events != "Home Run") %>%
  group_by(fieldingTeam) %>% 
  summarise(n(), expected_ba = sum(fit_dummy)/n(), actual_ba = sum(hit)/n())

with(statcast.2016, table(hit))

original_ave_by_team <- statcast.2016.predicted %>% 
  filter(events != "Home Run") %>%
  summarise(n(), actual_ba = sum(hit)/n())

expected_v_actual_ave_plot <- ungroup(expected_ave_by_team) %>% ggplot(aes(expected_ba, actual_ba)) + geom_text(aes(label = fieldingTeam), color = "#006BA4", fontface = "bold") + geom_abline(intercept = 0, color = "#C85200") + scale_x_continuous(limits = c(.250,.450)) + scale_y_continuous(limits = c(.250,.450)) + xlab("\nExpected Batting Average Against") + ylab("Actual Batting Average Against\n") + ggtitle("\nExpected vs. Actual Batting Average Against: Statcast Data (2016)\n") + theme_bp_grey()

expected_v_actual_ave_plot

ggsave("expected_v_actual_ave_plot.png", expected_v_actual_ave_plot, scale = 1.1, width = 14, height = 8.5, units = "in")

write.csv(expected_ave_by_team, "expected_ave_by_team.csv", row.names = FALSE)  

# plot batted ball location

statcast.2016.predicted.original <- select(statcast.2016.predicted, -hc_y, -hc_x, -hit_distance_sc, -hit_angle, -hit_speed) %>%
  left_join((select(statcast.2016.scaled_original, hit_distance_sc, hit_angle, hit_speed, hc_x, hc_y, row)), by = "row")

statcast.2016.predicted.original$miss <- with(statcast.2016.predicted.original, ifelse(hit != fit_dummy, "Incorrect", "Correct"))

statcast.2016.predicted.original$hit_label <- with(statcast.2016.predicted.original, ifelse(hit == 1, "Hit", "Out")) %>% as.factor()

tab_condensed_factor <- c("#C85200","#006BA4")

misses_plotted <- filter(statcast.2016.predicted.original, !is.na(miss)) %>% 
  ggplot(aes(x = hc_x, y = (hc_y*-1))) + 
  geom_point(aes(color = hit_label), alpha = .5) + 
  xlim(0,250) +
  ylim(-250, 0) +
  ggtitle("\nComparing Correct & Incorrect Model Predictons\n") +
  facet_wrap(~miss) +
  geom_segment(x=128, xend = 18, y=-208, yend = -100) + 
  geom_segment(x=128, xend = 237, y=-208, yend = -100) + 
  geom_curve(x = 128-45, xend = 128 + 45, y = -208 + 48, yend = -208 + 48, curvature = -.7, linetype = "dotted") + 
  geom_curve(x = 19, xend = 237, y = -100, yend = -100, curvature = -.65) + 
  theme_battedball_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed_factor, "Outcome")

misses_plotted

ggsave("misses_plotted.png", misses_plotted, scale = .8, width = 14, height = 8.5, units = "in")

# code and calculate accuracy by distance bucket

statcast.2016.predicted.original$distance_bucket <- cut(statcast.2016.predicted.original$hit_distance_sc, breaks = c(0,45,90,125,175,200,250,300,350,400,450,500), include.lowest = TRUE)

statcast.2016.predicted.original$miss_num <- with(statcast.2016.predicted.original, ifelse(miss == "Correct", 1, 0)) %>%
  as.numeric()

averages_misses_correct_pred <- statcast.2016.predicted.original %>%
  group_by(distance_bucket) %>% 
  summarise(n(), correct_perc = round(mean(miss_num, na.rm = TRUE),3))

accuracy_by_distance_plot <- ungroup(averages_misses_correct_pred) %>% 
  filter(!is.na(distance_bucket)) %>%
  ggplot(aes(x = distance_bucket, y = correct_perc)) + 
  scale_y_continuous(limits = c(.50, 1), labels = percent) +
  geom_point(alpha = .8, size = 7, fill = "#C85200", color = "#006BA4", shape = 21, stroke = 1.5) +
  geom_text(aes(label = percent(correct_perc)), size = 5, vjust = 2.5, fontface = "bold") + 
  xlab("\nDistance Range (in feet)") + 
  ylab("% Correctly Predicted (note axis is truncated)\n") + 
  ggtitle("\nAccuracy Rate by Distance Range\n") + 
  theme_bp_grey()

accuracy_by_distance_plot

ggsave("accuracy_by_distance_plot.png", accuracy_by_distance_plot, scale = 1.1, width = 11, height = 8.5, units = "in")

# export original data 

write.csv(statcast, "statcast.csv", row.names = FALSE)

