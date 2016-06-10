## Modeling with Statcast data
## Bill Petti
## https://github.com/BillPetti
## Created May 27, 2016
## Data pulled after games on May 28, 2016

# Load packages and functions

require(pacman)
p_load(RMySQL, dplyr, reshape2, ggplot2, grid, gridExtra, ROCR, randomForest, gam)

#load tableau color palette
source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/tableau_colorblind_palette")

#load custom confusion matrix and classification model evaluation function
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

statcast.2016 <- mlbamid.df.2016 %>% group_by(batter, year) %>% do(scrape_statcast(.$batter,.$year))

# code if the batted ball resulted in a hit or an out

statcast.2016$hit <- with(statcast.2016, ifelse(grepl("Single", statcast.2016$events), 1,
                                                ifelse(grepl("Double", statcast.2016$events), 1,
                                                       ifelse(grepl("Triple", statcast.2016$events), 1, 
                                                              ifelse(grepl("Home Run", statcast.2016$events), 1, 0)))))

# create a variable for the fielding team

statcast.2016$fieldingTeam <- with(statcast.2016, ifelse(inning_topbot == "bot", away_team, home_team)) %>% 
  as.factor()

# include row names for unique record identification

statcast.2016$row <- row.names(statcast.2016) %>% as.numeric()

# subset 

working_data <- ungroup(statcast.2016) %>%
  select(hit_distance_sc:hit_angle, hc_x, hc_y, hit, stand, fieldingTeam, home_team, row) %>%
  filter(!is.na(hit_distance_sc)) %>%
  filter(!is.na(hit_angle)) %>% 
  filter(!is.na(hit_speed)) %>%
  arrange(desc(hit))

working_data$rows <- row.names(working_data)
table(working_data$hit)

working_data$stand <- as.factor(working_data$stand)
working_data$home_team <- as.factor(working_data$home_team)

working_data_original <- working_data

# normalize exit velocity, launch angle and distance
# scaled features

scaled_data <- scale(working_data[,c(1:5)])
scale_values <- attr(scaled_data, 'scaled:scale')
# hit_distance_sc       hit_speed       hit_angle            hc_x            hc_y 
#       104.40557        13.65910        24.23876        40.49578        39.97242 

center_values <- attr(scaled_data, 'scaled:center')
# hit_distance_sc       hit_speed       hit_angle            hc_x            hc_y 
#       214.65345        89.29212        11.20313       127.86669       119.34791

working_data_scaled <- cbind(
  select(working_data, hit:row), 
  scaled_data
)

# back_transform = t(apply(scaled_data, 1, function(r) r*attr(scaled_data,'scaled:scale') + attr(scaled_data, 'scaled:center')))

# to transform new values to values for use in the model use (value - center)/scale

# create training and test sets
# scaled data

set.seed(42)
train <- sample_frac(working_data_scaled, .50, replace = FALSE)
test <- setdiff(working_data_scaled, train)
nrow(train) + nrow(test) == nrow(working_data_scaled)

with(train, table(hit)) %>% prop.table()
with(test, table(hit)) %>% prop.table()

# non-scaled data

set.seed(42)
orig_train <- sample_frac(working_data, .75, replace = FALSE)
orig_test <- setdiff(working_data, orig_train)
nrow(orig_train) + nrow(orig_test) == nrow(working_data)

orig_train <- select(orig_train, -rows)
with(orig_train, table(hit)) %>% prop.table()
with(orig_test, table(hit)) %>% prop.table()

# build training models(s)

# model hit/no-hit random forest

# with all variables

set.seed(42)
rf.1 <- randomForest(as.factor(hit) ~ ., data = select(train, -row), ntree = 501, importance = TRUE)

print(rf.1)

plot(rf.1)

varImpPlot(rf.1)

# predict values and tune for optimal out of sample accuracy

predict_fit.rf.1 <- data.frame(fits = predict(rf.1, orig_test, type = "prob")[,2], actuals = orig_test$hit)
pred.rf.1 <- prediction(predict_fit.rf.1$fits, predict_fit.rf.1$actuals)
roc.pred.rf.1 <- performance(pred.rf.1, measure = "tpr", x.measure = "fpr")
plot(roc.pred.rf.1)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.rf.1, pred.rf.1)
opt
opt <- opt[3]
predict_fit.rf.1$fits <- with(predict_fit.rf.1, ifelse(fits > opt, 1, 0)) 

rf.1_confusion <- confusionMatrix(model = rf.1, x = test, y = test$hit)

#       fits
# actuals     0     1
#       0 10160   751
#       1   786  5214
#                     V1
# sensitivity      0.869
# precision        0.874
# specificity      0.931
# overall_accuracy 0.909
# f1_measure       0.872

# predict_fit.rf.1 <- data.frame(fits = predict(rf.1, test), actuals = test$hit)
# confuse <- with(predict_fit, table(actuals, fits))
# confuse

# without home_field, stand, or fieldingTeam

set.seed(42)
rf.2 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -home_team, -stand, -fieldingTeam), ntree = 501, importance = TRUE)
# rf.2 <- randomForest(as.factor(hit) ~ ., ntree = 501, data = select(train, -home_team, -fieldingTeam, -stand), importance = TRUE)

print(rf.2) # accuracy does not improve past the selected ntree

plot(rf.2)

varImpPlot(rf.2)

# predict_fit.rf.2 <- data.frame(fits = predict(rf.2, train, type = "prob")[,2], actuals = train$hit)
# 
# pred.rf.2 <- prediction(predict_fit.rf.2$fits, predict_fit.rf.2$actuals)
# 
# roc.pred.rf.2 <- performance(pred.rf.2, measure = "tpr", x.measure = "fpr")
# 
# plot(roc.pred.rf.2)
# 
# abline(a = 0, b = 1)
# 
# rf.2.opt <- opt.cut(roc.pred.rf.2, pred.rf.2)
# 
# rf.2.opt
# 
# rf.2.opt <- rf.2.opt[3]
# 
# predict_fit.rf.2$fits <- with(predict_fit.rf.2, ifelse(fits > rf.2.opt, 1, 0)) 

rf.2_confusion_test <- confusionMatrix(model = rf.2, x = test, y = test$hit)

#           fits
# actuals     0     1
#       0 10417   494
#       1   905  5095
#                     V1
# sensitivity      0.849
# precision        0.912
# specificity      0.955
# overall_accuracy 0.917
# f1_measure       0.879

rf.2.cv <- rfcv(train[,c(5:9)], as.factor(train[,1]), cv.fold = 5)

# can we improve the model by altering the number of variables randomly tried at each branch?

tune.rf.2 <- tuneRF(train[,c(5:9)], as.factor(train[,1]), stepFactor = .5, plot = TRUE)

# with just speed, angle, and distance

set.seed(42)
rf.3 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -hc_x, -hc_y, -home_team, -stand, -fieldingTeam), ntree = 501, importance = TRUE)
# rf.2 <- randomForest(as.factor(hit) ~ ., ntree = 501, data = select(train, -home_team, -fieldingTeam, -stand), importance = TRUE)

print(rf.3) # accuracy does not improve past the selected ntree

plot(rf.3)

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

rf.3_confusion <- confusionMatrix(model = rf.3, x = test, y = test$hit)

#         fits
# actuals    0    1
#       0 5034  426
#       1  799 2197
#                     V1
# sensitivity      0.733
# precision        0.838
# specificity      0.922
# overall_accuracy 0.855
# f1_measure       0.782

# the optimal model appears to be rf.2, which excludes information about the fielding team, the park, and the handedness of the batter. Excluding distance from the model (rf.3), even though we have the location information, appears to significantly reduce the accuracy of the model.

# examine whether the model is missing systematically for certain records

test_set_rows <- test$row

rf.2_full_test_data <- cbind(filter(working_data, row %in% test_set_rows), predict_fit.rf.2$fits)

names(rf.2_full_test_data)[12] <- "fits"

rf.2_mean_table <- select(rf.2_full_test_data, -rows) %>% 
  group_by(hit, fits) %>%
  summarise_each(funs(mean(., na.rm = TRUE),n()))

rf.2_full_test_data$type <- with(rf.2_full_test_data, ifelse(hit == fits, 1, 0))

rf.2_full_test_data$hit_label <- with(rf.2_full_test_data, ifelse(hit == 1, "Hit", "Out"))

# plot the performance of the model based on the three features included

# condensed tab palette

tab_condensed <- c("#006BA4", "#C85200")

rf.2_plot1 <- ggplot(rf.2_full_test_data, aes(hit_angle, hit_distance_sc)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nLaunch Angle") +
  ylab("Batted Ball Distance (ft.)\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

rf.2_plot2 <- ggplot(rf.2_full_test_data, aes(hit_speed, hit_distance_sc)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nSpeed off the Bat") +
  ylab("Batted Ball Distance (ft.)\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

rf.2_plot3 <- ggplot(rf.2_full_test_data, aes(hit_angle, hit_speed)) +
  geom_point(aes(color = as.factor(type)), alpha = .2) + 
  xlab("\nLaunch Angle") +
  ylab("Speed off the Bat\n") +
  facet_wrap(~hit_label) + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed, guide = FALSE)

grid_plot_rf.2 <- grid.arrange(rf.2_plot1, rf.2_plot2, rf.2_plot3, ncol = 3)
grid_plot_rf.2

ggsave("grid_plot_rf.2.png", grid_plot_rf.2, scale = 1.2, width = 11, height = 8.5, units = "in")

# only hit_distance_sc

set.seed(42)
rf.4 <- randomForest(as.factor(hit) ~ ., data = select(train, -row, -home_team, -fieldingTeam, -stand, -hit_speed, -hit_angle), importance = TRUE)
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

rf.4_confusion <- confusionMatrix(model = rf.4, x = test, y = test$hit)

#         fits
# actuals    0    1
#       0 3921 1344
#       1 1516 1675
#                     V1
# sensitivity      0.525
# precision        0.555
# specificity      0.745
# overall_accuracy 0.662
# f1_measure       0.539


## GAM models for hit/no hit

gam.1 <- gam(hit ~ ns(hit_distance_sc,4) + ns(hit_speed,4) + ns(hit_angle,4), data = orig_train, family = "binomial")
summary(gam.1)

predict_fit.gam.1 <- data.frame(fits = predict(gam.1, orig_test, type = "response"), actuals = orig_test$hit)
pred.gam.1 <- prediction(predict_fit.gam.1$fits, predict_fit.gam.1$actuals)
roc.pred.gam.1 <- performance(pred.gam.1, measure = "tpr", x.measure = "fpr")
plot(roc.pred.gam.1)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.gam.1, pred.gam.1)
opt
opt <- opt[3]
predict_fit.gam.1$fits <- with(predict_fit.gam.1, ifelse(fits > opt, 1, 0)) 

confusionMatrix(df = predict_fit.gam.1)

#         fits
# actuals    0    1
#       0 4560  705
#       1  758 2433
#                     V1
# sensitivity      0.762
# precision        0.775
# specificity      0.866
# overall_accuracy 0.827
# f_measure        0.769

gam.2 <- gam(hit ~ s(hit_distance_sc, df = 5) + s(hit_speed, df = 4) + s(hit_angle, df = 4), data = orig_train, family = "binomial")
summary(gam.2)
plot(gam.2, se = TRUE)

predict_fit.gam.2 <- data.frame(fits = predict(gam.2, orig_test, type = "response"), actuals = orig_test$hit)
pred.gam.2 <- prediction(predict_fit.gam.2$fits, predict_fit.gam.2$actuals)
roc.pred.gam.2 <- performance(pred.gam.2, measure = "tpr", x.measure = "fpr")
plot(roc.pred.gam.2)
abline(a = 0, b = 1)
opt <- opt.cut(roc.pred.gam.2, pred.gam.2)
opt
opt <- opt[3]
predict_fit.gam.2$fits <- with(predict_fit.gam.2, ifelse(fits > opt, 1, 0)) 

confusionMatrix(df = predict_fit.gam.2)

#         fits
# actuals    0    1
#       0 4500  765
#       1  761 2430
#                     V1
# sensitivity      0.762
# precision        0.761
# specificity      0.855
# overall_accuracy 0.820
# f_measure        0.761

## predicted probabilities using rf.2

# create a data set with all combinations of angle, speed, and distance

pred_data_emp <- ungroup(statcast.2016) %>% 
  select(hit_distance_sc, hit_angle, hit_speed, hc_x, hc_y) %>%
  round(.) %>%
  unique(.) %>%
  filter(complete.cases(.))

# empirical data

pred_data_emp_scaled <- pred_data_emp

pred_data_emp_scaled$hit_distance_sc <- (pred_data_emp_scaled$hit_distance_sc - center_values[1]) / scale_values[1]

pred_data_emp_scaled$hit_speed <- (pred_data_emp_scaled$hit_speed - center_values[2]) / scale_values[2]

pred_data_emp_scaled$hit_angle <- (pred_data_emp_scaled$hit_angle - center_values[3]) / scale_values[3]

pred_data_emp_scaled$hc_x <- (pred_data_emp_scaled$hc_x - center_values[4]) / scale_values[4]

pred_data_emp_scaled$hc_y <- (pred_data_emp_scaled$hc_y - center_values[5]) / scale_values[5]

pred_prob_hit <- predict(rf.2, pred_data_emp_scaled, type = "prob")[,2]
pred_prob_hit_fits <- cbind(pred_data_emp, pred_prob_hit)

angle_speed_pred <- pred_prob_hit_fits %>% ggplot(aes(hit_speed, hit_angle)) + geom_tile(aes(fill = pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nSpeed off the Bat") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

ggsave("angle_speed_pred.png", angle_speed_pred, scale = 1.2, width = 11, height = 8.5, units = "in")

angle_dist_pred <- pred_prob_hit_fits %>% ggplot(aes(hit_distance_sc, hit_angle)) + geom_tile(aes(fill = pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nBatted Ball Distance") + ylab("Launch Angle\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

ggsave("angle_dist_pred.png", angle_dist_pred, scale = 1.2, width = 11, height = 8.5, units = "in")

speed_dist_pred <- pred_prob_hit_fits %>% ggplot(aes(hit_distance_sc, hit_speed)) + geom_tile(aes(fill = pred_prob_hit), alpha = .5, size = .05, color = "white") + scale_fill_gradient(low = "#006BA4", high = "#C85200", "Probability\nof Hit\n", labels = percent) + xlab("\nBatted Ball Distance") + ylab("Speed off the Bat\n") + ggtitle("\nPredicted Probability of a Hit Using Statcast Data (2016)\n") + theme_bp_grey()

ggsave("speed_dist_pred.png", speed_dist_pred, scale = 1.2, width = 11, height = 8.5, units = "in")

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
# RMSE = [1] 0.1121632

compare_prob_plot <- compare_prob %>% ggplot(aes(ave_prob, empirical_ave)) + geom_point(aes(size = count), alpha = .5, color = "#006BA4") + stat_smooth(method = "lm", color = "#C85200") + scale_x_continuous(labels = percent) + scale_y_continuous(limits = c(0,1), labels = percent)+ xlab("\nPredicted Batting Average") + ylab("Empirical Batting Average\n") + ggtitle("\nPredicted vs. Empirical Batting Average: Statcast Data (2016)\n") + annotate(geom = "text", x = .94, y = .80, label = "RMSE = .112", fontface = "bold") + theme_bp_grey()

compare_prob_plot

ggsave("compare_prob_plot.png", compare_prob_plot, scale = 1.2, width = 11, height = 8.5, units = "in")

# apply predictive model to all Statcast data in 2016

statcast.2016.scaled <- rbind(test, train) %>%
  arrange(row)

# statcast.2016.scaled$hit_distance_sc <- (statcast.2016.scaled$hit_distance_sc - center_values[1]) / scale_values[1]
# 
# statcast.2016.scaled$hit_speed <- (statcast.2016.scaled$hit_speed - center_values[2]) / scale_values[2]
# 
# statcast.2016.scaled$hit_angle <- (statcast.2016.scaled$hit_angle - center_values[3]) / scale_values[3]
# 
# statcast.2016.scaled$hc_x <- (statcast.2016.scaled$hc_x - center_values[4]) / scale_values[4]
# 
# statcast.2016.scaled$hc_y <- (statcast.2016.scaled$hc_y - center_values[5]) / scale_values[5]

statcast.2016.fit <- predict(rf.2, statcast.2016.scaled, type = "prob")[,2] %>%
  as.data.frame()
statcast.2016.scaled <- cbind(statcast.2016.scaled, statcast.2016.fit)
names(statcast.2016.scaled)[11] <- "fit" 
statcast.2016.predicted <- left_join(
  select(statcast.2016.scaled, row, fit),
  statcast.2016, by = "row") %>%
  mutate(fit_dummy = with(., ifelse(fit > rf.2.opt, 1, 0)))

with(statcast.2016.predicted, table(hit, fit_dummy))

statcast.2016.predicted$not_converted <- with(statcast.2016.predicted, ifelse(hit == 1 & fit_dummy == 0, 1, 0))

statcast.2016.predicted$converted_stolen_hit <- with(statcast.2016.predicted, ifelse(hit == 0 & fit_dummy == 1, 1, 0))

conversion_by_team <- filter(statcast.2016.predicted, events != "Home Run") %>% 
  group_by(fieldingTeam) %>% 
  summarise(unexpected_plays = round(sum(ifelse(fit_dummy == 1 & hit == 0, 1, NA)/sum(fit_dummy), na.rm = TRUE),3), efficiency = round((length(hit)-sum(hit))/(length(hit)-sum(fit_dummy)),3)) %>%
  arrange(desc(efficiency))

write.csv(conversion_by_team, "conversion_by_team.csv", row.names = FALSE)            
conversion_by_team_plot <- ungroup(conversion_by_team) %>% ggplot(aes(efficiency, unexpected_plays)) + geom_text(aes(label = fieldingTeam), color = "#006BA4", fontface = "bold") + stat_smooth(method = "lm", color = "#C85200") + scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent)+ xlab("\n% of Expected Outs Made") + ylab("% of Unexpected Outs Converted\n") + ggtitle("\nExpected Outs vs. Unexpected Outs: Statcast Data (2016)\n") + theme_bp_grey()

conversion_by_team_plot

ggsave("conversion_by_team_plot.png", conversion_by_team_plot, scale = 1.2, width = 11, height = 8.5, units = "in")

# plot batted ball location

statcast.2016.predicted$miss <- with(statcast.2016.predicted, ifelse(hit != fit_dummy, "Incorrect", "Correct"))

statcast.2016.predicted$hit_label <- with(statcast.2016.predicted, ifelse(hit == 1, "Hit", "Out")) %>% as.factor()

tab_condensed_factor <- c("#C85200","#006BA4")

misses_plotted <- statcast.2016.predicted %>% 
  ggplot(aes(x = hc_x, y = (hc_y*-1))) + 
  geom_point(aes(color = as.factor(hit_label)), alpha = .5) + 
  xlim(0,250) +
  ylim(-250, 0) +
  ggtitle("\nComparing Correct & Incorrect Model Predictons\n") +
  facet_wrap(~miss) +
  geom_segment(x=128, xend = 19, y=-208, yend = -100) + 
  geom_segment(x=128, xend = 236, y=-208, yend = -100) + 
  theme_battedball_grey() + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) +
  scale_color_manual(values = tab_condensed_factor, "Outcome")

ggsave("misses_plotted.png", misses_plotted, scale = .8, width = 14, height = 8.5, units = "in")

averages_misses_correct_pred <- select(statcast.2016.predicted, miss, hit_distance_sc:hit_angle) %>%
  group_by(miss) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))
  