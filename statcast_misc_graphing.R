# Load packages and functions

require(pacman)
p_load(RMySQL, dplyr, magrittr, reshape2, ggplot2, grid, gridExtra, devtools)

# install baseballr package

install_github("BillPetti/baseballr")
require(baseballr)
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

# acquire a list of MLBAM ids for hitters that have put at least one ball in play during the 2016 season

# connect to the FanGraph's database

con <- dbConnect(RMySQL::MySQL(), dbname = fg_dbname, username = fg_username, password = fg_password, host = fg_host, port = 3306)

mlbamid.df.2016 <- dbGetQuery(con, "select * from gd_savant where type = 'X' order by batter") 

lookup <- dbGetQuery(con, "select batter, b_height from gd_pitch where type = 'X' and substr(GameDate, 1, 4) > 2014 group by batter") 

mlbamid.df.2016 %<>% mutate(year = substr(game_date, 1, 4))

mlbamid.df.2016 <- mlbamid.df.2016 %>% left_join(lookup, by = "batter")

dbDisconnect(con)

# code the type of batted ball outcome

statcast.2016$hit_type <- with(statcast.2016, ifelse(grepl("Single", statcast.2016$events), 1, ifelse(grepl("Double", statcast.2016$events), 2, ifelse(grepl("Triple", statcast.2016$events), 3, ifelse(grepl("Home Run", statcast.2016$events), 4, 0))))) %>%
  as.factor()

# code the run values for each outcome (based on 2016 run values)

statcast.2016$run_value <- with(statcast.2016, ifelse(hit_type == 1, .462, ifelse(hit_type == 2, .762, ifelse(hit_type == 3, 1.03, ifelse(hit_type == 4, 1.4, 0))))) %>%
  as.numeric()

# recode stand as factor

statcast.2016$stand <- as.factor(statcast.2016$stand)

# code each batted ball for edge location using baseballr package

names(mlbamid.df.2016)[17] <- "des2"

test <- edge_code(mlbamid.df.2016)

test$count <- with(test, paste0(balls, "-", strikes))
test$pitch_category <- with(test, ifelse(pitch_type %in% c("FF", "FC", "SI", "FT", "FA", "FO"), "Fastballs", "Other"))
test$velocity_range <- with(test, ifelse(start_speed <= 75, "Less than 75 mph", ifelse(start_speed > 75 & start_speed <= 80, "75 to 80 mph", ifelse(start_speed > 80 & start_speed <= 85, "80 to 85 mph", ifelse(start_speed > 85 & start_speed <= 90, "85 to 90 mph", ifelse(start_speed > 90 & start_speed <= 95, "90 to 95 mph", ifelse(start_speed > 95 & start_speed <= 100, "95 to 100 mph", "> 100 mph")))))))

test2 <- test %>%
  filter(hit_distance_sc != "null" & hit_angle != "null" & hit_speed != "null") %>% filter(!events %in% c("Bunt Lineout", "Sac Bunt", "Bunt Pop Out", "Sacrifice Bunt DP", "Bunt Groundout"))

test2$hit_speed %<>% as.numeric()
test2$hit_angle %<>% as.numeric()
test2$hit_distance_sc %<>% as.numeric()
test2$release_spin_rate %<>% as.numeric()
test2$stand %<>% factor(levels = c("R", "L"), labels = c("Right Handed Batter", "Left Handed Batter"))
test2$count %<>% as.factor()
test2$count %<>% factor(levels = c("3-0", "3-1", "2-0", "3-2", "1-0", "2-1", "0-0", "1-1", "2-2", "0-1", "1-2", "0-2"))
test2$pitch_category %<>% as.factor()
test2$velocity_range %<>% factor(levels = c("Less than 75 mph", "75 to 80 mph", "80 to 85 mph", "85 to 90 mph", "90 to 95 mph", "95 to 100 mph", "> 100 mph"))

# Launch Angle and Pitch Location, by handedness

set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= -1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  group_by(stand) %>% 
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(px, hit_angle)) +
  geom_point(alpha = .1) + 
  stat_smooth() + 
  geom_vline(xintercept = .9, linetype = 2) +
  geom_vline(xintercept = -.9, linetype = 2) + 
  annotate("rect", xmin = -2, xmax = 2, ymin = 12, ymax = 28, alpha = .2, fill = "#C85200") + 
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  facet_grid(~stand) + 
  ggtitle("\nHorizontal Pitch Location and Launch Angle, by Batter Handedness\n") + 
  xlab("\nHorizontal Location of Pitch, Catcher's View\n") + 
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Horiz_Angle.png", scale = 1.2, width = 14, height = 8.5, units = "in")

test2 %>%
  filter(px <= 1.5 & px >= -1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  group_by(stand) %>%
  summarise(mean(hit_angle))

# Launch by Vertical Angle and Stand

set.seed(42)
test2 %>%
  filter(pz <= 5 & pz >= 1 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  group_by(stand) %>% 
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(hit_angle, pz)) +
  geom_point(alpha = .1) + 
  stat_smooth() + 
  geom_hline(yintercept = 3.5, linetype = 2) +
  geom_hline(yintercept = 1.5, linetype = 2) + 
  annotate("rect", xmin = 12, xmax =28, ymin = 0, ymax = Inf, alpha = .2, fill = "#C85200") + 
  scale_y_continuous(breaks = seq(from = 0, to = 5, by = .5)) + 
  scale_x_continuous(breaks = seq(from = -80, to = 80, by = 10)) +
  facet_grid(~stand) + 
  ggtitle("\nVertical Pitch Location and Launch Angle, by Batter Handedness\n") +
  xlab("\nLaunch Angle of Batted Ball in Degrees\n") +
  ylab("\nVertical Location of Pitch\n") + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Vert_Angle.png", scale = 1.2, width = 14, height = 8.5, units = "in")

# by pitch category, by handedness

set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= -1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(px, hit_angle)) +
  geom_point(alpha = .1) + 
  stat_smooth() + 
  geom_vline(xintercept = .9, linetype = 2) +
  geom_vline(xintercept = -.9, linetype = 2) + 
  annotate("rect", xmin = -2, xmax = 2, ymin = 12, ymax = 28, alpha = .2, fill = "#C85200") + 
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  facet_wrap(stand~pitch_category) + 
  ggtitle("\nHorizontal Pitch Location and Launch Angle, by Batter Handedness and Pitch Category\n") + 
  xlab("\nHorizontal Location of Pitch, Catcher's View\n") + 
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Horiz_Angle_PitchCategory.png", scale = 1.2, width = 16, height = 12, units = "in")

# by velocity range, by handedness

set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= -1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(px, hit_angle)) +
  geom_point(alpha = .1) + 
  stat_smooth() + 
  geom_vline(xintercept = .9, linetype = 2) +
  geom_vline(xintercept = -.9, linetype = 2) + 
  annotate("rect", xmin = -2, xmax = 2, ymin = 12, ymax = 28, alpha = .2, fill = "#C85200") + 
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  facet_wrap(stand~velocity_range, nrow = 2) + 
  ggtitle("\nHorizontal Pitch Location and Launch Angle, by Batter Handedness and Pitch Velocity Range\n") + 
  xlab("\nHorizontal Location of Pitch, Catcher's View\n") + 
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Horiz_Angle_VeloRange.png", scale = 1.2, width = 18, height = 12, units = "in")

# by velocity range, by handedness

set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= -1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>% 
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(px, hit_angle)) +
  geom_point(alpha = .2) + 
  stat_smooth() + 
  geom_vline(xintercept = .9, linetype = 2) +
  geom_vline(xintercept = -.9, linetype = 2) + 
  annotate("rect", xmin = -2, xmax = 2, ymin = 12, ymax = 28, alpha = .2, fill = "#C85200") + 
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  facet_wrap(stand~velocity_range, nrow = 2) + 
  ggtitle("\nHorizontal Pitch Location and Launch Angle, by Batter Handedness and Pitch Velocity Range\n") + 
  xlab("\nHorizontal Location of Pitch, Catcher's View\n") + 
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Horiz_Angle_VeloRange.png", scale = 1.1, width = 22, height = 15, units = "in")

# descriptive stats

# angle by stand

test2 %>%
  filter(pz <= 4 & pz >= 1 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  group_by(stand) %>%
  summarise(round(mean(hit_angle), 1))

# angle by pitch velo and stand

angle_by_pitchvelo <- test2 %>%
  filter(pz <= 5 & pz >= 1 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  group_by(stand, velocity_range) %>%
  summarise("average_launch_angle" = round(mean(hit_angle), 1))

angle_velo_aov <- aov(formula = hit_angle ~ stand*velocity_range, data = test2)

summary(angle_velo_aov)

TukeyHSD(angle_velo_aov)

# angle by edge and stand

angle_by_edge <- test2 %>%
  filter(pz <= 4 & pz >= 1 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  group_by(stand, location) %>%
  summarise(average_launch_angle = round(mean(hit_angle), 1))

angle_edge_aov <- aov(formula = hit_angle ~ stand*location, data = test2)

summary(angle_edge_aov)

TukeyHSD(angle_edge_aov)

# plot angle by edge and stand

angle_by_edge %>%
  filter(location != "Out of Zone") %>%
  ggplot(aes(location, average_launch_angle, fill = stand)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = .9) + 
  geom_text(aes(label = average_launch_angle), position = position_dodge(width = 1), vjust = -.5, fontface = "bold", size = 4.5) + 
  ggtitle("\nAverage Launch Angle by Batter Handedness and Pitch Location\n") + 
  xlab("\nPitch Location\n") + 
  ylab("\nAverage Launch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() +
  theme(legend.position = "top") + 
  scale_fill_manual(values = tab_condensed, "")
    
ggsave("Launch_Angle_Edge.png", scale = 1.2, width = 14, height = 8.5, units = "in")

# mean angle by count and stand

angle_by_count <- test2 %>%
  filter(px <= 1.5 & px >= - 1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  group_by(stand, count) %>%
  summarise("average_launch_angle" = round(mean(hit_angle), 1))

# plot angle by velo and stand

angle_by_pitchvelo %>%
  ggplot(aes(velocity_range, average_launch_angle, fill = stand)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = .9) + 
  geom_text(aes(label = average_launch_angle), position = position_dodge(width = 1), vjust = -.5, fontface = "bold", size = 4.5) + 
  ggtitle("\nAverage Launch Angle by Batter Handedness and Pitch Velocity\n") + 
  xlab("\nPitch Velocity Range\n") + 
  ylab("\nAverage Launch Angle of Batted Ball in Degrees\n") +
  theme_bp_grey() +
  theme(legend.position = "top") + 
  scale_fill_manual(values = tab_condensed, "")

ggsave("Launch_Angle_PitchVelo.png", scale = 1.2, width = 14, height = 8.5, units = "in")

# exploded boxplot -- edge and stand

test2 %>%
  filter(location != "Out of Zone") %>%
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(location, hit_angle)) + 
  geom_jitter(alpha = .2) +
  geom_boxplot(outlier.shape = NA, color = "#C85200", alpha = 0) +
  ggtitle("\nExploded Boxplots of Average Launch Angle by Batter Handedness and Pitch Location\n") + 
  xlab("\nPitch Location\n") + 
  ylab("\nAverage Launch Angle of Batted Ball in Degrees\n") +
  facet_grid(stand~.) + 
  theme_bp_grey() +
  theme(legend.position = "top", strip.text.y = element_text(size = 14, face = "bold")) + 
  scale_fill_manual(values = tab_condensed, "")

ggsave("Exploded_Launch_Angle_Edge.png", scale = 1.1, width = 16, height = 10.5, units = "in")

# hit_angle by release_spin_rate

set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= -1.5 & pz <= 4 & pz >= 1 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(release_spin_rate, hit_angle)) +
  #annotate("rect", xmin = -.9, xmax = .9, ymax = 3.5, ymin = 1.5, alpha = 0, color = "black") + 
  geom_point(alpha = .3) +
  stat_smooth() +
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  #scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) +
  facet_grid(~stand) + 
  ggtitle("\Release Spin Rate of Pitch and Launch Angle, by Batter Handedness\n") +
  xlab("\nRelease Spin Rate of Pitch\n") +
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") + 
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))




set.seed(42)
test2 %>%
  filter(px <= 1.5 & px >= - 1.5 & hit_angle <= 80, hit_angle >= -80 & hit_speed >= 90) %>%
  sample_frac(size = .25, replace = FALSE) %>%
  ggplot(aes(px, hit_angle)) +
  geom_point(alpha = .2) + 
  stat_smooth() + 
  geom_vline(xintercept = .9, linetype = 2) +
  geom_vline(xintercept = -.9, linetype = 2) + 
  annotate("rect", xmin = -1.5, xmax = 1.5, ymin = 12, ymax = 28, alpha = .2, fill = "#C85200") +
  facet_wrap(stand~count, nrow = 2) + 
  scale_y_continuous(breaks = seq(from = -80, to = 80, by = 10)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 1.5, by = .5)) + 
  xlab("\nHorizontal Pitch Location, Catcher's View\n") +
  ylab("\nLaunch Angle of Batted Ball in Degrees\n") + 
  labs(title = "\nHorizontal Location of Pitch and Launch Angle, by Batter Handedness\n", caption = "Source: 25% sample of Statcast data through 8/11/2016\nIncludes batted balls >= 90 mph\nAcquired atbaseballsavant.com") +
  theme_bp_grey() + 
  theme(strip.text.x = element_text(size = 14, face = "bold"))

ggsave("Launch_Angle_Horiz_Count.png", scale = 1.2, width = 24, height = 14, units = "in")
