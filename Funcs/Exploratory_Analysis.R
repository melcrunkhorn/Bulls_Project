library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(performance)
library(psych)
library(ggrepel)

## Reading in data
## tidy data
player_stats_tidy <- read_csv("data/tidy_data/player_stats_tidy2.csv")
View(player_stats_tidy)

## Data frame structure
str(player_stats_tidy)
head(player_stats_tidy, 20)
tail(player_stats_tidy, 20)

#Missing values
sum(is.na(player_stats_tidy))
which(is.na(player_stats_tidy), arr.ind = TRUE)

## filtering data for G >= 20
player_stats_tidy_filtered <- player_stats_tidy %>%
  filter(G >= 20)
view(player_stats_tidy_filtered)

## Distributions of variables ###binwidth issues

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = PTS), colour = "black", fill = "dodgerblue", binwidth = 80)

ggplot(data = player_stats_tidy_filtered, aes(x = PTS)) +
  geom_histogram(colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = FG), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = FGp), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = X3P), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = X3Pp), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = X2P), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = X2Pp), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = ORB), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = DRB), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = AST), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = STL), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = BLK), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = TOV), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = ASTTOVR), colour = "black", fill = "dodgerblue")

ggplot(data = player_stats_tidy_filtered, aes(x = FG)) +
  geom_histogram(colour = "black", fill = "dodgerblue", binwidth = 4)

ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = PTS_per_game), colour = "black", fill = "dodgerblue")
 
ggplot(data = player_stats_tidy_filtered) +
  geom_histogram(mapping = aes(x = PTS_per_min), colour = "black", fill = "dodgerblue")

###Relationships of the variables

player_stats_tidy %>%
  ggplot(mapping = aes(x = player_name, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = player_name, y = FG)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = FG, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = FGp, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = X3P, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = X3Pp, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = X2P, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = X2Pp, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = ORB, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = DRB, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = AST, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = STL, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = BLK, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = TOV, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

player_stats_tidy %>%
  ggplot(mapping = aes(x = eFGp, y = PTS)) +
  geom_point() +
  geom_smooth(method = "lm")

#### group_by position

# between teams variation- compares the differences in mean distance to goals for each attacking team

bw_teams <- df %>%
  filter(outcome == "Goal") %>%
  group_by(attacking_team) %>%
  summarise(mean = mean(distance_to_goal))
bw_teams

# between teams variation- compares the differences in mean distance to goals for each attacking team

bw_pos <- player_stats_tidy_filtered %>%
  group_by(Pos) %>%
summarise(PTS_per_min)

view(bw_pos)

bw_pos <- player_stats_tidy_filtered %>%
  group_by(player_name) %>%
  summarise(PTS_per_min)

view(bw_pos)

###how much do these players in these positions differ by
between_sd <- sd(bw_pos$PTS_per_min)
between_sd

wn_pos <- player_stats_tidy_filtered %>%
  filter(Pos == "SG") %>%
  group_by(player_name) %>%
  summarise(PTS_per_min)
wn_pos

within_sd <- sd(wn_pos$PTS_per_min)
within_sd
## see differences between positions and also within positions 
###select variables and then do scatterplots on each of them for exploratory analysis




