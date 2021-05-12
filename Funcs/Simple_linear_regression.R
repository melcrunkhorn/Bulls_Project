library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(broom)
library(performance)
library(psych)
library(ggrepel)

## read in the data
player_stats_tidy <- read_csv("data/tidy_data/player_stats_tidy.csv")
View(player_stats_tidy)

## filtering data for G >= 20
player_stats_tidy_filtered <- player_stats_tidy %>%
  filter(G >= 20)
view(player_stats_tidy_filtered)

## Team stats 1
df_team_Stats1 <- read_csv("data/project_data/2018-19_nba_team-statistics_1.csv")
View(df_team_Stats1)


##missing values 

sum(is.na(df_team_Stats1)) 

##checking for missing values in teams stats 2 
sum(is.na(df_team_Stats2))

mean_points <- player_stats_tidy_filtered %>%
  group_by(Team = "Chicago Bulls") %>%
  summarise(mean_PTS = mean(PTS))


###Exploratory Data Analysis
##does eFG% correlate with wins 
##changing the column name of eFG%

names(df_team_Stats1)[18] <- "eFGp"
view(df_team_Stats1)

## relationship between eFGp and points
ggplot(data = df_team_Stats1, aes(x = eFGp, y = W)) + 
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed")

### There appears to be a linear relationship between effective field goal percentage and wins.
### As effective field goal percentage increases, there is an increase in wins.

#correlation co-efficient eFGp and W

with(df_team_Stats1, cor(x = eFGp, y = W))

##The correlation co-efficient = 0.78 suggesting a strong positive correlation between 
### eFGp and wins as 0.78 is approaching the value of 1. 

# Simple Linear Regression

fit <- lm(W ~ eFGp, data = df_team_Stats1)
tidy(fit,conf.int = TRUE)

summary(fit)

### The intercept co-efficient = -282.36, meaning that when wins are 0,
## the expected effective field goal percentage = -282.36, which does
## not make much practical sense, but is a starting point for the model.
## The slope co-efficient = 616.90, meaning that for every 1 unit that eFGp is 
## increased, expected wins increase by 616.90.
### The r squared value = 0.5974, meaning that 59.76% of the variance in wins
### is explained by the variance in effective field goal percentage.

lm(formula = W ~ eFGp, data = df_team_Stats1)

#Independence 
library(Lahman)
library(car)
car::durbinWatsonTest(fit)

### the durbinwatson statistic = 1.35, which is close to the recommended value of 2
### meaning that this assumption is not failed and there is independence of observations.

# Are there any outliers

std_res <- rstandard(fit)
points <- 1:length(std_res)
library(ggplot2)

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## There does not appear to be any outliers as all standardised residuals are less 
### than 3.

# Leverage Points

hats <- hatvalues(fit)
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

### There are no hat values greater than 1, however it will be useful to 
## investigate the points above 0.1, as they appear to stand out from the rest of 
## the values. 

#investigate points above 0.10

hat_labels <- if_else(hats >= 0.10, paste(points), "")
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.005)

ggplot(data = df_team_Stats1, aes(x = eFGp, y = W)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed") +
  geom_text(aes(label = hat_labels), nudge_x = 1)

## Determine if the points could be considered high influence

cook <- cooks.distance(fit)
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

cook_labels <- if_else(cook >= 0.015, paste(points), "")

# investigate points above 0.10 that are standing out above the rest

cook_labels <- if_else(cook >= 0.10, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_x = 1)

ggplot(data = df_team_Stats1, aes(x = eFGp, y = W)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype ="dashed") +
  geom_text(aes(label = cook_labels), nudge_x = 1)

###There are two teams that could be influencing the model.

## Create a new df without the high influencing points

outliers <- c(2, 28)
df_team_Stats1_filtered <- df_team_Stats1 %>%
  filter(!case_no %in% outliers)

# re-run linear regression with filtered_df
fit2 <- lm(W ~ eFGp, data = df_team_Stats1_filtered)
tidy(fit2, conf.int= TRUE)

summary(fit2)
call:
  lm(formula = W ~ eFGp, data = df_team_Stats1_filtered)

## plot without high influence points

ggplot(data = df_team_Stats1_filtered, aes(x = eFGp, y = W)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed")

# homoscedasticity - test for homoscedasticity by plotting the residuals against the 
##fitted values.

res <- residuals(fit)
fitted <- predict(fit)

ggplot(data = NULL, aes(x = fitted, y =res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

### There does not appear to be evidence of heteroscedasticity


# Normality - are the residuals normally distrubuted

ggplot(data = NULL, aes(x = res)) +
  geom_histogram(colour = "black", fill = "dodgerblue", binwidth = 10)

## There appears to be some slight skewness, likely from the points investigated for 
##influence. These values did not appear to be influencing the results of the model.
##To address this slightly non-normal distribution a possible option is to collect more data, 
### and also there are potentially other factors that contribute to winning.

ggplot(data = NULL, aes(sample = res)) +
  stat_qq() + stat_qq_line()
