## Chicago Bulls - Multiple linear regressions
## Using df_nonTOT_tidy


library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(performance)
library(psych)
library(ggrepel)


##Read in data
df_nonTOT_tidy <- read_csv("data/tidy_data/df_nonTOT_clean.csv")
view(df_nonTOT_tidy)

## relocate PTS_per_MP and PTS_per_game
df_nonTOT_tidy <- df_nonTOT_tidy %>%
  relocate(PTS_per_MP, .after = eFGp)
df_nonTOT_tidy <- df_nonTOT_tidy %>%
  relocate(PTS_per_game, .after = PTS_per_MP)



## create new df "dat_SF" 
dat_SF <- df_nonTOT_tidy %>% filter(df_nonTOT_tidy$Pos == "SF")
## create new df "dat_SG" 
dat_SG <- df_nonTOT_tidy %>% filter(df_nonTOT_tidy$Pos == "SG") 
## create new df "dat_PF" 
dat_PF <- df_nonTOT_tidy %>% filter(df_nonTOT_tidy$Pos == "PF")
## create new df "dat_PG" 
dat_PG <- df_nonTOT_tidy %>% filter(df_nonTOT_tidy$Pos == "PG") 
## create new df "dat_C" and create variables per game
dat_C <- df_nonTOT_tidy %>% filter(df_nonTOT_tidy$Pos == "C")

## Write_csv data file for each Pos
write_csv(dat_SF, file = "data/tidy_data/dat_SF.csv")
write_csv(dat_SG, file = "data/tidy_data/dat_SG.csv")
write_csv(dat_PF, file = "data/tidy_data/dat_PF.csv")
write_csv(dat_PG, file = "data/tidy_data/dat_PG.csv")
write_csv(dat_C, file = "data/tidy_data/dat_C.csv")


summary(df_nonTOT_tidy$Pos)

head(df_nonTOT_tidy, 20)

df_nonTOT_tidy <- df_nonTOT_tidy %>%
  filter(df_nonTOT_tidy$G >= 20)

## Change Pos to factor (C = 1, PF = 2, PG = 3, SF = 4, SG = 5)
df_nonTOT_tidy$Pos <- as.factor(df_nonTOT_tidy$Pos)
str(df_nonTOT_tidy$Pos)

table(df_nonTOT_tidy$Pos)
levels(df_nonTOT_tidy$Pos)

  ## eFG and Team Wins
ggplot(df_nonTOT_tidy, aes(x = eFGp, y = W)) +
  geom_point(alpha = 0.5, colour = "purple") +
  geom_smooth(method = "lm")

## PTS_per_MP vs W
ggplot(df_nonTOT_tidy, aes(x = PTS_per_MP, y = W)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

### eFG% vs PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = PTS_per_MP, y = eFGp)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

### TrV vs PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = PTS_per_MP, y = TrV)) +
  geom_point(alpha = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm")

### eFF vs W
ggplot(df_nonTOT_tidy, aes(x = EFF, y = W)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

### Tm_use_total vs W
ggplot(df_nonTOT_tidy, aes(x = Tm_use_total, y = W)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## PTS_per_game vs W
ggplot(df_nonTOT_tidy, aes(x = PTS_per_game, y = W)) +
  geom_point(alpha = 0.5, colour = "green") +
  geom_smooth(method = "lm")

### STL_MP vs W
ggplot(df_nonTOT_tidy, aes(x = STL_MP, y = W)) +
  geom_point(alpha = 0.5, fill = "red") +
  geom_smooth(method = "lm")

### TOV_MP vs W
ggplot(df_nonTOT_tidy, aes(x = TOV_MP, y = W)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## Age vs EFF
ggplot(df_nonTOT_tidy, aes(x = Age, y = EFF)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## Age vs eFGp
ggplot(df_nonTOT_tidy, aes(x = Age, y = eFGp)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## Age vs Tm_use_total
ggplot(df_nonTOT_tidy, aes(x = Age, y = Tm_use_total)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

##Box plot of position vs PTS_per_MP
ggplot(data = df_nonTOT_tidy, aes(x = Pos, y = PTS_per_MP)) +
  geom_boxplot(aes(fill = Pos)) +
  scale_fill_discrete(name = "Pos", labels = c("1 = C", "2 = PF", "3 = PG","4 = SF", "5 = SG"))

##Multiple linear regression
#response variable pts_per_min, explan variables eFGp, STL_MP, TOV_MP, Age,TRB

## Relationship between variables

## Age v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = Age, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## eFGp v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = eFGp, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## STL_MP v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = STL_MP, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## TOV_MP v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = TOV_MP, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## TRB v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = TRB, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## X3Pp v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = X3Pp, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

## X2Pp v PTS_per_MP
ggplot(df_nonTOT_tidy, aes(x = X2Pp, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "magenta") +
  geom_smooth(method = "lm")

##Muticollinearity - took out X3Pp and X2Pp as error figure margins too large
pairs(formula = PTS_per_MP ~ Age + eFGp + STL_MP + TOV_MP + TRB, data = df_nonTOT_clean)
### correct graphs did not run 

### Multiple linear regression
fit <- lm(PTS_per_MP ~ Age + eFGp + STL_MP + TOV_MP + TRB, data = df_nonTOT_clean)
tidy(fit,conf.int = TRUE)

summary(fit)


### Multiple linear regression interpretation example 
# Intercept coefficient = when all continuous explanatory variables are equal to 0, the estimated points per minute are 7.69.

# slope coef for Kicks = when Kicks are increased by 1, the goals decreases by 0.0408, when all other variables remain fixed.

# slope coef for Marks = when Marks are increased by 1, goals increases by 0.000401, when all other variables remain fixed.

# slope coef for Handballs = when Handballs are increased by 1, goals increase by 0.00372, when all other variables remain fixed.

# slope coef for Inside.50s = when Inside.50s are increased by 1, goals increase by 0.242, when all other variables remain fixed.

# slope coef for Marks.Inside.50 = when Marks.Inside.50 are increased by 1, goals increase by 0.379, when all other variables remain fixed.
## Testing for independence
car::durbinWatsonTest(fit)

### the durbinwatson statistic = 2.12, which is close to the recommended value of 2
### meaning that this assumption is not failed and there is independence of observations.

##outliers
std_res <- rstandard(fit)
points <- 1:length(std_res)
library(ggplot2)

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

##There appears to be 2 outliers with points above 3 and below -3. 
### label the outliers

res_labels <- if_else(abs(std_res) >= 2.5, paste(points), "")

ggplot(data = NULL, aes(x = points, y = std_res, label = res_labels)) +
  geom_point() +
  geom_text(nudge_x = 2) +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")


# Leverage Points

hats <- hatvalues(fit)
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

### There are no hat values greater than 1, however it will be useful to 
## investigate the points above 0.075, as they appear to stand out from the rest of 
## the values. 

#investigate points above 0.075

hat_labels <- if_else(hats >= 0.075, paste(points), "")
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.005)

## Determine if the points could be considered high influence

cook <- cooks.distance(fit)
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

cook_labels <- if_else(cook >= 0.015, paste(points), "")

# investigate points above 0.025 that are standing out above the rest

cook_labels <- if_else(cook >= 0.025, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_x = 1)

## Create a new df without the high influencing points

outliers <- c(32, 55,104, 192, 222, 237)
df_nonTOT_clean_filtered <- df_nonTOT_clean %>%
  filter(!case_no %in% outliers)

# re-run linear regression with filtered_df
fit2 <- lm(PTS_per_MP ~ Age + eFGp + STL_MP + TOV_MP + TRB, data = df_nonTOT_clean_filtered)
tidy(fit2, conf.int= TRUE)

summary(fit2)


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

### Multicolinearity among explanatory variables
car::vif(fit)

### all vif values are quite close to 1, so does not appear to be any 
## multicollinearity influencing the standard errors of the co-efficients

### Linearity

car::avPlots(fit)
## error figure margins too large




## 
ggplot(data = df, aes(x = weight, y = vo2_max, colour = gender)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_fill_discrete(name = "Gender", labels = c("O = Female", "1 = Male"))


##

## Variables that show slight significance to increased Wins : TM_use_total, EFF, PTS_per_MP (possible leverage point with game)
## Variables that show increase in TrV: PTS_per_MP,
## Increased PTS_per_MP shows positive trend/increase eFGp 
## Increased Age ~up to 30 shows increased trend of EFF, slight decrease trend in Tm_use_total with increasing Age




# more walks per game are seeing greater total runs per game. Showing a moderate strength relationship.
##Could use correlation coefficient to find figure

lm_bb <- lm(R_per_game ~ BB_per_game, data = dat)
tidy(lm_bb, conf.int = TRUE)

# shows that teams with 0 walks will be able to get 2.17 runs from other means
## team will increase walks by 1, we would see .752 runs per game increase. with a 95% CI of between 1.92 and 2.41.

## counfounding variables - HR hitters get more walks. Due to pitchers purposely 
# pitching outside of strike zone.

ggplot(dat, aes(x = HR_per_game, y = BB_per_game)) +
  geom_point(alhpa = 0.5, colour = "darkgreen") +
  geom_smooth(method = "lm")

# shows some relationship of HR and walks

ggplot(dat, aes(x = HR_per_game, y = R_per_game)) +
  geom_point(alhpa = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm")
#strong positive relationship. 


## Pairs plot for visualising relationship between > 2 variables

pairs(formula = ~R_per_game + BB_per_game + HR_per_game, data = dat)

## HOw many more R_per_game when controlling BB_per_game

lm_bb_hr <- lm(R_per_game ~ BB_per_game + HR_per_game, data = dat)
tidy(lm_bb_hr, conf.int = TRUE)

# CI = 1.45 - 1.85
# Y-intercept = 1.65, BB_per_game = 0.476 and HR_per_game = 1.37

## for every 1 increase of BB_per_game, R_per_game will increase by .476

fit <- lm(R_per_game ~ BB_per_game + HR_per_game + S_per_game + D_per_game + T_per_game,
          data = dat)
tidy(fit, conf.int = TRUE)


### Predicted value formula important to remember

# as such if 
#HR_per_game = 1.5
#BB_per_game = 3.5
#S_per_game = 6.2
#D_per_game = 2.2
#T_per_game = 0.2
-3.00 + 0.358 * 1.5 + 1.481 * 3.5 + 0.564 * 6.2 + 0.730 * 2.2 + 1.270 * 0.2
## = 8.0773


## can use head(predict(fit)) to give summary of top 6 using prediction formula without specifically saying which values to use.

### avPlot used to assess R_per_game against all other variables.
car::avPlots(fit)

### multicollinearity can be done by hand using a pairs plot. We want some relationship,
# however not a direct linear relationship

#VIF - variance inflation factor.
car::vif(fit)
# next step - need to square root output to find actual variance

sqrt(car::vif(fit))

# looking aware for values close to 5 for VIF as would imply high multicolinearity


## can use model to then fit into model.

new_dat <- Teams %>% 
  filter(yearID == 2016) %>%
  mutate(HR_per_game = HR / G,
         R_per_game = R / G,
         SB_per_game = SB / G,
         BB_per_game = BB / G,
         S_per_game = (H-HR-X2B-X3B)/G,
         D_per_game = X2B / G,
         T_per_game = X3B / G)

summary(new_dat$yearID)
### New varaiable creation
new_dat <- mutate(new_dat, exp_R_per_game = predict(fit, newdata = new_dat))

view(new_dat)

## model testing
ggplot(new_dat, aes(exp_R_per_game, R_per_game, label = teamID)) +
  geom_point(colour = "dodgerblue") +
  geom_text(nudge_x = 0.1, cex = 3) +
  geom_abline(linetype = "dashed", colour = "magenta")

## dashed line is if everything was predicted correctly, would line up on line
# points above the line - under estimated, below the line = over estimated

# Looking at correlation of Wins to exp_R_game

ggplot(new_dat, aes(x = W, y = exp_R_per_game, label = teamID)) +
  geom_point(colour = "dodgerblue") +
  geom_text(nudge_x = 2, cex = 3)
  
## creating player metric, player specific (some confounding issues to player details, need to normalise to the opportunities a player gets)

pa_per_game <- Batting %>%
  filter(yearID == 2016) %>%
  group_by(teamID) %>%
  summarise(pa_per_game = sum(AB + BB + HBP + SF + SH)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

pa_per_game

### also to include other player
players <- Batting %>% filter(between(yearID, 2011, 2015)) %>%
  group_by(playerID) %>%
  mutate(PA = AB + BB + HBP + SF + SH)

head(players, 10)

## now for plate appearances rate

players <- players %>%
  summarise(G = sum(PA) / pa_per_game,
            BB_per_game = sum(BB) / G, 
            S_per_game = sum(H-X2B-X3B-HR) / G,
            D_per_game = sum(X2B) / G, 
            T_per_game = sum(X3B) / G, 
            HR_per_game = sum(HR) / G,
            BA_per_game = sum(H) / sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 200) %>%
  select(-G)

players


## Expected runs player metric

players <- mutate(players, exp_Runs = predict(fit, newdata = players))

players %>%
  ggplot(aes(x = exp_Runs)) +
  geom_histogram(binwidth = 0.5, colour = "black", fill = "dodgerblue")

## adding salary and position

players <- Salaries %>%
  filter(yearID == 2016) %>%
  select(playerID, salary) %>%
  right_join(players, by = "playerID")

view(players)

## now to assess data frame
players %>% 
  select(nameFirst, nameLast, POS, salary, exp_Runs) %>% 
  arrange(desc(exp_Runs), salary) %>%
  top_n(10)


## scatter plot

players %>% ggplot(aes(x = salary/1000000, y = exp_Runs, color = POS)) + 
  geom_point() +
  xlab("Salary (Millions)")




