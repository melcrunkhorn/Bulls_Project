## Chicago Bulls - Multiple linear regressions
## Using df_nonTOT_clean looking at PTS_per_MP


library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(performance)
library(psych)
library(ggrepel)
library(plotly)


##Read in data df_nonTOT_clean
df_nonTOT_clean <- read_csv("data/tidy_data/df_nonTOT_clean.csv")


## relocate PTS_per_MP and PTS_per_game
df_nonTOT_clean <- df_nonTOT_clean %>%
  relocate(PTS_per_MP, .after = eFGp)
df_nonTOT_clean <- df_nonTOT_clean %>%
  relocate(PTS_per_game, .after = PTS_per_MP)
df_nonTOT_clean <- df_nonTOT_clean %>%
  relocate(TRB_MP, .after = PTS_per_game)
df_nonTOT_clean <- df_nonTOT_clean %>%
  relocate(TRB, .after = TRB_MP)

df_nonTOT_clean <- df_nonTOT_clean %>%
  mutate(WinP_Tm = ((W / Tm_G) * 100)) 

df_nonTOT_clean <- df_nonTOT_clean %>%
  relocate(WinP_Tm, .after = L)


## create new df "datC" and create variables/game
datC <- df_nonTOT_clean %>% filter(df_nonTOT_clean$Pos == "C")

## create new df "datPF" 
datPF <- df_nonTOT_clean %>% filter(df_nonTOT_clean$Pos == "PF")

## create new df "datPG" 
datPG <- df_nonTOT_clean %>% filter(df_nonTOT_clean$Pos == "PG") 

## create new df "datSF" 
datSF <- df_nonTOT_clean %>% filter(df_nonTOT_clean$Pos == "SF")

## create new df "datSG" 
datSG <- df_nonTOT_clean %>% filter(df_nonTOT_clean$Pos == "SG") 

## Write_csv data file for each Pos
write_csv(datC, file = "data/tidy_data/datC.csv")
write_csv(datPF, file = "data/tidy_data/datPF.csv")
write_csv(datPG, file = "data/tidy_data/datPG.csv")
write_csv(datSF, file = "data/tidy_data/datSF.csv")
write_csv(datSG, file = "data/tidy_data/datSG.csv")

summary(df_nonTOT_clean$Pos)

head(df_nonTOT_clean, 20)

df_nonTOT_clean <- df_nonTOT_clean %>%
  filter(df_nonTOT_clean$G >= 40)

## Change Position variable to a factor (C = 1, PF = 2, PG = 3, SF = 4, SG = 5)
df_nonTOT_clean$Pos <- as.factor(df_nonTOT_clean$Pos)
  str(df_nonTOT_clean$Pos)

table(df_nonTOT_clean$Pos)
levels(df_nonTOT_clean$Pos)

 

lm_bb <- lm(PTS_per_MP ~ EFF, data = df_nonTOT_clean)
tidy(lm_bb, conf.int = TRUE)

# This demonstrates that players with 0 Pts_per_MP will be able to get 0.273 Pts_per_MP from other means of scoring points.
## Players who have an increase in EFF by 1, would have 0.000178 increase in PTS_per_MP, with a 95% CI of between 0.253-0.293.

## Confounding variables: Positions that require players to conduct a large volume of shooting may have greater missed shots,as they are taking more shots, 
## so their eFGp may not directly mean a win has occurred.
## There are other game play factors that contribute to scoring points in basketball, including the fact that five players are required to work together to score points.
## A good SG in a poorly performing team, will demonstrate possible poor trend of statistics and winning totals.
## A good SG in a poorly performing team, will demonstrate possible poor results due to the individual player working hard, but not winning games.

ggplot(df_nonTOT_clean, aes(x = eFGp, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "darkgreen") +
  geom_smooth(method = "lm")

# shows moderate relationship of eFGp and PTS_per_MP

ggplot(df_nonTOT_clean, aes(x = eFGp, y = WinP_Tm)) +
  geom_point(alpha = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm")
#strong positive relationship. 

ggplot(df_nonTOT_clean, aes(x = Age, y = WinP_Tm)) +
  geom_point(alpha = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm")

ggplot(df_nonTOT_clean, aes(x = PTS_per_MP, y = WinP_Tm)) +
  geom_point(alpha = 0.5, colour = "darkgreen") +
  geom_smooth(method = "lm")

ggplot(df_nonTOT_clean, aes(x = EFF, y = PTS_per_MP)) +
  geom_point(alpha = 0.5, colour = "dodgerblue") +
  geom_smooth(method = "lm")


## Pairs plot for visualising relationship between > 2 variables

pairs(formula = ~ PTS_per_MP + eFGp + TRB_MP + Tm_use_total + EFF + TrV, data = df_nonTOT_clean)

## How many more Wins occur when controlling for other factors

lm_bb_hr <- lm(PTS_per_MP ~  eFGp + TRB_MP + Tm_use_total + EFF + TrV, data = df_nonTOT_clean)
tidy(lm_bb_hr, conf.int = TRUE)

#tidy(lm_bb_hr, conf.int = TRUE)
# A tibble: 6 x 7
#term            estimate  std.error statistic   p.value    conf.low  conf.high
#<chr>              <dbl>      <dbl>     <dbl>     <dbl>       <dbl>      <dbl>
#  1 (Intercept)  -0.382      0.0181       -21.1   1.68e- 60 -0.418      -0.347    
#2 eFGp          0.699      0.0315        22.2   2.73e- 64  0.637       0.761    
#3 TRB_MP       -0.0330     0.0175        -1.88  6.04e-  2 -0.0674      0.00146  
#4 Tm_use_total  2.39       0.0367        65.0   3.24e-174  2.31        2.46     
#5 EFF           0.00000965 0.00000429     2.25  2.53e-  2  0.00000120  0.0000181
#6 TrV          -0.00000803 0.0000104     -0.774 4.40e-  1 -0.0000284   0.0000124



## For every 1 increase in eFGp, PTS_per_MP will increase by 0.699.
## For every 1 increase in TRB_MP, PTS_per_MP will decrease by -0.0330.
## For every 1 increase in Tm_use_total,PTS_per_MP will increase by 2.39.
## For every 1 increase in EFF, PTS_per_min will increase by 0.00000965.
## For every 1 increase in TrV, PTS_per_min will decrease by -0.00000803.
fit <- lm(PTS_per_MP ~ eFGp + TRB_MP + Tm_use_total + EFF + TrV,
          data = df_nonTOT_clean)
tidy(fit, conf.int = TRUE)


### Predicted value formula 

#> tidy(fit, conf.int = TRUE)
# A tibble: 6 x 7
#term            estimate  std.error statistic   p.value    conf.low  conf.high
#<chr>              <dbl>      <dbl>     <dbl>     <dbl>       <dbl>      <dbl>
#1 (Intercept)  -0.382      0.0181       -21.1   1.68e- 60 -0.418      -0.347    
#2 eFGp          0.699      0.0315        22.2   2.73e- 64  0.637       0.761    
#3 TRB_MP       -0.0330     0.0175        -1.88  6.04e-  2 -0.0674      0.00146  
#4 Tm_use_total  2.39       0.0367        65.0   3.24e-174  2.31        2.46     
#5 EFF           0.00000965 0.00000429     2.25  2.53e-  2  0.00000120  0.0000181
#6 TrV          -0.00000803 0.0000104     -0.774 4.40e-  1 -0.0000284   0.0000124

# as such if 
#eFG = 0.55
#TRB_MP = .2
#Tm_use_total = 0.2
#EFF = 1500
#TrV = 600
#Predictive formula to PTS_per_MP
-0.382 + 0.699 * 0.55 + -0.0330 * 0.2 + 2.39 * 0.20 + 0.00000965 * 1500 + -0.00000803 * 600
## [1] 0.483507


### avPlot used to assess PTS_per_MP against all other explanatory variables.
car::avPlots(fit)

## Multicollinearity (pairs plot). 
## Although we would like to see a relationship between these variables, a direct linear relationshipis not necessary.

## VIF - variance inflation factor.
car::vif(fit)

## Use square root output for the actual variance

sqrt(car::vif(fit))

## Values close to 5 for VIF indicates that there would be high multicollinearity.
## These values are close to one, so no multicollinearity exists.

#> car::avPlots(fit)
#> car::vif(fit)
####eFGp       TRB_MP Tm_use_total          EFF          TrV 
###1.428056     1.265889     2.282248     3.430486     1.844460 
#> sqrt(car::vif(fit))
###eFGp       TRB_MP Tm_use_total          EFF          TrV 
##1.195013     1.125117     1.510711     1.852157     1.358109 

## can use this model to then fit into predictave model.

##Model Testing

## Create model testing dataframe with relevant columns
model_testing <- df_nonTOT_clean[c(1:20)]

summary(model_testing)

### New df creation
model_testing <- mutate(model_testing, exp_PTS_per_MP = predict(fit, newdata = model_testing))

## move exp_PTS_per_MP
model_testing <- model_testing %>%
  relocate(exp_PTS_per_MP, .after = WinP_Tm)

## Model Testing
ggplot(model_testing, aes(exp_PTS_per_MP, PTS_per_MP, label = Tm, colour = Pos)) +
  geom_point(alpha = 0.5) +
  geom_text(nudge_x = 0.105, cex = 3) +
  geom_abline(linetype = "dashed", colour = "magenta")

## If all predictions were 100% accurate, all data points would line up on the line.
## Data points that fall above the line are under estimated by the model.
## Data points that fall below the line are over estimated by the model.


## Create Team average of exp_PTS_per MP
model_Tm_test <- model_testing %>%
  group_by(Tm) %>%
  summarise(Tm_exp_Pts_per_MP = mean(exp_PTS_per_MP),
                Tm_Pts_per_MP = mean(PTS_per_MP)) %>%
  ungroup()


## Ungroup(model_Tm_test <- model_Tm_test)
## join model_Tm_test and model_testing %>%
model_testing  <- model_Tm_test %>%
  select("Tm", "Tm_exp_Pts_per_MP", "Tm_Pts_per_MP") %>%
  right_join(model_testing, by = "Tm")

# Looking at correlation of WinP_Tm to Tm_exp_Pts_per_MP

ggplot(model_testing, aes(x = Tm_exp_Pts_per_MP, y = WinP_Tm, label = Tm)) +
  geom_point(colour = "magenta") +
  geom_text(nudge_x = .002, cex = 3)

ggplot(model_testing, aes(x = Tm_Pts_per_MP, y = WinP_Tm, label = Tm)) +
  geom_point(colour = "green") +
  geom_text(nudge_x = .002, cex = 3)

model_testingTM_plot <- model_testing %>%
  select("Tm", "Tm_Pts_per_MP", "WinP_Tm")

ggplot(model_testing, aes(x = Tm_exp_Pts_per_MP, y = WinP_Tm, label = Tm)) +
  geom_point(alpha = 0.3) +
  geom_point(data = model_testingTM_plot, aes(x = Tm_Pts_per_MP, y = WinP_Tm, colour = "", fill = "")) +
  geom_text(nudge_x = .00002, nudge_y = 1, cex = 3) +
  labs(title = "Team Win Percentage vs Expected points per minute", 
     subtitle = "Teams with increased pts/min have increased winning percentages", 
     colour = "Actual pts/min",
     fill = "Expected pts/min") +
  xlab("Pts per minute (Expected/Actual)") +
  ylab("Win Percentage") +
  scale_colour_manual(values = c("magenta")) +
  theme_light()
  

## model testing with Tm_exp_Pts_per_MP and Tm_Pts_per_MP
ggplot(model_testing, aes(Tm_exp_Pts_per_MP, Tm_Pts_per_MP, label = Tm, colour = Tm)) +
  geom_point(alpha = 0.5) +
  geom_text(nudge_x = 0.005, cex = 3) +
  geom_abline(linetype = "dashed", colour = "green")
  
## Assessing the data frame with player metrics to base selections off.
model_testing %>% 
  select(player_name, Tm, Age, Pos, salary, TrV, EFF, Tm_use_total,PTS_per_MP, TRB_MP, exp_PTS_per_MP) %>% 
  arrange(desc(exp_PTS_per_MP), salary) %>%
  top_n(20)


## Scatter plot with salary

model_testing %>% ggplot(aes(x = salary/1000000, y = exp_PTS_per_MP, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)")


### Interactive Graph

library(plotly)
## create scatterplot first and save to the object gg


gg <- ggplot(model_testing, aes(x = Tm_exp_Pts_per_MP, y = WinP_Tm, label = Tm)) +
  geom_point(alpha = 0.3) +
  geom_point(data = model_testingTM_plot, aes(x = Tm_Pts_per_MP, y = WinP_Tm, colour = "", fill = "")) +
  geom_text(nudge_x = .00002, nudge_y = 1, cex = 3) +
  labs(title = "Team Win Percentage vs Expected points per minute", 
       subtitle = "Teams with increased pts/min have increased winning percentages", 
       caption = "Data sourced from the fNBA stats package....",
       colour = "Actual pts/min",
       fill = "Expected pts/min") +
  xlab("Pts per minute (Expected/Actual)") +
  ylab("Win Percentage") +
  scale_colour_manual(values = c("magenta")) +
  theme_light() 

ggplotly(gg)

gg_sal <- ggplot(data = model_testing, aes(x = salary/1000000, y = exp_PTS_per_MP, color = Pos, label = "Salary (Millions)")) + 
  geom_point() 
  
  ggplotly(gg_sal)


model_testing %>% ggplot(aes(x = salary/1000000, y = TrV, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)")

model_testing %>% ggplot(aes(x = salary/1000000, y = EFF, color = Pos)) + 
  geom_point() +
  xlab("Salary (Millions)")

