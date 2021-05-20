### Team wins dataframe


library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(performance)
library(psych)
library(ggrepel)


## Reading in data
## player_stats_tidy
player_stats_tidy <- read_csv("data/tidy_data/player_stats_tidy.csv")
View(player_stats_tidy)
## Df_team_Stats1
df_Stats1_Tm_W <- read_csv("data/tidy_data/df_team_Stats1.csv")
## Df_team_Stats2
df_Stats2_Tm_W <- read_csv("data/tidy_data/df_team_Stats2.csv")
## df_TOT_players
df_TOT_players_Tm_W <- read_csv("data/tidy_data/df_TOT_players.csv")

## rename team stats with T_var in Stats2

df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "New York Knicks", "Team"] <- "NYK"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Philadelphia 76ers", "Team"] <- "PHI"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Washington Wizards", "Team"] <- "WAS"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Oklahoma City Thunder", "Team"] <- "OKC"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Houston Rockets", "Team"] <- "HOU"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Charlotte Hornets", "Team"] <- "CHO"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Los Angeles Clippers", "Team"] <- "LAC"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Atlanta Hawks", "Team"] <- "ATL"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Milwaukee Bucks", "Team"] <- "MIL"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Toronto Raptors", "Team"] <-"TOR"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Brooklyn Nets", "Team"] <- "BRK"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Golden State Warriors", "Team"] <- "GSW"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Minnesota Timberwolves", "Team"] <-"MIN"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Sacramento Kings", "Team"] <- "SAC"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Boston Celtics", "Team"] <- "BOS"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Los Angeles Lakers", "Team"] <- "LAL"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Utah Jazz", "Team"] <- "UTA"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Indiana Pacers", "Team"] <-  "IND"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "San Antonio Spurs", "Team"] <- "SAS"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Portland Trail Blazers", "Team"] <-"POR"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Denver Nuggets", "Team"] <- "DEN"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Dallas Mavericks", "Team"] <- "DAL"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "New Orleans Pelicans", "Team"] <- "NOP"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Phoenix Suns", "Team"] <- "PHO"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Orlando Magic", "Team"] <- "ORL"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Miami Heat", "Team"] <- "MIA"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Chicago Bulls", "Team"] <- "CHI"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Cleveland Cavaliers", "Team"] <- "CLE"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Detroit Pistons", "Team"] <- "DET"
df_Stats1_Tm_W[df_Stats1_Tm_W$Team == "Memphis Grizzlies", "Team"] <- "MEM"

view(df_Stats1_Tm_W)

##Create right join from Stats_1 Wins and Losses, 
##Right join Stats1 Wins, Losses, and create Wins and Tm_Losses
## Change team variable name
df_Stats1_Tm_W <- df_Stats1_Tm_W %>%
  rename(Tm = Team)

player_stats_tidy <- player_stats_tidy %>%
  mutate(TOT = case_when(Tm == "MIL" ~ "nonTOT", 
                         Tm == "LAC" ~ "nonTOT",
                         Tm == "ATL" ~ "nonTOT",
                         Tm == "BOS" ~ "nonTOT", 
                         Tm == "CHO" ~ "nonTOT",
                         Tm == "CHI" ~ "nonTOT",
                         Tm =="MIN" ~ "nonTOT", 
                         Tm == "DEN" ~ "nonTOT",
                         Tm == "CLE" ~ "nonTOT",
                         Tm =="DAL" ~ "nonTOT", 
                         Tm == "GSW" ~ "nonTOT",
                         Tm == "DET" ~ "nonTOT",
                         Tm =="HOU" ~ "nonTOT", 
                         Tm == "SAS" ~ "nonTOT",
                         Tm == "IND" ~ "nonTOT",
                         Tm =="LAL" ~ "nonTOT", 
                         Tm == "MEM" ~ "nonTOT",
                         Tm == "MIA" ~ "nonTOT",
                         Tm =="NOP" ~ "nonTOT",
                         Tm == "BRK" ~ "nonTOT",
                         Tm == "NYK" ~ "nonTOT",
                         Tm == "OKC" ~ "nonTOT",
                         Tm == "ORL" ~ "nonTOT",
                         Tm == "PHI" ~ "nonTOT",
                         Tm == "PHX" ~ "nonTOT",
                         Tm == "PHO" ~ "nonTOT",
                         Tm == "POR" ~ "nonTOT",
                         Tm == "SAC" ~ "nonTOT",
                         Tm == "TOR" ~ "nonTOT",
                         Tm == "UTA" ~ "nonTOT",
                         Tm == "WAS" ~ "nonTOT"))

##replace NAs

player_stats_tidy <- replace_na(player_stats_tidy, list(TOT = "TOT"))

## Subset to df_new_TOT_joined with TOT players removed
df_new_TOT_joined<- player_stats_tidy[!(player_stats_tidy$TOT == "TOT"),]

## Join team Wins and Losses to df_new_TOT_joined
df_new_TOT_joined <- df_Stats1_Tm_W %>%
  select(Tm, W, L) %>%
  right_join(df_new_TOT_joined, by="Tm")

## Export to tidy data as df_nonTOT_clean
write_csv(df_new_TOT_joined, file = "data/tidy_data/df_nonTOT_clean.csv")



