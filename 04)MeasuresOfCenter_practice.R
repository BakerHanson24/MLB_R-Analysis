##  1.1.1 Calculate measures of center (mean,median,mode) for Variables

library(Lahman)
library(dplyr)
library(ggplot2)
data("Teams")
View(Teams)

TeamWins_160plus <- Teams %>% 
                      select(yearID,teamID,G,W) %>% 
                      filter(G >= 160)

Wins_Mean <- mean(TeamWins_160plus$W)
Wins_Mean

Wins_Median <- median(TeamWins_160plus$W)
Wins_Median

Wins_Mode <- mode(TeamWins_160plus$W)
Wins_Mode




ggplot(TeamWins_160plus, aes(W)) +
    geom_histogram(binwidth = 1)
