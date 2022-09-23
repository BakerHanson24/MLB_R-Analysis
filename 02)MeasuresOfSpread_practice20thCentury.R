## Measures of Spread practice, using Teams table from Sean Lahman MLB Database ##

library(dplyr)
library(ggplot2)
library(tidyverse)
library(Lahman)
search()

data("Teams")
str(Teams)
colnames(Teams)

## Let's have some fun looking at regular season 
## wins in the 20th century so far (not 2020 season, COVID)

RegWins <- Teams %>% 
            filter(yearID >= 2000, yearID <= 2021, yearID != 2020) %>% 
            mutate(WinPct = round(W/G,3)) %>% 
            select(yearID, lgID, franchID, W, WinPct)


head(RegWins)

## Minimum Wins
MinWins <- min(RegWins$W)

## Max wins
MaxWins <- max(RegWins$W)

## Mean wins
MeanWins <- mean(RegWins$W)

## Variance of Wins
VarWins <- var(RegWins$W)

## SD of Wins
SdWins <- sqrt(var(RegWins$W))

## SD distances from mean
negSD1 <- MeanWins - SdWins
negSD2 <- MeanWins - 2*SdWins
negSD3 <- MeanWins - 3*SdWins
posSD1 <- MeanWins + SdWins
posSD2 <- MeanWins + 2*SdWins
posSD3 <- MeanWins + 3*SdWins


## MAD: mean absolute deviation
distances <- RegWins$W - mean(RegWins$W)
MadWins <- mean(abs(distances))


## ---------- Let's figure out some Outliers ----------- ##

## create IQR function
IQR <- function(x) {
  quantile(x,0.75) - quantile(x,0.25)
}

## use IQR function
IQR_Wins <- IQR(RegWins$W)
IQR_Wins

## lowerthreshold
lowThresh_Wins <- quantile(RegWins$W,0.25) - 1.5*IQR_Wins
highThresh_Wins <- quantile(RegWins$W,0.75) - 1.5*IQR_Wins

##create Outliers function
Outliers_Func <- function(x) {
  (x < (quantile(x,0.25) - (1.5*IQR(x)))) | (x > (quantile(x,0.75) + (1.5*IQR(x))))
}

RegWinsOutliers <- !Outliers_Func(RegWins$W)
RegWins$W[53]

summary(RegWins$W)
rm(Outliers_Func)


Winsplus_Outliers <- cbind(RegWins,RegWinsOutliers)

Winsplus_Outliers


## rename last column
Winsplus_Outliers <- rename(Winsplus_Outliers, Outlier = RegWinsOutliers)


## Create filtered 21st century DF, only outliers
Century21_Outliers <- Winsplus_Outliers %>% 
    filter(Outlier == TRUE)

Century21_Outliers %>% filter(franchID == "SEA")



## ---------------------------------------------------- ##

## JOIN RegWins with Teams using left_join, analyse WS Champs ##

WsChamps <- RegWins %>% 
            left_join(Teams, by = c("yearID","franchID"),
                      suffix = c("_first","_second")) %>% 
            select(yearID,lgID_first,franchID,W_first,WinPct,WSWin)

colnames(WsChamps) <- c("yearID","lgID","franchID","W","WinPct","WSWin")

WsChamps

View(WsChamps)
