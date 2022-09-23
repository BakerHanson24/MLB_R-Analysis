head(Salaries)
head(Teams)


## Join Salaries to Teams
Sal_Teams <- Salaries %>% 
              left_join(Teams, by = c("yearID","teamID","lgID")) %>% 
              transmute(yearID, teamID, lgID, salary,
                     WinPct = round(W/G,3), G, W, L, WCWin, DivWin,
                     LgWin, WSWin, name, park)

Payrolls <- Sal_Teams %>% 
            group_by(yearID, lgID, teamID, WinPct, G, W, L, WCWin,
                     DivWin, LgWin, WSWin, name, park) %>% 
            summarize(payroll = round(sum(salary/1000000),2))



## Create base plot ##
Pay4Gold_plot <- ggplot(Payrolls, aes(yearID, payroll)) +
                ##geom_point(shape = 18) +
                labs(title = "MLB Payrolls", x = "Season", 
                     y = "Payroll in Millions") +
                geom_point(data = Payrolls %>% 
                             filter(WSWin == "Y"), color = "blue")
## create pay variable
low_Pay <- Payrolls %>% 
  group_by(yearID) %>% 
  summarize(low_Pay = min(payroll))
quart1_Pay <- Payrolls %>% 
  group_by(yearID) %>% 
  summarize(quart1_Pay = quantile(payroll,0.25))

mid_Pay <- Payrolls %>% 
            group_by(yearID) %>% 
            summarize(mid_Pay = median(payroll))

quart3_Pay <- Payrolls %>% 
  group_by(yearID) %>% 
  summarize(quart3_Pay = quantile(payroll,0.75))

max_Pay <- Payrolls %>% 
  group_by(yearID) %>% 
  summarize(max_Pay = max(payroll))

## add quartile lines to the base plot:
    ## first create function for repetition

Pay4Gold_plot <- Pay4Gold_plot +
  geom_line(data = low_Pay, aes(x = yearID, y = low_Pay,
                              color = "green")) +
  geom_line(data = quart1_Pay, aes(x = yearID, y = quart1_Pay,
                                color = "green")) +
  geom_line(data = mid_Pay, aes(x = yearID, y = mid_Pay,
                                color = "green"), size = 1.5) +
  geom_line(data = quart3_Pay, aes(x = yearID, y = quart3_Pay,
                                color = "green")) +
  geom_line(data = max_Pay, aes(x = yearID, y = max_Pay,
                                color = "green")) +
  theme_dark()

Pay4Gold_plot  +
    theme(axis.ticks.x = element_line(color = "black"))
  






library(dplyr)
library(ggplot2)

CopyPlot_Gold <- ggplot(Payrolls, aes(yearID, payroll)) +
                ##geom_point(shape = 18) +
  stat_quantile(quantiles = c(0.25,0.5,0.75)) +
  labs(title = "MLB Payrolls", x = "Season", 
       y = "Payroll in Millions") +
  geom_point(data = Payrolls %>% 
               filter(WSWin == "Y"), color = "blue")


CopyPlot_Gold
