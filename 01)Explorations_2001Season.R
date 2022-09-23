#Let's rock with some Lahman practice, set Working Directory
setwd("C://Users/Elise/Documents/Baker Hanson/Data/HDevProjects/MLB_SeanLahman/R")
getwd()

# Load Lahman package, and get Batting Data.Frame
library(Lahman)
?Lahman
data(Batting)

# Let's explore the Data.Frame
str(Batting)
# Convert playerID, yearID into Factors
Batting$playerID <- factor(Batting$playerID)
Batting$yearID <- factor(Batting$yearID)

# load dplyr and ggplot
library(dplyr)
library(ggplot2)

# Let's look at the 2001 Mariners, and how they compare with the rest of the MLB! 

MLB2001 <- Batting %>% 
                filter(yearID==2001)

HR2001_Teams <- MLB2001 %>% 
          select(lgID, teamID, HR) %>% 
          group_by(teamID, lgID) %>% 
          summarize(HR = sum(HR)) %>% 
          arrange(desc(HR))
# Let's see how the Mariners HR's compared with the rest of the League
ggplot(data=HR2001_Teams, aes(x=teamID, y=HR, color=lgID), title(main="2001 Home Runs")) + 
  geom_point(size=5)










