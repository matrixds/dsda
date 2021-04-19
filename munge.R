#####################################################################
# Welcome to the Data Science for Decision Analysts workshop!
# Follow along with these scripts and the instructor 
#####################################################################
# Loading Libraries of functions that will be needed
#####################################################################

install.packages('tidyverse')
library(tidyverse)
library(lubridate)

#####################################################################
# Reading a csv file into R and putting it in data table format
#####################################################################

weather <- read.csv("dsda/data/Vineyard_weather_1948-2017.csv")

#####################################################################
# Lets do some exploration of this data set in the console
# We can use commands such as summary(mydata$column_name) to view some statistics
# We should be looking for missing values or inconsistencies that jump at us
#####################################################################

#####################################################################
# There are some NAs in the rain data, lets explore that column more
#####################################################################

rain <- weather %>%
  filter(RAIN)

norain <- weather %>%
  filter(!RAIN)

nodata <- weather %>% 
  filter(is.na(RAIN))

# Now that we have explored these subsets we can remove them from the session to save memory
rm(nodata)
rm(norain)
rm(rain)

#####################################################################
# There are some NAs in the PRCP column
# what are interesting ways to get rid of them?
# what impact does this manipulation have on my analysis
#####################################################################

weather$PRCP <- as.numeric(weather$PRCP)

weather$PRCP = ifelse(is.na(weather$PRCP),
                      ave(weather$PRCP, FUN = function(x) mean(x, na.rm = TRUE)),
                      weather$PRCP)

weather$RAIN = ifelse((( weather$PRCP > 0 ) & ( is.na(weather$RAIN ))), TRUE, weather$RAIN)

#####################################################################
# Harvest is in september
# So we want to separate the date field into different fields
# This will allow us to the calculate weekly statistics
#####################################################################

weather$DAY <- day(weather$DATE)
weather$MONTH <- month(weather$DATE)
weather$YEAR <- year(weather$DATE)
weather$WEEK <- week(weather$DATE)

#####################################################################
# We now have a column for Day, Month, Year and Week
# How should we think about accumulating rows
# The final output should be a row per week
#####################################################################

weekly_weather <- weather %>%
  group_by(YEAR, WEEK)  %>%
  summarise(PRCP = sum(PRCP), 
            TMAX = max(TMAX), 
            TMIN = min(TMIN),
            RAIN = sum(RAIN))

#####################################################################
# Our aggregation by week has converted our RAIN column to numeric
# If we want a categorical value, we must pick a way to measure it
# Lets assume that for the mold to form we need rain on 3 of the 7 days
# Another option would be if PRCP is greater than 0.7 level
# Let's also change the name of the column to STORM
#####################################################################

names(weekly_weather)[6]<-"STORM"

weekly_weather$STORM <- ifelse(weekly_weather$PRCP >= 0.35 & weekly_weather$TMAX<=80, 1, 0)

#weekly_weather$STORM <- ifelse(weekly_weather$STORM >= 3, 1, 0)

#####################################################################
# We know that the harvest happens ussually in the 40th week of the year
# This means we are interested in storms in the 5 weeks prior to the 40th
# Lets subset our data to only the relevant fields
#####################################################################

weekly_weather$WEEK <- as.numeric(weekly_weather$WEEK)

weekly_relevant <- weekly_weather %>% 
  filter(WEEK >= 35 & WEEK < 40)

#####################################################################
# Lets do a couple of plots and see what we can glimpse from the data
#####################################################################

weekly_relevant %>%
ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       y = "Storm",
       x = "Week") + theme_bw(base_size = 15)

#####################################################################
# That wasnt very helpful, lets facet it by year
#####################################################################

weekly_relevant %>%
  ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  facet_wrap(~ YEAR, ncol = 6) +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       subtitle = "Data plotted by year",
       y = "Storm",
       x = "Month") + theme_bw(base_size = 15)

#####################################################################
# Lets see which variables have a higher effect on STORM
#####################################################################

pairs(weekly_relevant)

#####################################################################
# lets look at the last 20 years only
#####################################################################

relevant2 <- weekly_relevant%>%
  filter(YEAR >= 1997)

relevant2 %>%
  ggplot(aes(x = WEEK, y = STORM)) +
  geom_point(color = "darkorchid4") +
  facet_wrap(~ YEAR, ncol = 4) +
  labs(title = "Storm on weeks 35 to 40 - Vineyard",
       subtitle = "Data plotted by year",
       y = "Storm",
       x = "Month") + theme_bw(base_size = 15)

#####################################################################
# Lets see which variables have a higher effect on STORM
#####################################################################

pairs(relevant2)

#####################################################################
# Your turn!
# create 5 exploratory visuals using the R Graphics Cookbook Recipies
#####################################################################

# this is just a place holder to get you started
weather %>%
  ggplot(aes(x = ?, y = ?))