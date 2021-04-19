#####################################################################
# You must have run the munge.R script to start work here
#####################################################################
# Lets start our model with a basic frequentist count
# lets assume we only need a storm in one of the five weeks
# And we want to know how many years did this rain happen
#####################################################################

frequentist_count <- weekly_relevant %>% 
    group_by(YEAR) %>%
    summarise(STORM = sum(STORM))

frequentist_count$STORM <- ifelse(frequentist_count$STORM > 1,
                                 TRUE,
                                 FALSE)
storm = nrow(frequentist_count%>%filter(STORM == TRUE))
nostorm = nrow(frequentist_count%>%filter(STORM == FALSE))
prob_storm = storm/(storm + nostorm)

#####################################################################
# What else can we do with all this data
# Let's try a Logistic Regression
# Because we won't know the information for the week in question
# We need to structure the data set as a time series
# we want to predict next week based on the last two week's values
#####################################################################

TS_weather = data.frame("STORM" = NA, 
                        "PRCPt1"  = NA, 
                        "TMAXt1" = NA, 
                        "TMINt1" = NA, 
                        "PRCPt2" = NA, 
                        "WEEK" = NA)

for (i in 1:nrow(weekly_weather)){
  TS_weather[i,1] <- weekly_weather[i,6]
  if (i == 1){
    TS_weather[i,2] = 0
    TS_weather[i,3] = 0
    TS_weather[i,4] = 0
    TS_weather[i,5] = 0
    TS_weather[i,6] = 0
  }else if (i==2){
    TS_weather[i,2] = weekly_weather[i-1,3]
    TS_weather[i,3] = weekly_weather[i-1,4]
    TS_weather[i,4] = weekly_weather[i-1,5]
    TS_weather[i,5] = 0
    TS_weather[i,6] = weekly_weather[i-1,2]
  }else{
    TS_weather[i,2] = weekly_weather[i-1,3]
    TS_weather[i,3] = weekly_weather[i-1,4]
    TS_weather[i,4] = weekly_weather[i-1,5]
    TS_weather[i,5] = weekly_weather[i-2,3]
    TS_weather[i,6] = weekly_weather[i-1,2]
  }
}

TS_weather$STORM = factor(TS_weather$STORM, levels = c(0, 1))

#####################################################################
# The next step is to separate training and test set
# Lets randomly take 75% of our data as a training set
#####################################################################

library(caTools)
set.seed(123)
split = sample.split(TS_weather$STORM, SplitRatio = 0.75)
training_set = subset(TS_weather, split == TRUE)
test_set = subset(TS_weather, split == FALSE)

#####################################################################
# Now let's create our regression
# glm means: Generalized Linear Model
# family = binomial means its a logit regression
#####################################################################

classifier = glm(formula = STORM ~ .,
                 family = binomial,
                 data = training_set)

#####################################################################
# Lets get probabilities for the data in the test set
# then we pick a Threshold for predicting Storm or no Storm
# And create a confusion matrix to show us how we did
#####################################################################

prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set$STORM, y_pred > 0.5)

#####################################################################
# So if we knew the expected precipitation and MAX and MIN temp
# Say 0.5 inches of precipitation with a high of 65 and a low of 55 on week 39
# We can obtain a probability for the chance of a storm
#####################################################################

week40 <- data.frame("PRCPt1" = 0.5,"TMAXt1" = 65, "TMINt1" = 55, "PRCPt2" = 0.5, "WEEK" = 39)
probability_storm = predict(classifier, type = 'response', newdata = week40)

#####################################################################
# Lets try this again and get rid of WEEK, TMIN, and PRCPt2 in our model
# Remember we are hoping for a light warm rain
#####################################################################

classifier2 = glm(formula = STORM ~ PRCPt1 + TMAXt1,
                 family = binomial,
                 data = training_set)

#####################################################################
# Lets get probabilities for the data in the test set
# then we pick a Threshold for predicting Storm or no Storm
# And create a confusion matrix to show us how we did
#####################################################################

prob_pred2 = predict(classifier2, type = 'response', newdata = test_set)
y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)
cm2 = table(test_set$STORM, y_pred2 > 0.5)

#####################################################################
# So if we knew the Precipitation and WEEK
# Say 0.5 inches of precipitation and MAXTEMP = 85
# We can obtain a probability for the chance of a storm
#####################################################################

week40 <- data.frame("PRCPt1" = 0.5,"TMAXt1" = 65)
probability_storm2 = predict(classifier2, type = 'response', newdata = week40)

#####################################################################
# Let's save the classifier as an object.
# This is done se we can call it from anywhere
# It comes in handy when hosting a solution that uses an object
#####################################################################

saveRDS(classifier2, file = "~/data/prediction.RData")

#####################################################################
# Let's do an interesting Viz now that we have only 2 features
#####################################################################

library(ElemStatLearn)
library(ROCR)

#some tedious munging here
drops <- c("TMINt1","PRCPt2","WEEK")
set = test_set[, !(names(test_set) %in% drops)]
X1 = seq(min(set$PRCPt1) - 1, max(set$PRCPt1) + 1, by = 0.01)
X2 = seq(min(set$TMAXt1) - 1, max(set$TMAXt1) + 1, by = 0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PRCPt1', 'TMAXt1')
prob_set = predict(classifier2, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
#add a color variable
set$Color <- 'red'
set$Color[set$STORM == 1] <- 'green'

#now plot and view our decision boundary
plot(x = set$PRCPt1,
     y = set$TMAXt1,
     main = 'Logistic Regression Result (Test Set)',
     xlab = 'Percipitation Week t-1', 
     ylab = 'Max Tempature Week t-1',
     xlim = range(X1), 
     ylim = range(X2),
     col = set$Color)
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

#####################################################################
# Your turn!
# explore varous feature combinations and lags to improve your model performance
#####################################################################

