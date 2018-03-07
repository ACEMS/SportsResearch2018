
## -----------
# Stats in Sport workshop, 22/02/18
## -----------


## -----------
# Fit a classification and regression tree (CART)
## -----------


# set the working directory
#setwd("c://Work/Work18/courses/stats_in_sport_bne_0218")

# read the Excel (csv) data file
nfl <- read.csv('NFLPlaybyPlay2015.csv')

# view the file within R
View(nfl)

# install and load libraries
install.packages(c("dplyr", "rpart"))
install.packages("rpart.plot")
library(dplyr);library(rpart) 
library(rpart.plot)

# First, make a data frame of only pass plays.
pass <- nfl %>% filter(PlayType %in% c("Pass"))

# Now predict "InterceptionThrown" using "down" and "distance".
fit <- rpart(InterceptionThrown ~ down+ydstogo,data=pass, cp=0.0005)

# One way to plot your results
par(mfrow=c(1,1), xpd=TRUE)
plot(fit)
text(fit)

# Another way to plot your results
prp(fit, type=1, fallen.leaves=TRUE, extra=1, cex=0.7)

# Display your results
fit

# Say we want to predict whether the upcoming play is a run or a pass.# First, eliminate special teams plays and # create a column run = 1 if the team ran the ball and 0 otherwise

nfl.run.or.pass <- nfl %>% filter(PlayType %in% c("Run", "Pass")) %>% mutate(run = ifelse(PlayType=="Run", 1, 0))

# Now predict whether a team will run the ball using the down and distance.
fit <- rpart(run ~ down+ydstogo,data=nfl.run.or.pass, cp=0.01)
prp(fit, type=1, fallen.leaves=TRUE, extra=1, cex=0.7)


## -----------
# Fit a boosted regression tree
## -----------


# install and load the package "gbm"
install.packages("gbm")
library(gbm)

# fit a boosted regression tree to predict "InterceptionThrown"
fit <- gbm(InterceptionThrown ~ down+ydstogo+qtr+ScoreDiff,data=pass)

# display results
summary(fit)

# plot results
plot(fit)

summary(fit)

# predict results for the daaset
fit.p <- predict.gbm(fit, newdata=pass, n.trees=100, type="response")

boxplot(fit.p~pass$InterceptionThrown)



