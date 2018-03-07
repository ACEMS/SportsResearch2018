# ----------------------------------------------------------
require('depmixS4') # Will install the package if not already installed
library(depmixS4) # Load the HMM library
data(speed) # load the data from the library
View(speed) # Look at the data - stored in the data frame speed
summary(speed)

library(ggplot2) # load for plotting
library(reshape2) # load for manipulating data frames, melt command
speed = cbind(speed[,c('rt','corr','Pacc')],list(t=1:nrow(speed))) #cbind = add a column for time to the speed dataframe
mspeed = melt(speed,measure.vars=c('rt','corr','Pacc')) # reshape speed from wide format into long format for plotting
# Plot! with facets for each variable
ggplot(mspeed,aes(x=t,y=value,group=variable)) +
  geom_line() + 
  facet_wrap(~variable,scales='free',ncol=1)

# Define the simple model for HMM
modsimple = depmix(response=rt~1,data=speed,nstates=2,trstart=runif(4))
fmsimple = fit(modsimple) # fit the model
summary(fmsimple)

head(fmsimple@posterior) # have a look at the posterior = states, state probabilities, estimated for the observations

mspeed = rbind(mspeed,data.frame(t=1:nrow(speed),variable='state',value=fmsimple@posterior$state)) # add state outcomes to mspeed and plot
ggplot(mspeed,aes(x=t,y=value,group=variable)) +
  geom_line() + 
  facet_wrap(~variable,scales='free',ncol=1) + xlim(1,150)

# Define the model with covariates for the transition matrix
modtrans = depmix(response=rt~1,transition=~Pacc,data=speed,nstates=2,instart=runif(2))
fmtrans = fit(modtrans) # fit the model
summary(fmtrans)
fmtrans

# add state outcomes to mspeed and plot
mspeed = rbind(mspeed,data.frame(t=1:nrow(speed),variable='state.trans',value=fmtrans@posterior$state))
ggplot(mspeed,aes(x=t,y=value,group=variable)) +
  geom_line() + 
  facet_wrap(~variable,scales='free',ncol=1) 


# Define the model with covariates for transition matrix AND multivariate response = 2 response variables, rt and corr
# rt=response time, corr = correct or not
modmulti = depmix(list(rt~1,corr~1),data=speed,nstates=2,
                  family=list(gaussian(),multinomial('identity')),transition=~scale(Pacc),
                  instart=runif(2))
fmmulti = fit(modmulti)
summary(fmmulti)

# add state outcomes to mspeed and plot
mspeed = rbind(mspeed,data.frame(t=1:nrow(speed),variable='state.multi',value=fmmulti@posterior$state))
ggplot(mspeed,aes(x=t,y=value,group=variable)) +
  geom_line() + 
  facet_wrap(~variable,scales='free',ncol=1) 