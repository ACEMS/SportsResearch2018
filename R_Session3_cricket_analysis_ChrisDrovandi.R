#please see the following link for instructions on how to call JAGS from R. http://www.jkarreth.net/files/bayes-cph_Tutorial-JAGS.pdf 


batsmen = read.csv("batsmen.csv",header=T)

# number of dismissals
batsmen$outs = batsmen$Inns - batsmen$NO

# centering decade covariate to reduce posterior correlation between parameters
batsmen$decade_centre = (batsmen$decade - mean(batsmen$decade))/sd(batsmen$decade)

# prepare data for Bayesian analysis
x = (model.matrix( ~ decade_centre + I(decade_centre^2) + I(decade_centre^3), batsmen))
y = batsmen$Avg
n = batsmen$outs
N = length(y)
np = dim(x)[2]
# note the below column is an estimate of the variance of the runs scored by a batsmen in an innings
# the trimmed standard deviation (removing top 10% of run scores) is used for robustness
V = batsmen$var 


# The Bayesian model
batsmen_model <- function(){
  
  for (i in 1:N){
    y[i] ~ dnorm(lp[i],n[i]/V[i])
    yp[i] ~ dnorm(lp[i],n[i]/V[i])
    for (r in 1:np){
      vec[i,r] <- beta.all[r]*x[i,r]
    }
    lp[i] <- sum(vec[i,1:np]) + mu[i]
  }
  
  for (i in 1:N){
    mu[i] ~ dnorm(0,phi_mu)  
  }
  
  for (r in 1:np){
    beta.all[r] ~ dnorm(0,0.001)
  }
  
  phi_mu ~ dunif(0,100)

}


# Performing the Bayesian analysis
jags.data <- list("y","x","n","N","np","V")
jags.params <- c("beta.all","phi_mu","mu","yp")


library(R2jags)
library(rjags)
jags.inits <- function() { list(beta.all=rep(0.1,np),phi_mu=1) }
jagsfit <- jags(data=jags.data, inits=jags.inits, jags.params,n.chains=1,n.thin=5,DIC=TRUE,
                n.iter=1000000,n.burnin=5000, model.file=batsmen_model)


# extracting results and plotting
beta.samples = jagsfit$BUGSoutput$sims.list$beta.all

dec_grid = seq(min(batsmen$decade_centre),max(batsmen$decade_centre),0.001)
mu = matrix(0,nrow=dim(beta.samples)[1], ncol=length(dec_grid))

for (i in 1:dim(beta.samples)[1]){
  mu[i,] = beta.samples[i,1] +   beta.samples[i,2]*dec_grid +  beta.samples[i,3]*dec_grid^2 + beta.samples[i,4]*dec_grid^3
}

q = apply(mu, MARGIN = 2, FUN = quantile, probs = c(0.025,0.5,0.975))
x11()
plot(sd(batsmen$decade)*dec_grid + mean(batsmen$decade),q[2,],type="l",ylim=c(10,70),xlab="Decade",ylab="Batting Average")
points(batsmen$decade,y)

mu.samples = jagsfit$BUGSoutput$sims.list$mu
mu.median = apply(mu.samples, MARGIN=2, FUN = median)

ret = sort(mu.median, decreasing = TRUE, index.return = TRUE)
batsmen_sort = batsmen[ret$ix,]

mu.samples.sort = mu.samples[,ret$ix]
mu.median.sort = ret$x



x11()
# in final ranking only consider players with at least 25 matches
batsmen_sort_sub = batsmen_sort[batsmen_sort$Mat>=25, ]
mu.samples.sort.sub = mu.samples.sort[,batsmen_sort$Mat>=25]
mu.median.sort.sub = mu.median.sort[batsmen_sort$Mat>=25]

plot(c(quantile(mu.samples.sort.sub[,1],probs=c(0.05,0.95))),c(30,30),type="l",ylim=c(0,30),xlim=c(-13,24), xlab = "Relative Score", ylab = "", yaxt="n")
points(mu.median.sort.sub[1],30)
text(-7,30,paste(batsmen_sort_sub$Player[1]))
for (i in 2:15){
  points(c(quantile(mu.samples.sort.sub[,i],probs=c(0.05,0.95))),c(30 - 2*(i-1),30 - 2*(i-1)),type="l")
  points(mu.median.sort.sub[i],30 - 2*(i-1))
  text(-7,30 - 2*(i-1),paste(batsmen_sort_sub$Player[i]))
}


