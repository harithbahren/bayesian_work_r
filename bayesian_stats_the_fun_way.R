# sd formula as defined in the book
my.sd <- function(val){
  val.mean <- mean(val)
  sqrt(mean((val.mean-val)^2))
}

# Chapter 12 # Question 2
temp.data <- c(100,99.8,101,100.5,99.7)
mean.temp <- mean(temp.data)
sd.temp <- my.sd(temp.data)
integrate(function(x) dnorm(x,mean=mean.temp,sd=sd.temp),100.4,200)

# Chapter 12 # Question 3
time.data <- c(2.5,3,3.5,4,2)
time.data.mean <- mean(time.data)
time.data.sd <- my.sd(time.data)
integrate(function(x) dnorm(x,mean=time.data.mean,sd=time.data.sd),10.10,200)

# Chapter 13
xs <- seq(0.005,0.01,by=0.00001)
xs.all <- seq(0.1,by=0.0001)
plot(xs,pbeta(xs,300,39700),lwd=3,
     ylab="Cumulative probability",
     xlab="Subscription rate",
     main="CDF Beta(300,39700)")
plot(xs,qbeta(xs,300,39700),lwd=3,
     ylab="Probability of subscription",
     xlab="Cumulative probability",
     main="Quantile of Beta(300,39700)")
snowfall.data <- c(7.8,9.4,10.0,7.9,9.4,7.0,7.0,7.1,8.9,7.4)
mean.snowfall <- mean(snowfall.data)
sd.snowfall <- sd(snowfall.data)
lower.bound.snowfall <- qnorm(0.0005,mean=mean.snowfall,sd=sd.snowfall)
upper.bound.snowfall <- qnorm(0.9995,mean=mean.snowfall,sd=sd.snowfall)
lower.bound.snowfall
upper.bound.snowfal

# Chapter13 Question3
lower.bound <- qbeta(0.025,10,20)
upper.bound <- qbeta(0.975,10,20)
lower.bound
upper.bound
# 40 more houses so she'll sell between 7 to 20 candies

# Chapter14 Question2
qbeta(0.975,25,17) # upperboundfor1stprior
qbeta(0.025,25,17) # lowerboundfor1stprior
qbeta(0.975,118,114) # upperboundfor2ndprior
qbeta(0.025,118,114) # lowerboundfor2ndprior
qbeta(0.975,88,44) # upperboundfor3rdprior
qbeta(0.025,88,44) # lowerboundfor3rdprior

# Chapter15 Question 1
n.trials <- 100000
# strong prior based on prob of clicking the link to the blog being 0.3 and applied to both A and B
prior.alpha <- 700
prior.beta <- 300  
a.samples <- rbeta(n.trials,36+prior.alpha,114+prior.beta)
b.samples <- rbeta(n.trials,50+prior.alpha,100+prior.beta)
# how many times b.samples is greater than a.samples
p.b_superior <- sum(b.samples > a.samples)/n.trials 
p.b_superior
# Variant B is superior 73% of the time from 96%

# Chapter 15 Question 2
n.trials <- 100000
# weaker prior based on the prob of clicking the link to the blog being 0.3
a.prior.alpha <- 30 
a.prior.beta <- 70
# medium strength prior based on lead designer's advice that the prob of clicking the link for B (no images) is 0.2
b.prior.alpha <- 20 
b.prior.beta <- 80
a.samples <- rbeta(n.trials,36+a.prior.alpha,114+a.prior.beta)
b.samples <- rbeta(n.trials,50+b.prior.alpha,100+b.prior.beta)
p.b_superior <- sum(b.samples > a.samples)/n.trials
p.b_superior
# Variant B is superior 65% of the time, less than before

# Chapter 15 Question 3
a.true.rate <- 0.25
b.true.rate <- 0.3
prior.alpha <- 300
prior.beta <- 700
number.of.samples <- 0
p.b_superior <- -1
while(p.b_superior < 0.95){
  number.of.samples <- number.of.samples + 100
  # generating data set of (no.of.samples/2) random numbers between 0 and 1, for numbers less than a.true.rate, they are assigned 'TRUE' and for numbers more than a.true rate, they are assigned 'FALSE'
  # the ratio of a.results with 'TRUE'/'FALSE' is 0.25 and b.results is 0.3 to reflect the true rate and create our 'likelihood' probability
  a.results <- runif(number.of.samples/2) <= a.true.rate 
  b.results <- runif(number.of.samples/2) <= b.true.rate
  # random sampling of a beta distribution with a mean of ~0.275
  a.samples <- rbeta(n.trials, 
                     sum(a.results==TRUE)+prior.alpha,
                     sum(a.results==FALSE)+prior.beta)
  # random sampling of a beta distribution with a mean of ~0.3
  b.samples <- rbeta(n.trials, 
                     sum(b.results==TRUE)+prior.alpha,
                     sum(b.results==FALSE)+prior.beta)
  # comparing how many times b.samples is greater than a.samples
  p.b_superior <- sum(b.samples > a.samples)/n.trials
}
number.of.samples
# repeat for Lead Designer but using separate priors for A (30,70) and B (20,80)

# Chapter19 Question 1
dx <- 0.01
hypotheses <- seq(0,1,by=dx)
bayes.factor <- function(h_top,h_bottom){
  ((h_top)^24*(1-h_top)^76)/((h_bottom)^24*(1-h_bottom)^76)
}
bfs.v1 <- bayes.factor(hypotheses,0.5)
bfs.v2 <- bayes.factor(hypotheses,0.24)
plot(hypotheses,bfs.v1,type='l')
plot(hypotheses,bfs.v2,type='l')
# only difference in the plots is the y-axis, hence changing the strength of the hypothesis only changes the scale of the distribution
plot(hypotheses,bfs.v1/sum(bfs.v1),type='l')
points(hypotheses,bfs.v2/sum(bfs.v2))

# Chapter 19 Question 2
dx <- 0.01
hypotheses <- seq(0,1,by=dx)
bayes.factor <- function(h_top,h_bottom){
  ((h_top)^24*(1-h_top)^76)/((h_bottom)^24*(1-h_bottom)^76)
}
bfs <- bayes.factor(hypotheses,0.5)
vals <- replicate(length(hypotheses)-1,1.05) 
# generating a list of 1.05 that is (1 less than the number of hypotheses) long
vals <- c(1,vals)
# including the first prior which is 1
priors <- cumprod(vals)
# creating a list of priors where 1.05 is cumulatively multiplied to the previous value
posteriors <- bfs*priors
p.posteriors <- posteriors/sum(posteriors)
plot(hypotheses,p.posteriors,type='l')
points(hypotheses,bfs/sum(bfs))
# stronger prior odds does not affect the posterior odds that much bc the Bayes factor is so low 