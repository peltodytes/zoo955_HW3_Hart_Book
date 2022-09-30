#Hart & Book HW3 ZOO955
require(dplyr)

# intercept
B0 = 1
# slope
B1 = 5

data = data.frame(x = seq(1,100), lambda = NA, y = NA, log10y = NA)

# populating the columns
for(i in 1:nrow(data)){
  
  data$lambda[i] = B0 + B1*(data$x[i])
  data$y[i] = rpois(1, data$lambda[i])
  data$log10y[i] = log10(data$y[i])
  
}

plot(data$x, data$y)
plot(data$x, data$log10y)

# not very Poisson-ish
plot(density(data$y))
# I thought the variable lambda might account for what we're seeing so I tried to normalize for that. Still looks a little strange
plot(density(data$y/data$lambda))

# log transformation doesn't seem to make it any more Gaussian
plot(density(data$log10y))


plm = glm(y ~ x, family = 'poisson', data = data)
# intercept & slope are way off, not sure if this is just from randomness of model or because of a data issue
summary(plm)

llm = glm(log10y ~ x, family = 'gaussian', data = data)
# intercept & slope are way off
summary(llm)


# New Attempt

# Data generation borrowed from StackExchange user ocram 
#sample size
n <- 100
#regression coefficients
beta0 <- 1
beta1 <- 0.2
#generate covariate values
x <- runif(n=n, min=10, max=11.5)
#compute mu's
mu <- exp(beta0 + beta1 * x)
#generate Y-values
y <- rpois(n=n, lambda=mu)
#data set
data <- data.frame(y=y, x=x, logy=log(y))
data

plot(density(data$y))
plot(density(log(data$y)))
plot(density(data$logy))

plm = glm(y ~ x, family = poisson, data = data)
summary(plm)

llm = glm(logy ~ x, family = gaussian, data = data)
summary(llm)

llm$coefficients
