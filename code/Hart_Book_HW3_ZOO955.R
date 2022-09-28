#Hart & Book HW3 ZOO955
require(dplyr)

B0 = 1
B1 = 5

data = data.frame(x = seq(1,100), lambda = NA, y = NA, log10y = NA)

for(i in 1:nrow(data)){
  
  data$lambda[i] = B0 + B1*(data$x[i])
  data$y[i] = rpois(1, data$lambda[i])
  data$log10y[i] = log10(data$y[i])
  
}

plot(density(data$y))
plot(density(data$y/data$lambda))

plot(density(data$log10y))


plm = glm(y ~ x, family = 'poisson', data = data)
summary(plm)

llm = glm(log10y ~ x, family = 'gaussian', data = data)
summary(llm)
