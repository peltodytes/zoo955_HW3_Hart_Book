#Hart & Book HW3 ZOO955
require(readxl); require(dplyr)

data = read_excel('BSB_tagging_data.xlsx')

hist(data$Length_at_capture)
hist(log(data$Length_at_capture))

pois_response = rpois(1000, 5) %>% sort()
pred = rnorm(1000, mean = 4, sd = 1) %>%  sort()

data = as.data.frame(cbind(pois_response, pred)) 
plot(data$pred, data$pois_response)

logpois_data = log10(pois_data)

glm()