o.ring.data <- data.frame(
  temp=c(53.00,57.00,58.00,63.00,66.00,67.00,67.00,67.00,68.00,69.00,70.00,70.00,70.00,70.00,72.00,73.00,75.00,75.00,76.00,76.00,78.00,79.00,81.00),
damage=c(5.00,1.00,1.00,1.00,0.00,0.00,0.00,0.00,0.00,0.00,1.00,0.00,1.00,0.00,0.00,0.00,0.00,1.00,0.00,0.00,0.00,0.00,0.00))

library(magrittr)
library(dplyr)

o.ring.data %>% 
  transmute(temp,damage_indicator=ifelse(damage > 0,1,0)) %>%
  glm(formula = damage_indicator ~ temp, data = ., family = 'binomial') %>% summary

plot(o.ring.data)