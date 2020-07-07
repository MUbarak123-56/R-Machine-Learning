## WHAT IS A MODEL?

head(mtcars)
pairs(mtcars[1:7], lower.panel = NULL)

library(ggplot2)
ggplot(mtcars) + 
  geom_point(aes(wt,mpg)) + 
  labs(x = "Vehicle weight",y = "Vehicle Fuel Efficiency in Miles per Gallon")
mt.model = lm(mpg ~wt, data = mtcars)
coeff = coef(mt.model)
ggplot(mtcars) + 
  geom_point(aes(wt,mpg)) + 
  geom_abline(intercept = coeff[1], slope = coeff[2], color= 'red') +
  labs(x = "Vehicle weight",y = "Vehicle Fuel Efficiency in Miles per Gallon") +
  theme_classic()
