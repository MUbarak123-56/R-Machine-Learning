## SAMPLING STATISTICS AND MODEL TRAINING IN R

## Sampling in R

# Simple Random Sampling
iris.df <- data.frame(iris)
sample.index <- sample(1:nrow(iris.df), nrow(iris)*0.75, replace = FALSE)
head(iris[sample.index,])
nrow(iris[-sample.index,])
nrow(iris[sample.index,])
summary(iris)
summary(iris[sample.index,]) ## Our goal is to get the same representation like the regular iris dataset

# Stratified Random Sampling
install.packages('splitstackshape')
library(splitstackshape) 
summary(stratified(iris, "Sepal.Length", 0.7))
summary(stratified(iris, c("Petal.Width","Sepal.Width"), 0.7))

# Systematic Random Sampling
sys.sample = function(N, n) {   
  k = ceiling(N/n)    
  r = sample(1:k, 1)    
  sys.samp = seq(r, r + k * (n - 1), k) 
}
systematic.index <- sys.sample(nrow(iris), nrow(iris)*0.75)
summary(iris[systematic.index,])

## Training and Testing in R
# Training and Test sets: Regression Modeling

set.seed(123)
x <- rnorm(100,2,1)
y <- exp(x) + rnorm(5,0,2)
plot(x,y)
linear <- lm(y ~ x)
z <- data.frame(x, y)
mod <- lm(y ~ x, data = z)
coeff <- coef(mod)
ggplot(z,aes(x,y)) + geom_point() + geom_abline(intercept = coeff[1], slope = coeff[2])
summary(mod)

z.samples <- sample(1:nrow(z), nrow(z)*0.7, replace = FALSE)
z.train <- z[z.samples,]
z.test <- z[-z.samples,]

mod.linear <- lm(y ~ x, data = z.train)
pred <- predict(mod.linear, z.test)
RMSE.df <- data.frame(predicted = pred, actual = z.test$y, SE = ((pred - z.test$y)^2/length(pred)))
head(RMSE.df)
sqrt(sum(RMSE.df$SE))

library(splines)
z.quadratic <- lm(y ~ ns(x,2) + x, data = z)
quadratic.output <- predict(z.quadratic, z.test)
RMSE.quad.df <- data.frame(predicted = quadratic.output, actual = z.test$y, SE = ((quadratic.output - z.test$y)^2/length(quadratic.output)))
head(RMSE.quad.df)
sqrt(sum(RMSE.quad.df$SE))

z.poly <- lm(y ~ ns(x,4), data = z)
polyn.output <- predict(z.poly, z.test)
RMSE.polyn.df = data.frame(predicted = polyn.output, actual = z.test$y,    SE = ((polyn.output - z.test$y)^2/length(polyn.output)))
head(RMSE.polyn.df)
sqrt(sum(RMSE.polyn.df$SE))

## TRAINING AND TESTING: CLASSIFICATION MODELING
iris.df <- iris
iris.df$Species <- as.character(iris.df$Species)
iris.df$Species[iris.df$Species != "setosa"] <- "other"
iris.df$Species <- as.factor(iris.df$Species)
iris.sample <- sample(1:nrow(iris.df), nrow(iris.df)*0.7, replace = FALSE)
training.iris <- iris.df[iris.sample,]
test.iris <- iris.df[-iris.sample,]
library(randomForest)
iris.rf <- randomForest(Species ~ ., data = training.iris)
iris.prediction <- predict(iris.rf, test.iris)
table(iris.prediction, test.iris$Species)

# Cross Validation
set.seed(123)
x <- rnorm(100, 2, 1) 
y = exp(x) + rnorm(5, 0, 2) 
data <- data.frame(x, y)
data.shuffled <- data[sample(nrow(data)), ] 
folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
errors <- c(0)
for (i in 1:10) {    
  fold.indexes <- which(folds == i, arr.ind = TRUE)
  test.data <- data[fold.indexes, ]    
  training.data <- data[-fold.indexes, ]
  train.linear <- lm(y ~ x, training.data)    
  train.output <- predict(train.linear, test.data)    
  errors <- c(errors, sqrt(sum(((train.output - test.data$y)^2/length(train.output))))) 
}
errors[2:11]
mean(errors[2:11])
