## REGRESSION IN A NUTSHELL

# LINEAR REGRESSION

model <- lm(mpg ~ disp, data = mtcars)
coeff <- coef(model)

library(ggplot2)
ggplot(mtcars, aes(x = disp, y = mpg)) + 
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2]) +
  labs(x = "Engine size (cubic inches)",y = "Fuel Efficiency (Miles per Gallon")
summary(model)

# MULTIVARIATE REGRESSION
lm.wt <- lm(mpg ~ disp + wt, data = mtcars) # with wt and disp
summary(lm.wt)

lm.cyl <- lm(mpg ~ disp + cyl + wt, data = mtcars) # with wt, disp and cyl
summary(lm.cyl)

lm.cyl.wt <- lm(mpg ~ cyl + wt, data = mtcars) # using cyl and wt
summary(lm.cyl.wt)

lm.all <- lm(mpg ~ ., data = mtcars) # using all independent variables
summary(lm.all)

# REGULARIZATION

### LASSO REGRESSION

#install.packages("lasso2")
library(lasso2)
lm.lasso <- l1ce(mpg ~., data = mtcars)
summary(lm.lasso)$coefficients

lm.lasso2 <- l1ce(mpg ~ cyl + hp + wt + am + carb, data = mtcars)
summary(lm.lasso2)$coefficients

lm.lasso3 <- l1ce(mpg ~ cyl + hp + wt, data = mtcars)
summary(lm.lasso3)$coefficients

lm.lasso4 <- l1ce(mpg ~ cyl + wt, data = mtcars)
summary(lm.lasso4)$coefficients

# POLYNOMIAL REGRESSION
pop <- data.frame(uspop)
pop$uspop <- as.numeric(pop$uspop)
pop$year <- seq(from = 1790, to = 1970, by = 10)
ggplot(pop, aes(uspop, year)) +
  geom_point() +
  labs(title = "US population from 1790 to 1970", x = "Year", y = "Population")

lm1 <- lm(uspop ~ year, data = pop)
coeff2 <- coef(lm1)
ggplot(pop, aes(uspop, year)) +
  geom_point() +
  geom_abline(intercept = coeff2[1], slope = coeff2[2], lty = 2, col = "red") +
  labs(title = "US population from 1790 to 1970", x = "Year", y = "Population")
summary(lm1)

lm2 <- lm(uspop ~ poly(year,2), data = pop)
summary(lm2)

plot(y = pop$uspop, x = pop$year, main = "United States Population From 1790 to 1970",    xlab = "Year", ylab = "Population")
pop$lm2.predict = predict(lm2, newdata = pop)
lines(sort(pop$year), fitted(lm2)[order(pop$year)], col = "blue",    lty = 2)

lm3 <- lm(uspop ~ poly(year,3), data = pop)
lm4 <- lm(uspop ~ poly(year,4), data = pop)
lm5 <- lm(uspop ~ poly(year,5), data = pop)
lm6 <- lm(uspop ~ poly(year,6), data = pop)
par(mfrow = c(2, 3)) 
plot(resid(lm1), main = "Degree 1", xlab = "Sequential Year",    ylab = "Fit Residual") 
plot(resid(lm2), main = "Degree 2", xlab = "Sequential Year",    ylab = "Fit Residual") 
plot(resid(lm3), main = "Degree 3", xlab = "Sequential Year",    ylab = "Fit Residual") 
plot(resid(lm4), main = "Degree 4", xlab = "Sequential Year",    ylab = "Fit Residual") 
plot(resid(lm5), main = "Degree 5", xlab = "Sequential Year",    ylab = "Fit Residual") 
plot(resid(lm6), main = "Degree 6", xlab = "Sequential Year",    ylab = "Fit Residual")
par(mfrow = c(1,1))

c(sum(abs(resid(lm1))), sum(abs(resid(lm2))), sum(abs(resid(lm3))), sum(abs(resid(lm4))), sum(abs(resid(lm5))), sum(abs(resid(lm6))))

## GOODNESS OF FIT DATA

## ROOT MEAN SQUARE ERROR
uspop.2020 <- data.frame(year = c(1980, 1990, 2000, 2010), uspop = c(226.5, 249.6, 282.2, 309.3)) 
uspop.2020.predict <- uspop.2020

pop2 <- data.frame(uspop) 
pop2$uspop <- as.numeric(pop$uspop) 
pop2$year <- seq(from = 1790, to = 1970, by = 10) 

uspop.2020.predict$lm1 <- predict(lm(uspop ~ poly(year, 1), data = pop2),    uspop.2020)
uspop.2020.predict$lm2 <- predict(lm(uspop ~ poly(year, 2), data = pop2),    uspop.2020)
uspop.2020.predict$lm3 <- predict(lm(uspop ~ poly(year, 3), data = pop2),    uspop.2020)
uspop.2020.predict$lm4 <- predict(lm(uspop ~ poly(year, 4), data = pop2),    uspop.2020)
uspop.2020.predict$lm5 <- predict(lm(uspop ~ poly(year, 5), data = pop2),    uspop.2020)
uspop.2020.predict$lm6 <- predict(lm(uspop ~ poly(year, 6), data = pop2),    uspop.2020)

c(sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm1)^2)),
  sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm2)^2)),    
  sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm3)^2)),    
  sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm4)^2)),    
  sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm5)^2)),    
  sqrt(mean((uspop.2020.predict$uspop - uspop.2020.predict$lm6)^2)))

## MODEL SIMPLICITY AND GOODNESS OF FIT
table((summary(lm1)$coefficients[, 4]) < 0.05)
summary(lm1)$r.squared

model.order <- c(1,2,3,4,5,6)
coef.true <- c(  
  table((summary(lm1)$coefficients[,4])<0.05) - 1  ,
  table((summary(lm2)$coefficients[,4])<0.05) - 1  ,
  table((summary(lm3)$coefficients[,4])<0.05)[2] - 1  ,
  table((summary(lm4)$coefficients[,4])<0.05)[2] - 1  ,
  table((summary(lm5)$coefficients[,4])<0.05)[2] - 1  ,
  table((summary(lm6)$coefficients[,4])<0.05)[2] - 1
)
coef.false <- c(0,0,
                table((summary(lm3)$coefficients[,4])<0.05)[1]  ,
                table((summary(lm4)$coefficients[,4])<0.05)[1]  ,
                table((summary(lm5)$coefficients[,4])<0.05)[1]  ,
                table((summary(lm6)$coefficients[,4])<0.05)[1]
)
model.rsq <- c(  
  summary(lm1)$r.squared  ,
  summary(lm2)$r.squared  ,
  summary(lm3)$r.squared  ,
  summary(lm4)$r.squared  ,
  summary(lm5)$r.squared  ,
  summary(lm6)$r.squared
)
model.comparison <- data.frame(model.order, model.rsq, coef.true, coef.false) 
model.comparison$goodness <- (model.comparison$coef.true / model.comparison $model.order)
model.comparison

model.comparison$rank <- sqrt(model.comparison$goodness^2 + model.comparison$model.rsq^2) 
model.comparison

## LOGISTIC REGRESSION
data <- data.frame(tumor.size <- c(1, 2, 3, 4, 5, 6, 7, 8, 9,20), malignant <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1))
tumor.lm <- lm(malignant ~ tumor.size, data = data)
plot(y = data$malignant, x = data$tumor.size, main = "Tumor Malignancy by Size",    ylab = "Type (0 = benign, 1 = cancerous)", xlab = "Tumor Size")
abline(a = coef(tumor.lm[1]), b = coef(tumor.lm[2]))
coef(tumor.lm)
summary(tumor.lm)$r.squared

## THE DECISION BOUNDARY
plot(y = data$malignant, x = data$tumor.size, main = "Tumor Malignancy by Size",    ylab = "Type (0 = benign, 1 = cancerous)", xlab = "Tumor Size") 
abline(v = 4.5)
e <- exp(1) 
curve(1/(1 + e^-x), -10, 10, main = "The Sigmoid Function", xlab = "Input",    ylab = "Probability")

lengths <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 
t1 = -4.5 
t2 = 1 
g = t1 + t2 * lengths 
s = 1/(1 + e^-g) 
data.frame(lengths, g, s)

plot(y = s, x = lengths, pch = 1, main = "Sigmoid Function Inputs and Rounding Estimates", xlab = "Tumor Lengths", ylab = "Probability of Class 1 Typification")
points(y = round(s), x = lengths, pch = 3)

## BINARY CLASSIFICATION
plot(iris$Sepal.Length ~ iris$Sepal.Width, main = "Iris Flower Sepal Length vs Sepal Width",    xlab = "Sepal Width", ylab = "Sepal Length")

iris.binary <- iris 
iris.binary$binary <- as.numeric(iris[, 5] == "setosa")
iris.logistic <- glm(binary ~ Sepal.Width + Sepal.Length, data = iris.binary,    family = "binomial") 
iris.logistic

slope.iris <- coef(iris.logistic)[2]/(-coef(iris.logistic)[3]) 
int.iris <- coef(iris.logistic)[1]/(-coef(iris.logistic)[3])
slope.iris
int.iris

iris.binary$binary[iris.binary$binary == 0] <- 2
plot(Sepal.Length ~ Sepal.Width, data = iris.binary, pch = (binary), main = "Iris Flower Sepal Length vs Sepal Width", xlab = "Sepal Width", ylab = "Sepal Length")
abline(a = int.iris, b = slope.iris)

## MULTICLASS CLASSIFICATION
multi <- data.frame(x1 = c(0.03, 0.24, 0.21, 0, 0, 0.23, 0.6, 0.64, 0.86, 0.77), x2 = c(0.07, 0.06, 0.19, 1.15, 0.95, 1, 0.81, 0.64, 0.44, 0.74), lab = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3))
plot(x2 ~ x1, pch = lab, cex = 2, data = multi, main = "Multi-Class Classification",    xlab = "x", ylab = "y")

par(mfrow = c(1, 3)) 
multi$lab2 <- c(1, 1, 1, 4, 4, 4, 4, 4, 4, 4) 
plot(x2 ~ x1, pch = lab2, cex = 2, data = multi, main = "Multi-Class Classification",    xlab = "x", ylab = "y")
multi$lab3 <- c(4, 4, 4, 2, 2, 2, 4, 4, 4, 4) 
plot(x2 ~ x1, pch = lab3, cex = 2, data = multi, main = "Multi-Class Classification",    xlab = "x", ylab = "y")
multi$lab4 <- c(4, 4, 4, 4, 4, 4, 3, 3, 3, 3) 
plot(x2 ~ x1, pch = lab4, cex = 2, data = multi, main = "Multi-Class Classification",    xlab = "x", ylab = "y")
par(mfrow = c(1,1))

library(nnet) 
multi.model <- multinom(lab ~ x2 + x1, data = multi, trace = F) 
multi.model

multi.int.1 <- -coef(multi.model)[1]/coef(multi.model)[3]
multi.slope.1 <- -coef(multi.model)[5]/coef(multi.model)[3]
multi.int.2 <- -coef(multi.model)[2]/coef(multi.model)[4] 
multi.slope.2 <- -coef(multi.model)[6]/coef(multi.model)[4]

plot(x2 ~ x1, pch = lab, cex = 2, data = multi, main = "Multi-Class Classification",    xlab = "x", ylab = "y") 
abline(multi.int.1, multi.slope.1) 
abline(multi.int.2, multi.slope.2)

## LOGISTIC REGRESSION WITH CARET
##install.packages("caret")
library(caret)
data("GermanCredit")
Train <- createDataPartition(GermanCredit$Class, p = 0.6, list = FALSE) 
training <- GermanCredit[Train, ] 
testing <- GermanCredit[-Train, ]
mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + CreditHistory.Critical, data = training, method = "glm",    family = "binomial")
predictions <- predict(mod_fit, testing[, -10]) 
table(predictions, testing[, 10])

mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + CreditHistory.Critical, data = training,    method = "LogitBoost",    family = "binomial")
predictions <- predict(mod_fit, testing[, -10]) 
table(predictions, testing[, 10])


