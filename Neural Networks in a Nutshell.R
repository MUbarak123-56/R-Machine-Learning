## NEURAL NETWORKS IN A NUTSHELL

## SINGLE LAYER NEURAL NETWORKS

x1 <- c(0,0,1,1)
x2 <- c(0,1,0,1)
logic <- data.frame(x1,x2)
logic$AND <-as.numeric(x1 & x2)
logic

# install.packages('neuralnet')
library(neuralnet)
set.seed(123)
AND <- c(rep(0,3), 1)
binary.data <- data.frame(expand.grid(c(0,1),c(0,1)), AND)
net <- neuralnet(AND ~ Var1 + Var2, binary.data, hidden = 0, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best")
prediction(net)

## MULTIPLE COMPUTE OUTPUTS
set.seed(123)
AND <- c(rep(0,7), 1)
OR <- c(0,rep(1,7))
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
net <- neuralnet(OR + AND ~ Var1 + Var2 + Var3, binary.data, hidden = 0, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best")
prediction(net)

## HIDDEN COMPUTE NODES
set.seed(123)
AND <- c(rep(0,7), 1)
binary.data <- data.frame(expand.grid(c(0,1), c(0,1), c(0,1)), AND, OR)
net <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 1, err.fct = "ce", linear.output = FALSE)
plot(net, rep = "best")

set.seed(123)
net2 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 2, err.fct = "ce", linear.output = FALSE)
plot(net2, rep = "best")

set.seed(123)
net4 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 4, err.fct = "ce", linear.output = FALSE)
net8 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 8, err.fct = "ce", linear.output = FALSE)
plot(net4, rep = "best")
plot(net8, rep = "best")

set.seed(123)
net6 <- neuralnet(AND ~ Var1 + Var2 + Var3, binary.data, hidden = 6, err.fct = "ce", linear.output = FALSE)
plot(net6, rep = "best")

## MULTILAYER NEURAL NETWORKS
x1 <- c(0,0,1,1)
x2 <- c(0,1,0,1)
logic <- data.frame(x1,x2)
logic$AND <- as.numeric(x1 & x2)
logic$OR <- as.numeric(x1 | x2)
logic
logic$AND <- as.numeric(x1 & x2) + 1
logic$OR <- as.numeric(x1 | x2) + 1

par(mfrow = c(1,1))
plot(x = logic$x1, y = logic$x2, pch = logic$AND, cex = 2, 
     main = "Simple Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5))
plot(x = logic$x1, y = logic$x2, pch = logic$OR, cex = 2, 
     main = "Simple Classification of Two Types",
     xlab = "x", ylab = "y", xlim = c(-0.5,1.5), ylim = c(-0.5, 1.5))
par(mfrow = c(1,1))

x1 <- c(0, 0, 1, 1) 
x2 <- c(0, 1, 0, 1) 
logic <- data.frame(x1, x2) 
logic$AND <- as.numeric(x1 & x2) 
logic$OR <- as.numeric(x1 | x2) 
logic$XOR <- as.numeric(xor(x1, x2)) 
logic$XNOR <- as.numeric(x1 == x2) 
logic

logic$XOR <- as.numeric(xor(x1, x2)) + 1 
logic$XNOR <- as.numeric(x1 == x2) + 1
par(mfrow = c(1, 1))
plot(x = logic$x1, y = logic$x2, pch = logic$XOR, cex = 2, main = "Non-Linear Classification of Two Types",    xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5))
plot(x = logic$x1, y = logic$x2, pch = logic$XNOR, cex = 2, main = "Non-Linear Classification of Two Types",    xlab = "x", ylab = "y", xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5))

logic$XOR <- as.numeric(xor(x1,x2))
set.seed(123)
net.xor <- neuralnet(XOR ~ x1 + x2, logic, hidden = 0, err.fct = "ce", linear.output = FALSE)
prediction(net.xor)
plot(net.xor, rep = "best")

set.seed(123)
and.net <- neuralnet(AND ~ x1 + x2, logic, hidden = 2, err.fct = "ce", linear.output = FALSE)
and.result <- data.frame(prediction(and.net)$rep1)

or.net <- neuralnet(OR ~ x1 + x2, logic, hidden = 2, err.fct = "ce", linear.output = FALSE)
or.result <- data.frame(prediction(or.net)$rep1)

as.numeric(xor(round(and.result$AND), round(or.result$OR)))

xor.data <- data.frame(and.result$AND, or.result$OR, as.numeric(xor(round(and.result$AND), round(or.result$OR)))) 
names(xor.data) <- c("AND", "OR", "XOR")
xor.net <- neuralnet(XOR ~ AND + OR, data = xor.data, hidden = 0, err.fct = "ce", linear.output = FALSE)
prediction(xor.net)
plot(xor.net, rep = "best")

## NEURAL NETWORKS FOR REGRESSION
install.packages("mlbench")
library(mlbench)
data("BostonHousing")
lmfit <- lm(medv ~ ., data = BostonHousing)
lm.predict <- predict(lmfit)
plot(BostonHousing$medv, lm.predict, 
     main = "Linear regression predictions vs actual",
     xlab = "Actual", ylab = "prediction")
library(nnet)
nnet.fit1 <- nnet(medv ~ ., data = BostonHousing, size = 2)
nnet.predict1 <- predict(nnet.fit1)
plot(BostonHousing$medv, nnet.predict1, main = "Neural Network predictions vs actual",
     xlab = "Actual", ylab = "Prediction")
summary(BostonHousing$medv)
summary(BostonHousing$medv/50)
nnet.fit2 <- nnet(medv/50 ~ ., data = BostonHousing, size = 2, maxit = 1000, trace = FALSE)
nnet.predict2 <- predict(nnet.fit2)*50
plot(BostonHousing$medv, nnet.predict2, main = "Neural Network predictions vs actual with normalized response inputs",
     xlab = "Actual", ylab = "Prediction")
mean((lm.predict - BostonHousing$medv)^2)
mean((nnet.predict2 - BostonHousing$medv)^2)

library(caret)
mygrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(4,5,6))
nnetfit <- train(medv/50 ~ ., data = BostonHousing, method = "nnet", maxit = 1000, tuneGrid = mygrid, trace = F)

## NEURAL NETWORKS FOR CLASSIFICATION
iris.df <- iris
smp_size <- floor(0.75*nrow(iris.df))

set.seed(123)
train_ind <- sample(seq_len(nrow(iris.df)), size = smp_size)
train <- iris.df[train_ind,]
test <- iris.df[-train_ind,]
iris.nnet <- nnet(Species ~ ., data = train, size = 4, decay = 0.0001, maxit = 500, trace = FALSE)
predictions <- predict(iris.nnet, test[,1:4], type = "class")
table(predictions, test$Species)

## NEURAL NETWORKS WITH CARET

# Regression
##install.packages("car")
library(car)
library(caret)
trainIndex <- createDataPartition(Prestige$income, p = 0.7, list = F) 
prestige.train <- Prestige[trainIndex, ] 
prestige.test <- Prestige[-trainIndex, ]
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6,    7)) 
prestige.fit <- train(income ~ prestige + education, data = prestige.train, method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)
prestige.predict <- predict(prestige.fit, newdata = prestige.test)
summary(prestige.test$income)
sqrt(mean((prestige.predict - prestige.test$income)^2))
prestige.fit <- train(income ~ prestige + education, data = prestige.train, method = "neuralnet")
prestige.predict <- predict(prestige.fit, newdata = prestige.test)
sqrt(mean((prestige.predict - prestige.test$income)^2))

## Classification
iris.caret <- train(Species ~ ., data = train, method = "nnet", trace = FALSE) 
predictions <- predict(iris.caret, test[, 1:4]) 
table(predictions, test$Species)

iris.caret.m <- train(Species ~ ., data = train, method = "multinom", trace = FALSE) 
predictions.m <- predict(iris.caret.m, test[, 1:4]) 
table(predictions.m, test$Species)
