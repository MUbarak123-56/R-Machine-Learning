## SUPERVISED AND UNSUPERVISED MACHINE LEARNING

## SUPERVISED

# REGRESSION

head(mtcars)
library(ggplot2)
ggplot(mtcars, aes(x = disp, mpg)) + 
  geom_point() +
  labs(x = "Engine Size (cubic inches)", y = "Fuel Efficiency (Miles per Gallon)")
mod1 <- lm(mpg ~ disp, data = mtcars)
mon <- coef(mod1)
mon[2]*200 + mon[1]

# TRAINING AND TESTING MODELS

summary(mod1)
split_size <- 0.8
sample_size <- floor(split_size*nrow(mtcars))
set.seed(123)
train_indices <- sample(seq_len(nrow(mtcars)), size = sample_size)
train <- mtcars[train_indices,]
test <- mtcars[-train_indices,]

mod2 <- lm(mpg~disp, data = train)
new.data <- data.frame(disp = test$disp)
test$output <- predict(mod2, new.data)
sqrt(sum(test$mpg - test$output)^2/nrow(test))

# CLASSIFICATION

# LOGISTIC REGRESSION
ggplot(mtcars, aes(x = mpg, y = am)) +
  geom_point() +
  labs(x = "Fuel Efficiency (Miles per Gallon)", y = "Vehicle Transmission Type (0 = Automatic, 1 = Manual)")

#install.packages("caTools")
library(caTools)
Label.train = train[,9]
Data.train = train[,-9]
model = LogitBoost(Data.train, Label.train)
Data.test = test
Lab = predict(model, Data.test, type = "raw")
data.frame(row.names(test), test$mpg, test$am, Lab)

# SUPERVISED CLUSTERING METHODS
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(x = "Petal Length", y = "Petal Width")
data <- data.frame(iris$Petal.Length, iris$Petal.Width)

iris.kmeans <- kmeans(data, 2)
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans$cluster,    xlab = "Petal Length", ylab = "Petal Width") 
points(iris.kmeans$centers, pch = 8, cex = 2)

iris.kmeans3 <- kmeans(data, 3)
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster,    xlab = "Petal Length", ylab = "Petal Width")
points(iris.kmeans3$centers, pch = 8, cex = 2)

par(mfrow = c(1, 2))
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = iris.kmeans3$cluster, xlab = "Petal Length", ylab = "Petal Width", main = "Model Output")
plot(x = iris$Petal.Length, y = iris$Petal.Width, pch = as.integer(iris$Species), xlab = "Petal Length", ylab = "Petal Width", main = "Actual Data")
par(mfrow = c(1, 1))
table(iris.kmeans3$cluster, iris$Species)

# MIXED METHODS

# TREE BASED MODELS
#3install.packages('party')
library(party)
tree <- ctree(mpg ~., data = mtcars)
plot(tree)

tree.train <- ctree(mpg ~., data = train)
plot(tree.train)

test$mpg.tree <- predict(tree.train,test)
test$class <- predict(tree.train, test, type = "node")
data.frame(row.names(test), test$mpg, test$mpg.tree, test$class)

# RANDOM FORESTS
#install.packages("randomForest")
library(randomForest)

mtcars.rf <- randomForest(mpg ~.,data = mtcars, ntree = 1000, keep.forest = FALSE, importance = FALSE)
plot(mtcars.rf, log = "y", title = "")

# NEUTRAL NETWORKS
set.seed(123)
#install.packages('nnet')
library(nnet)
iris.nn <-nnet(Species ~., data = iris, size = 2)
table(iris$Species, predict(iris.nn, iris, type = "class"))

# SUPPORT VECTOR MACHINES
install.packages('e1071')
library(e1071)
iris.svm <- svm(Species~., data = iris)
table(iris$Species, predict(iris.svm, iris, type = "class"))

## UNSUPERVISED LEARNING
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2), matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
plot(x)

cl <- kmeans(x, 2)
plot(x, pch = cl$cluster)
cl[2]

