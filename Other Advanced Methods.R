## OTHER ADVANCED METHODS

# NAIVE BAYES CLASSIFICATION

library(e1071)
iris_sample <- sample(1:nrow(iris), 0.8*nrow(iris), replace = FALSE)
train <- iris[iris_sample,]
test <- iris[-iris_sample,]
nb_model <- naiveBayes(Species ~ ., data = train)
predictions <- predict(nb_model, test)
table(predictions, test$Species)

# PRINCIPAL COMPONENT ANALYSIS

pca <- princomp(mtcars, scores = TRUE, cor = TRUE)
summary(pca)
plot(pca)
pca$loadings[,1:5]

scores.df <- data.frame(pca$scores)
scores.df$car <- row.names(scores.df)
plot(x = scores.df$Comp.1, y = scores.df$Comp.2, xlab = "Comp1 (mpg,cyl)", ylab = "Comp2 (qsec, gear, am)")
text(scores.df$Comp.1, scores.df$Comp.2, labels = scores.df$car, cex = 0.7, pos = 3)

## LINEAR DISCRIMINANT ANALYSIS
iris.pca <- prcomp(iris[,-5], center = T, scale. = T)
iris.pca$sdev^2/sum(iris.pca$sdev^2)
table(iris$Species)

library(MASS)
iris.lda <- lda(Species ~ ., data = iris, prior = c(1/3, 1/3, 1/3))
iris.lda$svd^2/sum(iris.lda$svd^2)
iris.lda$scaling

iris.lda.predictions <- predict(iris.lda, iris)
table(iris.lda.predictions$class, iris$Species)

combined <- data.frame(Species = iris[, "Species"], pca = iris.pca$x, lda = iris.lda.predictions$x)
library(ggplot2)
library(gridExtra)
lda.plot <- ggplot(combined) + geom_point(aes(lda.LD1, lda.LD2, shape = Species)) + scale_shape_manual(values = c(0, 1, 2))
pca.plot <- ggplot(combined) + geom_point(aes(pca.PC1, pca.PC2, shape = Species)) + scale_shape_manual(values = c(0, 1, 2))
grid.arrange(pca.plot, lda.plot)

## SUPPORT VECTOR MACHINES

s <- sample(150,100)
col <- c("Petal.Length", "Petal.Width", "Species")
iris.train <- iris[s,col]
iris.test <- iris[-s,col]
svmfit <- svm(Species ~ ., data = iris.train, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, iris.train[,col])
tuned <- tune(svm, Species ~ ., data = iris.train, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tuned)
svmfit <- svm(Species ~ ., data = iris.train, kernel = "linear", cost = 1, scale = FALSE)
plot(svmfit, iris.train[,col])

data(cats)
model <- svm(Sex ~ ., data = cats)
print(model)
summary(model)
plot(model, cats)

data.samples <- sample(1:nrow(cats), nrow(cats)*0.7, replace = FALSE)
training_data <- cats[data.samples,]
testing_data <- cats[-data.samples,]
svm.cats <- svm(Sex ~ ., data = training_data)
prediction.svm <- predict(svm.cats, testing_data[, -1], type = "class")
table(testing_data[,1], prediction.svm)

## K-NEAREST NEIGHBORS
knn.ex <- head(mtcars[,1:3])
knn.ex
knn.ex$dist <- sqrt((knn.ex$cyl - 6)^2 + (knn.ex$disp - 225)^2) 
knn.ex[order(knn.ex[, 4]), ]

## REGRESSION USING kNN

library(caret)
data(BloodBrain)
inTrain <- createDataPartition(logBBB, p = 0.8)[[1]]
trainX <- bbbDescr[inTrain,]
trainY <- logBBB[inTrain]
testX <- bbbDescr[-inTrain,]
testY <- logBBB[-inTrain]
fit <- knnreg(trainX, trainY, k = 3)
plot(testY, predict(fit, testX))

## CLASSIFICATION USING kNN

Sys.getenv("R_ARCH")
install.packages("RWeka")
library(RWeka)
iris <- read.arff(system.file("arff", "iris.arff", package = "RWeka"))
classifier <- IBk(class ~ ., data = iris) 
summary(classifier)
classifier <- IBk(class ~ ., data = iris, control = Weka_control(K = 20, X = TRUE))
evaluate_Weka_classifier(classifier, numFolds = 10)
