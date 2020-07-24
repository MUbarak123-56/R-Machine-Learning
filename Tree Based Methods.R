## TREE BASED MODELS

#install.packages('rpart')
library(rpart)
head(cu.summary)
fit <- rpart(
  Mileage ~ Price + Country + Reliability + Type,
  method = "anova",
  data = cu.summary
)
plot(fit, uniform = TRUE, margin = 0.1)
text(fit, use.n = TRUE, all = TRUE, cex = .8)
rsq.rpart(fit)[1]
plotcp(fit)
fit$cptable

fit.pruned <- prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"])
par(mfrow = c(1, 2))
plot(fit, uniform = TRUE, margin = 0.1, main = "Original Tree")
text(fit, use.n = TRUE, all = TRUE, cex = 0.8)
plot(fit.pruned, uniform = TRUE, margin = 0.1, main = "Pruned Tree") 
text(fit.pruned, use.n = TRUE, all = TRUE, cex = 0.8)

## DECISION TREES FOR REGRESSION
cu.summary.complete <- cu.summary[complete.cases(cu.summary),] 
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) * 0.7, replace = FALSE) 
training.data <- cu.summary.complete[data.samples, ] 
test.data <- cu.summary.complete[-data.samples, ]
fit <- rpart(Mileage~Price + Country + Reliability + Type,   method="anova", data=training.data  )
fit.pruned<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
prediction <- predict(fit.pruned, test.data)
output <- data.frame(test.data$Mileage, prediction)
RMSE = sqrt(sum((output$test.data.Mileage - output$prediction)^2)/nrow(output)) 
RMSE

## DECISION TREES FOR CLASSIFICATION
cu.summary.complete <- cu.summary[complete.cases(cu.summary),] 
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) *    0.7, replace = FALSE) 
training.data <- cu.summary.complete[data.samples, ] 
test.data <- cu.summary.complete[-data.samples, ]
fit <- rpart(Type ~ Price + Country + Reliability + Mileage, method = "class", data = training.data)
fit.pruned <- prune(fit, cp = fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"])
prediction <- predict(fit.pruned, test.data, type = "class")
table(prediction, test.data$Type)

## CONDITIONAL INTERFERENCE TREE
##install.packages('party')
library(party)
fit2 <- ctree(Mileage ~ Price + Country + Reliability + Type, data = na.omit(cu.summary))
plot(fit2)

fit3 <- ctree(Type ~ Price + Country + Reliability + Mileage, data = na.omit(cu.summary))
plot(fit3)

## CONDITIONAL INTERFERENCE TREE REGRESSION
set.seed(123)
cu.summary.complete <- cu.summary[complete.cases(cu.summary),] 
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) * 0.7, replace = FALSE) 
training.data <- cu.summary.complete[data.samples, ] 
test.data <- cu.summary.complete[-data.samples, ]
fit.ctree <- ctree(Mileage ~ Price + Country + Reliability + Type, data = training.data)
prediction.ctree <- predict(fit.ctree, test.data)
output <- data.frame(test.data$Mileage, prediction.ctree)
RMSE = sqrt(sum((output$test.data.Mileage - output$Mileage)^2)/nrow(output)) 
RMSE

## CONDITIONAL INTERFERENCE TREE CLASSIFICATION
set.seed(456)
data.samples <- sample(1:nrow(cu.summary), nrow(cu.summary) * 0.7, replace = FALSE) 
training.data <- cu.summary[data.samples, ] 
test.data <- cu.summary[-data.samples, ]
fit.ctree <- ctree(Type ~ Price + Country + Reliability + Mileage, data = training.data)
prediction.ctree <- predict(fit.ctree, test.data)
table(test.data$Type, prediction.ctree)

## RANDOM FORESTS

## RANDOM FOREST REGRESSION
library(randomForest) 
set.seed(123)
cu.summary.complete <- cu.summary[complete.cases(cu.summary),]
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) * 0.7, replace = FALSE)
training.data <- cu.summary.complete[data.samples, ] 
test.data <- cu.summary.complete[-data.samples, ]
fit.rf <- randomForest(Mileage ~ Price + Country + Reliability + Type, data = training.data)
prediction.rf <- predict(fit.rf, test.data)
output <- data.frame(test.data$Mileage, prediction.rf)
RMSE = sqrt(sum((output$test.data.Mileage - output$prediction.rf)^2)/nrow(output)) 
RMSE

## RANDOM FOREST CLASSIFICATION
set.seed(456)
cu.summary.complete <- cu.summary[complete.cases(cu.summary),] 
data.samples <- sample(1:nrow(cu.summary.complete), nrow(cu.summary.complete) * 0.7, replace = FALSE) 
training.data <- cu.summary.complete[data.samples, ]
test.data <- cu.summary.complete[-data.samples, ]
fit.rf <- randomForest(Type ~ Price + Country + Reliability + Mileage, data = training.data)
prediction.rf <- predict(fit.rf, test.data)
table(test.data$Type, prediction.rf)
