## MACHINE LEARNING WITH CARET PACKAGE

# THE TITANIC DATASET
##install.packages('titanic')
library(titanic)
head(titanic_train)
str(titanic_train)

## DATA WRANGLING

table(titanic_train$Embarked)
train <- titanic_train
train$Embarked[train$Embarked == ""] <- "S"
table(is.na(train$Age))[2]/table(is.na(train$Age))[1]
summary(train$Age)
train$is_age_missing <- ifelse(is.na(train$Age),1,0)
train$travelers <- train$SibSp + train$Parch + 1
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$is_age_missing <- as.factor(train$is_age_missing)
train2 <- subset(train, select = c(Survived, Pclass, Sex, Age,    SibSp, Parch, Fare, Embarked, is_age_missing, travelers))

## caret Unleashed
library(caret)
dummy <- dummyVars(~., train2[,-1])
dummy_train <- predict(dummy, train2[,-1])
head(dummy_train)
pre.process <- preProcess(dummy_train, method = "bagImpute")
imputed.data <- predict(pre.process, dummy_train)
head(imputed.data)
train$Age <- imputed.data[,6]
head(train$Age, 20)

set.seed(123)
partition_indexes <- createDataPartition(train$Survived, times = 1, p = 0.7, list = FALSE)
titanic.train <- train[partition_indexes, ] 
titanic.test <- train[-partition_indexes, ] 
## caret Under The Hood
nnet.params <- getModelInfo("nnet")
nnet.params$nnet$parameters

## MODEL TRAINING
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "grid")
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),                         
                         nrounds = c(50, 75, 100),                         
                         max_depth = 6:8,                        
                         min_child_weight = c(2.0, 2.25, 2.5),            
                         colsample_bytree = c(0.3, 0.4, 0.5),             
                         gamma = 0,                         
                         subsample = 1                         
                         ) 
head(tune.grid)

##install.packages("doSNOW")
library(doSNOW) 
cl <- makeCluster(3, type = "SOCK") 
registerDoSNOW(cl)
caret.cv <- train(Survived ~ ., data = titanic.train, method = "xgbTree", tuneGrid = tune.grid, trControl = train.control)
stopCluster(cl)

## COMPARING MULTIPLE caret models
caret.rf <- train(Survived ~ ., data = titanic.train, method = "rf", trControl = train.control)
                  