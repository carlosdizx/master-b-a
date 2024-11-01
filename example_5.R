library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# -----------------------------------
randomnessControl <- function(x)
{
  if (sample(1:1000, 1) == x)
  {
    return("No tienes problemas de aleatoriedad")
  }
  else
  {
    return("Tienes problemas con la aleatoriedad. Vuelve a la última instrucción
'set.seed' y des de esa línea (incluida), ejecuta una única vez las instrucciones
necesarias hasta la solución.")
  }
}
# -----------------------------------


mammals <- read.csv("data/mammals.csv")

set.seed(1000)
split <- sample.split(mammals$sleep_total, SplitRatio = 0.85)
train <- subset(mammals, split==TRUE)
test <- subset(mammals, split==FALSE)

mammalsTree <- rpart(sleep_total~., data=train, method="class", minbucket=5)
mammalsTree

prp(mammalsTree)
randomnessControl(889)

MAE <- function(actual, predicted) { mean(abs(actual-predicted))}

mammalsPrediction <- predict(mammalsTree, newdata = test, type = "class")
mammalsPrediction <- as.numeric(as.character(mammalsPrediction))

mammalsPrediction

MAE(test$sleep_total, mammalsPrediction)

set.seed(950)
numFolds <- trainControl( method = "cv", number = 10 )
cpGrid <- expand.grid( .cp = seq(0.01,0.1,0.005))
train(sleep_total ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid, na.action = na.pass )

mammalsTreeCV <- rpart(sleep_total~., data=train, method="class", cp = 0.05)

mammalsPredictionCV <- predict(mammalsTreeCV, newdata = test, type = "class")
mammalsPredictionCV <- as.numeric(as.character(mammalsPredictionCV))

randomnessControl(978)
MAE(test$sleep_total, mammalsPredictionCV)

randomForest(target ~ ., data = train, ntree=200, nodesize=25)
