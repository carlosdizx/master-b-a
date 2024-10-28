library(caTools)
library(ROCR)

RNGversion("4.0.3")

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


diabetes <- read.csv("data/diabetes.csv")

str(diabetes)

summary(diabetes)

head(diabetes)

dim(diabetes)

is.na(diabetes)

# --------------------------------
set.seed(1000)

split <- sample.split(diabetes$Outcome, SplitRatio = 0.75)
train <- subset(diabetes, split == TRUE)
test <- subset(diabetes, split == FALSE)

diabetesModel <- glm(Outcome ~ ., data = train, family = "binomial")

predictTest <- predict(diabetesModel, type = "response", newdata = test)

confMatrix <- table(test$Outcome, predictTest > 0.36)
randomnessControl(174)
confMatrix

accuracy <- sum(diag(confMatrix)) / sum(confMatrix)
print(paste("Model accuracy percentage:", accuracy))

baseline_accuracy <- sum(diabetes$Outcome == 0) / nrow(diabetes) * 100
print(paste("Baseline model accuracy:", round(baseline_accuracy, 2), "%"))

sensitivity <- confMatrix[2, 2] / sum(confMatrix[2, ])
print(paste("Model sensitivity:", sensitivity))

specificity <- confMatrix[1, 1] / sum(confMatrix[1, ])
print(paste("Model specificity:", specificity))

predictTest <- predict(diabetesModel, type="response", newdata = test)
pred <- prediction(predictTest, test$Outcome)
ROC = performance(pred, "tpr", "fpr")
plot(ROC, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(1.2,-0.4))


x <- as.numeric(performance(pred, "auc")@y.values)
randomnessControl(313)
sx