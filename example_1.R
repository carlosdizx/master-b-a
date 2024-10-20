train <- read.csv(file = "data/AAT1 - flats.csv", header = TRUE)
modelo <- lm(Precio ~ M2, data = train)

# precio = B0 + B1*M2; M2 varaible independiente; precio variable objetivo o dependiente
summary(modelo)


test <- read.csv(file = "data/AAT1 - flats_test.csv", header = TRUE)

predict(modelo, newdata = test, interval = "confidence")
predict(modelo, newdata = test, interval = "predict")


flatPred <- cbind(train, predict(modelo, interval = "prediction"))
p <- ggplot(flatPred, aes(M2, Precio))
geom_point()
stat_smooth(method = lm)
p + theme_bw()
geom_line(aes(y = Iwr), color = "red", linetype = "dashed")
geom_line(aes(y = upr), color = "red", linetype = "dashed")