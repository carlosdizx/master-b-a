train <- read.csv(file = "data/AAT1 - flats.csv", header = TRUE)
modelo <- lm(Precio ~ M2, data = train)

# precio = B0 + B1 M2; M2 varaible


train_test <- read.csv(file = "data/AAT1 - flats_test.csv", header = TRUE)

# predict(modelo, newdata = train_test)
print(train_test)