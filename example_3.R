library(ggplot2)

moneyball <- read.csv("data/baseball.csv")

str(moneyball)

moneyball$Team

summary(moneyball)
summary(moneyball$OOBP)


quantile(moneyball$RS)

m <- ggplot(moneyball, aes(x = W, y = Team, color = factor(Playoffs))) +
  geom_point() +
  scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")
m + xlab("Wins")
m

moneyball$RD <- moneyball$RS - moneyball$RA

moneyball[which.min(moneyball$RD),]$Year

ggplot(moneyball, aes(x = RD, y = W, color = factor(Playoffs))) +
  geom_point() +
  scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")

modelW <- lm(W ~ RD, data = moneyball)

summary(modelW)
summary(modelW)["r.squared"]

coef_model <- coef(modelW)
coef_model

m <- coef_model["RD"]
b <- coef_model["(Intercept)"]
target_wins <- 95


m
b
target_wins


required_RD <- (target_wins - b) / m
required_RD

print(b + m * required_RD)


ggplot(moneyball, aes(x = RD, y = W)) +
  geom_point(aes(color = factor(Playoffs))) +
  scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs") +
  geom_abline(intercept = b, slope = m, color = "blue") +  # Línea de regresión
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +  # Línea de 95 victorias
  labs(title = "Wins vs Run Differential",
       x = "Run Differential (RD)",
       y = "Wins")

modelRS <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(modelRS)["r.squared"]

newModelRS <- lm(RS ~ OBP + SLG, data = moneyball)
summary(newModelRS)["r.squared"]

modelRA <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(modelRA)


coef_new_model <- coef(newModelRS)

OBP <- 0.339
SLG <- 0.430

predicted_RS <- coef_new_model["(Intercept)"] + coef_new_model["OBP"] * OBP + coef_new_model["SLG"] * SLG

cat("La predicción de RS utilizando OBP y SLG es:", predicted_RS, "\n")

coef_modelRA <- coef(modelRA)

OOBP <- 0.307
OSLG <- 0.373

predicted_RA <- coef_modelRA["(Intercept)"] + coef_modelRA["OOBP"] * OOBP + coef_modelRA["OSLG"] * OSLG

cat("La predicción de RA utilizando OOBP y OSLG es:", predicted_RA, "\n")

# ------------------------

coef_new_model <- coef(newModelRS)

OBP <- 0.339
SLG <- 0.430

predicted_RS <- coef_new_model["(Intercept)"] + coef_new_model["OBP"] * OBP + coef_new_model["SLG"] * SLG

cat("La predicción de RS utilizando OBP y SLG es:", predicted_RS, "\n")

coef_modelRA <- coef(modelRA)

OOBP <- 0.307
OSLG <- 0.373

predicted_RA <- coef_modelRA["(Intercept)"] + coef_modelRA["OOBP"] * OOBP + coef_modelRA["OSLG"] * OSLG

cat("La predicción de RA utilizando OOBP y OSLG es:", predicted_RA, "\n")


# ---------------
