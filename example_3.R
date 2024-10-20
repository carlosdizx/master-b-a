moneyball <- read.csv("data/baseball.csv")

str(moneyball)

moneyball$Team

summary(moneyball)
summary(moneyball$OOBP)


quantile(moneyball$RS)

m <- ggplot(moneyball, aes(x = W, y = Team,color = factor(Playoffs)))+
  geom_point() +
  scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")
m + xlab("Wins")

moneyball$RD <- moneyball$RS - moneyball$RA

moneyball[which.min(moneyball$RD), ]$Year

ggplot(moneyball, aes(x = RD,y = W,color = factor(Playoffs))) +
  geom_point() +
  scale_color_manual(values = c("#000000", "#FF2D00"), name = "Made Playoffs")
