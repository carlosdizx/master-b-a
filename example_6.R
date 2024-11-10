library(forecast)
library(tseries)
ventas <- read.csv("./data/ventas.csv", sep=",")
ventas_ts <- ts(ventas$Sales_k,start=c(1972),frequency=12)

autoplot(ventas_ts)

# test ADF
# Hipotesis nula - No Estacionaria (no rechazamos si p-valor > 1%)
adf.test(ventas_ts, k= 12)

ventas_ts_d1 <- diff(ventas_ts, differences = 1)
adf.test(ventas_ts_d1, k=12)

ventas_ts_d2 <- diff(ventas_ts, differences = 2)
adf.test(ventas_ts_d2, k=12)

autoplot(ventas_ts_d2)

# PACF test
Pacf(ventas_ts_d2)

# ACF test
Acf(ventas_ts_d2)

# modelo
modelo <- Arima(y = ventas_ts,order = c(7,2,6))
modelo

# predicción
forecast(modelo,h=12)