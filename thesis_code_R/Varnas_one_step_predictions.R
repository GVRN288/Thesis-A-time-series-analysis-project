##### Libraries #### 

library(readxl)
library(dplyr)
library(tsibble)
library(ggplot2)
library(feasts)
library(fable)
library(patchwork)
library(gridExtra)
library(fable.prophet)


#####

##### Metrics functions for evaluation #####

## Mean absolute error
mae <- function(x, y, n) {
  d <- x-y
  mae <- sum(abs(d)/n)
  return(mae)
}

## Root mean squared error
rmse <- function(x, y, n) {
  d <- x-y
  rmse <- sqrt(sum(d^2)/n)
  return(rmse)
}

## Mean absolute percentage error
mape <- function(x, y, n) {
  d <- x-y
  p <- 100*abs(d/x)
  mape <- sum(abs(p))/n
  return(mape)
}

#####

##### Import data #####

data <- read_excel("στοιχεία_πωλήσεων.xlsx", col_types = c("date", "numeric", "numeric", "numeric"))
data$Time <- yearmonth(data$Time)
data <- tsibble(data, index = Time, regular = T)

data.KR <- data[,c(1,2)];train.KR <- data.KR |> filter_index("2010 Jan" ~ "2020 Dec");test.KR <- data.KR |> filter_index("2021 Jan" ~ "2022 Dec")
data.PE <- data[,c(1,3)];train.PE <- data.PE |> filter_index("2010 Jan" ~ "2020 Dec");test.PE <- data.PE |> filter_index("2021 Jan" ~ "2022 Dec")
data.N6 <- data[,c(1,4)];train.N6 <- data.N6 |> filter_index("2010 Jan" ~ "2020 Dec");test.N6 <- data.N6 |> filter_index("2021 Jan" ~ "2022 Dec")

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.KR <- data.KR[1:131+i,]
  mn <- train.KR |> model(Mean = MEAN(KR)) |> forecast(h = 1)
  nv <- train.KR |> model(Naive = NAIVE(KR)) |> forecast(h = 1)
  sn <- train.KR |> model(SNaive = SNAIVE(KR)) |> forecast(h = 1)
  dr <- train.KR |> model(Drift = RW(KR~drift())) |> forecast(h = 1)  
    
  f1[i] <- mn$.mean
  f2[i] <- nv$.mean
  f3[i] <- sn$.mean
  f4[i] <- dr$.mean
}
test.KR <- data.KR[133:156,]

p1 <- rbind(cbind(mae(test.KR$KR, f1, length(f1)), rmse(test.KR$KR, f1, length(f1)), mape(test.KR$KR, f1, length(f1))),
cbind(mae(test.KR$KR, f2, length(f2)), rmse(test.KR$KR, f2, length(f2)), mape(test.KR$KR, f2, length(f2))),
cbind(mae(test.KR$KR, f3, length(f3)), rmse(test.KR$KR, f3, length(f3)), mape(test.KR$KR, f3, length(f3))),
cbind(mae(test.KR$KR, f4, length(f4)), rmse(test.KR$KR, f4, length(f4)), mape(test.KR$KR, f4, length(f4))))

plot(data.KR$KR, type = "l", ylab = "Orzo", xlab = "Time index", lwd = 1.5)
lines(c(rep(NA, 132),f1), col = "red", lwd = 1)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 1)
lines(c(rep(NA, 132),f3), col = "green", lwd = 1)
lines(c(rep(NA, 132),f4), col = "yellow", lwd = 1)
legend("topleft", legend=c("Mean", "Naive", "S-Naive", "Drift"),
       col=c("red", "blue", "green", "yellow"), lty = 1)

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.PE <- data.PE[1:131+i,]
  mn <- train.PE |> model(Mean = MEAN(PE)) |> forecast(h = 1)
  nv <- train.PE |> model(Naive = NAIVE(PE)) |> forecast(h = 1)
  sn <- train.PE |> model(SNaive = SNAIVE(PE)) |> forecast(h = 1)
  dr <- train.PE |> model(Drift = RW(PE~drift())) |> forecast(h = 1)  
  
  f1[i] <- mn$.mean
  f2[i] <- nv$.mean
  f3[i] <- sn$.mean
  f4[i] <- dr$.mean
}

p2 <- rbind(cbind(mae(test.PE$PE, f1, length(f1)), rmse(test.PE$PE, f1, length(f1)), mape(test.PE$PE, f1, length(f1))),
            cbind(mae(test.PE$PE, f2, length(f2)), rmse(test.PE$PE, f2, length(f2)), mape(test.PE$PE, f2, length(f2))),
            cbind(mae(test.PE$PE, f3, length(f3)), rmse(test.PE$PE, f3, length(f3)), mape(test.PE$PE, f3, length(f3))),
            cbind(mae(test.PE$PE, f4, length(f4)), rmse(test.PE$PE, f4, length(f4)), mape(test.PE$PE, f4, length(f4))))

plot(data.PE$PE, type = "l", ylab = "Penne", xlab = "Time index", lwd = 1.5)
lines(c(rep(NA, 132),f1), col = "red", lwd = 1)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 1)
lines(c(rep(NA, 132),f3), col = "green", lwd = 1)
lines(c(rep(NA, 132),f4), col = "yellow", lwd = 1)
legend("topleft", legend=c("Mean", "Naive", "S-Naive", "Drift"),
       col=c("red", "blue", "green", "yellow"), lty = 1)

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.N6 <- data.N6[1:131+i,]
  mn <- train.N6 |> model(Mean = MEAN(N6)) |> forecast(h = 1)
  nv <- train.N6 |> model(Naive = NAIVE(N6)) |> forecast(h = 1)
  sn <- train.N6 |> model(SNaive = SNAIVE(N6)) |> forecast(h = 1)
  dr <- train.N6 |> model(Drift = RW(N6~drift())) |> forecast(h = 1)  
  
  f1[i] <- mn$.mean
  f2[i] <- nv$.mean
  f3[i] <- sn$.mean
  f4[i] <- dr$.mean
}

p3 <- rbind(cbind(mae(test.N6$N6, f1, length(f1)), rmse(test.N6$N6, f1, length(f1)), mape(test.N6$N6, f1, length(f1))),
            cbind(mae(test.N6$N6, f2, length(f2)), rmse(test.N6$N6, f2, length(f2)), mape(test.N6$N6, f2, length(f2))),
            cbind(mae(test.N6$N6, f3, length(f3)), rmse(test.N6$N6, f3, length(f3)), mape(test.N6$N6, f3, length(f3))),
            cbind(mae(test.N6$N6, f4, length(f4)), rmse(test.N6$N6, f4, length(f4)), mape(test.N6$N6, f4, length(f4))))

plot(data.N6$N6, type = "l", ylab = "N6", xlab = "Time index", lwd = 1.5)
lines(c(rep(NA, 132),f1), col = "red", lwd = 1)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 1)
lines(c(rep(NA, 132),f3), col = "green", lwd = 1)
lines(c(rep(NA, 132),f4), col = "yellow", lwd = 1)
legend("topleft", legend=c("Mean", "Naive", "S-Naive", "Drift"),
       col=c("red", "blue", "green", "yellow"), lty = 1)

writexl::write_xlsx(data.frame(rbind(p1,p2,p3)), "simple.xlsx")

# ETS + ARIMA

### training models

arima.KR <- train.KR |> model(ARIMA(KR));report(arima.KR)
arima.PE <- train.PE |> model(ARIMA(PE));report(arima.PE)
arima.N6 <- train.N6 |> model(ARIMA(N6));report(arima.N6)

ets.KR <- train.KR |> model(ETS(KR));report(ets.KR)
ets.PE <- train.PE |> model(ETS(PE));report(ets.PE)
ets.N6 <- train.N6 |> model(ETS(N6));report(ets.N6)

### One-step forecasts

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.KR <- data.KR[1:131+i,]
  et <- train.KR |> model(ETS(KR)) |> forecast(h = 1)
  ar <- train.KR |> model(ARIMA(KR)) |> forecast(h = 1)
  f1[i] <- et$.mean
  f2[i] <- ar$.mean
  print(i)
}

plot(data.KR$KR, lwd = 2,  type = "l", ylab = "Orzo", xlab = "Time index")
#lines(c(rep(NA, 132),f1), col = "red", lwd = 2)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 2)

p1 <- rbind(cbind(mae(test.KR$KR, f1, length(f1)), rmse(test.KR$KR, f1, length(f1)), mape(test.KR$KR, f1, length(f1))),
      cbind(mae(test.KR$KR, f2, length(f2)), rmse(test.KR$KR, f2, length(f2)), mape(test.KR$KR, f2, length(f2))))

### PE

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.PE <- data.PE[1:131+i,]
  et <- train.PE |> model(ETS(PE)) |> forecast(h = 1)
  ar <- train.PE |> model(ARIMA(PE)) |> forecast(h = 1)
  f1[i] <- et$.mean
  f2[i] <- ar$.mean
  print(i)
}

plot(data.PE$PE, type = "l", ylab = "Penne", xlab = "Time index", lwd = 2)
#lines(c(rep(NA, 132),f1), col = "red", lwd = 2)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 2)

p2 <- rbind(cbind(mae(test.PE$PE, f1, length(f1)), rmse(test.PE$PE, f1, length(f1)), mape(test.PE$PE, f1, length(f1))),
            cbind(mae(test.PE$PE, f2, length(f2)), rmse(test.PE$PE, f2, length(f2)), mape(test.PE$PE, f2, length(f2))))
## N6
f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.N6 <- data.N6[1:131+i,]
  et <- train.N6 |> model(ETS(N6)) |> forecast(h = 1)
  ar <- train.N6 |> model(ARIMA(N6)) |> forecast(h = 1)
  f1[i] <- et$.mean
  f2[i] <- ar$.mean
  print(i)
}

plot(data.N6$N6, type = "l", ylab = "N6", xlab = "Time index", lwd = 2)
#lines(c(rep(NA, 132),f1), col = "red", lwd = 2)
lines(c(rep(NA, 132),f2), col = "blue", lwd = 2)

p3 <- rbind(cbind(mae(test.N6$N6, f1, length(f1)), rmse(test.N6$N6, f1, length(f1)), mape(test.N6$N6, f1, length(f1))),
            cbind(mae(test.N6$N6, f2, length(f2)), rmse(test.N6$N6, f2, length(f2)), mape(test.N6$N6, f2, length(f2))))

writexl::write_xlsx(data.frame(rbind(p1,p2,p3)), "ETS_ARIMA.xlsx")

# Bagging

## KR
f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.KR <- data.KR[1:131+i,]
  sim <- train.KR |> model(stl = STL(KR))
  sim <- sim |> generate(new_data = train.KR, times = 100, bootstrap_block_size = 8) |> select(-.model, -KR)
  ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 1)
  f1[i] <- (ets_forecasts |> summarise(bagged_mean = mean(.mean)))$bagged_mean
  print(i)
}

plot(data.KR$KR, type = "l", ylab = "Orzo", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),f1), col = "red", lwd = 2)

rbind(cbind(mae(test.KR$KR, f1, length(f1)), rmse(test.KR$KR, f1, length(f1)), mape(test.KR$KR, f1, length(f1))),
      cbind(mae(test.KR$KR, f2, length(f2)), rmse(test.KR$KR, f2, length(f2)), mape(test.KR$KR, f2, length(f2))))

## PE
f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.PE <- data.PE[1:131+i,]
  sim <- train.PE |> model(stl = STL(PE))
  sim <- sim |> generate(new_data = train.PE, times = 100, bootstrap_block_size = 8) |> select(-.model, -PE)
  ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 1)
  f1[i] <- (ets_forecasts |> summarise(bagged_mean = mean(.mean)))$bagged_mean
  print(i)
}

plot(data.PE$PE, type = "l", ylab = "Penne", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),f1), col = "red", lwd = 2)

rbind(cbind(mae(test.PE$PE, f1, length(f1)), rmse(test.PE$PE, f1, length(f1)), mape(test.PE$PE, f1, length(f1))))

## N6
f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.N6 <- data.N6[1:131+i,]
  sim <- train.N6 |> model(stl = STL(N6))
  sim <- sim |> generate(new_data = train.N6, times = 100, bootstrap_block_size = 8) |> select(-.model, -N6)
  ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 1)
  f1[i] <- (ets_forecasts |> summarise(bagged_mean = mean(.mean)))$bagged_mean
  print(i)
}

plot(data.N6$N6, type = "l", ylab = "N6", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),f1), col = "red", lwd = 2)

rbind(cbind(mae(test.N6$N6, f1, length(f1)), rmse(test.N6$N6, f1, length(f1)), mape(test.N6$N6, f1, length(f1))))

##### Neural Network Autoregressive (NNAR) Model #####

nn.KR <- train.KR |> model(NNETAR(KR))
nn.KR  <- nn.KR |> forecast(h = 24)  
p1 <- nn.KR |>  autoplot(train.KR, level = 0) + autolayer(filter_index(data.KR, "2021 Jan" ~ .)) + labs(x = "Year", y = "Sales", title = "Orzo") + theme_minimal()

plot(data.KR$KR, type = "l", ylab = "Orzo", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),nn.KR[,4]$.mean), col = "red", lwd = 2)

nn.PE <- train.PE |> model(NNETAR(PE))
nn.PE  <- nn.PE |> forecast(h = 24)                                                                                                                     
p2 <- nn.PE |>  autoplot(train.PE, level = 0) + autolayer(filter_index(data.PE, "2021 Jan" ~ .)) + labs(x = "Year", y = "Sales", title = "Penne") + theme_minimal()

plot(data.PE$PE, type = "l", ylab = "Penne", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),nn.PE[,4]$.mean), col = "red", lwd = 2)

nn.N6 <- train.N6 |> model(NNETAR(N6))
nn.N6  <- nn.N6 |> forecast(h = 24)                                                                                                                     
p3 <- nn.N6 |>  autoplot(train.N6, level = 0) + autolayer(filter_index(data.N6, "2021 Jan" ~ .)) + labs(x = "Year", y = "Sales", title = "N6") + theme_minimal()

plot(data.N6$N6, type = "l", ylab = "N6", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),nn.N6[,4]$.mean), col = "red", lwd = 2)


grid.arrange(p1,p2,p3)

mae(data.KR[c(133:156),2], nn.KR[,4], 24)
rmse(data.KR[c(133:156),2], nn.KR[,4], 24)
mape(data.KR[c(133:156),2], nn.KR[,4], 24)

mae(data.PE[c(133:156),2], nn.PE[,4]$.mean, 24)
rmse(data.PE[c(133:156),2], nn.PE[,4]$.mean, 24)
mape(data.PE[c(133:156),2], nn.PE[,4]$.mean, 24)

mae(data.N6[c(133:156),2], nn.N6[,4]$.mean, 24)
rmse(data.N6[c(133:156),2], nn.N6[,4]$.mean, 24)
mape(data.N6[c(133:156),2], nn.N6[,4]$.mean, 24)


## Example of Bagging and ARIMA combination forecast which along with the neural network produce the Combination Forecast

f1 <- c();f2 <- c();f3 <- c();f4 <- c()
for (i in 1:24) {
  train.KR <- data.KR[1:131+i,]
  sim <- train.KR |> model(stl = STL(KR))
  sim <- sim |> generate(new_data = train.KR, times = 100, bootstrap_block_size = 8) |> select(-.model, -KR)
  ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 1)
  f1[i] <- (ets_forecasts |> summarise(bagged_mean = mean(.mean)))$bagged_mean
  
  ar <- train.KR |> model(ARIMA(KR)) |> forecast(h = 1)
  f2[i] <- ar$.mean
  print(i)
}

mn <- c()
for (i in 1:length(f1)) {
  mn[i] <- mean(f1[i], f2[i], f3[i], na.rm = T)
}

mae(data.KR[c(133:156),2], mn, 24)
rmse(data.KR[c(133:156),2], mn, 24)
mape(data.KR[c(133:156),2], mn, 24)

devtools::install_github("ellisp/forecastxgb-r-package/pkg")
library(forecastxgb)

## XGBOOST

## KR
model <- xgbar(as.ts(train.KR$KR))
fc <- forecast(model, h = 24)

plot(data.KR$KR, type = "l", ylim = c(5000, 250000), lwd = 2, xlab = "Time index", ylab = "Orzo")
lines(as.numeric(c(rep(NA, 132), fc$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))

mae(data.KR[c(133:156),2], as.numeric(fc$mean), 24)
rmse(data.KR[c(133:156),2], as.numeric(fc$mean), 24)
mape(data.KR[c(133:156),2], as.numeric(fc$mean), 24)

## PE
model <- xgbar(as.ts(train.PE$PE))
fc <- forecast(model, h = 24)

plot(data.PE$PE, type = "l", ylim = c(5000, 100000), lwd = 2, xlab = "Time index", ylab = "Penne")
lines(as.numeric(c(rep(NA, 132), fc$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))


mae(data.PE[c(133:156),2], as.numeric(fc$mean), 24)
rmse(data.PE[c(133:156),2], as.numeric(fc$mean), 24)
mape(data.PE[c(133:156),2], as.numeric(fc$mean), 24)



## N6
model <- xgbar(as.ts(train.N6$N6))
fc <- forecast(model, h = 24)

plot(data.N6$N6, type = "l", ylim = c(5000, 600000), lwd = 2, xlab = "Time index", ylab = "N6")
lines(as.numeric(c(rep(NA, 132), fc$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))

mae(data.N6[c(133:156),2], as.numeric(fc$mean), 24)
rmse(data.N6[c(133:156),2], as.numeric(fc$mean), 24)
mape(data.N6[c(133:156),2], as.numeric(fc$mean), 24)
