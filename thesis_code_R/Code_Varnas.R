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

#####

##### Time-series plots #####
KR_plot <- data |> autoplot(KR, lwd = 1.2) + labs(title = "Orzo", subtitle = "Sales", y = "Number") + theme_classic()
PE_plot <- data |> autoplot(PE, lwd = 1.2) + labs(title = "Penne", subtitle = "Sales", y = "Number") + theme_classic()
N6_plot <- data |> autoplot(N6, lwd = 1.2) + labs(title = "N6", subtitle = "Sales", y = "Number") + theme_classic()
grid.arrange(KR_plot, PE_plot, N6_plot)

## Seasonal plots - monthly
p1 <- data |> gg_season(KR, labels = "right", labels_repel = T) + labs(y = "Sales", title = "Seasonal plot: Orzo") + theme_classic()
p2 <- data |> gg_season(PE, labels = "right", labels_repel = T) + labs(y = "Sales", title = "Seasonal plot: Penne") + theme_classic()
p3 <- data |> gg_season(N6, labels = "right", labels_repel = T) + labs(y = "Sales", title = "Seasonal plot: N6") + theme_classic()
grid.arrange(p1,p2,p3)

## Scatterplots
SC1 <- data |> ggplot(aes(x = KR, y = PE)) + geom_point() + labs(x = "Orzo", y = "Penne") + theme_classic()
SC2 <- data |> ggplot(aes(x = KR, y = N6)) + geom_point() + labs(x = "Orzo", y = "N6") + theme_classic()
SC3 <- data |> ggplot(aes(x = PE, y = N6)) + geom_point() + labs(x = "Penne", y = "N6") + theme_classic()
grid.arrange(SC1,SC2,SC3)

## Lag-plot
stats::lag.plot(data$KR, set = c(1:4, 9:12), pch = ".", col = "gold", labels = F, do.lines = F, cex = 10)
stats::lag.plot(data$PE, set = c(1:4, 9:12), pch = ".", col = "blue", labels = F, do.lines = F, cex = 10)
stats::lag.plot(data$N6, set = c(1:4, 9:12), pch = ".", col = "green", labels = F, do.lines = F, cex = 10)

## ACF plots

AC1 <- data |> ACF(KR, lag_max = 50) |> autoplot() + labs(title="Orzo ACF") + theme_classic()
AC2 <- data |> ACF(PE, lag_max = 50) |> autoplot() + labs(title="Penne ACF") + theme_classic()
AC3 <- data |> ACF(N6, lag_max = 50) |> autoplot() + labs(title="N6 ACF") + theme_classic()
grid.arrange(AC1,AC2,AC3)

## PACF plots

PAC1 <- data |> PACF(KR, lag_max = 50) |> autoplot() + labs(title="Orzo PACF") + theme_classic()
PAC2 <- data |> PACF(PE, lag_max = 50) |> autoplot() + labs(title="Penne PACF") + theme_classic()
PAC3 <- data |> PACF(N6, lag_max = 50) |> autoplot() + labs(title="N6 PACF") + theme_classic()
grid.arrange(PAC1,PAC2,PAC3)


grid.arrange(AC1,AC2,AC3,PAC1,PAC2,PAC3)
#####

##### Features #####

### Subset dataset

data.KR <- data[,c(1,2)];train.KR <- data.KR |> filter_index("2010 Jan" ~ "2020 Dec");test.KR <- data.KR |> filter_index("2021 Jan" ~ "2022 Dec")
data.PE <- data[,c(1,3)];train.PE <- data.PE |> filter_index("2010 Jan" ~ "2020 Dec");test.PE <- data.PE |> filter_index("2021 Jan" ~ "2022 Dec")
data.N6 <- data[,c(1,4)];train.N6 <- data.N6 |> filter_index("2010 Jan" ~ "2020 Dec");test.N6 <- data.N6 |> filter_index("2021 Jan" ~ "2022 Dec")


### Quartiles
rbind(data.KR %>% features(KR, quantile),
data.PE %>% features(PE, quantile),
data.N6 %>% features(N6, quantile))
writexl::write_xlsx(rbind(data.KR %>% features(KR, quantile),
                          data.PE %>% features(PE, quantile),
                          data.N6 %>% features(N6, quantile)), "Quartiles.xlsx")
  
### STL features
rbind(data.KR %>% features(KR, feat_stl),
data.PE %>% features(PE, feat_stl),
data.N6 %>% features(N6, feat_stl))
writexl::write_xlsx(rbind(data.KR %>% features(KR, feat_stl),
                          data.PE %>% features(PE, feat_stl),
                          data.N6 %>% features(N6, feat_stl)), "STL.xlsx")
  
### Shannon entropy
rbind(data.KR %>% features(KR, feat_spectral),
data.PE %>% features(PE, feat_spectral),
data.N6 %>% features(N6, feat_spectral))
writexl::write_xlsx(rbind(data.KR %>% features(KR, feat_spectral),
                          data.PE %>% features(PE, feat_spectral),
                          data.N6 %>% features(N6, feat_spectral)), "Entropy.xlsx")
  
### Ljung-box test
rbind(data.KR %>% features(KR, ljung_box),
data.PE %>% features(PE, ljung_box),
data.N6 %>% features(N6, ljung_box))
writexl::write_xlsx(rbind(data.KR %>% features(KR, ljung_box),
                          data.PE %>% features(PE, ljung_box),
                          data.N6 %>% features(N6, ljung_box)), "Ljung.xlsx")
  
### KPSS test for unit root
rbind(data.KR %>% features(KR, unitroot_kpss),
data.PE %>% features(PE, unitroot_kpss),
data.N6 %>% features(N6, unitroot_kpss))

writexl::write_xlsx(rbind(data.KR %>% features(KR, unitroot_kpss),
                          data.PE %>% features(PE, unitroot_kpss),
                          data.N6 %>% features(N6, unitroot_kpss)), "KPSS.xlsx")
#####

##### Box-Cox transformation #####

lambda.KR <- guerrero(data$KR)
lambda.PE <- guerrero(data$PE)
lambda.N6 <- guerrero(data$N6)

p1 <- data |> autoplot(box_cox(KR, lambda.KR)) + labs(y = "Transformed Orzo sales") + theme_classic()
p2 <- data |> autoplot(box_cox(PE, lambda.PE)) + labs(y = "Transformed Penne sales") + theme_classic()
p3 <- data |> autoplot(box_cox(N6, lambda.KR)) + labs(y = "Transformed N6 sales") + theme_classic()
grid.arrange(p1,p2,p3)

#####

##### Time-series decomposition #####

## Additive
dcmp.KR <- data |> model(stl = STL(KR));components(dcmp.KR)
dcmp.PE <- data |> model(stl = STL(PE));components(dcmp.PE)
dcmp.N6 <- data |> model(stl = STL(N6));components(dcmp.N6)

ss1 <- components(dcmp.KR) |> as_tsibble() |> autoplot(KR, colour="black") + geom_line(aes(y=trend), colour = "#D55E00", lwd = 1) + 
  labs(y = "Orzo", title = "Orzo sales") + theme_classic()
ss2 <- components(dcmp.PE) |> as_tsibble() |> autoplot(PE, colour="black") + geom_line(aes(y=trend), colour = "#D55E00", lwd = 1) + 
  labs(y = "Penne", title = "Penne sales") + theme_classic()
ss3 <- components(dcmp.N6) |> as_tsibble() |> autoplot(N6, colour="black") + geom_line(aes(y=trend), colour = "#D55E00", lwd = 1) + 
  labs(y = "N6", title = "N6 sales") + theme_classic()
grid.arrange(ss1,ss2,ss3)

components(dcmp.KR) |> autoplot() + theme_minimal()
components(dcmp.PE) |> autoplot() + theme_minimal()
components(dcmp.N6) |> autoplot() + theme_minimal()

## Multiplicative time-series decomposition

data |> model(classical_decomposition(KR, type = "multiplicative")) |>
  components() |> autoplot() + labs(title = "Classical additive decomposition of total US retail employment")

data |> model(classical_decomposition(PE, type = "multiplicative")) |>
  components() |> autoplot() + labs(title = "Classical additive decomposition of total US retail employment")

data |> model(classical_decomposition(N6, type = "multiplicative")) |>
  components() |> autoplot() + labs(title = "Classical additive decomposition of total US retail employment")

## Seasonally adjusted data

components(dcmp.KR) |> as_tsibble() |> autoplot(KR, colour = "gray") + geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Orzo") + theme_minimal()
components(dcmp.PE) |> as_tsibble() |> autoplot(PE, colour = "gray") + geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Penne") + theme_minimal()
components(dcmp.N6) |> as_tsibble() |> autoplot(N6, colour = "gray") + geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "N6") + theme_minimal()

#####

##### Moving average model #####

aus_exports <- data |> mutate(`7-MA` = slider::slide_dbl(KR, mean, .before = 3, .after = 3, .complete = TRUE))
aus_exports |> autoplot(KR) + geom_line(aes(y = `7-MA`), colour = "#D55E00", cex = 2) +
  labs(y = "% of GDP", title = "") + guides(colour = guide_legend(title = "series")) + theme_classic()

aus_exports <- data |> mutate(`7-MA` = slider::slide_dbl(PE, mean, .before = 3, .after = 3, .complete = TRUE))
aus_exports |> autoplot(PE) + geom_line(aes(y = `7-MA`), colour = "#D55E00", cex = 2) +
  labs(y = "% of GDP", title = "") + guides(colour = guide_legend(title = "series")) + theme_classic()

aus_exports <- data |> mutate(`7-MA` = slider::slide_dbl(N6, mean, .before = 3, .after = 3, .complete = TRUE))
aus_exports |> autoplot(N6) + geom_line(aes(y = `7-MA`), colour = "#D55E00", cex = 2) +
  labs(y = "% of GDP", title = "") + guides(colour = guide_legend(title = "series")) + theme_classic()

#####

##### Simple Forecasting #####


fit.KR <- train.KR |> model(Mean = MEAN(KR), 
                            `Naïve` = NAIVE(KR), 
                            `Seasonal naïve` = SNAIVE(KR),
                            Drift = RW(KR~drift()))
fit.PE <- train.PE |> model(Mean = MEAN(PE), 
                            `Naïve` = NAIVE(PE),
                            `Seasonal naïve` = SNAIVE(PE),
                            Drift = RW(PE~drift()))
fit.N6 <- train.N6 |> model(Mean = MEAN(N6),
                            `Naïve` = NAIVE(N6),
                            `Seasonal naïve` = SNAIVE(N6),
                            Drift = RW(N6~drift()))

# Generate forecasts for 24 months

fit.KR <- fit.KR |> refit(test.KR, reestimate = T) 
fit.PE <- fit.PE |> refit(test.PE) 
fit.N6 <- fit.N6 |> refit(test.N6)

fit.KR |> refit(test.KR) |> accuracy()
fit.PE |> refit(test.PE) |> accuracy()
fit.N6 |> refit(test.N6) |> accuracy()

fit.KR[[1]][[1]]$fit$fitted
fit.KR[[2]][[1]]$fit$.fitted
fit.KR[[3]][[1]]$fit$.fitted
fit.KR[[4]][[1]]$fit$.fitted

f <- fitted(fit.KR)
plot(data.KR$KR, type = "l")
lines(fit.KR[[1]][[1]]$fit$fitted, col = "red")
lines(fit.KR[[2]][[1]]$fit$.fitted, col = "blue")
lines(fit.KR[[3]][[1]]$fit$.fitted, col = "green")
lines(fit.KR[[4]][[1]]$fit$.fitted, col = "brown")

fit <- fit.KR %>% 
  stream(test.KR)

## Accuracy metrics

gg_tsresiduals(train.KR |> model(Mean = MEAN(KR))) + ggtitle("Mean")
gg_tsresiduals(train.KR |> model(Mean = NAIVE(KR)))+ ggtitle("Naive")
gg_tsresiduals(train.KR |> model(Mean = SNAIVE(KR)))+ ggtitle("S-Naive")
gg_tsresiduals(train.KR |> model(Mean = RW(KR~drift())))+ ggtitle("Drift")

gg_tsresiduals(train.PE |> model(Mean = MEAN(PE))) + ggtitle("Mean")
gg_tsresiduals(train.PE |> model(Mean = NAIVE(PE)))+ ggtitle("Naive")
gg_tsresiduals(train.PE |> model(Mean = SNAIVE(PE)))+ ggtitle("S-Naive")
gg_tsresiduals(train.PE |> model(Mean = RW(PE~drift())))+ ggtitle("Drift")

gg_tsresiduals(train.N6 |> model(Mean = MEAN(N6))) + ggtitle("Mean")
gg_tsresiduals(train.N6 |> model(Mean = NAIVE(N6)))+ ggtitle("Naive")
gg_tsresiduals(train.N6 |> model(Mean = SNAIVE(N6)))+ ggtitle("S-Naive")
gg_tsresiduals(train.N6 |> model(Mean = RW(N6~drift())))+ ggtitle("Drift")

gridExtra::grid.arrange(p1,p2,p3,p4)
ggpubr::ggarrange(p1,p2,p3,p4)

# Plot forecasts against actual values

p1 <- fit.KR |> autoplot(train.KR, level = NULL, cex = 0.9) + autolayer(filter_index(data.KR, "2021 Jan" ~ .), colour = "black") +
  labs(y = "Sales", title = "Forecasts for Orzo sales") +
  guides(colour = guide_legend(title = "Forecast")) + theme_classic()

p2 <- fit.PE |> autoplot(train.PE, level = NULL, cex = 0.9) + autolayer(filter_index(data.PE, "2019 Mar" ~ .), colour = "black") +
  labs(y = "Sales", title = "Forecasts for Penne sales") +
  guides(colour = guide_legend(title = "Forecast")) + theme_classic()

p3 <- fit.N6 |> autoplot(train.N6, level = NULL, cex = 0.9) + autolayer(filter_index(data.N6, "2019 Mar" ~ .), colour = "black") +
  labs(y = "Sales", title = "Forecasts for No 6 sales") +
  guides(colour = guide_legend(title = "Forecast")) + theme_classic()

gridExtra::grid.arrange(p1,p2,p3)

accuracy(fit.KR,test.KR)
accuracy(fit.PE,test.PE)
accuracy(fit.N6,test.N6)

## KR Evaluation

mae(data.KR[c(111:156),2], fit.KR[1:46,4], 46);rmse(data.KR[c(111:156),2], fit.KR[1:46,4], 46);mape(data.KR[c(111:156),2], fit.KR[1:46,4], 46)
mae(data.KR[c(111:156),2], fit.KR[47:92,4], 46);rmse(data.KR[c(111:156),2], fit.KR[47:92,4], 46);mape(data.KR[c(111:156),2], fit.KR[47:92,4], 46)
mae(data.KR[c(111:156),2], fit.KR[93:138,4], 46);rmse(data.KR[c(111:156),2], fit.KR[93:138,4], 46);mape(data.KR[c(111:156),2], fit.KR[93:138,4], 46)
mae(data.KR[c(111:156),2], fit.KR[139:184,4], 46);rmse(data.KR[c(111:156),2], fit.KR[139:184,4], 46);mape(data.KR[c(111:156),2], fit.KR[139:184,4], 46)

## PE Evaluation

mae(data.PE[c(111:156),2], fit.PE[1:46,4], 46);rmse(data.PE[c(111:156),2], fit.PE[1:46,4], 46);mape(data.PE[c(111:156),2], fit.PE[1:46,4], 46)
mae(data.PE[c(111:156),2], fit.PE[47:92,4], 46);rmse(data.PE[c(111:156),2], fit.PE[47:92,4], 46);mape(data.PE[c(111:156),2], fit.PE[47:92,4], 46)
mae(data.PE[c(111:156),2], fit.PE[93:138,4], 46);rmse(data.PE[c(111:156),2], fit.PE[93:138,4], 46);mape(data.PE[c(111:156),2], fit.PE[93:138,4], 46)
mae(data.PE[c(111:156),2], fit.PE[139:184,4], 46);rmse(data.PE[c(111:156),2], fit.PE[139:184,4], 46);mape(data.PE[c(111:156),2], fit.PE[139:184,4], 46)

## N6 Evaluation

mae(data.N6[c(111:156),2], fit.N6[1:46,4], 46);rmse(data.N6[c(111:156),2], fit.N6[1:46,4], 46);mape(data.N6[c(111:156),2], fit.N6[1:46,4], 46)
mae(data.N6[c(111:156),2], fit.N6[47:92,4], 46);rmse(data.N6[c(111:156),2], fit.N6[47:92,4], 46);mape(data.N6[c(111:156),2], fit.N6[47:92,4], 46)
mae(data.N6[c(111:156),2], fit.N6[93:138,4], 46);rmse(data.N6[c(111:156),2], fit.N6[93:138,4], 46);mape(data.N6[c(111:156),2], fit.N6[93:138,4], 46)
mae(data.N6[c(111:156),2], fit.N6[139:184,4], 46);rmse(data.N6[c(111:156),2], fit.N6[139:184,4], 46);mape(data.N6[c(111:156),2], fit.N6[139:184,4], 46)


## Forecasting with transformations

train.KR |> model(RW(KR ~ drift())) |> forecast(h = 12) |>
  autoplot(train.KR, point_forecast = lst(mean, median), level = 80) +
  labs(title = "", y = "")

#####

##### ETS Models #####

ets.KR <- train.KR |> model(ETS(KR));report(ets.KR) #
res.1 <- gg_tsresiduals(ets.KR) + ggtitle("ETS(KR)")
ets.KR <- ets.KR |> forecast(h = 12)

ets.PE <- train.PE |> model(ETS(PE));report(ets.PE) #
res.2 <- gg_tsresiduals(ets.PE)+ ggtitle("ETS(PE)")
ets.PE <- ets.PE |> forecast(h = 12)

ets.N6 <- train.N6 |> model(ETS(N6));report(ets.N6) #
res.3 <- gg_tsresiduals(ets.N6)+ ggtitle("ETS(N6)")
ets.N6 <- ets.N6 |> forecast(h = 12)

p1 <- ets.KR |>
  autoplot(train.KR, point_forecast = lst(mean, median), level = 0) +
  autolayer(filter_index(data.KR, "2019 Mar" ~ .)) + labs(title = "Orzo", y = "Sales") + theme_minimal()
p2 <- ets.PE |>
  autoplot(train.PE, point_forecast = lst(mean, median), level = 0) +
  autolayer(filter_index(data.PE, "2019 Mar" ~ .))+ labs(title = "Penne", y = "Sales") + theme_minimal()
p3 <- ets.N6 |>
  autoplot(train.N6, point_forecast = lst(mean, median), level = 0) +
  autolayer(filter_index(data.N6, "2019 Mar" ~ .)) + labs(title = "N6", y = "Sales") + theme_minimal()

grid.arrange(p1,p2,p3)
grid.arrange(res.1,res.2,res.3)


mae(data.KR[c(145:156),2], ets.KR[,4], 12)
rmse(data.KR[c(145:156),2], ets.KR[,4], 12)
mape(data.KR[c(145:156),2], ets.KR[,4], 12)

mae(data.PE[c(145:156),2], ets.PE[,4], 12)
rmse(data.PE[c(145:156),2], ets.PE[,4], 12)
mape(data.PE[c(145:156),2], ets.PE[,4], 12)

mae(data.N6[c(145:156),2], ets.N6[,4], 12)
rmse(data.N6[c(145:156),2], ets.N6[,4], 12)
mape(data.N6[c(145:156),2], ets.N6[,4], 12)

accuracy(ets.KR,test.KR)
accuracy(ets.PE,test.PE)
accuracy(ets.N6,test.N6)

#####

##### S-ARIMA Models #####

arima.KR <- train.KR |> model(ARIMA(KR));report(arima.KR) #ARIMA(0,1,1)(2,0,0)[12]
res.KR <- gg_tsresiduals(arima.KR) + ggtitle("ARIMA(KR)")
arima.KR <- arima.KR |> forecast(h = 12)
a.1 <- arima.KR |> autoplot(data.KR, level = 0) + labs(x = "", y = "", title = "Orzo sales ARIMA forecast") + theme_classic()

arima.PE <- train.PE |> model(ARIMA(PE));report(arima.PE) #ARIMA(0,1,1)(1,0,2)[12] w/ drift
res.PE <- gg_tsresiduals(arima.PE) + ggtitle("ARIMA(PE)")
arima.PE <- arima.PE |> forecast(h = 12)
a.2 <- arima.PE |> autoplot(data.PE, level = 0) + labs(x = "", y = "", title = "Orzo sales ARIMA forecast") + theme_classic()

arima.N6 <- train.N6 |> model(ARIMA(N6));report(arima.N6) #ARIMA(1,1,1)(2,0,0)[12] w/ drift
res.N6 <- gg_tsresiduals(arima.N6) + ggtitle("ARIMA(N6)")
arima.N6 <- arima.N6 |> forecast(h = 12)
a.3 <- arima.N6 |> autoplot(data.N6, level = 0) + labs(x = "", y = "", title = "Orzo sales ARIMA forecast") + theme_classic()

grid.arrange(a.1,a.2,a.3)

mae(data.KR[c(145:156),2], arima.KR[,4], 12)
rmse(data.KR[c(145:156),2], arima.KR[,4], 12)
mape(data.KR[c(145:156),2], arima.KR[,4], 12)

mae(data.PE[c(145:156),2], arima.PE[,4], 12)
rmse(data.PE[c(145:156),2], arima.PE[,4], 12)
mape(data.PE[c(145:156),2], arima.PE[,4], 12)

mae(data.N6[c(145:156),2], arima.N6[,4], 12)
rmse(data.N6[c(145:156),2], arima.N6[,4], 12)
mape(data.N6[c(145:156),2], arima.N6[,4], 12)

accuracy(arima.KR,test.KR)
accuracy(arima.PE,test.PE)
accuracy(arima.N6,test.N6)

#####

##### Neural Network Autoregressive (NNAR) Model #####

nn.KR <- train.KR |> model(NNETAR(KR, scale_inputs = T))
nn.KR  <- nn.KR |> forecast(h = 12)  
p1 <- nn.KR |>  autoplot(train.KR, level = 0) + autolayer(filter_index(data.KR, "2019 Mar" ~ .)) + labs(x = "Year", y = "Sales", title = "Orzo") + theme_minimal()

nn.PE <- train.PE |> model(NNETAR(PE, n_networks = 10))
nn.PE  <- nn.PE |> forecast(h = 12)                                                                                                                     
p2 <- nn.PE |>  autoplot(train.PE, level = 0) + autolayer(filter_index(data.PE, "2019 Mar" ~ .)) + labs(x = "Year", y = "Sales", title = "Penne") + theme_minimal()

nn.N6 <- train.N6 |> model(NNETAR(N6, n_networks = 10))
nn.N6  <- nn.N6 |> forecast(h = 12)                                                                                                                     
p3 <- nn.N6 |>  autoplot(train.N6, level = 0) + autolayer(filter_index(data.N6, "2019 Mar" ~ .)) + labs(x = "Year", y = "Sales", title = "N6") + theme_minimal()

grid.arrange(p1,p2,p3)

mae(data.KR[c(145:156),2], nn.KR[,4], 12)
rmse(data.KR[c(145:156),2], nn.KR[,4], 12)
mape(data.KR[c(145:156),2], nn.KR[,4], 12)

mae(data.PE[c(145:156),2], nn.PE[,4], 12)
rmse(data.PE[c(145:156),2], nn.PE[,4], 12)
mape(data.PE[c(145:156),2], nn.PE[,4], 12)

mae(data.N6[c(145:156),2], nn.N6[,4], 12)
rmse(data.N6[c(145:156),2], nn.N6[,4], 12)
mape(data.N6[c(145:156),2], nn.N6[,4], 12)


accuracy(nn.KR, test.KR)
accuracy(nn.PE, test.PE)
accuracy(nn.N6, test.N6)

#####

##### Bagging forecasting #####

### KR

cement_stl <- train.KR |> model(stl = STL(KR))

cement_stl |> generate(new_data = train.KR, times = 10, bootstrap_block_size = 8) |>
  autoplot(.sim) + autolayer(train.KR, KR) + guides(colour = "none") + labs(title = "", y="") + theme_minimal()

sim <- cement_stl |> generate(new_data = train.KR, times = 100, bootstrap_block_size = 8) |> select(-.model, -KR)
ets_forecasts <- sim |> model(ets = MEAN(.sim)) |> forecast(h = 12)

e1 <- ets_forecasts |> update_tsibble(key = .rep) |> autoplot(.mean) + autolayer(train.KR, KR) + guides(colour = "none") +
  labs(title = "", y="")+ theme_minimal()

bagged.KR <- ets_forecasts |> summarise(bagged_mean = mean(.mean))

p1 <- train.KR |> model(ets = ETS(KR)) |> forecast(h = 46) |> autoplot(train.KR, level = 0) + autolayer(filter_index(data.KR, "2018 Apr" ~ .))  +
  labs(title = "", y="") + theme_minimal()


### PE

cement_stl <- train.PE |> model(stl = STL(PE))

cement_stl |> generate(new_data = train.PE, times = 10, bootstrap_block_size = 8) |>
  autoplot(.sim) + autolayer(train.PE, PE) + guides(colour = "none") + labs(title = "", y="")

sim <- cement_stl |> generate(new_data = train.PE, times = 100, bootstrap_block_size = 8) |> select(-.model, -PE)
ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 46)

e2 <- ets_forecasts |> update_tsibble(key = .rep) |> autoplot(.mean) + autolayer(data.PE, PE) + guides(colour = "none") +
  labs(title = "", y="")+ theme_minimal()

bagged.PE <- ets_forecasts |> summarise(bagged_mean = mean(.mean))

p2 <- train.PE |> model(ets = ETS(PE)) |> forecast(h = 46) |> autoplot(train.PE, level = 0) + autolayer(filter_index(data.PE, "2018 Apr" ~ .))+ autolayer(bagged.PE, bagged_mean, col = "#D55E00") +
  labs(title = "", y="") + theme_minimal()


### N6

cement_stl <- train.N6 |> model(stl = STL(N6))

cement_stl |> generate(new_data = train.N6, times = 10, bootstrap_block_size = 8) |>
  autoplot(.sim) + autolayer(train.N6, N6) + guides(colour = "none") + labs(title = "", y="")

sim <- cement_stl |> generate(new_data = train.N6, times = 100, bootstrap_block_size = 8) |> select(-.model, -N6)
ets_forecasts <- sim |> model(ets = ETS(.sim)) |> forecast(h = 46)

e3 <- ets_forecasts |> update_tsibble(key = .rep) |> autoplot(.mean) + autolayer(data.N6, N6) + guides(colour = "none") +
  labs(title = "", y="") + theme_minimal()

bagged.N6 <- ets_forecasts |> summarise(bagged_mean = mean(.mean))

p3 <- train.N6 |> model(ets = ETS(N6)) |> forecast(h = 46) |> autoplot(train.N6, level = 0) + autolayer(filter_index(data.N6, "2018 Apr" ~ .))+ autolayer(bagged.N6, bagged_mean, col = "#D55E00") +
  labs(title = "", y="") + theme_minimal()

grid.arrange(e1,e2,e3)

grid.arrange(p1,p2,p3)

## Evaluation of bagged models

mae(data.KR[c(145:156),2], bagged.KR[,2], 12)
rmse(data.KR[c(145:156),2], bagged.KR[,2], 12)
mape(data.KR[c(145:156),2], bagged.KR[,2], 12)

mae(data.PE[c(145:156),2], bagged.PE[,2], 12)
rmse(data.PE[c(145:156),2], bagged.PE[,2], 12)
mape(data.PE[c(145:156),2], bagged.PE[,2], 12)

mae(data.N6[c(145:156),2], bagged.N6[,2], 12)
rmse(data.N6[c(145:156),2], bagged.N6[,2], 12)
mape(data.N6[c(145:156),2], bagged.N6[,2], 12)

accuracy(ets_forecasts, test.KR)

#####

##### Combination forecasting #####

cafe_models <- train.KR |> model(ets = ETS(KR),snaive = SNAIVE(KR),arima = ARIMA(KR)) |>
  mutate(combination = (ets + snaive + arima) / 3)
cafe_fc <- cafe_models |> forecast(h = 46)
accuracy(cafe_fc, test.KR)

cafe_models <- train.PE |> model(ets = ETS(PE),snaive = SNAIVE(PE),arima = ARIMA(PE)) |>
  mutate(combination = (ets + snaive + arima) / 3)
cafe_fc <- cafe_models |> forecast(h = 46)
accuracy(cafe_fc, test.PE)

cafe_models <- train.N6 |> model(ets = ETS(N6),snaive = SNAIVE(N6),arima = ARIMA(N6)) |>
  mutate(combination = (ets + snaive + arima) / 3)
cafe_fc <- cafe_models |> forecast(h = 46)
accuracy(cafe_fc, test.N6)


#####

### Cross-validation ###

df.KR <- data.KR |> stretch_tsibble(.init = 12, .step = 12) |> relocate(Time, KR, .id)
df.PE <- data.PE |> stretch_tsibble(.init = 12, .step = 12) |> relocate(Time, PE, .id)
df.N6 <- data.N6 |> stretch_tsibble(.init = 12, .step = 12) |> relocate(Time, N6, .id)

### CV FOR ALL METHODS
df1 <- df.KR |> model(MEAN(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df2 <- df.PE |> model(MEAN(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df3 <- df.N6 |> model(MEAN(N6)) |> forecast(h = 1) |> accuracy(data.N6)

df4 <- df.KR |> model(NAIVE(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df5 <- df.PE |> model(NAIVE(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df6 <- df.N6 |> model(NAIVE(N6)) |> forecast(h = 1) |> accuracy(data.N6)

df7 <- df.KR |> model(SNAIVE(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df8 <- df.PE |> model(SNAIVE(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df9 <- df.N6 |> model(SNAIVE(N6)) |> forecast(h = 1) |> accuracy(data.N6)

df10 <- df.KR |> model(Drift = RW(KR~drift())) |> forecast(h = 1) |> accuracy(data.KR)
df11 <- df.PE |> model(Drift = RW(PE~drift())) |> forecast(h = 1) |> accuracy(data.PE)
df12 <- df.N6 |> model(Drift = RW(N6~drift())) |> forecast(h = 1) |> accuracy(data.N6)

df13 <- df.KR |> model(ARIMA(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df14 <- df.PE |> model(ARIMA(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df15 <- df.N6 |> model(ARIMA(N6)) |> forecast(h = 1) |> accuracy(data.N6)

df16 <- df.KR |> model(ETS(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df17 <- df.PE |> model(ETS(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df18 <- df.N6 |> model(ETS(N6)) |> forecast(h = 1) |> accuracy(data.N6)

df19 <- df.KR |> model(NNETAR(KR)) |> forecast(h = 1) |> accuracy(data.KR)
df20 <- df.PE |> model(NNETAR(PE)) |> forecast(h = 1) |> accuracy(data.PE)
df21 <- df.N6 |> model(NNETAR(N6)) |> forecast(h = 1) |> accuracy(data.N6)


rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
      df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
      df21)
writexl::write_xlsx(rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
                          df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
                          df21), "cv_results.xlsx")
