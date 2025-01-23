
##### Libraries #### 
library(nnfor)
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

data <- read_excel("στοιχεία_πωλήσεων.xlsx", col_types = c("date", "numeric", "numeric", "numeric"))
data$Time <- yearmonth(data$Time)
data <- tsibble(data, index = Time, regular = T)

### Subset dataset

data.KR <- data[,c(1,2)];train.KR <- data.KR |> filter_index("2010 Jan" ~ "2020 Dec");test.KR <- data.KR |> filter_index("2022 Jan" ~ "2022 Dec")
data.PE <- data[,c(1,3)];train.PE <- data.PE |> filter_index("2010 Jan" ~ "2020 Dec");test.PE <- data.PE |> filter_index("2022 Jan" ~ "2022 Dec")
data.N6 <- data[,c(1,4)];train.N6 <- data.N6 |> filter_index("2010 Jan" ~ "2020 Dec");test.N6 <- data.N6 |> filter_index("2022 Jan" ~ "2022 Dec")

### MLPs

fit <- mlp(as.ts(train.KR), hd = 10)
print(fit)
plot(fit)
frc1 <- forecast(fit, h=24)
plot(frc1)

plot(data.KR$KR, type = "l", ylim = c(5000, 200000), ylab = "KR", xlab = "Time index", lwd = 2)
lines(as.numeric(c(rep(NA, 132), frc1$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))

fit <- mlp(as.ts(train.PE), hd = 20, reps = 100, difforder = c(1,12))
print(fit)
plot(fit)
frc2 <- forecast(fit,h=24)
plot(frc2)

plot(data.PE$PE, type = "l", ylim = c(5000, 100000), ylab = "Penne", xlab = "Time index", lwd = 2)
lines(as.numeric(c(rep(NA, 132), frc2$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))

fit <- mlp(as.ts(train.N6))
print(fit)
plot(fit)
frc3 <- forecast(fit,h=24)
plot(frc3)

plot(data.N6$N6, type = "l", ylim = c(5000, 600000), ylab = "N6", xlab = "Time index", lwd = 2)
lines(as.numeric(c(rep(NA, 132), frc3$mean)), type = "l", col = "blue", lwd = 2, lty = 1)
legend("topleft", fill = c("black", "blue"), legend = c("True values", "Predicted values"))


### 

