library(torch)
library(zeallot)
library(luz)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(lubridate)

demand_dataset <- dataset(
  name = "demand_dataset",
  initialize = function(x, n_timesteps, sample_frac = 1) {
    self$n_timesteps <- n_timesteps
    self$x <- torch_tensor((x - train_mean) / train_sd)
    
    n <- length(self$x) - self$n_timesteps
    
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))
  },
  .getitem = function(i) {
    start <- self$starts[i]
    end <- start + self$n_timesteps - 1
    
    list(
      x = self$x[start:end],
      y = self$x[end + 1]
    )
  },
  .length = function() {
    length(self$starts)
  }
)

train <- data.KR[1:108,]$KR %>% as.matrix()
valid <- data.KR[109:132,]$KR %>%   as.matrix()
test <- data.KR[133:156,]$KR %>%   as.matrix()

train_mean <- mean(train)
train_sd <- sd(train)
n_timesteps <- 1

train_ds <- demand_dataset(train, n_timesteps)
valid_ds <- demand_dataset(valid, n_timesteps)
test_ds <- demand_dataset(test, n_timesteps)
dim(train_ds[1]$x)
dim(train_ds[1]$y)

batch_size <- 64

train_dl <- train_ds %>%
  dataloader(batch_size = batch_size, shuffle = TRUE)
valid_dl <- valid_ds %>%
  dataloader(batch_size = batch_size)
test_dl <- test_ds %>%
  dataloader(batch_size = length(test_ds))

b <- train_dl %>%
  dataloader_make_iter() %>%
  dataloader_next()

dim(b$x)
dim(b$y)

model <- nn_module(
  initialize = function(input_size,
                        hidden_size,
                        dropout = 0.1,
                        num_layers = 1,
                        rec_dropout = 0) {
    self$num_layers <- num_layers
    
    self$rnn <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = rec_dropout,
      batch_first = TRUE
    )
    
    self$dropout <- nn_dropout(dropout)
    self$output <- nn_linear(hidden_size, 1)
  },
  forward = function(x) {
    (x %>%
      self$rnn())[[1]][, dim(x)[2], ] %>%
      self$dropout() %>%
      self$output()
  }
)

input_size <- 1
hidden_size <- 32
num_layers <- 2
rec_dropout <- 0.1

model <- model %>%
  setup(optimizer = optim_adam, loss = nn_mse_loss()) %>%
  set_hparams(
    input_size = input_size,
    hidden_size = hidden_size,
    num_layers = num_layers,
    rec_dropout = rec_dropout
  )

rates_and_losses <- model %>% 
  lr_finder(train_dl, start_lr = 1e-3, end_lr = 1)
rates_and_losses %>% plot()

fitted <- model %>%
  fit(train_dl, epochs = 100, valid_data = valid_dl,
      callbacks = list(
        luz_callback_early_stopping(patience = 100),
        luz_callback_lr_scheduler(
          lr_one_cycle,
          max_lr = 0.1,
          epochs = 100,
          steps_per_epoch = length(train_dl),
          call_on = "on_batch_end")
      ),
      verbose = TRUE)

plot(fitted)

demand_viz <- data.KR[133:156,]
demand_viz_matrix <- data.KR[133:156,]$KR %>% as.matrix()
viz_ds <- demand_dataset(demand_viz_matrix, n_timesteps)
viz_dl <- viz_ds %>% dataloader(batch_size = length(viz_ds))
preds <- predict(fitted, viz_dl)
preds <- preds$to(device = "cpu") %>% as.matrix()
preds <- c(rep(NA, n_timesteps), preds)
pred_ts <- demand_viz %>% add_column(forecast = preds*3*train_sd + train_mean)
plot(data.KR$KR, type = "l", ylab = "KR", xlab = "Time index", lwd = 2)
lines(c(rep(NA, 132),pred_ts$forecast), col = "red", lwd = 2)


