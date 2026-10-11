# direction models: probabilities of down / none / up for each quarter
# all return a matrix with columns down, none, up

library(glmnet)

classes <- c(down = -1, none = 0, up = 1)

# benchmark: how often each direction happened in that hour over the last 28 days (+0.5 smoothing)
clim_probs <- function(train, test, days = 28) {
  last <- max(train$date)
  counts <- train |>
    filter(date > last - days) |>
    count(hour, dir) |>
    complete(hour = 0:23, dir = classes, fill = list(n = 0)) |>
    mutate(n = n + 0.5) |>
    group_by(hour) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    select(hour, dir, p) |>
    pivot_wider(names_from = dir, values_from = p)
  P <- as.matrix(counts[match(test$hour, counts$hour), as.character(classes)])
  colnames(P) <- names(classes)
  P
}

# multinomial lasso, lambda chosen by BIC. extra = inputs added to the calendar, spot and weather
da_extra <- c("morning_up", "morning_down", "week_up", "week_down")
mnl_probs <- function(train, test, extra = da_extra) {
  y   <- factor(train$dir, levels = classes)
  fit <- glmnet(model_matrix(train, extra), y, family = "multinomial", type.multinomial = "grouped")
  dev <- (1 - fit$dev.ratio) * fit$nulldev
  bic <- dev + log(nrow(train)) * fit$df * (length(classes) - 1)
  P   <- predict(fit, model_matrix(test, extra), s = fit$lambda[which.min(bic)], type = "response")[, , 1]
  colnames(P) <- names(classes)
  P
}

# xgboost, fixed settings (no tuning), seed = forecast date
xgb_dir_params <- list(objective = "multi:softprob", num_class = 3, learning_rate = 0.05,
                       max_depth = 4, min_child_weight = 20, subsample = 0.8,
                       colsample_bytree = 0.8, nthread = 1)
xgb_dir_rounds <- 300

xgb_probs <- function(train, test, seed, cols = feature_cols) {
  X  <- as.matrix(train[, cols])
  y  <- match(train$dir, classes) - 1
  set.seed(seed)
  m  <- xgb.train(params = xgb_dir_params, data = xgb.DMatrix(X, label = y),
                  nrounds = xgb_dir_rounds, verbose = 0)
  P  <- predict(m, as.matrix(test[, cols]))
  if (is.null(dim(P))) P <- matrix(P, ncol = 3, byrow = TRUE)
  colnames(P) <- names(classes)
  P
}

# scores per quarter, probabilities clipped so a single miss can't give an infinite log loss
log_loss <- function(P, dir) {
  P <- pmax(P, 1e-4); P <- P / rowSums(P)
  -log(P[cbind(seq_along(dir), match(dir, classes))])
}
brier <- function(P, dir) {
  Y <- outer(dir, classes, "==") * 1
  rowSums((P - Y)^2)
}
