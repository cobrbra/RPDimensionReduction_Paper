library(devtools)
load_all("../../RPEnsemble/")
load_all("../../dr/")
library(tidyverse)
library(latex2exp)

set.seed(1234)
n_train <- 800
n_val <- 200
n_test <- 500

# Train/test split
ids <- tvt(n_train, n_val, n_test)
ps <- seq(5, 50, 5)
n_trials <- 10
pw_knn <- matrix(0, n_trials, length(ps))
pw_save <- matrix(0, n_trials, length(ps))

for (p_index in 1:length(ps)) {
  p <- ps[p_index]
  print(paste0("p = ", p))
  for (trial in 1:n_trials) {
    simulated_data_0 <- RPModel(5, n = n_train + n_val + n_test, p = p) 
    simulated_data_0$data$x[, 1:2] <- sqrt((1/(0.5 + 0.1^2))) * simulated_data_0$data$x[, 1:2]
    
    XTrain_0 <- simulated_data_0$data$x[ids$train, ]
    YTrain_0 <- simulated_data_0$data$y[ids$train]
    
    XVal_0 <- simulated_data_0$data$x[ids$val, ]
    YVal_0 <- simulated_data_0$data$y[ids$val]
    
    XTest_0 <- simulated_data_0$data$x[ids$test, ]
    YTest_0 <- simulated_data_0$data$y[ids$test]
    train_val_data_0 <- data.frame(cbind(rbind(XTrain_0, XVal_0), Y = c(YTrain_0, YVal_0)))
    
    decompose_knn_0 <- RPDecomposeA(XTrain = XTrain_0, YTrain = YTrain_0, XVal = XVal_0, YVal = YVal_0, B1 = 500, B2 = 50, d = 2, base = "knn", estmethod = "samplesplit")
    save_0 <- dr(data = train_val_data_0, formula = as.formula(paste("Y ~ ", paste0("V", 1:p, collapse = "+"))), method = "save", nslices = 2)
    true_proj_0 <- simulated_data_0$subspace %*% t(simulated_data_0$subspace)
    est_proj_knn_0 <- decompose_knn_0$v[, 1:2] %*% t(decompose_knn_0$v[, 1:2])
    est_proj_save_0 <- save_0$evectors[, 1:2] %*% t(save_0$evectors[, 1:2])
    pw_knn[trial, p_index] <- sum(diag(true_proj_0 %*% est_proj_knn_0)) / 2
    pw_save[trial, p_index] <- sum(diag(true_proj_0 %*% est_proj_save_0)) / 2
    
  }
}

powers <- data.frame(power = c(as.vector(pw_knn), as.vector(pw_save)),
                     p = rep(ps, each = n_trials, times = 2), 
                     method = rep(c("RP-kNN", "SAVE"), each = n_trials*length(ps)),
                     trial = rep(1:n_trials, times = length(ps)*2))

fig <- powers %>% 
  group_by(p, method) %>% 
  mutate(mean_power = mean(power), sd_power = sd(power)) %>% 
  dplyr::select(-c(trial, power)) %>% 
  ggplot(aes(x = p, y = mean_power, colour = method,
             ymin = mean_power - sd_power, ymax = mean_power + sd_power)) + 
  geom_point() + geom_errorbar(size = 0.1, alpha = 0.3) + 
  theme_minimal() + labs(x = TeX("$p$"), y = "Power") + 
  theme(legend.title = element_blank())

ggsave("results/figures/Model0Changingp.png", fig, width = 7, height = 5)
