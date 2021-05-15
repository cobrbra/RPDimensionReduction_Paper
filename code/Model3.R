# creating data

set.seed(1234)
data <- matrix(0, 10000, 2)
thetas <- runif(10000, min = 0, max = 2*pi)
Y <- rep(0:1, each = 5000)
sigma <- 0.1
Z1s <- rnorm(10000)
Z2s <- rnorm(10000)
data[5001:8750, 1] <- sigma*Z1s[5001:8750] 
data[5001:8750, 2] <- sigma*Z2s[5001:8750] 
data[8751:10000, 1] <- sigma*Z1s[8751:10000] + 2*cos(thetas)[8751:10000] 
data[8751:10000, 2] <- sigma*Z2s[8751:10000] + 2*sin(thetas)[8751:10000] 
data[1:5000, 1] <- sigma*Z1s[1:5000] + sin(thetas)[1:5000] 
data[1:5000, 2] <- sigma*Z2s[1:5000] + cos(thetas)[1:5000] 
plot(data[, 1], data[, 2], col = ifelse(Y == 0, "blue", "red"))


# wham a load of extra dimension on that bad boy
noise_data <- matrix(rnorm(10000*10), 10000, 8)
full_data <- as.data.frame(cbind(data, noise_data))
full_data <- scale(full_data, center = TRUE, scale = TRUE)


train_ids <- sample(1:10000, 8000)
test_ids <- setdiff(1:10000, train_ids)
train_data <- full_data[train_ids, ]
test_data <- full_data[test_ids, ]
y_train <- Y[train_ids]
train_data <- cbind(train_data, Y = y_train)
y_test <- Y[test_ids]
test_data <- cbind(test_data, Y = y_test)

pairs(train_data[, 1:4], col = ifelse(y_train == 0, "blue", "red"))

library(devtools)
load_all("../../dr/")
load_all("../../RPEnsemble/")

SAVE <- dr(formula = as.formula(paste0("Y ~ ", paste0("V", 1:10, collapse = "+"))), data = as.data.frame(train_data), method = "save")
PHD <- dr(formula = as.formula(paste0("Y ~ ", paste0("V", 1:10, collapse = "+"))), data = as.data.frame(train_data), method = "phd")

decompose_knn_1 <- RPDecomposeA(XTrain = as.data.frame(train_data)[1:6000, ] %>% dplyr::select(-Y), YTrain = y_train[1:6000], XVal = as.data.frame(train_data)[6001:8000, ] %>% dplyr::select(-Y), YVal = y_train[6001:8000], B1 = 1, B2 = 1000, d = 2, base = "knn", estmethod = "samplesplit")
save_est_proj_1 <- SAVE$evectors[, 1:2] %*% t(SAVE$evectors[, 1:2])
phd_est_proj_1 <- PHD$evectors[, 1:2] %*% t(PHD$evectors[, 1:2])

rp_knn_est_proj_1 <- decompose_knn_1$v[, 1:2] %*% t(decompose_knn_1$v[, 1:2])
true_proj_1 <- diag(10)[, 1:2] %*% t(diag(10)[, 1:2])
rp_knn_pw_1 <- sum(diag(true_proj_1 %*% rp_knn_est_proj_1)) / 2
save_pw_1 <- sum(diag(true_proj_1 %*% save_est_proj_1)) / 2
phd_pw_1 <- sum(diag(true_proj_1 %*% phd_est_proj_1)) / 2
