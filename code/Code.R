# install.packages("devtools")
library(devtools)

# devtools::install_github("cobrbra/RPEnsemble")
# library(RPEnsemble)

# install.packages("cowplot")
library(cowplot)

# install.packages("latex2exp")
library(latex2exp)

# install.packages("tidyverse")
library(tidyverse)

# install.packages("dr")
library(dr)

# install.packages("class")
library(class)

# install.packages("tsne")
library(tsne)

# install.packages("pracma")
library(pracma)

load_all("../../RPEnsemble/")


### Example Figure
set.seed(1234)

x0 <- data.frame(x = runif(n = 100, min = -.5, max = .5), y = runif(n = 100, min = -.5, max = 2), Label = factor(0, levels = 0:1))
x1 <- data.frame(x = runif(n = 100, min = -.5, max = .5), y = runif(n = 100, min = -2, max = .5), Label = factor(1, levels = 0:1))
ex1 <- bind_rows(x0, x1)

p1 <- ex1 %>% 
  ggplot(aes(x = x, y = y, colour = Label)) + geom_point() + xlim(-2,2) + ylim(-2,2) + labs(x = TeX("$x_1$"), y = TeX("$x_2$")) + 
  theme_minimal()  + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

x0 <- data.frame(x = runif(n = 100, min = -.5, max = .1), y = runif(n = 100, min = -2, max = 2), Label = factor(0, levels = 0:1))
x1 <- data.frame(x = runif(n = 100, min = -.1, max = .5), y = runif(n = 100, min = -2, max = 2), Label = factor(1, levels = 0:1))
ex2 <- bind_rows(x0, x1)

p2 <- ex2 %>% 
  ggplot(aes(x = x, y = y, colour = Label)) + geom_point() + xlim(-2,2) + ylim(-2,2)+ labs(x = TeX("$x_1$"), y = TeX("$x_2$")) + 
  theme_minimal()  + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

x0 <- data.frame(x = runif(n = 100, min = -.5, max = .5), y = runif(n = 100, min = -.5, max = 2), Label = factor(0, levels = 0:1))
x1 <- data.frame(x = runif(n = 100, min = -.5, max = .5), y = runif(n = 100, min = -2, max = .5), Label = factor(1, levels = 0:1))
ex3 <- bind_rows(x0, x1) %>% 
  mutate(x_rot = (1/sqrt(2)) * (x - y), y_rot = (1/sqrt(2)) * (x + y)) 

p3 <- ex3 %>% 
  ggplot(aes(x = x_rot, y = y_rot, colour = Label)) + geom_point() + xlim(-2,2) + ylim(-2,2)+ labs(x = TeX("$x_1$"), y = TeX("$x_2$")) + 
  theme_minimal()  + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

x0 <- data.frame(x = runif(n = 100, min = -.5, max = .1), y = runif(n = 100, min = -2, max = 2), Label = factor(0, levels = 0:1))
x1 <- data.frame(x = runif(n = 100, min = -.1, max = .5), y = runif(n = 100, min = -2, max = 2), Label = factor(1, levels = 0:1))
ex4 <- bind_rows(x0,x1) %>% 
  mutate(x_rot = (1/sqrt(2)) * (x - y), y_rot = (1/sqrt(2)) * (x + y)) 

p4 <- ex4 %>% 
  ggplot(aes(x = x_rot, y = y_rot, colour = Label)) + geom_point() + xlim(-2,2) + ylim(-2,2)+ labs(x = TeX("$x_1$"), y = TeX("$x_2$")) + 
  theme_minimal()  + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

example_figure <- plot_grid(p1, p2, p3, p4, nrow = 2, labels = "AUTO")
ggsave("results/figures/ExampleFigures.png", example_figure)



### Simulation Studies

# Simulation parameters

set.seed(1234)

p <- 100

n_train <- 800
n_val <- 200
n_test <- 500


# Train/test split
ids <- tvt(n_train, n_val, n_test)

## Simulation model 1
# Simulate data
simulated_data_1 <- RPModel(0, n = n_train + n_val + n_test, p = p) # changed to model 0 so that highest-variance dimensions aren't the sufficient dimensions

XTrain_1 <- simulated_data_1$data$x[ids$train, ]
YTrain_1 <- simulated_data_1$data$y[ids$train]

XVal_1 <- simulated_data_1$data$x[ids$val, ]
YVal_1 <- simulated_data_1$data$y[ids$val]

XTest_1 <- simulated_data_1$data$x[ids$test, ]
YTest_1 <- simulated_data_1$data$y[ids$test]

# RPEnsemble Method: LDA
decompose_lda_1 <- RPDecomposeA(XTrain = XTrain_1, YTrain = YTrain_1, XVal = XVal_1, YVal = YVal_1, B1 = 1000, B2 = 10, d = 3, base = "LDA", estmethod = "samplesplit")

reduce_lda_1_train <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = rbind(XTrain_1, XVal_1), YTest = c(YTrain_1, YVal_1), d = 2, decomposition = decompose_lda_1) %>% 
  mutate(Model = 1, method = "RP-LDA", Y = YTest) %>% 
  dplyr::select(-YTest)

reduce_lda_1_test <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = XTest_1, YTest = YTest_1, d = 2, decomposition = decompose_lda_1) %>% 
  mutate(Model = 1, method = "RP-LDA", Y = YTest) %>% 
  dplyr::select(-YTest)

# RPEnsemble Method: QDA
decompose_qda_1 <- RPDecomposeA(XTrain = XTrain_1, YTrain = YTrain_1, XVal = XVal_1, YVal = YVal_1, B1 = 1000, B2 = 10, d = 3, base = "QDA", estmethod = "samplesplit")

reduce_qda_1_train <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = rbind(XTrain_1, XVal_1), YTest = c(YTrain_1, YVal_1), d = 2, decomposition = decompose_qda_1) %>% 
  mutate(Model = 1, method = "RP-QDA", Y = YTest) %>% 
  dplyr::select(-YTest)

reduce_qda_1_test <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = XTest_1, YTest = YTest_1, d = 2, decomposition = decompose_qda_1) %>% 
  mutate(Model = 1, method = "RP-QDA", Y = YTest) %>% 
  dplyr::select(-YTest)

# RPEnsemble Method: KNN
decompose_knn_1 <- RPDecomposeA(XTrain = XTrain_1, YTrain = YTrain_1, XVal = XVal_1, YVal = YVal_1, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "samplesplit")

reduce_knn_1_train <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = rbind(XTrain_1, XVal_1), YTest = c(YTrain_1, YVal_1), d = 2, decomposition = decompose_knn_1) %>% 
  mutate(Model = 1, method = "RP-KNN", Y = YTest) %>% 
  dplyr::select(-YTest)

reduce_knn_1_test <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = XTest_1, YTest = YTest_1, d = 2, decomposition = decompose_knn_1) %>% 
  mutate(Model = 1, method = "RP-KNN", Y = YTest) %>% 
  dplyr::select(-YTest)

# SIR Method
train_val_data_1 <- data.frame(cbind(rbind(XTrain_1, XVal_1), Y = c(YTrain_1, YVal_1)))
sir_1 <- dr(data = train_val_data_1, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "sir", nslices = 2)

reduce_sir_1_train <- as.matrix(train_val_data_1 %>% dplyr::select(-Y)) %*% sir_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = c(YTrain_1, YVal_1), Model = 1, method = "SIR") %>% 
  dplyr::select(-c(Dir1, Dir2))

reduce_sir_1_test <- XTest_1 %*% sir_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = YTest_1, Model = 1, method = "SIR") %>% 
  dplyr::select(-c(Dir1, Dir2))

# SAVE Method
save_1 <- dr(data = train_val_data_1, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "save", nslices = 2)

reduce_save_1_train <- as.matrix(train_val_data_1 %>% dplyr::select(-Y)) %*% save_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = c(YTrain_1, YVal_1), Model = 1, method = "SAVE") %>% 
  dplyr::select(-c(Dir1, Dir2))

reduce_save_1_test <- XTest_1 %*% save_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = YTest_1, Model = 1, method = "SAVE") %>% 
  dplyr::select(-c(Dir1, Dir2))

# PHD method
phd_1 <-  dr(data = train_val_data_1, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "phd", nslices = 2)

reduce_phd_1_train <- as.matrix(train_val_data_1 %>% dplyr::select(-Y)) %*% phd_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = c(YTrain_1, YVal_1), Model = 1, method = "PHD") %>% 
  dplyr::select(-c(Dir1, Dir2))

reduce_phd_1_test <- XTest_1 %*% phd_1$evectors[, 1:2] %>% 
  as.data.frame() %>% 
  mutate(Dim_1 = Dir1, Dim_2 = Dir2, Y = YTest_1, Model = 1, method = "PHD") %>% 
  dplyr::select(-c(Dir1, Dir2))

# PCA Method
decompose_pca_1 <- svd(rbind(XTrain_1, XVal_1))$v[, 1:2]
colnames(decompose_pca_1) <- c("Dim_1", "Dim_2")

reduce_pca_1_train <- as.matrix(train_val_data_1 %>% dplyr::select(-Y)) %*% decompose_pca_1 %>% 
  as.data.frame() %>% 
  mutate(Y = c(YTrain_1, YVal_1), Model = 1, method = "PCA")

reduce_pca_1_test <- XTest_1 %*% decompose_pca_1 %>% 
  as.data.frame() %>% 
  mutate(Y = YTest_1, Model = 1, method = "PCA")

# Linearised t-SNE Method
reduce_tsne_1_train <- as.data.frame(tsne(X = rbind(XTrain_1, XVal_1)))
colnames(reduce_tsne_1_train) <- c("Dim_1", "Dim_2")
decompose_ltsne_1 <- matrix(0, p, 2)
decompose_ltsne_1[, 1] <- lm(formula = Y ~ . - 1, data = as.data.frame(cbind(Y = reduce_tsne_1_train$Dim_1, rbind(XTrain_1, XVal_1))))$coefficients
decompose_ltsne_1[, 2] <- lm(formula = Y ~ . - 1, data = as.data.frame(cbind(Y = reduce_tsne_1_train$Dim_2, rbind(XTrain_1, XVal_1))))$coefficients
decompose_ltsne_1 <- pracma::gramSchmidt(decompose_ltsne_1)$Q
colnames(decompose_ltsne_1) <- c("Dim_1", "Dim_2")

reduce_ltsne_1_train <- as.matrix(train_val_data_1 %>% dplyr::select(-Y)) %*% decompose_ltsne_1 %>% 
  as.data.frame() %>% 
  mutate(Y = c(YTrain_1, YVal_1), Model = 1, method = "Lt-SNE")

reduce_ltsne_1_test <- XTest_1 %*% decompose_ltsne_1 %>% 
  as.data.frame() %>% 
  mutate(Y = YTest_1, Model = 1, method = "Lt-SNE")



# # Plot eigenvalue spectrum
# sim1_ev <- data.frame(sd = 1:length(decompose_1$eigv), eigv = decompose_1$eigv) %>% 
#   ggplot(aes(x = sd, y = eigv)) + geom_point() + 
#   theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
#   theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# 
# # Plot two-dimensional test set representations
# sim1_rv <- reduce_1 %>% 
#   ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
#   labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
#   theme_minimal() + theme(legend.position = "none")

# Get false discovery, power and classification statistics
rp_lda_est_proj_1 <- decompose_lda_1$v[, 1:2] %*% t(decompose_lda_1$v[, 1:2])
rp_qda_est_proj_1 <- decompose_qda_1$v[, 1:2] %*% t(decompose_qda_1$v[, 1:2])
rp_knn_est_proj_1 <- decompose_knn_1$v[, 1:2] %*% t(decompose_knn_1$v[, 1:2])

sir_est_proj_1 <- sir_1$evectors[, 1:2] %*% t(sir_1$evectors[, 1:2])
save_est_proj_1 <- save_1$evectors[, 1:2] %*% t(save_1$evectors[, 1:2])
phd_est_proj_1 <- phd_1$evectors[, 1:2] %*% t(phd_1$evectors[, 1:2])

pca_est_proj_1 <- decompose_pca_1 %*% t(decompose_pca_1)
ltsne_est_proj_1 <- decompose_ltsne_1 %*% t(decompose_ltsne_1)

true_proj_1 <- simulated_data_1$subspace %*% t(simulated_data_1$subspace)

rp_lda_fd_1 <- sum(diag((diag(100) -  true_proj_1) %*% rp_lda_est_proj_1))
rp_lda_pw_1 <- sum(diag(true_proj_1 %*% rp_lda_est_proj_1))
rp_lda_class_1 <- class::knn(train = (reduce_lda_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_lda_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_lda_1_train$Y, k = 5) %>% 
  {mean(. == reduce_lda_1_test$Y)}
rp_qda_fd_1 <- sum(diag((diag(100) -  true_proj_1) %*% rp_qda_est_proj_1))
rp_qda_pw_1 <- sum(diag(true_proj_1 %*% rp_qda_est_proj_1))
rp_qda_class_1 <- class::knn(train = (reduce_qda_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_qda_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_qda_1_train$Y, k = 5) %>% 
  {mean(. == reduce_qda_1_test$Y)}
rp_knn_fd_1 <- sum(diag((diag(100) -  true_proj_1) %*% rp_knn_est_proj_1))
rp_knn_pw_1 <- sum(diag(true_proj_1 %*% rp_knn_est_proj_1))
rp_knn_class_1 <- class::knn(train = (reduce_knn_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_knn_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_knn_1_train$Y, k = 5) %>% 
  {mean(. == reduce_knn_1_test$Y)}

sir_fd_1 <- sum(diag((diag(100) - true_proj_1) %*% sir_est_proj_1))
sir_pw_1 <- sum(diag(true_proj_1 %*% sir_est_proj_1))
sir_class_1 <- class::knn(train = (reduce_sir_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_sir_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_sir_1_train$Y, k = 5) %>% 
  {mean(. == reduce_sir_1_test$Y)}
save_fd_1 <- sum(diag((diag(100) - true_proj_1) %*% save_est_proj_1))
save_pw_1 <- sum(diag(true_proj_1 %*% save_est_proj_1))
save_class_1 <- class::knn(train = (reduce_save_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_save_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_save_1_train$Y, k = 5) %>% 
  {mean(. == reduce_save_1_test$Y)}
phd_fd_1 <- sum(diag((diag(100) - true_proj_1) %*% phd_est_proj_1))
phd_pw_1 <- sum(diag(true_proj_1 %*% phd_est_proj_1))
phd_class_1 <- class::knn(train = (reduce_phd_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             test = (reduce_phd_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                             cl = reduce_phd_1_train$Y, k = 5) %>% 
  {mean(. == reduce_phd_1_test$Y)}

pca_fd_1 <- sum((diag((diag(100) - true_proj_1) %*% pca_est_proj_1)))
pca_pw_1 <- sum(diag(true_proj_1 %*% pca_est_proj_1))
pca_class_1 <- class::knn(train = (reduce_pca_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                          test = (reduce_pca_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                          cl = reduce_pca_1_train$Y, k = 5) %>% 
  {mean(. == reduce_pca_1_test$Y)}

ltsne_fd_1 <- sum((diag((diag(100) - true_proj_1) %*% ltsne_est_proj_1)))
ltsne_pw_1 <- sum(diag(true_proj_1 %*% ltsne_est_proj_1))
ltsne_class_1 <- class::knn(train = (reduce_ltsne_1_train %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                            test = (reduce_ltsne_1_test %>% dplyr::select(Dim_1, Dim_2) %>% as.data.frame()), 
                            cl = reduce_ltsne_1_train$Y, k = 5) %>% 
  {mean(. == reduce_ltsne_1_test$Y)}

model_1_performance <- data.frame(Model = c("RP-LDA", "RP-QDA", "RP-KNN", "SIR", "SAVE", "PHD", "PCA", "Lt-SNE"), 
                                   Power = c(rp_lda_pw_1, rp_qda_pw_1, rp_knn_pw_1, sir_pw_1, save_pw_1, phd_pw_1, pca_pw_1, ltsne_pw_1),
                                   False_Discovery = c(rp_lda_fd_1, rp_qda_fd_1, rp_knn_fd_1, sir_fd_1, save_fd_1, phd_fd_1, pca_fd_1, ltsne_fd_1),
                                   Accuracy = c(rp_lda_class_1, rp_qda_class_1, rp_knn_class_1, sir_class_1, save_class_1, phd_class_1, pca_class_1, ltsne_class_1))


## Simulation model 2
# Simulate data
simulated_data_2 <- RPModel(2, n = n_train + n_val + n_test, p = p)

XTrain_2 <- simulated_data_2$data$x[ids$train, ]
YTrain_2 <- simulated_data_2$data$y[ids$train]

XVal_2 <- simulated_data_2$data$x[ids$val, ]
YVal_2 <- simulated_data_2$data$y[ids$val]

XTest_2 <- simulated_data_2$data$x[ids$test, ]
YTest_2 <- simulated_data_2$data$y[ids$test]

# RPEnsemble Method: LDA
decompose_lda_2 <- RPDecomposeA(XTrain = XTrain_2, YTrain = YTrain_2, XVal = XVal_2, YVal = YVal_2, B1 = 1000, B2 = 10, d = 3, base = "LDA", estmethod = "samplesplit")
reduce_lda_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, reduced_dim = 2, XTest = XTest_2, YTest = YTest_2, d = 2, decomposition = decompose_lda_2) %>% 
  mutate(Model = 2, method = "RP-LDA")

# RPEnsemble Method: QDA
decompose_qda_2 <- RPDecomposeA(XTrain = XTrain_2, YTrain = YTrain_2, XVal = XVal_2, YVal = YVal_2, B1 = 1000, B2 = 10, d = 3, base = "QDA", estmethod = "samplesplit")
reduce_qda_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, reduced_dim = 2, XTest = XTest_2, YTest = YTest_2, d = 2, decomposition = decompose_qda_2) %>% 
  mutate(Model = 2, method = "RP-QDA")

# RPEnsemble Method: KNN
decompose_knn_2 <- RPDecomposeA(XTrain = XTrain_2, YTrain = YTrain_2, XVal = XVal_2, YVal = YVal_2, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "samplesplit")
reduce_knn_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, reduced_dim = 2, XTest = XTest_2, YTest = YTest_2, d = 2, decomposition = decompose_knn_2) %>% 
  mutate(Model = 2, method = "RP-KNN")

# SIR Method
train_val_data_2 <- data.frame(cbind(rbind(XTrain_2, XVal_2), Y = c(YTrain_2, YVal_2)))
sir_2 <- dr(data = train_val_data_2, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "sir", nslices = 2)

# SAVE Method
save_2 <- dr(data = train_val_data_2, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "save", nslices = 2)

# PHD Method
phd_2 <- dr(data = train_val_data_2, formula = as.formula(paste("Y ~ ", paste0("V", 1:100, collapse = "+"))), method = "phd", nslices = 2)


# # Plot eigenvalue spectrum
# sim2_ev <- data.frame(sd = 1:length(decompose_2$eigv), eigv = decompose_2$eigv) %>% 
#   ggplot(aes(x = sd, y = eigv)) + geom_point() + 
#   theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
#   theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# 
# # Plot two-dimensional test set representations
# sim2_rv <- reduce_2 %>% 
#   ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
#   labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
#   theme_minimal() + theme(legend.position = "none")

# Get false discovery and power statistics
rp_lda_est_proj_2 <- decompose_lda_2$v[, 1:3] %*% t(decompose_lda_2$v[, 1:3])
rp_qda_est_proj_2 <- decompose_qda_2$v[, 1:3] %*% t(decompose_qda_2$v[, 1:3])
rp_knn_est_proj_2 <- decompose_knn_2$v[, 1:3] %*% t(decompose_knn_2$v[, 1:3])

sir_est_proj_2 <- sir_2$evectors[, 1:3] %*% t(sir_2$evectors[, 1:3])
save_est_proj_2 <- save_2$evectors[, 1:3] %*% t(save_2$evectors[, 1:3])
phd_est_proj_2 <- phd_2$evectors[, 1:3] %*% t(phd_2$evectors[, 1:3])

true_proj_2 <- simulated_data_2$subspace %*% t(simulated_data_2$subspace)

rp_lda_fd_2 <- sum(diag((diag(100) -  true_proj_2) %*% rp_lda_est_proj_2))
rp_lda_pw_2 <- sum(diag(true_proj_2 %*% rp_lda_est_proj_2))
rp_qda_fd_2 <- sum(diag((diag(100) -  true_proj_2) %*% rp_qda_est_proj_2))
rp_qda_pw_2 <- sum(diag(true_proj_2 %*% rp_qda_est_proj_2))
rp_knn_fd_2 <- sum(diag((diag(100) -  true_proj_2) %*% rp_knn_est_proj_2))
rp_knn_pw_2 <- sum(diag(true_proj_2 %*% rp_knn_est_proj_2))

sir_fd_2 <- sum(diag((diag(100) - true_proj_2) %*% sir_est_proj_2))
sir_pw_2 <- sum(diag(true_proj_2 %*% sir_est_proj_2))
save_fd_2 <- sum(diag((diag(100) - true_proj_2) %*% save_est_proj_2))
save_pw_2 <- sum(diag(true_proj_2 %*% save_est_proj_2))
phd_fd_2 <- sum(diag((diag(100) - true_proj_2) %*% phd_est_proj_2))
phd_pw_2 <- sum(diag(true_proj_2 %*% phd_est_proj_2))

sim_evs <- plot_grid(sim1_ev, sim2_ev, nrow = 2, labels = "AUTO")
ggsave(filename = "results/figures/sim_evs.png", plot = sim_evs, width = 8, height = 6)

write_tsv(x = bind_rows(reduce_1, reduce_2), file = "results/sim_reductions.tsv")
sim_rvs <- plot_grid(sim1_rv, sim2_rv, nrow = 1, labels = "AUTO")
ggsave(filename = "results/figures/sim_rvs.png", plot = sim_rvs, width = 8, height = 4)
