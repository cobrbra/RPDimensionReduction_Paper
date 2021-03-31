### This is a script to experiment with how the RPEnsemble Classifier deals with Model 1 from Tim's
### original paper when the first two covariates are scaled.

library(devtools)
library(tidyverse)

load_all("../../RPEnsemble/")

# Simulation parameters

set.seed(1234)

p <- 100

n_train <- 800
n_val <- 200
n_test <- 500


# Train/test split
ids <- tvt(n_train, n_val, n_test)

## Simulation model 1
# Simulate data (with normalisation on first two covariates)
simulated_data_1 <- RPModel(1, n = n_train + n_val + n_test, p = p) 
simulated_data_1$data$x[, 1:2] <- (1/sqrt(5)) * simulated_data_1$data$x[, 1:2]

XTrain_1 <- simulated_data_1$data$x[ids$train, ]
YTrain_1 <- simulated_data_1$data$y[ids$train]

XVal_1 <- simulated_data_1$data$x[ids$val, ]
YVal_1 <- simulated_data_1$data$y[ids$val]

XTest_1 <- simulated_data_1$data$x[ids$test, ]
YTest_1 <- simulated_data_1$data$y[ids$test]

train_val_data_1 <- data.frame(cbind(rbind(XTrain_1, XVal_1), Y = c(YTrain_1, YVal_1)))

d_range <- c(2, 5, 10)
B1_range <- 10^(1:3)
B2_range <- 5 * 10^(0:2)

results <- tibble(model = character(), d = vector(), B1 = vector(), B2 = vector(), accuracy = vector())

for (d in d_range) {
  for (B1 in B1_range) {
    for (B2 in B2_range) {
      print(paste0("Parameters: d = ", d, ", B1 = ", B1, ", B2 = ", B2, "."))
      
      rp_lda_out <- RPParallel(XTrain = XTrain_1, 
                               YTrain = YTrain_1, 
                               XVal = XVal_1, 
                               YVal = YVal_1,
                               XTest = XTest_1, 
                               d = d, 
                               B1 = B1, 
                               B2 = B2, 
                               base = "LDA", 
                               estmethod = "samplesplit")
      
      rp_lda_ec <- RPEnsembleClass(RP.out = rp_lda_out, 
                                   n = n_train, 
                                   n.val = n_val, 
                                   n.test = n_test,
                                   p1 = mean(YTrain_1 == 1), 
                                   samplesplit = TRUE, 
                                   alpha = RPalpha(rp_lda_out, Y = YVal_1, p1 = mean(YTrain_1 == 1)))
      
      lda_accuracy <- mean(rp_lda_ec == YTest_1)
      
      results <- results %>% 
        add_row(model = "RP-LDA", d = d, B1 = B1, B2 = B2, accuracy = lda_accuracy)
      
      rp_qda_out <- RPParallel(XTrain = XTrain_1, 
                               YTrain = YTrain_1, 
                               XVal = XVal_1, 
                               YVal = YVal_1,
                               XTest = XTest_1, 
                               d = d, 
                               B1 = B1, 
                               B2 = B2, 
                               base = "QDA", 
                               estmethod = "samplesplit")
      
      rp_qda_ec <- RPEnsembleClass(RP.out = rp_qda_out, 
                                   n = n_train, 
                                   n.val = n_val, 
                                   n.test = n_test,
                                   p1 = mean(YTrain_1 == 1), 
                                   samplesplit = TRUE, 
                                   alpha = RPalpha(rp_qda_out, Y = YVal_1, p1 = mean(YTrain_1 == 1)))
      
      qda_accuracy <- mean(rp_qda_ec == YTest_1)
      
      results <- results %>% 
        add_row(model = "RP-QDA", d = d, B1 = B1, B2 = B2, accuracy = qda_accuracy)
      
      rp_knn_out <- RPParallel(XTrain = XTrain_1, 
                               YTrain = YTrain_1, 
                               XVal = XVal_1, 
                               YVal = YVal_1,
                               XTest = XTest_1, 
                               d = d, 
                               B1 = B1, 
                               B2 = B2, 
                               base = "knn", 
                               estmethod = "samplesplit")
      
      rp_knn_ec <- RPEnsembleClass(RP.out = rp_knn_out, 
                                   n = n_train, 
                                   n.val = n_val, 
                                   n.test = n_test,
                                   p1 = mean(YTrain_1 == 1), 
                                   samplesplit = TRUE, 
                                   alpha = RPalpha(rp_knn_out, Y = YVal_1, p1 = mean(YTrain_1 == 1)))
      
      knn_accuracy <- mean(rp_knn_ec == YTest_1)
      
      results <- results %>% 
        add_row(model = "RP-kNN", d = d, B1 = B1, B2 = B2, accuracy = knn_accuracy)
      
      print("Results:")
      print(results)
    }
  }
}

write_tsv(x = results, file = "results/Model1RPEnsemble.tsv")
