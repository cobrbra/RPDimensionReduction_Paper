load_all("../../RPEnsemble/")

set.seed(1234)

# Simulation parameters
p <- 100

n_train <- 100
n_val <- 0
n_test <- 100


# Train/test split
ids <- tvt(n_train, n_val, n_test)

# Simulation model 1
simulated_data_1 <- RPModel(1, n = n_train + n_test, p = 100)

XTrain_1 <- simulated_data_1$x[ids$train, ]
YTrain_1 <- simulated_data_1$y[ids$train]

reduction_1 <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, B1 = 1000, d = 3, base = "QDA")

# Simulation model 2
simulated_data_2 <- RPModel(2, n = 200, p = 100)

XTrain_2 <- simulated_data_2$x[ids$train, ]
YTrain_2 <- simulated_data_2$y[ids$train]

reduction_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, B1 = 1000, d = 3, base = "QDA")

# Simulation model 3
simulated_data_3 <- RPModel(3, n = 200, p = 100)

XTrain_3 <- simulated_data_3$x[ids$train, ]
YTrain_3 <- simulated_data_3$y[ids$train]

reduction_3 <- RPReduce(XTrain = XTrain_3, YTrain = YTrain_3, B1 = 1000, d = 3, base = "QDA")

# Simulation model 4
simulated_data_4 <- RPModel(4, n = 200, p = 100)

XTrain_4 <- simulated_data_4$x[ids$train, ]
YTrain_4 <- simulated_data_4$y[ids$train]

reduction_4 <- RPReduce(XTrain = XTrain_4, YTrain = YTrain_4, B1 = 1000, d = 3, base = "QDA")


