load_all("../../RPEnsemble/")

set.seed(1234)

p <- 100
n <- 200
lambda <- 10

mu0 <- c(1,rep(0,p-1))
mu1 <- c(-1,rep(0,p-1))

sig0 <- diag(p)
sig1 <- diag(p)
sig1[2,2] <- lambda


X0 <- mvrnorm(n = n/2, mu = mu0, Sigma = sig0)
X1 <- mvrnorm(n = n/2, mu = mu1, Sigma = sig1)

data <- list()
data$XTrain <- rbind(X0,X1)
data$YTrain <- rep(c(0,1), each = n/2)

