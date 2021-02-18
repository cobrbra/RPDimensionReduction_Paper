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
ggsave("figures/ExampleFigures.png", example_figure)



### Simulation Studies

# Simulation parameters

set.seed(1234)

p <- 100

n_train <- 1000
n_val <- 0
n_test <- 500


# Train/test split
ids <- tvt(n_train, n_val, n_test)

# Simulation model 1
simulated_data_1 <- RPModel(1, n = n_train + n_test, p = p)

XTrain_1 <- simulated_data_1$x[ids$train, ]
YTrain_1 <- simulated_data_1$y[ids$train]

XTest_1 <- simulated_data_1$x[ids$test, ]
YTest_1 <- simulated_data_1$y[ids$test]


decompose_1 <- RPDecomposeA(XTrain = XTrain_1, YTrain = YTrain_1, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "loo")
reduce_1 <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, reduced_dim = 2, XTest = XTest_1, YTest = YTest_1, d = 2, decomposition = decompose_1) %>% 
  mutate(Model = 1)


sim1_ev <- data.frame(sd = 1:length(decompose_1$eigv), eigv = decompose_1$eigv) %>% 
  ggplot(aes(x = sd, y = eigv)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))

sim1_rv <- reduce_1 %>% 
  ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
  labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
  theme_minimal() + theme(legend.position = "none")

# Simulation model 2
simulated_data_2 <- RPModel(2, n = n_train + n_test, p = p)

XTrain_2 <- simulated_data_2$x[ids$train, ]
YTrain_2 <- simulated_data_2$y[ids$train]

XTest_2 <- simulated_data_2$x[ids$test, ]
YTest_2 <- simulated_data_2$y[ids$test]


decompose_2 <- RPDecomposeA(XTrain = XTrain_2, YTrain = YTrain_2, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "loo")
reduce_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, reduced_dim = 2, XTest = XTest_2, YTest = YTest_2, d = 2, decomposition = decompose_2) %>% 
  mutate(Model = 2)


sim2_ev <- data.frame(sd = 1:length(decompose_2$eigv), eigv = decompose_2$eigv) %>% 
  ggplot(aes(x = sd, y = eigv)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))

sim2_rv <- reduce_2 %>% 
  ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
  labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
  theme_minimal() + theme(legend.position = "none")

# Simulation model 3
simulated_data_3 <- RPModel(3, n = n_train + n_test, p = p)

XTrain_3 <- simulated_data_3$x[ids$train, ]
YTrain_3 <- simulated_data_3$y[ids$train]

XTest_3 <- simulated_data_3$x[ids$test, ]
YTest_3 <- simulated_data_3$y[ids$test]


decompose_3 <- RPDecomposeA(XTrain = XTrain_3, YTrain = YTrain_3, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "loo")
reduce_3 <- RPReduce(XTrain = XTrain_3, YTrain = YTrain_3, reduced_dim = 2, XTest = XTest_3, YTest = YTest_3, d = 2, decomposition = decompose_3) %>% 
  mutate(Model = 3)


sim3_ev <- data.frame(sd = 1:length(decompose_3$eigv), eigv = decompose_3$eigv) %>% 
  ggplot(aes(x = sd, y = eigv)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))

sim3_rv <- reduce_3 %>% 
  ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
  labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
  theme_minimal() + theme(legend.position = "none")

# Simulation model 4
simulated_data_4 <- RPModel(4, n = n_train + n_test, p = p) 

XTrain_4 <- simulated_data_4$x[ids$train, ]
YTrain_4 <- simulated_data_4$y[ids$train]

XTest_4 <- simulated_data_4$x[ids$test, ]
YTest_4 <- simulated_data_4$y[ids$test]


decompose_4 <- RPDecomposeA(XTrain = XTrain_4, YTrain = YTrain_4, B1 = 1000, B2 = 10, d = 3, base = "knn", estmethod = "loo")
reduce_4 <- RPReduce(XTrain = XTrain_4, YTrain = YTrain_4, reduced_dim = 2, XTest = XTest_4, YTest = YTest_4, d = 2, decomposition = decompose_4) %>% 
  mutate(Model = 4)


sim4_ev <- data.frame(sd = 1:length(decompose_4$eigv), eigv = decompose_4$eigv) %>% 
  ggplot(aes(x = sd, y = eigv)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))

sim4_rv <- reduce_4 %>% 
  ggplot(aes(x = Dim_1, y = Dim_2, colour = YTest)) + geom_point() + 
  labs(x = TeX("$\\hat{r}_1"), y = TeX("$\\hat{r}_2")) + 
  theme_minimal() + theme(legend.position = "none") + lims(x = c(-10, 10), y = c(-10, 10))


sim_evs <- plot_grid(sim1_ev, sim2_ev, sim3_ev, sim4_ev, nrow = 4, labels = "AUTO")
ggsave(filename = "figures/sim_evs.png", plot = sim_evs, width = 8, height = 12)

write_tsv(x = bind_rows(reduce_1, reduce_2, reduce_3, reduce_4), file = "data/sim_reductions.tsv")
sim_rvs <- plot_grid(sim1_rv, sim2_rv, sim3_rv, sim4_rv, nrow = 2, labels = "AUTO")
ggsave(filename = "figures/sim_rvs.png", plot = sim_rvs, width = 8, height = 8)
