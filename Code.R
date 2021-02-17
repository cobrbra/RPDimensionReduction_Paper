# install.packages("devtools")
# library(devtools)
# 
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

n_train <- 100
n_val <- 0
n_test <- 100


# Train/test split
ids <- tvt(n_train, n_val, n_test)

# Simulation model 1
simulated_data_1 <- RPModel(1, n = n_train + n_test, p = 100)

XTrain_1 <- simulated_data_1$x[ids$train, ]
YTrain_1 <- simulated_data_1$y[ids$train]

reduction_1 <- RPReduce(XTrain = XTrain_1, YTrain = YTrain_1, B1 = 1000, B2 = 20, d = 3, base = "QDA")

sim1_ev <- data.frame(sd = 1:length(reduction_1$d), ev = reduction_1$d) %>% 
  ggplot(aes(x = sd, y = ev)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# ggsave(filename = "figures/sim1_ev.png", plot = sim1_ev, width = 8, height = 3)

# Simulation model 2
simulated_data_2 <- RPModel(2, n = 200, p = 100)

XTrain_2 <- simulated_data_2$x[ids$train, ]
YTrain_2 <- simulated_data_2$y[ids$train]

reduction_2 <- RPReduce(XTrain = XTrain_2, YTrain = YTrain_2, B1 = 1000, B2 = 20, d = 3, base = "QDA")

sim2_ev <- data.frame(sd = 1:length(reduction_2$d), ev = reduction_2$d) %>% 
  ggplot(aes(x = sd, y = ev)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# ggsave(filename = "figures/sim2_ev.png", plot = sim2_ev, width = 8, height = 3)

# Simulation model 3
simulated_data_3 <- RPModel(3, n = 200, p = 100)

XTrain_3 <- simulated_data_3$x[ids$train, ]
YTrain_3 <- simulated_data_3$y[ids$train]

reduction_3 <- RPReduce(XTrain = XTrain_3, YTrain = YTrain_3, B1 = 1000, B2 = 20, d = 3, base = "QDA")

sim3_ev <- data.frame(sd = 1:length(reduction_3$d), ev = reduction_3$d) %>% 
  ggplot(aes(x = sd, y = ev)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# ggsave(filename = "figures/sim3_ev.png", plot = sim3_ev, width = 8, height = 3)

# Simulation model 4
simulated_data_4 <- RPModel(4, n = 200, p = 100)

XTrain_4 <- simulated_data_4$x[ids$train, ]
YTrain_4 <- simulated_data_4$y[ids$train]

reduction_4 <- RPReduce(XTrain = XTrain_4, YTrain = YTrain_4, B1 = 1000, B2 = 20, d = 3, base = "QDA")

sim4_ev <- data.frame(sd = 1:length(reduction_4$d), ev = reduction_4$d) %>% 
  ggplot(aes(x = sd, y = ev)) + geom_point() + 
  theme_minimal() + labs(x = "Subspace Dimension", y = "Eigenvalue") +
  theme(axis.title.y = element_text(vjust = 2), axis.title.x = element_text(vjust = -2))
# ggsave(filename = "figures/sim4_ev.png", plot = sim4_ev, width = 8, height = 3)

sim_evs <- plot_grid(sim1_ev, sim2_ev, sim3_ev, sim4_ev, nrow = 4, labels = "AUTO")
ggsave(filename = "figures/sim_evs.png", plot = sim_evs, width = 8, height = 12)
