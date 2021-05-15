library(devtools)
load_all("../../RPEnsemble/")
library(tidyverse)
library(latex2exp)
library(cowplot)

gen_data <- function(Delta, n1 = 100, n2 = NULL) {
  if (is.null(n2)) {
    n2 <- n1
  }
  
  X1 <- matrix(rnorm(n = 2*n1, mean = rep(c(Delta / 2, 0), each = n1)), n1, 2)
  X2 <- matrix(rnorm(n = 2*n2, mean = rep(c(- Delta / 2, 0), each = n2)), n2, 2)
  Y <- c(rep(1, n1), rep(0, n2))
  
  return(list(x = rbind(X1, X2), y = Y))
}

set.seed(1234)
Delta_range <- seq(0, 15, 0.1) 
n_trials <- 2000
power <- matrix(0, 4, length(Delta_range))
b2s <- c(2,5,10,20)
for (del_index in 1:length(Delta_range)) {
  Delta <- Delta_range[del_index]
  for (b2_index in 1:length(b2s)) {
    b2 <- b2s[b2_index]
    for (trial in 1:n_trials) {
      data <- gen_data(Delta)
      power[b2_index, del_index] <- power[b2_index, del_index] + (1/n_trials) * RPDecomposeA(XTrain = data$x, YTrain = data$y, B1 = 1, B2 = b2, d = 1, base = "LDA")$v[1,1]^2
    }
  }
}

plot(Delta_range, colMeans(power))

results <- as.data.frame(t(power))
colnames(results) <- b2s
results$Delta <- Delta_range
results <- results %>% 
  pivot_longer(cols = - c(Delta), names_to = "B2", values_to = "power") %>% 
  mutate(B2 = factor(as.numeric(B2)))

write_tsv(results, "results/ChangingDelta.tsv")

fig <- results %>% 
  ggplot(aes(x = Delta, y = power, colour = B2)) + geom_line() +
  labs(x = TeX("$\\Delta"), y = "Power") + ylim(0.5, 1) + theme_minimal() 
 
sims <- rowMeans((rep(1, length(Delta_range)) %*% t(sin(1:1000)^2)) * (1 - pnorm(Delta_range %*% t(abs(cos(1:1000))) / 2)) * pnorm(Delta_range %*% t(abs(sin(1:1000))) / 2) +
              (rep(1, length(Delta_range)) %*% t(cos(1:1000)^2)) * (1 - pnorm(Delta_range %*% t(abs(sin(1:1000))) / 2)) * pnorm(Delta_range %*% t(abs(cos(1:1000))) / 2) +
               0.5 * (1 - pnorm(Delta_range %*% t(abs(sin(1:1000))) / 2)) * (1 - pnorm(Delta_range %*% t(abs(cos(1:1000))) / 2)) +
               0.5 * pnorm(Delta_range %*% t(abs(sin(1:1000))) / 2) * pnorm(Delta_range %*% t(abs(cos(1:1000))) / 2))

sims_data <- data.frame(Delta = Delta_range, power = sims)

sims_fig <- sims_data %>% 
  ggplot(aes(x = Delta, y = power)) + geom_line() +
  labs(x = TeX("$\\Delta"), y = "Power") + ylim(0.5, NA) + theme_minimal()
  
p <- plot_grid(fig, sims_fig, labels = "AUTO", rel_widths = c(1.5, 1))
ggsave("results/figures/ChangingDelta.png", p, width = 10, height = 5)
