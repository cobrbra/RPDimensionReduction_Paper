library(devtools)
load_all("../../RPEnsemble/")
library(tidyverse)
library(latex2exp)

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
  ggplot(aes(x = Delta, y = power, colour = B2)) + geom_smooth(se = FALSE) +
  labs(x = TeX("$\\Delta"), y = "Power") + ylim(0.5, 1) + theme_minimal()

ggsave("results/figures/ChangingDelta.png", fig, width = 5, height = 5)
