library(tidyverse)

set.seed(12191) # To ensure reproducibility
n <- 100
fix_dist <- rnorm(n, mean=0, sd=1)

var_means <- seq(0,0.4,0.001)
var_means_grid <- expand.grid(varMeans = var_means, sds=1)
var_dists <- apply(var_means_grid, 1, function(x) rnorm(n, mean=x[1], sd=x[2]))

wilcox_pvals <- apply(var_dists, 2, function(x) wilcox.test(x, fix_dist)$p.val)
ks_pvals <- apply(var_dists, 2, function(x) ks.test(x, fix_dist)$p.val)
t_pvals <- apply(var_dists, 2, function(x) t.test(x, fix_dist)$p.val)

p_vals <- data.frame(ks_pvals, wilcox_pvals, t_pvals)
p_vals <- p_vals %>% gather(test, pVal)

ggplot(p_vals, aes(x=test, y=pVal)) + geom_boxplot() + 
  theme_classic() + geom_hline(yintercept = 0.05, linetype='dashed', color='red')

n <- 100
fix_pois <- rpois(n, lambda=1)

var_lambdas <- seq(1,1.4,0.001)
var_pois <- lapply(var_lambdas, function(x) rpois(n, lambda = x))

wilcox_pvals <- sapply(var_pois, function(x) wilcox.test(x, fix_pois)$p.val)
ks_pvals <- sapply(var_pois, function(x) ks.test(x, fix_pois)$p.val)
t_pvals <- sapply(var_pois, function(x) t.test(x, fix_pois)$p.val)

p_vals <- data.frame(ks_pvals, wilcox_pvals, t_pvals)
p_vals <- p_vals %>% gather(test, pVal)

ggplot(p_vals, aes(x=test, y=pVal)) + geom_boxplot() + 
  theme_classic() + geom_hline(yintercept = 0.05, linetype='dashed', color='red')