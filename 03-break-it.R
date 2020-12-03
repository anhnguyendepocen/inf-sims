# load packages
library(tidyverse)

# create a "population" to sample from
pop <- c(1, 2, 3, 4, 5, 6)
# pop <- rnorm(1000)

# population mean; what we're trying to estimate
mean(pop)

# size of sample 
n <- 50

# ----
# 10,000 samples
# ----

# take 10,000 samples; store each
x_bar  <- numeric(10000)          # a container to store each sample mean
se_hat <- numeric(length(x_bar))  # a container to store each estimated se
pb <- progress::progress_bar$new(total = length(x_bar))  # initialize progress bar
for (i in 1:length(x_bar)) {
  # take a sample
  x <- sample(pop, size = n, replace = TRUE)
  # store estimate of population mean and se
  x_bar[i]  <- mean(x)
  se_hat[i] <- sd(x)/sqrt(n)
  # update progress bar
  pb$tick()
}

sims <- data.frame(x_bar, se_hat) %>%
  mutate(sim_id = 1:n(),
         lwr95 = x_bar - 2*se_hat,
         upr95 = x_bar + 2*se_hat, 
         capture = case_when(lwr95 > mean(pop) ~ "Missed High",
                             upr95 < mean(pop) ~ "Missed Low",
                             TRUE ~ "Captured")) %>%
  glimpse()

# summarize capture rates
prop.table(table(sims$capture))

# plot the cis for each sample
gg <- ggplot(sims[1:50, ], aes(x = x_bar, y = sim_id, 
                       xmin = lwr95, xmax = upr95, 
                       color = capture)) + 
  geom_pointrange(size = 0.4, shape = "+") + 
  labs(title = "First 50 of 10,000 Simulations"); gg



