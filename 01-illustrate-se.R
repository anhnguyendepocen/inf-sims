
# load packages
library(tidyverse)

# create a "population" to sample from
pop <- c(1, 2, 3, 4, 5, 6)

# population mean; what we're trying to estimate
mean(pop)

# ----
# one sample
# ----

# take one sample
n <- 50  # size of each sample
x <- sample(pop, size = n, replace = TRUE)

# alternatively, you can draw from a distribution
# x <- rexp(n = n, rate = 0.1)

# sample mean; estimate of population mean
mean(x)

# run this code several times. you'll notice that the sample
# mean is different from, but close to the population mean. 
# but how close?

# ----
# five samples
# ----

# take five samples; print sample mean for each
for (i in 1:5) {
  # take a sample
  x <- sample(pop, size = n, replace = TRUE)
  # print the sample mean; estimate of population mean
  print(mean(x))
}

# ----
# 100 samples
# ----

# take 100 samples; store each
x_bar <- numeric(100)  # a container to store each sample mean
for (i in 1:100) {
  # take a sample
  x <- sample(pop, size = n, replace = TRUE)
  # print the sample mean; estimate of population mean
  x_bar[i] <- mean(x)
}

# plot x_bars 
gg_data <- data.frame(x_bar)
ggplot(gg_data, aes(x = x_bar)) + 
  geom_histogram() + 
  geom_vline(xintercept = mean(pop), 
             color = scales::muted("green"))

# summarize x_bars
mean(x_bar)  # we know it's unbiased
sd(x_bar)    # 100 samples; okay approximation to actual SE
sqrt(mean((pop- mean(pop))^2))/sqrt(n) # actual SE

# ----
# 10,000 samples
# ----

# take 10,000 samples; store each
x_bar <- numeric(10000)  # a container to store each sample mean
pb <- progress::progress_bar$new(total = length(x_bar))  # initialize progress bar
for (i in 1:length(x_bar)) {
  # take a sample
  x <- sample(pop, size = n, replace = TRUE)
  # store estimate of population mean
  x_bar[i] <- mean(x)
  # update progress bar
  pb$tick()
}

# plot x_bars 
gg_data <- data.frame(x_bar)
ggplot(gg_data, aes(x = x_bar)) + 
  geom_histogram() + 
  geom_vline(xintercept = mean(pop), 
             color = scales::muted("green"))

# summarize x_bars
mean(x_bar)  # we know it's unbiased
sd(x_bar)    # 10,000 samples; very good approximation to actual SE
sqrt(mean((pop- mean(pop))^2))/sqrt(n) # actual SE




