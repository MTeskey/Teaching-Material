setwd('PATH-TO-DATA')
library(rstan)

# Import data
data = read.csv('coinflip_data.csv', header = F)

# Create data file for stan
stan_data = list(N = dim(data)[1],
                 k = sum(data[,1]))

# Fit stan model
stan_fit = stan(file = 'binomial_model.stan',
                data = stan_data)

# Print model results
print(stan_fit)

# We can also extract the samples and plot the estimated posterior distribution, or
# compute summary statistics
stan_samples = extract(stan_fit)
mean(stan_samples$p)
hist(stan_samples$p,
     probability = T, ylab = '',
     xlab = 'p', main = 'Posterior distribution for p')
abline(v = mean(stan_samples$p),
       lty = 2, lwd = 2, col = 'red')
