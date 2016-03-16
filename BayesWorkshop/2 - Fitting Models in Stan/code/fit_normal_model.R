setwd('DIRECTORY')
library(rstan)

# Load and plot data
data = read.csv('example_data.csv', header = F)
hist(data[,1], col = 'black', xlab = 'X')

# Fit stan model with short chains
stan_data = list(N = dim(data)[1],
                 x = data[,1])
stan_fit = stan(file = 'normal_fit.stan',
                data = stan_data,
                warmup = 10, iter = 20,
                chains = 3)

# Plot chains for each parameter
traceplot(stan_fit)

# View summary + diagnostics
print(stan_fit)

# View autocorrelation
arr = as.array(stan_fit)
acf(arr[,1,1])

# Plot samples
stan_samples = extract(stan_fit)
hist(stan_samples$mu, probability = T,
     xlab = 'Mu', ylab = '', col = 'black',
     xlim = c(-.5,.5), ylim = c(0,5),
     main = 'Posterior Samples')
grid(nx = NULL, ny = NULL)

# Add true posterior
x = seq(from = -.5, to = .5, length.out = 100)
y = dnorm(x, 
          mean = sum(stan_data$x)/(stan_data$N + 1),
          sd = sqrt(1/(stan_data$N + 1)))
lines(x,y,
     lty = 2, lwd = 2, col = 'red')
legend(x = -.5, y = 5,
       lty = 2, col = 'red',
       legend = 'True Posterior')
