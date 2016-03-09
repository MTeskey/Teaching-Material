# Computes and plots the log/likelihood function for a binomial experiment

# Number of coin flips
N = 4

# Number of heads
k = 3



### LIKELIHOOD ###
lik = function(p) choose(N,k) * p^k * (1-p)^(N-k)

# Plot
x = seq(from = 0, to = 1, length.out = 100)
plot(x, lik(x), xlim = c(0,1),
     type = 'l', lwd = 2,
     xlab = 'p', ylab = 'Likelihood',
     main = 'Binomial Likelihood')
grid(nx = NULL, ny = NULL)

# Indicate maximum likelihood estimate
abline(v = k/N, lty = 2, lwd = 2, col = 'red')



### LOG LIKELIHOOD ###
loglik = function(p) k*log(p) + (N-k)*log(1-p)

# Plot
x = seq(from = 0, to = 1, length.out = 100)
plot(x, loglik(x), xlim = c(0,1),
     type = 'l', lwd = 2,
     xlab = 'p', ylab = 'Log-Likelihood',
     main = 'Binomial Log-Likelihood')
grid(nx = NULL, ny = NULL)

# Indicate maximum likelihood estimate
abline(v = k/N, lty = 2, lwd = 2, col = 'red')


