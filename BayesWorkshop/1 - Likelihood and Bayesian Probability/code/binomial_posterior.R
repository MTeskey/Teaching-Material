# Computes and plots the likelihood and posterior distributions
# for a binomial parameter p for a specified beta prior

# Number of coin flips
N = 4

# Number of heads
k = 3

# Normalized likelihood
lik = function(p){
    p^k * (1-p)^(N-k) * (factorial(N+1)/(factorial(k)*factorial(N-k)))
}

# Beta prior parameters
alpha = 20
beta = 20

# Plot
x = seq(from = 0, to = 1, length.out = 100)
y_max = max(c(dbeta(x, alpha, beta),
              lik(x),
              dbeta(x, alpha + k, beta + N - k)))
plot(0, 
     xlim = c(0,1), ylim = c(0, y_max),
     type = 'l', lwd = 2,
     xlab = 'p', ylab = '')
grid(nx = NULL, ny = NULL)

# Plot prior
lines(x, dbeta(x, alpha, beta),
      lty = 2, lwd = 2, col = 'blue')

# Plot likelihood
lines(x, lik(x),
      lty = 1, lwd = 2, col = 'black')

# Plot posterior
lines(x, dbeta(x, alpha + k, beta + N - k),
      lty = 2, lwd = 2, col = 'red')

legend(x = 0, y = y_max,
       fill = c('blue', 'black', 'red'),
       legend = c('Prior', 'Likelihood', 'Posterior'))



