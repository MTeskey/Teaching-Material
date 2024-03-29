setwd('../Slides -- Factor Analysis/R code for plots')

# Generate raw data
x = rnorm(50, mean = 0, sd = 1)
y = x + rnorm(50, mean = 0, sd = .5)
x = c(x, 1.38)
y = c(y, 2.35)

# Do PCA
svd.fit = svd(matrix(c(x,y), ncol = 2))
svd.fit$v[,2] = -svd.fit$v[,2]

# Plot 1
setEPS()
postscript("../Latex/figures/pca1.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(x, y,
     xlab = 'Talkativeness',
     ylab = 'Openness',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
grid(NULL, NULL)
dev.off()


# Plot 2
setEPS()
postscript("../Latex/figures/pca2.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(x, y,
     xlab = 'Talkativeness',
     ylab = 'Openness',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
arrows(x0 = 0, y0 = 0,
       x1 = 1, y1 = 0,
       length = .15, lwd = 2, col = 'blue')
arrows(x0 = 0, y0 = 0,
       x1 = 0, y1 = 1,
       length = .15, lwd = 2, col = 'blue')
grid(NULL, NULL)
dev.off()


# Plot 3
setEPS()
postscript("../Latex/figures/pca3.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(x, y,
     xlab = 'Talkativeness',
     ylab = 'Openness',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
arrows(x0 = 0, y0 = 0,
       x1 = 1, y1 = 0,
       length = .15, lwd = 2, col = 'blue')
arrows(x0 = 1, y0 = 0,
       x1 = 1.38, y1 = 0,
       length = .15, lwd = 2, col = 'blue')
arrows(x0 = 1.38, y0 = 0,
       x1 = 1.38, y1 = 1,
       length = .15, lwd = 2, col = 'blue')
arrows(x0 = 1.38, y0 = 1,
       x1 = 1.38, y1 = 2,
       length = .15, lwd = 2, col = 'blue')
arrows(x0 = 1.38, y0 = 2,
       x1 = 1.38, y1 = 2.35,
       length = .15, lwd = 2, col = 'blue')
grid(NULL, NULL)
dev.off()


# Plot 4
setEPS()
postscript("../Latex/figures/pca4.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(x, y,
     xlab = 'Talkativeness',
     ylab = 'Openness',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
arrows(x0 = 0, y0 = 0,
       x1 = svd.fit$v[1,1], y1 = svd.fit$v[2,1],
       length = .15, lwd = 2, col = 'red')
arrows(x0 = 0, y0 = 0,
       x1 = svd.fit$v[1,2], y1 = svd.fit$v[2,2],
       length = .15, lwd = 2, col = 'red')
grid(NULL, NULL)
dev.off()


# Plot 5
setEPS()
postscript("../Latex/figures/pca5.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(x, y,
     xlab = 'Talkativeness',
     ylab = 'Openness',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
arrows(x0 = 0, y0 = 0,
       x1 = svd.fit$v[1,1], y1 = svd.fit$v[2,1],
       length = .15, lwd = 2, col = 'red')
arrows(x0 = svd.fit$v[1,1], y0 = svd.fit$v[2,1],
       x1 = 2*svd.fit$v[1,1], y1 = 2*svd.fit$v[2,1],
       length = .15, lwd = 2, col = 'red')
arrows(x0 = 2*svd.fit$v[1,1], y0 = 2*svd.fit$v[2,1],
       x1 = 2.7*svd.fit$v[1,1], y1 = 2.7*svd.fit$v[2,1],
       length = .15, lwd = 2, col = 'red')
arrows(x0 = 2.7*svd.fit$v[1,1], y0 = 2.7*svd.fit$v[2,1],
       x1 = 2.7*svd.fit$v[1,1] + .6*svd.fit$v[1,2], 
       y1 = 2.7*svd.fit$v[2,1] + .6*svd.fit$v[2,2],
       length = .15, lwd = 2, col = 'red')
grid(NULL, NULL)
dev.off()


# Plot 6
setEPS()
postscript("../Latex/figures/pca6.eps",
           width = 4, height = 4)
par(mar = c(4, 4, 2, 2) + .1,
    oma = c(.1, .1, 0, .1) + 0.1)
plot(svd.fit$u[,1]*svd.fit$d[1], svd.fit$u[,2]*svd.fit$d[2],
     xlab = 'Extraversion',
     ylab = 'Noise',
     pch = 16,
     xlim = c(-3,3),
     ylim = c(-3,3))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
arrows(x0 = 0, y0 = 0,
       x1 = 1, y1 = 0,
       length = .15, lwd = 2, col = 'red')
arrows(x0 = 0, y0 = 0,
       x1 = 0, y1 = 1,
       length = .15, lwd = 2, col = 'red')
grid(NULL, NULL)
dev.off()

