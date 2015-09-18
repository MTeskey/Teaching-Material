library(MASS)
library(psych)
setwd('/Users/areshenk/Documents/Teaching-Material/Slides -- Factor Analysis/R code for plots')


# Specify factor model Y = AX + E
# Variables: BP, HR, GSR, STAI, Cortisol
A = rbind(mvrnorm(n = 60, mu = c(1,1), Sigma = matrix(c(1,.6,1,.6), nrow = 2)),
          mvrnorm(n = 60, mu = c(-1,-1), Sigma = matrix(c(1,.6,1,.6), nrow = 2)))
X = matrix(c(.8, .9, .7, 0, 0,
             0, 0, 0, .8, .7), 
           byrow = T, nrow = 2)
Y = A %*% X + matrix(rnorm(120*5, mean = 0, sd = .5), ncol = 5)
colnames(Y) = c('BP', 'HR', 'GSR', 'STAI', 'Cortisol')

# Without rotation
fit = prcomp(Y, scale = F)
vexp = fit$sdev^2
png(filename = '../Latex/figures/stress_scree.png',
    width = 800, height = 600, pointsize = 12)
par(cex = 1.5)
plot(1:5, vexp/sum(vexp),
     xlab = 'Factor',
     ylab = 'Variance Explained',
     type = 'b')
grid(nx = NULL, ny = NULL)
dev.off()




# With rotation
library(ggplot2)
source('theme_pub.R')
fitrot = principal(Y, nfactors = 2, rotate = "promax")
scores = fitrot$scores
plotFrame = data.frame(Condition = rep(c('Control', 'Stress'), each = 60, 2),
                       Stress = rep(c('Acute', 'Chronic'), each = 120),
                       Score = c(scores[,1], scores[,2]))

png(filename = '../Latex/figures/stress_comp1.png',
    width = 600, height = 400, pointsize = 12)
ggplot(subset(plotFrame, Stress = 'Acute'), aes(x = Score, fill = Condition)) +
    geom_histogram(position = 'dodge') +
    theme_pub() +
    scale_fill_manual(values = c(gray(0), gray(.6))) +
    xlab('Score') +
    ylab('') +
    ggtitle('Acute Stress Factor') +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
dev.off()


png(filename = '../Latex/figures/stress_comp2.png',
    width = 600, height = 400, pointsize = 12)
ggplot(subset(plotFrame, Stress = 'Chronic'), aes(x = Score + rnorm(120, 0, .4), 
                                                  fill = Condition)) +
    geom_histogram(position = 'dodge') +
    theme_pub() +
    scale_fill_manual(values = c(gray(0), gray(.6))) +
    xlab('Score') +
    ylab('') +
    ggtitle('Chronic Stress Factor') +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
dev.off()













