setwd('/media/areshenk/StorageDisk1/Writings/Teaching-Material/Slides -- Factor Analysis/R code for plots')
require(foreign)
raw = read.spss('personality.sav', 
                 to.data.frame = F,
                 use.value.labels = F)
data = as.data.frame(raw)
varNames = as.character(attr(raw, "variable.labels"))
fit = prcomp(data[complete.cases(data),])
rot = varimax(fit$rotation[,1:5])

# Scree
vexp = fit$sdev^2
png(filename = '../Latex/figures/personality_scree1.png',
    width = 800, height = 600, pointsize = 12)
par(cex = 1.5)
plot(1:44, vexp/sum(vexp),
     xlab = 'Factor',
     ylab = 'Variance Explained',
     type = 'b')
grid(nx = NULL, ny = NULL)
dev.off()

# Rotated plot
library(ggplot2)
library(reshape)
plotFrame = data.frame(rot$loadings[1:44,1:5])
plotFrame$Variable = varNames
plotFrame = melt(plotFrame)
colnames(plotFrame) = c('Variable', 'Factor', 'Loading')

png(filename = '../Latex/figures/personality_factor_rot.png',
    width = 800, height = 800, pointsize = 10.5)
ggplot(plotFrame, aes(Factor, Variable)) + 
    geom_tile(aes(fill = Loading), colour = "white") + 
    scale_fill_gradient2(limits=c(-1, 1),
                         low = "blue", mid = 'white',  high = "red") +
    xlab('Factor') +
    ylab('Variable') +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 16,face="bold"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15))
dev.off()
