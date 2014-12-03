###Barplot for Ahmed

###2dec2014

library(gplots)
dat <- read.csv('../data/detect.csv')[,-1]
head(dat)
dat <- as.matrix(na.omit(dat))
rownames(dat) <- read.csv('../data/detect.csv')[-5,1]
mu <- t(dat[,1:3])
se <- t(dat[,4:6])
barplot2(mu,beside=TRUE,plot.ci=TRUE,ci.u=mu+se,ci.l=mu-se,
         ylim=c(0,1),ylab='Probability of Detection',col=grey(seq(1,0.1,length=3)))
legend('topright',legend=c('Pre-HWA','Pre-HWA & Post-HeRE','Post-HWA'),
       fill=grey(seq(1,0.1,length=3)))
