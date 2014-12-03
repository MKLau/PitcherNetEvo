###SEM for ahmed's HeRE chapter

library(lavaan)
###http://lavaan.ugent.be/tutorial/index.html
library(semPlot)
library(vegan)

###Reading in the data
pred <- read.table("../data/predictors.csv", header=T, sep=",")
#env <- read.table("../data/environment.csv", header=T, sep=",")
attach(pred)
hwd <- hml <- grd <- lgd <- as.character(trt)
hwd[hwd!='HW'] <- '0';hwd[hwd=='HW'] <- '1';hwd <- as.numeric(hwd)
hml[hml!='HE'] <- '0';hml[hml=='HE'] <- '1';hml <- as.numeric(hml)
grd[grd!='G'] <- '0';grd[grd=='G'] <- '1';grd <- as.numeric(grd)
lgd[lgd!='L'] <- '0';lgd[lgd=='L'] <- '1';lgd <- as.numeric(lgd)

pairs(data.frame(NOVI,PLCI,trt,soilmoisture,soiltemp,soilpH,soilBulkDensity,DWD.volume.m2,LL.Depth,seedlingdensity,understoryPrcntCover))
dat <- data.frame(NOVI,PLCI,trt,hml,grd,lgd,hwd,soilmoisture,soiltemp,soilpH,soilBulkDensity,DWD.volume.m2,LL.Depth,seedlingdensity,understoryPrcntCover)
detach(pred)
attach(dat)

pca <- princomp(cbind(soilmoisture,soiltemp,soilpH,soilBulkDensity,DWD.volume.m2,LL.Depth,seedlingdensity,understoryPrcntCover))
summary(pca)
plot(pca$scores[,1:2],pch=as.character(as.numeric(trt)))
plot(envfit(pca$scores[,1:2],env=dat))
###pull out prcincipal components axes
env1 <- pca$scores[,1]
###sqrt.env1
env1 <- sqrt(env1 + abs(min(env1)))
env2 <- pca$scores[,2]
detach(dat)
dat <- data.frame(dat,env1,env2)
attach(dat)

summary(aov(PLCI~trt*(env1 + env2)))
summary(aov(NOVI~trt*(env1 + env2)))

###
m1 <- 
  '#regressions
PLCI ~ env1 + env2
env1 + env2 ~ hml + grd + lgd 

   #variances and covariances
env1 ~~ env1
env2 ~~ env2
PLCI ~~ PLCI
env1 ~~ env2

   #intercepts
PLCI ~ 1
env1 ~ 1 
env2 ~ 1

'

fit1 <- sem(m1, data=dat, estimator="mlr", missing="ml")  ## Fitting full SEM using lavaan oackage
varTable(fit1)
fit1
summary(fit1)
semPaths(fit1,what="std",layout="tree2",structural=FALSE,residuals=TRUE)
modindices(fit1)[modindices(fit1)$mi>=3,]
