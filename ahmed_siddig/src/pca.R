##Principal Components Analsis lab - R script

##Setup your R work session
setwd('F:/data/ordination')
library(vegan)
library(fields)
library(scatterplot3d)
library(ellipse)
library(mgcv)
library(akima)
library(energy)
source('F:/data/ordination/biostats.R')
predictors<-read.csv('predictors.csv',header=TRUE)
predictors
### rip.bird<-subset(birdhab,select=AMGO:WWPE)
### 
predic <-subset(predictors,select=soilmoisture:understoryPrcntCover)
predic
##Evaluate suitability of the data set
dim(predictors)

col.summary(predic) 
hist.plots(predictors,var='Moisture:NOVI') 
box.plots(covar1,var='Moisture:RBS') 
box.plots(predictors,var='Sap.density:cover', by= 'trt') 
predictors
###### Scatterplot matrix
pairs(stand.predic, var='soilmoisture:understoryPrcntCover')
pairs(stand.predic,lower.panel=panel.smooth,upper.panel=panel.cor,method='spearman')

## datat standaraization 
stand.predic <- data.stand(predic,var='soilmoisture:understoryPrcntCover',method='normalize',margin='row')
stand.predic
##Conduct the PCA
stand.predic.pca<-prcomp(apply(predic[,1:8],2,function(x) (x-mean(x))/sd(x)),scale=TRUE)
stand.predic.pca
predic.pca
predic
#Sample scores and ordination plots:
predic.pca$x
stand.predic.pca$x[,1:2]
z<-scale(predic)
z%*%predic.pca$rotation

#Simple bi-plots
predic.rda<-rda(predic,scale=TRUE)
biplot(predic.pca,choices=c(1,2),cex=c(.8,.7),col=as.numeric(factor(predictors$trt)))
plot(predic.pca$x[,1:2],pch=as.numeric(factor(predictors$trt)))
legend('topright',legend=levels(factor(predictors$trt)),pch=1:4)
plot(envfit(ord=predic.pca$x[,1:2],env=predic),col='red')
ordiplot(predic.rda,choices=c(1,2),type='text',scaling=2,col=as.numeric(factor(predictors$trt)))

########## MODEL 1 ############################
### HeRE Treatments affects Amphibians indirectly through local Environment changes in Harvard Forest
# 1- First HeRE Trt affects environment 
## 1-1 trt vs. all environmental factors as PC 
summary(aov((predic.pca$x[,1]~predictors$trt)))
 
## 1-2 Trt vs. partitioned environmental factors (soil,natural objects and vegetation) as PC
## 1-2-1 soil pc #################################### 
summary(aov((predic.soil.pca$x[,1]~predictors$trt)))

## 1-2-2 natural objects pc #########################
summary(aov((predic.no.pca$x[,1]~predictors$trt)))

## 1-2-3 vegetation PC ############################## 
summary(aov((predic.veg.pca$x[,1]~predictors$trt)))
 

###2- Environment predicts amphibians
# 2-1 ALL at one PC 
M.PLCI.PC <- lm(PLCI~predic.pca$x[,1], data=predictors) ## ALL Env. together !! 
summary(M.PLCI.PC)
M.NOVI.PC <- lm(NOVI~predic.pca$x[,1], data=predictors) 
summary(M.NOVI.PC)
# 2-2 Partitioned to three groups 
# 2-2-1 PLCI 
M.PLCI.soil <- lm(PLCI~predic.soil.pca$x[,1], data=predictors) ## SOIL FACTORS
summary(M.PLCI.soil)
plot(PLCI~predic.soil.pca$x[,1],xlab="Soil condition index", ylab="Redback Salamander Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(PLCI~predic.soil.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

M.PLCI.no <- lm(PLCI~predic.no.pca$x[,1], data=predictors) ## NATURAL OBJECTS  
summary(M.PLCI.no) 
plot(PLCI~predic.no.pca$x[,1],xlab="Natural cover objects index", ylab="Redback Salamander Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(PLCI~predic.no.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

M.PLCI.veg <- lm(PLCI~predic.veg.pca$x[,1], data=predictors) ## VEGETATION 
summary(M.PLCI.veg)
plot(PLCI~predic.veg.pca$x[,1],xlab="Understory Vegetaion condition index", ylab="Redback Salamander Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(PLCI~predic.veg.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

# 2-2-1 NOVI 
M.NOVI.soil <- lm(NOVI~predic.soil.pca$x[,1], data=predictors) ## SOIL FACTORS
summary(M.NOVI.soil)
plot(NOVI~predic.soil.pca$x[,1],xlab="Soil condition index", ylab="Red spotted newt Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(NOVI~predic.soil.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

M.NOVI.no <- lm(NOVI~predic.no.pca$x[,1], data=predictors) ## NATURAL OBJECTS  
summary(M.NOVI.no) 
plot(NOVI~predic.no.pca$x[,1],xlab="Natural cover objects index", ylab="Red spotted newt Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(NOVI~predic.no.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

M.NOVI.veg <- lm(NOVI~predic.veg.pca$x[,1], data=predictors) ## VEGETATION 
summary(M.NOVI.veg)
plot(NOVI~predic.veg.pca$x[,1],xlab="Understory Vegetaion conditions index", ylab="Red spotted newt Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(NOVI~predic.veg.pca$x[,1],data=predictors))
legend('top',legend='P-Value > 0.05', bty='n')

###########
summary(aov(PLCI~soilmoisture+soiltemp+soilpH+soilBulkDensity+DWD.volume.m2+
              LL.Depth+seedlingdensity+understoryPrcntCover,data=predictors))
summary(aov(NOVI~soilmoisture+soiltemp+soilpH+soilBulkDensity+DWD.volume.m2+
              LL.Depth+seedlingdensity+understoryPrcntCover,data=predictors))

## lm function MULTIPLE REGRESSION ### 
###################################### 
M.PLCI <- lm(PLCI~soilmoisture+soiltemp+soilpH+soilBulkDensity+DWD.volume.m2+
           LL.Depth+seedlingdensity+understoryPrcntCover,data=predictors)
summary(M.PLCI) 

M.NOVI <- lm(NOVI~soilmoisture+soiltemp+soilpH+soilBulkDensity+DWD.volume.m2+
               LL.Depth+seedlingdensity+understoryPrcntCover,data=predictors)
summary(M.NOVI)

###PLOTS Treatments predict temperature
summary(aov((predictors$temp~predictors$trt))) 
plot(stand.predic$soiltemp~predictors$trt)
plot(predictors$NOVI~predictors$trt)
plot(predictors$PLCI~predictors$trt)
plot(NOVI~soiltemp,data=predictors,pch=15,cex=1.00)
abline(lm(NOVI~soiltemp,data=predictors))
legend('bottomleft',legend='P-Value = 0.001',bty='n')

plot(PLCI~predic.no.pca$x[,1],xlab="Natural cover objects", ylab="Redback Salamander Abundance", data=predictors,pch=19,cex=1.00)
abline(lm(PLCI~predic.no.pca$x[,1],data=predictors))
legend('bottomleft',legend='P-Value > 0.05', bty='n') 

plot(NOVI~predic.pca$x[,1],data=predictors,pch=19,cex=1.00)
abline(lm(NOVI~predic.pca$x[,1],data=predictors))
legend('bottomleft',legend='P-Value > 0.05','R = -0.56',bty='n')


########## MODEL 2 ############################ 
### HeRE Treatments directly affects Amphibians of Harvard Forest 
adonis(predictors[,12:13]~predictors$trt)
summary(aov((predictors$NOVI~predictors$trt)))
summary(aov((predictors$PLCI~predictors$trt)))
plot(aov(predictors$PLCI~predictors$trt)) 

## export the PCA results 
write.csv(predic.soil.pca$x,file='F:/data/ordination/pcaSOIL.csv',row.names=FALSE)





####################################################
### perMANOVA  ##################################### 

###1- Data standardizations
predic.std <- apply(predic,2,function(x) (x-mean(x))/sd(x))
predic.rel <- apply(predic,2,function(x) x/max(x))
plot(predic.rel~predic.std)
###Dissimilarity matrices
## data.dist(birds[,-c(1:3)],method='euclidean')
## data.dist(birds[,-c(1:3)],method='bray')

### 2- assess the effects of HeRE treatments on the salamander habitat (trt vs. environment)
adonis(predic ~ trt, data=predictors,dist='euclidean') ## Non paramatric MANOVA ##
summary(manova(predic.std ~ trt,data=predictors)) ## Non paramatric MANOVA ##
summary(lm(predic.std~trt,data=predictors))
adonis(I(predic.std[,-6]) ~ trt,data=predictors, dist='euclidean') ## Non paramatric MANOVA ##

adonis(I(predic.std[,-2]) ~ trt,data=predictors, dist='euclidean') ## Non paramatric MANOVA ##


### 3- ANOVA of salamanders vs. trt
aov(NOVI ~ trt, data=predictors) 
summary(aov(NOVI ~ trt, data=predictors))
?aov
### 4- salamanders vs. Environment 
lm(PLCI ~ predictors$soilpH, data=predictors)
summary(lm(PLCI ~ predictors$soilpH, data=predictors))
?lm

###############################################################
###### ANCOVA #############################################3###

environment <- read.csv('environment.csv', header = T)
environment

Salamanders ~ HF-HeRE Treatments * (PC1+PC2+PC3) 

summary(aov((predictors$NOVI~predictors$trt*(environment$veg+environment$no+environment$soil))))

lm(predictors$NOVI~predictors$trt*(predic.veg.pca$x[,1]+predic.no.pca$x[,1]+predic.soil.pca$x[,1]))
summary(lm(predictors$NOVI~predictors$trt*(predic.veg.pca$x[,1]+predic.no.pca$x[,1]+predic.soil.pca$x[,1]))

lm(PLCI ~ environment$trt*(environment$soil+environment$no+environment$veg), data=environment)        
        
        
summary(aov((predictors$PLCI~predictors$trt)))
plot(aov(predictors$PLCI~predictors$trt)) 




