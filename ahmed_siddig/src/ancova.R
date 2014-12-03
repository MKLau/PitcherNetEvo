###Ancova with Ahmed

trt <- gl(4,4)
novi <- rnorm(length(trt),10,2)
soil <- rnorm(length(trt),1,2)
no <- rnorm(length(trt),5,2)
veg <- rnorm(length(trt),20,2)
pairs(data.frame(novi,trt,soil,no,veg))
anova(aov(novi~trt*soil))
anova(aov(novi~trt*no))
anova(aov(novi~trt*veg))

