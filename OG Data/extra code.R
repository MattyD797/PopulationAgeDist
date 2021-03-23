
install.packages('MuMIn')
library('MuMIn')



global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                  rad*ndvi + ndvi*water,
                  data=UMF, K=3591, mixture = "NB") 

dredge(global)


dredge <- dredge(global, fixed="p(camdays)")
dredge

AvgDelta4.2<-model.avg(dredge, subset = delta <4)
AvgDelta4.2
summary(AvgDelta4.2)
#95CI for conditional likelyhood
confint(AvgDelta4.2)

Avg95Conset2<-model.avg(dredge,subset=cumsum(weight)<=0.95)
Avg95Conset2
summary(Avg95Conset2)
#95CI for conditional likelyhood
confint(Avg95Conset2)

