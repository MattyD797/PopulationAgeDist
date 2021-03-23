
install.packages("ggpubr")
library("ggpubr")



ggscatter(covdata, x = "rad", y = "ndvi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Radiation", ylab = "NDVI")
cor.test(covdata$rad, covdata$ndvi, method = "spearman")



ggscatter(covdata, x = "rad", y = "npp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Radiation", ylab = "Nuclear Power Plant")
cor.test(covdata$rad, covdata$npp, method = "spearman")




ggscatter(covdata, x = "rad", y = "water", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Radiation", ylab = "Water")
cor.test(covdata$rad, covdata$water, method = "spearman")





ggscatter(covdata, x = "ndvi", y = "npp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NDVI", ylab = "NPP")
cor.test(covdata$rad, covdata$ndvi, method = "spearman")



ggscatter(covdata, x = "ndvi", y = "water", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NDVI", ylab = "Water")
cor.test(covdata$rad, covdata$npp, method = "spearman")




ggscatter(covdata, x = "npp", y = "water", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "NPP", ylab = "Water")
cor.test(covdata$rad, covdata$water, method = "spearman")



