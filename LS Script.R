#Unmarked repeated count data analysis (pcount)

#Set your working directory first (Session > Set Working Directory > Choose Directory)
setwd("~/Desktop/Lillian")

install.packages(c('readxl', 'unmarked', 'MuMIn'))
library('readxl')
library('unmarked')
library('MuMIn')


#Import Covariate Data
covdata <- read_excel("LS Data.xlsx", sheet = "covdata")
obscov <- as.vector(covdata[,2])      #Adjust to your camera days data
sitecov <- as.vector(covdata[,3:6])   #Adjust to your covariate data


#Import Abundance Data and create unmarkedFrame
abdata <- read_excel("LS Data.xlsx", sheet = "abdata")
boar.juv <- as.vector(abdata[,2])       #column for boar juveniles
boar.juv.UMF <- unmarkedFramePCount(y=boar.juv, siteCovs=sitecov, obsCovs=obscov) #make frame for boar juveniles
summary(boar.juv.UMF)

boar.adult <- as.vector(abdata[,3])       #column for species x age group
boar.adult.UMF <- unmarkedFramePCount(y=boar.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(boar.adult.UMF)

horse.juv <- as.vector(abdata[,4])       #column for species x age group
horse.juv.UMF <- unmarkedFramePCount(y=horse.juv, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(horse.juv.UMF)

horse.adult <- as.vector(abdata[,5])       #column for species x age group
horse.adult.UMF <- unmarkedFramePCount(y=horse.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(horse.adult.UMF)

moose.juv <- as.vector(abdata[,6])       #column for species x age group
moose.juv.UMF <- unmarkedFramePCount(y=moose.juv, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(moose.juv.UMF)

moose.adult <- as.vector(abdata[,7])       #column for species x age group
moose.adult.UMF <- unmarkedFramePCount(y=moose.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(moose.adult.UMF)

red.deer.juv <- as.vector(abdata[,8])       #column for species x age group
red.deer.juv.UMF <- unmarkedFramePCount(y=red.deer.juv, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(red.deer.juv.UMF)

red.deer.adult <- as.vector(abdata[,9])       #column for species x age group
red.deer.adult.UMF <- unmarkedFramePCount(y=red.deer.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(red.deer.adult.UMF)

roe.deer.juv <- as.vector(abdata[,10])       #column for species x age group
roe.deer.juv.UMF <- unmarkedFramePCount(y=roe.deer.juv, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(roe.deer.juv.UMF)

roe.deer.adult <- as.vector(abdata[,11])       #column for species x age group
roe.deer.adult.UMF <- unmarkedFramePCount(y=roe.deer.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(roe.deer.adult.UMF)

wolf.adult <- as.vector(abdata[,12])       #column for species x age group
wolf.adult.UMF <- unmarkedFramePCount(y=wolf.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(wolf.adult.UMF)

badger.adult <- as.vector(abdata[,13])       #column for species x age group
badger.adult.UMF <- unmarkedFramePCount(y=badger.adult, siteCovs=sitecov, obsCovs=obscov) #make frame for species x age group
summary(badger.adult.UMF)
#... ...


#### Testing distributions ####
zip1 <- pcount(~ camdays ~ rad, data=badger.adult.UMF, K=116, mixture = "ZIP")   # k = 1 + max observations
zip2 <- pcount(~ camdays ~ ndvi, data=badger.adult.UMF, K=116, mixture = "ZIP")
zip3 <- pcount(~ camdays ~ npp, data=badger.adult.UMF, K=116, mixture = "ZIP") 
zip4 <- pcount(~ camdays ~ water, data=badger.adult.UMF, K=116, mixture = "ZIP") 
zip5 <- pcount(~ camdays ~ elev, data=badger.adult.UMF, K=116, mixture = "ZIP") 

nb1 <- pcount(~ camdays ~ rad, data=badger.adult.UMF, K=116, mixture = "NB") 
nb2 <- pcount(~ camdays ~ ndvi, data=badger.adult.UMF, K=116, mixture = "NB")
nb3 <- pcount(~ camdays ~ npp, data=badger.adult.UMF, K=116, mixture = "NB") 
nb4 <- pcount(~ camdays ~ water, data=badger.adult.UMF, K=116, mixture = "NB") 
nb4 <- pcount(~ camdays ~ elev, data=badger.adult.UMF, K=116, mixture = "NB") 

fltest <- fitList(zip1, zip2, zip3, zip4, zip5, 
                  nb1, nb2, nb3, nb4, nb5)
testset<-modSel(fltest)
AICtestset<-data.frame(testset@Full[["model"]],testset@Full[["AIC"]])
AICtestset  #NB is best fit for horse juvenile



#### Boar Juvenile ####
boar.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                    rad*ndvi + rad*npp + rad*water + 
                    ndvi*npp + ndvi*water +
                    npp*water,
                  data=boar.juv.UMF, K=671, mixture = "NB") 
boar.juv.dredge <- dredge(boar.juv.global, fixed="p(camdays)")
write.csv(boar.juv.dredge, file="boar.juvenile.dredgedmodels.csv")


#### Boar Adult ####
boar.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                               rad*ndvi + rad*npp + rad*water + 
                               ndvi*npp + ndvi*water +
                               npp*water,
                           data=boar.adult.UMF, K=422, mixture = "NB") 
boar.adult.dredge <- dredge(boar.adult.global, fixed="p(camdays)")
write.csv(boar.adult.dredge, file="boar.adult.dredgedmodels.csv")


#### Horse Juvenile ####
horse.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                              rad*ndvi + rad*npp + rad*water + 
                              ndvi*npp + ndvi*water +
                              npp*water,
                             data=horse.juv.UMF, K=45, mixture = "NB") 
horse.juv.dredge <- dredge(horse.juv.global, fixed="p(camdays)")
write.csv(horse.juv.dredge, file="horse.juv.dredgedmodels.csv")


#### Horse Adult ####
horse.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                rad*ndvi + rad*npp + rad*water + 
                                ndvi*npp + ndvi*water +
                                npp*water,
                            data=horse.adult.UMF, K=345, mixture = "NB") 
horse.adult.dredge <- dredge(horse.adult.global, fixed="p(camdays)")
write.csv(horse.adult.dredge, file="horse.adult.dredgedmodels.csv")


#### Moose Juvenile ####
moose.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                              rad*ndvi + rad*npp + rad*water + 
                              ndvi*npp + ndvi*water +
                              npp*water,
                            data=moose.juv.UMF, K=21, mixture = "NB") 
moose.juv.dredge <- dredge(moose.juv.global, fixed="p(camdays)")
write.csv(moose.juv.dredge, file="moose.juv.dredgedmodels.csv")


#### Moose Adult ####
moose.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                rad*ndvi + rad*npp + rad*water + 
                                ndvi*npp + ndvi*water +
                                npp*water,
                              data=moose.adult.UMF, K=115, mixture = "NB") 
moose.adult.dredge <- dredge(moose.adult.global, fixed="p(camdays)")
write.csv(moose.adult.dredge, file="moose.adult.dredgedmodels.csv")


#### Red Deer Juvenile ####
red.deer.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                 rad*ndvi + rad*npp + rad*water + 
                                 ndvi*npp + ndvi*water +
                                 npp*water,
                            data=red.deer.juv.UMF, K=150, mixture = "NB") 
red.deer.juv.dredge <- dredge(red.deer.juv.global, fixed="p(camdays)")
write.csv(red.deer.juv.dredge, file="red.deer.juv.dredgedmodels.csv")


#### Red Deer Adult ####
red.deer.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                   rad*ndvi + rad*npp + rad*water + 
                                   ndvi*npp + ndvi*water +
                                   npp*water,
                               data=red.deer.adult.UMF, K=741, mixture = "NB") 
red.deer.adult.dredge <- dredge(red.deer.adult.global, fixed="p(camdays)")
write.csv(red.deer.adult.dredge, file="red.deer.adult.dredgedmodels.csv")


#### Roe Deer Juvenile ####
roe.deer.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                 rad*ndvi + rad*npp + rad*water + 
                                 ndvi*npp + ndvi*water +
                                 npp*water,
                               data=roe.deer.juv.UMF, K=24, mixture = "NB") 
roe.deer.juv.dredge <- dredge(roe.deer.juv.global, fixed="p(camdays)")
write.csv(roe.deer.juv.dredge, file="roe.deer.juv.dredgedmodels.csv")


#### Roe Deer Adult ####
roe.deer.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                   rad*ndvi + rad*npp + rad*water + 
                                   ndvi*npp + ndvi*water +
                                   npp*water,
                                 data=roe.deer.adult.UMF, K=267, mixture = "NB") 
roe.deer.adult.dredge <- dredge(roe.deer.adult.global, fixed="p(camdays)")
write.csv(roe.deer.adult.dredge, file="roe.deer.adult.dredgedmodels.csv")


#### Wolf Adult ####
wolf.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                               rad*ndvi + rad*npp + rad*water + 
                               ndvi*npp + ndvi*water +
                               npp*water,
                                data=wolf.adult.UMF, K=84, mixture = "NB") 
wolf.adult.dredge <- dredge(wolf.adult.global, fixed="p(camdays)")
write.csv(wolf.adult.dredge, file="wolf.adult.dredgedmodels.csv")


#### Badger Adult ####
badger.adult.global <-  pcount(~ camdays ~ rad + ndvi + npp + water +
                                 rad*ndvi + rad*npp + rad*water + 
                                 ndvi*npp + ndvi*water +
                                 npp*water,
                                data=badger.adult.UMF, K=116, mixture = "NB") 
badger.adult.dredge <- dredge(badger.adult.global, fixed="p(camdays)")
write.csv(badger.adult.dredge, file="badger.adult.dredgedmodels.csv")









##### Alternative Analysis #####


#### Boar Juvenile ####
boar.juv.global <-  pcount(~ camdays ~ rad + ndvi + npp 
                             rad*ndvi + rad*npp + rad*water + 
                             ndvi*npp + ndvi*water +
                             npp*water,
                           data=boar.juv.UMF, K=671, mixture = "NB") 
boar.juv.dredge <- dredge(boar.juv.global, fixed="p(camdays)")
write.csv(boar.juv.dredge, file="boar.juvenile.dredgedmodels.csv")





