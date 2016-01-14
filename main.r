#Team Rugrats
#Arno Timmer and Jan Pokorn
#Lesson 8
#Create a model to predict VCF values for gewata area
library(raster)
library(sp)
library(rgdal)
library(randomForest)
library(rasterVis)
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB1.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB2.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB3.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB4.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB5.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/GewataB7.rda")
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/vcfGewata.rda")

# Plot VFC and exclude values that are too big
plot(vcfGewata)
vcfGewata[vcfGewata > 100] <- NA
## build a brick containing all data and name them
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
## extract all data to a data.frame
df <- as.data.frame(getValues(alldata))
#check the correlation
#The biggest correlation is between  VCF and Landsat band 7, 5 and 3
pairs(alldata)
# Create a model
linReg <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = df)
#Create brick of layers without vcf 
layMINvcf <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
names(layMINvcf) <- c("band1", "band2", "band3", "band4", "band5", "band7")
#remove unusual values from predLM and layer vcfGewata
predLM <- predict(layMINvcf,model=linReg,na.rm=TRUE)
#remove unusual values from predLM and layer vcfGewata
predLM[predLM < 0] <- NA
#compute RMS
RMSE <- sqrt(cellStats((vcfGewata-predLM)^2, stat='mean'))

#plot the difference between VCF(tree cover band) and predicted tree cover with data out of other LANDSAT bands
plot(vcfGewata)
plot(predLM)
#are the differences between the predicted and actual tree cover the same for all 
#of the 3 classes we used for the random forest classfication? Using the training polygons from the random 
#forest classification, calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().

#load training polygons
load("/home/user/git/Geoscripting/AdvancedRasterAnalysis/data/trainingPoly.rda")

trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data

classes <- rasterize(trainingPoly, vcfGewata, field='Code')

difference <-zonal(vcfGewata, classes, fun='mean')
difference2 <-zonal(predLM, classes, fun='mean')

#Calculate RMSE for each class.
RMSEcrop <- sqrt(mean((difference[1,2]-difference2[1,2])^2))
RMSEfor <- sqrt(mean((difference[2,2]-difference2[2,2])^2))
RMSEwet <- sqrt(mean((difference[3,2]-difference2[3,2])^2))
#Visualize the results
rbind(RMSEcrop,RMSEfor,RMSEwet)
















