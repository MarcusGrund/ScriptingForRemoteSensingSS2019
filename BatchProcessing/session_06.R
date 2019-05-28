## Spectral separability and minimum distance classification

## load required R packages
library (raster)
library (rgdal)

## set working directory
setwd("./BatchProcessing/assignment_06")

## load ndvi stack file
ndvistack <- stack("ndvistack.tif")

## load shapefile
cal <- shapefile("calibration_polygons_multi.shp")

## attribute table
View(cal@data) 

## plot ndvi stack image
plotRGB(ndvistack, 3, 2, 1, stretch="lin")
plot(cal, add=T, col="green")

## extract pixel values as list
calpix <- extract(ndvistack, cal, df=FALSE)

## label individual tables
names(calpix) <- cal@data[,1]

## extract pixel values as data frame
calpix2 <- extract(ndvistack, cal, df=TRUE) 

## Testing the spectral separability

## color values for each polygon corresponding to its class
cropcol <- c(rep("#FF00FF", 3), "#DE8787", "#C8C800", rep("#00FF00", 3),"#008000", 
  rep("#784421", 5), rep("#008C8C", 2), "#FFFF00",
  rep ("#AA0000", 3), "#DEAA87","#FF00FF")

## create a color value for each pixel
cl <- rep(cropcol, sapply(calpix, nrow))

## Minimum distance classifier
classcentroids <- t(sapply(calpix, colMeans)) ## calculate the mean value per column and list element

for (i in 1:5){
  for (j in 1:5){
    plot(calpix2[,i+1], calpix2[,j+1], col=cl, pch=19, cex=0.1,
          xlab=paste("NDVI",i), ylab=paste("NDVI",j)) ## the '+1' is necessary to skip the first column with the class codes
    points(classcentroids[,i], classcentroids[,j], bg=cropcol, pch=21)
    readline ("Press ENTER for next plot")
  }
}


## Which NDVI combination allows the best separation of the different crop types?
## NDVI 1 (day 109) and NDVI 4 (day 205) or NDVI 3 (day 173) and NDVI5 (day 237).

## load extra script file
source('mindistclassifier.r')

map <- mindistclassifier(class.codes=1:22, cal.ref=classcentroids, image.stack=ndvistack)
plot(map)

map@legend@colortable <- c("#000000", cropcol, rep ("#000000", 233))
plot(map)
legend("topleft", legend=cal@data[,1], col=cropcol, cex=0.7, pch=19, bg="white")
writeRaster(map, "mindist_map.tif", format="GTiff", overwrite=TRUE)



## load validation shape file
val <- shapefile("validation_points.shp")

## merge class names to crop types
croptypes <- unlist(strsplit(cal@data[,1], "_"))[seq (1, 44, 2)]

## extract the predicted class codes, results in a table
prediction <- extract(map, val) 

## convert the class codes to crop types
prediction <- croptypes[prediction] 

## confusion matrix
cfm <- table(prediction, val@data[,1])
cfm

## overall accuracy
oac <- sum(diag (cfm)) / sum (cfm)
oac

## user's accuracy
users <- diag(cfm) / apply (cfm, 1, sum)
users

## producer's accuracy
producers <- diag(cfm) / apply (cfm, 2, sum)
producers

## Which percentage of the validation points has been classified correctly?
## 79.4%

## Which class has the lowest user's accuracy? Why?
## Beans (just 29.4%) are frequently confused with rice and tomatoes.

## Which class has the lowest producer's accuracy? Why?
## Tomatoes (46.3 %), these points were often classified as sunflowers and beans...

setwd("../..")

