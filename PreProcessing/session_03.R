#install.packages("raster")
#install.packages("rgdal")
library (raster)
library (rgdal)

setwd("~/Uni/ScriptingForRemoteSensingSS2019")
setwd("./PreProcessing")
getwd()


# Detection of clouds -----------------------------------------------------

#Landsat 5
clouds2011 <- (blue2011rad / the2011rad)
plot(clouds2011) ## All pixels in the cloud have values > 13
#identify threshold
clouds2011 <- clouds2011 > 13 
plot(clouds2011)
#Reclassify binary Raster layer and apply the mask
clouds2011 <- reclassify(clouds2011, matrix (c(0, 1, 1, NA), 2, 2, byrow=T))
ls52011dps.topo <- ls52011dps.topo * clouds2011
plotRGB(ls52011dps.topo, 4, 3, 2, stretch="lin", colNA="black")

#Landsat 8
blue2014rad <- ls82014.subs[[1]] * 1.2626E-02 - 63.12837
clouds2014 <- blue2014rad / (thea2014rad + theb2014rad)
plot(clouds2014)
#same threshold = 13
clouds2014 <- reclassify(clouds2014, matrix (c(0, 1, 1, NA), 2, 2, byrow=T))
ls82014dps.topo <- ls82014dps.topo * clouds2014
plotRGB(ls82014dps.topo, 4, 3, 2, stretch="lin", colNA="black")
#-> no clouds visible


# Write to image ----------------------------------------------------------

#GeoTiff format
setwd("..")
writeRaster(ls52011dps.topo, "Plots/ls52011dps_topo.tif", format="GTiff")
writeRaster(ls82014dps.topo, "Plots/ls82014dps_topo.tif", format="GTiff")


# Identifying water bodies ------------------------------------------------

#Question: How has the surface of Lake Shasta and Lake Trinity changed from 2011 to 2014?
#normalized difference water index (NDWI)
ndwi2014 <- (ls82014dps.topo[[2]] - ls82014dps.topo[[5]]) /(ls82014dps.topo[[2]] + ls82014dps.topo[[5]])
ndwi2011 <- (ls52011dps.topo[[2]] - ls52011dps.topo[[5]]) /(ls52011dps.topo[[2]] + ls52011dps.topo[[5]])
plot(ndwi2011)
plot(ndwi2014)
change <- ndwi2011 - ndwi2014 
plot(change)

plot(density(change)) ## the inflexion point is near 0.3 
loss <- change > 0.3
plot(loss)



# Calculation of total area of the surface decrease -----------------------

lossval <- getValues(loss) ## writes the pixel values to a table
table(lossval) ## determines the number of pixels per value
area <- table(lossval)[2] * 30 * 30 / 10000 ## change area in hectares


# Handling vector data ----------------------------------------------------

adm <- getData("GADM", country="USA", level=2) ## level indicates the hierarchical level of adminstrative units
plot(adm)
View(adm@data)
#Which row numbers have Shasta county and Trinity county?
#Shasta ROW_NUMBER_OF_SHASTA_COUNTY=228, Trinity ROW_NUMBER_OF_TRINITY_COUNTY=236
shasta.county <- adm[228,] 
trinity.county <- adm[236,]

crs(shasta.county) ## Geographic coordinates, WGS 84
crs(loss) ## UTM Zone 10, WGS 84
shasta.county.utm <- spTransform (shasta.county, crs(loss))
trinity.county.utm <- spTransform (trinity.county, crs(loss)) 
crs(shasta.county.utm)

shasta.loss <- mask(loss, shasta.county.utm)
plot(shasta.loss)
trinity.loss <- mask(loss, trinity.county.utm)
plot(trinity.loss)

#loss area Shasta
lossval_shasta <- getValues(shasta.loss) ## writes the pixel values to a table
table(lossval_shasta) ## determines the number of pixels per value
area <- table(lossval_shasta)[2] * 30 * 30 / 10000 ## change area in hectares
#4790.97

#loss area Trinity
lossval_trinity <- getValues(trinity.loss) ## writes the pixel values to a table
table(lossval_trinity) ## determines the number of pixels per value
area <- table(lossval_trinity)[2] * 30 * 30 / 10000 ## change area in hectares
#3280.68


# Visualization -----------------------------------------------------------

loss <- reclassify(loss, matrix (c(0, NA), 1, 2)) ## make non-change pixels transparent
pdf("Plots/map.pdf", 5, 4, useDingbats=F) ## open a *.pdf
par(pin=c(4.01, 3), cex=0.7) ## set the size of the panel
plot(loss, legend=F) ## plot the loss layer with coordinate axes
plotRGB(ls82014dps.topo, 4, 3, 2, stretch="lin", add=T) ## plot the background 
plot(loss, col='#FFFF00', add=T, legend=F) ## plot the loss area
legend("bottomright", pch=22, pt.bg="yellow", "Decrease") ## add a legend 
scalebar(20000, xy=c (530000, 4495000), type="bar", label=c(0, 10, 20), below="km") ## add a scalebar
dev.off() ## close the *.pdf

