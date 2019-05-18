#install.packages("raster")
#install.packages("rgdal")
library (raster)
library (rgdal)

setwd("~/Uni/ScriptingForRemoteSensingSS2019")
setwd("./PreProcessing")
getwd()


# 2011: Landsat 5 TM ---------------------------------------------------------------

setwd("./LS5_20110909")
blue2011 <- raster("LT50450322011252PAC01_B1.TIF")
green2011 <- raster("LT50450322011252PAC01_B2.TIF")
red2011 <- raster("LT50450322011252PAC01_B3.TIF")
nir2011 <- raster("LT50450322011252PAC01_B4.TIF")
swira2011 <- raster("LT50450322011252PAC01_B5.TIF")
swirb2011 <- raster("LT50450322011252PAC01_B7.TIF")
the2011 <- raster("LT50450322011252PAC01_B6.TIF")

ls52011 <- stack(blue2011,green2011,red2011,nir2011,swira2011,swirb2011)

plot(nir2011) ## plots the NIR band with the default inverted terrain.colors() gradient
plot(nir2011, col=gray.colors(255, 0, 1, 1)) ## plots the NIR band as black-and-white image
plotRGB(ls52011, 4, 3, 2, stretch="lin") ## plots a RGB composite of bands 4 (R), 3 (G) and 2 (B) with linear contrast stretch
dev.off()

#plot(ls52011)
#?plot
#?gray.colors

pdf("../Plots/nir2011_my.gradient.pdf")
my.gradient <- colorRampPalette(c("blue","yellow","red"))
plot(nir2011, col=my.gradient(100))
dev.off()
#?colorRampPalette
#colors()

# 2014: Landsat 8 OLI & TIRS ----------------------------------------------------

setwd("../LS8_20140901")
dir()

bands <- dir(pattern="LC80450322014244LGN00")
#bands
bands <- bands[4:9]
ls82014 <- stack (bands)
plotRGB(ls82014, 4, 3, 2, stretch="lin")
dev.off()

the1.2014 <- raster("LC80450322014244LGN00_B10.TIF")
the2.2014 <- raster("LC80450322014244LGN00_B11.TIF")

res(ls82014)
projection (ls82014,asText = FALSE)
dim(ls82014)
nlayers(ls82014)
extent(ls82014)
ncell(ls82014)

# Summary -----------------------------------------------------------------

setwd("..")
getwd()

pdf("Plots/nir2011.pdf")
plot(nir2011)
abline(v=510000)
abline(v=580000)
abline(h=4490000)
abline(h=4543000)
dev.off()

ls82014.subs <- crop(ls82014, extent(510000, 580000, 4490000, 4543000))
ls52011.subs <- crop(ls52011, extent(510000, 580000, 4490000, 4543000))
ls5the.subs <- crop(the2011, extent(510000, 580000, 4490000, 4543000))
ls8the1.subs <- crop(the1.2014, extent(510000, 580000, 4490000, 4543000))
ls8the2.subs <- crop(the2.2014, extent(510000, 580000, 4490000, 4543000))

dim(ls82014.subs)
dim(ls52011.subs)
res(ls82014.subs)[1]

projection(ls82014.subs)

image_area <- res(ls82014)[1]*res(ls82014)[2]*dim(ls82014.subs)[1]*dim(ls82014.subs)[2]/1e6
image_area

#matrix (c (-Inf, -1, NA), 1, 3)

ls82014.subs <- reclassify(ls82014.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))
ls52011.subs <- reclassify(ls52011.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))

#?reclassify

pdf("Plots/RGBPlots.pdf")
#layout(matrix(c(1,2),ncol=2,byrow=TRUE))
##par(mar=c(0,0,5,0))
#plot.new()
#title("Trinit and Shasta Lake, California",cex.main=1.5)
#title("Landsat 5 TM: September 9th, 2011",cex.main=1.5)
plotRGB(ls52011.subs, 4, 3, 2, stretch="lin")
#title("Landsat 8 OLI & TIRS: September 1th, 2014",cex.main=1.5)
plotRGB(ls82014.subs, 4, 3, 2, stretch="lin")
dev.off()

#save.image("PreProcessing.RData")
#load("PreProcessing.RData")



