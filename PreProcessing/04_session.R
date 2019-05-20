############################ Homework of Marcus Grund ############################
## This script is to preprocess Landsat 8 imagery and to detect a fire extent
## using a change detection approach based on a spectral index. The index was
## chosen for the following reasons: TODO
################################################################################

## load required R packages
library (raster)
library (rgdal)

## JULY image -----------------------------------------------------------------
## set according working directory for July images
## base directoy is where the script is located
setwd("./landsat/LC08_L1TP_045033_20180710_20180717_01_T1")

## load the needed bands of the July image acqired prior to the wildfire
## visible bands and nir, swir bands
bands <- dir(pattern="LC08_L1TP_045033_20180710_20180717_01_T1_B")
bands <- bands[4:9]

## thermal bands
the1Jul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B10.TIF')
the2Jul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B11.TIF')

## stack the bands of the July image to a multiband image
ls8Jul <- stack(bands)

## set working directory back to base directory
setwd("../../")

## clip the image and the needed bands to fire complex extent
ls8Jul.subs <- crop(ls8Jul, extent(470000, 550000, 4300000, 4380000))
the1Jul.subs <- crop(the1Jul, extent(470000, 550000, 4300000, 4380000))
the2Jul.subs <- crop(the2Jul, extent(470000, 550000, 4300000, 4380000))

## elimination of bad pixels
ls8Jul.subs <- reclassify(ls8Jul.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))
the1Jul.subs <- reclassify(the1Jul.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))
the2Jul.subs <- reclassify(the2Jul.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))

## Metadata-File: LC08_L1TP_045033_20180710_20180717_01_T1_MTL.txt
## SUN_ELEVATION = 64.96477774
## SUN_AZIMUTH = 125.62033089
## REFLECTANCE_MULT_BAND_X = 2.0000E-05
## REFLECTANCE_ADD_BAND_X = -0.100000
## RADIANCE_MULT_BAND_2 = 1.2440E-02
## RADIANCE_MULT_BAND_10 = 3.3420E-04
## RADIANCE_MULT_BAND_11 = 3.3420E-04
## RADIANCE_ADD_BAND_2 = -62.19756
## RADIANCE_ADD_BAND_10 = 0.10000
## RADIANCE_ADD_BAND_11 = 0.10000

## conversion of the raw DN values to TOA reflectance
ls8Jultoa <- (ls8Jul.subs * 2.0000E05 - 0.1) / sin (64.96477774 / 180 * pi)

## Dark pixel substraction
mnJul <- minValue(ls8Jultoa)
ls8Juldps <- ls8Jultoa - mnJul

## read digital elevation model image
dem <- raster("./srtm/srtm_merge_n38_39_w123_124_1arc_v2_utm10.tif")

## resample and clip the DEM to the image
dem <- resample(dem, ls8Jultoa) 

## get lope and aspect of the surface.
slp <- terrain(dem, opt="slope")
asp <- terrain(dem, opt="aspect")

## calculate the sun incidence angle for the time of image acquisition
hsJul <- hillShade(slp, asp, angle=64.96477774, direction=125.62033089)

## reclassify the hill shade and set values smaller than 0.1 to 0.1
hsJul <- reclassify(hsJul, matrix (c (-Inf, 0.1, 0.1), 1, 3))

## Correction of topographic effects
ls8Juldps.topo <- ls8Juldps * cos((90 - 64.96477774) / 180 * pi) / hsJul

## Cloud detection preprocessing
## The thermal bands can only be converted to radiances
the1Julrad <- the1Jul.subs * 3.3420E-04 + 0.10000
the2Julrad <- the2Jul.subs * 3.3420E-04 + 0.10000
blueJulrad <- ls8Jul.subs[[1]] * 1.2440E-02 - 62.19756

## Cloud detection
cloudsJul <- blueJulrad / (the1Julrad + the2Julrad)
cloudsJul <- cloudsJul > 13 #threshold = 13
plot(cloudsJul)
## -> minimal clouds visible
cloudsJul <- reclassify(cloudsJul, matrix (c(0, 1, 1, NA), 2, 2, byrow=T))
ls8Juldps.topo <- ls8Juldps.topo * cloudsJul

## plots a RGB composite of bands 4 (R), 3 (G) and 2 (B) with linear contrast stretch
plotRGB (ls8Juldps.topo, 4, 3, 2, stretch="lin", main="Juli")


## October image ---------------------------------------------------------------
## set according working directory for October images
setwd("./landsat/LC08_L1TP_045033_20181030_20181115_01_T1")

## load the needed bands of the October image acqired after to the wildfire
## visible bands and nir, swir bands
bands <- dir(pattern="LC08_L1TP_045033_20181030_20181115_01_T1_B")
bands <- bands[4:9]

## thermal bands
the1Oct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B10.TIF')
the2Oct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B11.TIF')

## stack the bands of the October image to a multiband image
ls8Oct <- stack(bands)

## set working directory back to base directory
setwd("../../")

## clip the October image and the thermal bands to fire complex extent
ls8Oct.subs <- crop(ls8Oct, extent(470000, 550000, 4300000, 4380000))
the1Oct.subs <- crop(the1Oct, extent(470000, 550000, 4300000, 4380000))
the2Oct.subs <- crop(the2Oct, extent(470000, 550000, 4300000, 4380000))

## elimination of bad pixels
ls8Oct.subs <- reclassify (ls8Oct.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))
the1Oct.subs <- reclassify(the1Oct.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))
the2Oct.subs <- reclassify(the2Oct.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))

## Metadata-File: LC08_L1TP_045033_20181030_20181115_01_T1_MTL.txt
## SUN_AZIMUTH = 160.69746486
## SUN_ELEVATION = 35.02522945
## REFLECTANCE_MULT_BAND_X = 2.0000E-05
## REFLECTANCE_ADD_BAND_X = -0.100000
## RADIANCE_MULT_BAND_2 = 1.3040E-02
## RADIANCE_MULT_BAND_10 = 3.3420E-04
## RADIANCE_MULT_BAND_11 = 3.3420E-04
## RADIANCE_ADD_BAND_2 = -65.20248
## RADIANCE_ADD_BAND_10 = 0.10000
## RADIANCE_ADD_BAND_11 = 0.10000

## conversion of the raw DN values to TOA reflectance
ls8Octtoa <- (ls8Oct.subs * 2.0000E05 - 0.1) / sin (35.02522945 / 180 * pi)

## Dark pixel substraction
mnOct <- minValue(ls8Octtoa)
ls8Octdps <- ls8Octtoa - mnOct

## read digital elevation model image
dem <- raster("./srtm/srtm_merge_n38_39_w123_124_1arc_v2_utm10.tif")

## resample and clip the DEM to the image
dem <- resample(dem, ls8Octtoa) 

## get lope and aspect of the surface.
slp <- terrain(dem, opt="slope")
asp <- terrain(dem, opt="aspect")

## calculate the sun incidence angle for the time of image acquisition
hsOct <- hillShade(slp, asp, angle=35.02522945, direction=160.69746486)

## reclassify the hill shade and set values smaller than 0.1 to 0.1
hsOct <- reclassify(hsOct, matrix (c (-Inf, 0.1, 0.1), 1, 3))

## Correction of topographic effects
ls8Octdps.topo <- ls8Octdps * cos((90 - 35.02522945) / 180 * pi) / hsOct

## Cloud detection preprocessing
## The thermal bands can only be converted to radiances
the1Octrad <- the1Oct.subs * 3.3420E-04 + 0.10000
the2Octrad <- the2Oct.subs * 3.3420E-04 + 0.10000
blueOctrad <- ls8Oct.subs[[1]] * 1.3040E-02 - 65.20248

## Cloud detection
cloudsOct <- blueOctrad / (the1Octrad + the2Octrad)
cloudsOct <- cloudsOct > 13 #threshold = 13
plot(cloudsOct)
## -> no clouds visible, no correction needed

## plots a RGB composite of bands 4 (R), 3 (G) and 2 (B) with linear contrast stretch
plotRGB (ls8Octdps.topo, 4, 3, 2, stretch="lin", main="October")

## Detection of the fire extent
## First approach by using the NDVI index
## Normalized Difference Vegetation Index (NDVI) for Landsat 8,
## NDVI is calculated as a ratio between the red (R) and near infrared (NIR) values intraditional fashion.
## NDVI = (Band 5 – Band 4) / (Band 5 + Band 4).
## Note: Band 5 has index 4 and Band 4 has index 3!
ndviJul <- (ls8Juldps.topo[[4]] - ls8Juldps.topo[[3]]) /(ls8Juldps.topo[[4]] + ls8Juldps.topo[[3]])
ndviOct <- (ls8Octdps.topo[[4]] - ls8Octdps.topo[[3]]) /(ls8Octdps.topo[[4]] + ls8Octdps.topo[[3]])
plot(ndviJul)
plot(ndviOct)

pdf("./Plots/NDVI.pdf")
change <- ndviJul - ndviOct 
plot(change, main="Change using NDVI index")
density(change, plot=TRUE, main="Density using NDVI index")
loss <- change > 0.25
plot(loss, main="Loss using NDVI index with Inflection point = 0.25")
dev.off()

## Calculation of total area
lossval <- getValues(loss) ## writes the pixel values to a table
table(lossval) ## determines the number of pixels per value
area <- table(lossval)[2] * 30 * 30 / 1000000 ## change area in square kilometrers
area ## 948.6531 km2 --> too small

## Second approach by using the NBR index
## Normalized Burn Ratio (NBR)
## NBR is calculated as a ratio between the NIR and SWIR values in traditional fashion.
## NBR = (Band 5 – Band 7) / (Band 5 + Band 7).
## Note: Band 5 has index 4 and Band 7 has index 6!
nbrJul <- (ls8Juldps.topo[[4]] - ls8Juldps.topo[[6]]) /(ls8Juldps.topo[[4]] + ls8Juldps.topo[[6]])
nbrOct <- (ls8Octdps.topo[[4]] - ls8Octdps.topo[[6]]) /(ls8Octdps.topo[[4]] + ls8Octdps.topo[[6]])
plot(nbrJul)
plot(nbrOct)

pdf("./Plots/NBR.pdf")
change <- nbrJul - nbrOct 
plot(change, main="Change using NBR index")
density(change, plot=TRUE, main="Density using NBR index")
loss <- change > 0.25
plot(loss, main="Loss using NBR index with Inflection point = 0.25")
dev.off()

## Calculation of total area
lossval <- getValues(loss) ## writes the pixel values to a table
table(lossval) ## determines the number of pixels per value
area <- table(lossval)[2] * 30 * 30 / 1000000 ## change area in square kilometrers
area ## 1290.946 km2




