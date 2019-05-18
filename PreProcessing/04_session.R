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

## load the image bands of the July image acqired prior to the wildfire
blueJul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B2.TIF')
greenJul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B3.TIF')
redJul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B4.TIF')
nirJul <- raster('LC08_L1TP_045033_20180710_20180717_01_T1_B5.TIF')

## set working directory back to base directory
setwd("../../")

## stack the bands of the July image to a multiband image
ls8Jul <- stack(blueJul, greenJul, redJul, nirJul)

## clip the image to fire complex extent
ls8Jul.subs <- crop(ls8Jul, extent(470000, 550000, 4300000, 4380000))

## elimination of bad pixels
ls8Jul.subs <- reclassify (ls8Jul.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))

## Metadata-File: LC08_L1TP_045033_20180710_20180717_01_T1_MTL.txt
## SUN_ELEVATION = 64.96477774
## SUN_AZIMUTH = 125.62033089
## REFLECTANCE_MULT_BAND_X = 2.0000E-05
## REFLECTANCE_ADD_BAND_X = -0.100000

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

## plots a RGB composite of bands 4 (R), 3 (G) and 2 (B) with linear contrast stretch
plotRGB (ls8Juldps.topo, 4, 3, 2, stretch="lin")


## October image ---------------------------------------------------------------
## set according working directory for October images
setwd("./landsat/LC08_L1TP_045033_20181030_20181115_01_T1")

## load the image bands of the October image acqired after to the wildfire
blueOct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B2.TIF')
greenOct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B3.TIF')
redOct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B4.TIF')
nirOct <- raster('LC08_L1TP_045033_20181030_20181115_01_T1_B5.TIF')

## set working directory back to base directory
setwd("../../")

## stack the bands of the October image to a multiband image
ls8Oct <- stack(blueOct, greenOct, redOct, nirOct)

## clip the October image to fire complex extent
ls8Oct.subs <- crop(ls8Oct, extent(470000, 550000, 4300000, 4380000))

## elimination of bad pixels
ls8Oct.subs <- reclassify (ls8Oct.subs, rcl=matrix(c(-Inf, -1, NA), 1, 3))

## Metadata-File: LC08_L1TP_045033_20181030_20181115_01_T1_MTL.txt
## SUN_AZIMUTH = 160.69746486
## SUN_ELEVATION = 35.02522945
## REFLECTANCE_MULT_BAND_X = 2.0000E-05
## REFLECTANCE_ADD_BAND_X = -0.100000

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

## plots a RGB composite of bands 4 (R), 3 (G) and 2 (B) with linear contrast stretch
plotRGB (ls8Octdps.topo, 4, 3, 2, stretch="lin")


## Normalized Difference Vegetation Index (NDVI) for Landsat 8,
## NDVI is calculated as a ratio between the red (R) and near infrared (NIR) values intraditional fashion.
## NDVI = (Band 5 – Band 4) / (Band 5 + Band 4).


## Normalized Burn Ratio (NBR)
## NBR is calculated as a ratio between the NIR and SWIR values in traditional fashion.
## In Landsat 8,
## NBR = (Band 5 – Band 7) / (Band 5 + Band 7).





