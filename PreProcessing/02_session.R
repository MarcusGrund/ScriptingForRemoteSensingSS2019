#install.packages("raster")
#install.packages("rgdal")
library (raster)
library (rgdal)

setwd("~/Uni/ScriptingForRemoteSensingSS2019")
setwd("./PreProcessing")
getwd()


# 2014: Landsat 8 OLI & TIRS ----------------------------------------------------

setwd("./LS8_20140901")

#Metadata-File: LC80450322014244LGN00_MTL.txt
#SUN_ELEVATION = 53.37968040
#REFLECTANCE_MULT_BAND_X = 2.0000E-05
#REFLECTANCE_ADD_BAND_X = -0.100000
#RADIANCE_MULT_BAND_10 = 3.3420E-04
#RADIANCE_MULT_BAND_11 = 3.3420E-04
#RADIANCE_ADD_BAND_10 = 0.10000
#RADIANCE_ADD_BAND_11 = 0.10000

bands <- dir(pattern="LC80450322014244LGN00")
bands <- bands[4:9]
ls82014 <- stack (bands)
#plotRGB (ls82014, 4, 3, 2, stretch="lin")

the1.2014 <- raster("LC80450322014244LGN00_B10.TIF")
the2.2014 <- raster("LC80450322014244LGN00_B11.TIF")

ls82014.subs <- crop(ls82014, extent(510000, 580000, 4490000, 4543000))
ls8the1.subs <- crop(the1.2014, extent(510000, 580000, 4490000, 4543000))
ls8the2.subs <- crop(the2.2014, extent(510000, 580000, 4490000, 4543000))
ls82014.subs <- reclassify (ls82014.subs, rcl=matrix (c (-Inf, -1, NA), 1, 3))
ls8the1.subs <- reclassify (ls8the1.subs, rcl=matrix (c (-Inf, -1, NA), 1, 3))
ls8the2.subs <- reclassify (ls8the2.subs, rcl=matrix (c (-Inf, -1, NA), 1, 3))


# Conversion to TOA-reflectance -------------------------------------------

ls82014toa <- (ls82014.subs * 2.0000E05 - 0.1) / sin (53.37968040 / 180 * pi)
plotRGB (ls82014toa, 4, 3, 2, stretch="lin")


# 2011: Landsat 5 TM ---------------------------------------------------------------

setwd("../LS5_20110909")
#Metadata-File: LT50450322011252PAC01_MTL.csv
#RADIANCE_MULT_BAND_1 = 0.766
#RADIANCE_MULT_BAND_2 = 1.448
#RADIANCE_MULT_BAND_3 = 1.044
#RADIANCE_MULT_BAND_4 = 0.876
#RADIANCE_MULT_BAND_5 = 0.120
#RADIANCE_MULT_BAND_6 = 0.055
#RADIANCE_MULT_BAND_7 = 0.066
#RADIANCE_ADD_BAND_1 = -2.28583
#RADIANCE_ADD_BAND_2 = -4.28819
#RADIANCE_ADD_BAND_3 = -2.21398
#RADIANCE_ADD_BAND_4 = -2.38602
#RADIANCE_ADD_BAND_5 = -0.49035
#RADIANCE_ADD_BAND_6 = 1.18243
#RADIANCE_ADD_BAND_7 = -0.21555

blue2011 <- raster ("LT50450322011252PAC01_B1.TIF")
green2011 <- raster ("LT50450322011252PAC01_B2.TIF")
red2011 <- raster ("LT50450322011252PAC01_B3.TIF")
nir2011 <- raster ("LT50450322011252PAC01_B4.TIF")
swira2011 <- raster ("LT50450322011252PAC01_B5.TIF")
swirb2011 <- raster ("LT50450322011252PAC01_B7.TIF")
the2011 <- raster ("LT50450322011252PAC01_B6.TIF")

ls52011 <- stack(blue2011,green2011,red2011,nir2011,swira2011,swirb2011)

ls52011.subs <- crop(ls52011, extent(510000, 580000, 4490000, 4543000))
the2011.subs <- crop(the2011, extent(510000, 580000, 4490000, 4543000))
ls52011.subs <- reclassify (ls52011.subs, rcl=matrix (c (-Inf, -1, NA), 1, 3))
ls5the.subs <- reclassify (the2011.subs, rcl=matrix (c (-Inf, -1, NA), 1, 3))
plotRGB(ls52011.subs, 4, 3, 2, stretch="lin")


# Conversion to radiances -------------------------------------------------

blue2011rad <- ls52011.subs[[1]] * 0.766 - 2.28583
green2011rad <- ls52011.subs[[2]] * 1.448 - 4.28819
red2011rad <- ls52011.subs[[3]] * 1.044 - 2.21398
nir2011rad <- ls52011.subs[[4]] * 0.876 - 2.38602
swira2011rad <- ls52011.subs[[5]] * 0.120 - 0.49035
swirb2011rad <- ls52011.subs[[6]] * 0.066 - 0.21555


# Conversion to TOA-reflectance -------------------------------------------

#DOY: 2011-09-09 --> 252
#d=1.0072409
#SUN_ELEVATION = 49.65690900
sza <- (90 - 49.65690900) / 180 * pi

blue2011ref <- (blue2011rad * pi * 1.0072409^2) / (1957 * cos (sza))
green2011ref <- (green2011rad * pi * 1.0072409^2) / (1826 * cos (sza))
red2011ref <- (red2011rad * pi * 1.0072409^2) / (1554 * cos (sza))
nir2011ref <- (nir2011rad * pi * 1.0072409^2) / (1036 * cos (sza))
swira2011ref <- (swira2011rad * pi * 1.0072409^2) / (215 * cos (sza))
swirb2011ref <- (swirb2011rad * pi * 1.0072409^2) / (80.76 * cos (sza))

ls52011toa <- stack(blue2011ref, green2011ref, red2011ref, nir2011ref,  swira2011ref, swirb2011ref)
plotRGB (ls52011toa, 4, 3, 2, stretch="lin")


# Both sensors ------------------------------------------------------------

#thermal bands
the2011rad <- ls5the.subs * 0.055 + 1.18243
thea2014rad <- ls8the1.subs * 3.3420E-04 + 0.10000
theb2014rad <- ls8the2.subs * 3.3420E-04 + 0.10000


# Dark pixel substraction -------------------------------------------------

mn2014 <- minValue(ls82014toa)
ls82014dps <- ls82014toa - mn2014
plotRGB (ls82014dps, 4, 3, 2, stretch="lin")
mn2011 <- minValue(ls52011toa)
ls52011dps <- ls52011toa - mn2011
plotRGB (ls52011dps, 4, 3, 2, stretch="lin")


# Correction of topographic effects ---------------------------------------

setwd("~/Uni/ScriptingForRemoteSensingSS2019")
setwd("./PreProcessing")
setwd("./LS8_20140901")
getwd()
dem <- raster("SRTM_ffB01_p045r032.tif")
dem <- resample(dem, ls82014toa) ## resample and clip the DEM to the image

slp <- terrain(dem, opt="slope")
asp <- terrain(dem, opt="aspect")
hs2014 <- hillShade(slp, asp, angle=53.37968040, direction=145.67767882)
hs2014 <- reclassify(hs2014, matrix (c (-Inf, 0.1, 0.1), 1, 3))

ls82014dps.topo <- ls82014dps * cos((90 - 53.37968040) / 180 * pi) / hs2014
plotRGB (ls82014dps, 4, 3, 2, stretch="lin")
plotRGB (ls82014dps.topo, 4, 3, 2, stretch="lin")

hs2011 <- hillShade(slp, asp, angle=49.65690900, direction=144.56665673)
hs2011 <- reclassify(hs2011, matrix (c (-Inf, 0.1, 0.1), 1, 3))
ls52011dps.topo <- ls52011dps * cos ((90 - 49.65690900) / 180 * pi) / hs2011
plotRGB (ls52011dps, 4, 3, 2, stretch="lin")
plotRGB (ls52011dps.topo, 4, 3, 2, stretch="lin")

