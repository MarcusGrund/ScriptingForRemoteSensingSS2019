## load required R packages
library (raster)
library (rgdal)

setwd("./BatchProcessing/assignment_05")

## Manipulating file names
metafiles <- dir(pattern="txt")
image.names <- unlist(strsplit(metafiles, "_MTL.txt"))
image.names

## Extracting parameters from the meta data
meta1 <- scan(metafiles[1], what="character") ## file name taken from vector
match("SUN_AZIMUTH", meta1) ## 'SUN_AZIMUTH' has the position 202
match("SUN_ELEVATION", meta1) ## 'SUN_ELEVATION' occurs in position 205
meta1[204] # SUN_AZIMUTH 
meta1[207] # SUN_ELEVATION

## Using loops
x <- 1
while (x < 5) { ## see forloop  explanation below
  print(x)
  x <- x + 1
  Sys.sleep(1) ## Sys.sleep is explained below
}

plot(1:10, 1:10, type="n") ## make an empty plot
for (k in 1:10){ ## for each number in 1:10...
  points (k, k, pch=19) ## …add the respective point to the plot...
  Sys.sleep (1) ## …and make the system sleep for 1 second (otherwise it is too
  ## fast).
}

plot(1:10, 1:10, type="n") ## make an empty plot
pb <- txtProgressBar(min=0, max=10, style=3) ## create an empty progress bar
for (n in 1:10){ ## for each number in 1:10...
  points(n, n, col=sample (8)[1], pch=sample (19)[1]) ## …add the respective
  ## point in an arbitrary color/symbol to the plot, ...
  Sys.sleep (1) ## …make the system sleep for 1 second, ...
  setTxtProgressBar(pb, n) ## ...and update the progress bar.
}

## Compiling the solar angles
angles <- matrix(0, 5, 2) ## empty table with five rows and two columns
rownames(angles) <- image.names ## label the rows
colnames(angles) <- c("sun.azimuth", "sun.elevation") ## label the columns

for (i in 1:5) { ## for each file...
  meta <- scan(metafiles[i], what="character") ## read the meta data file, ...
  x <- match("SUN_AZIMUTH", meta) ## find the azitmuth, ...
  y <- match("SUN_ELEVATION", meta) ## find the elevation angle, ...
  angles[i,1] <- as.numeric(meta[x+2]) ## copy azimuth to the ith row, 1st col
  angles[i,2] <- as.numeric(meta[y+2]) ## copy elevation to ith row, 2nd col
}
angles

## Final preparations

ex <- extent(571000, 583000, 4321000, 4330000) ## extent of the study site
file1 <- dir(pattern=image.names[1]) ## list of all band of the first image
im1 <- raster(file1[4]) ## read the blue band of the first image
im1 <- crop(im1, ex) ## clip it to the extent to create a master raster for the
## resampling process
dem <- raster("SRTM_ffB01_p044r033.tif")
dem <- resample(dem, im1) ## resample the DEM
slp <- terrain(dem, opt="slope") ## calculate slope
asp <- terrain(dem, opt="aspect") ## calculate aspect

## Batch processing
pb <- txtProgressBar(min=0, max=5, style=3) ## create an empty progress bar
for (i in 1:5) {
  file <- dir(pattern=image.names[i]) ## prepare a list with the corresponding bands
  im <- stack(file[4:9]) ## read these bands
  im <- crop(im, ex) ## clip them 
  im <- reclassify(im, rcl=matrix(c(-Inf, -1, NA), 1, 3)) ## eliminate bad pixels
  im <- (im * 2.0000E-05 - 0.100000) / sin(angles[i,2] / 180 * pi) ## convert the raw DNs to TOA reflectances
  im <- im - minValue(im) ## do dark pixel substraction
  hs <- hillShade(slp, asp, angle=angles[i,2], direction=angles[i,2]) ## derive the hillshade layer
  hs <- reclassify(hs, matrix (c (-Inf, 0.1, 0.1), 1, 3))
  im <- im * cos((90 - angles[i,2]) / 180 * pi) / hs ## apply the cosine correction
  writeRaster(im, paste(image.names[i], "_topo.tiff", sep=""), format("GTiff"), overwrite=TRUE) ## write image to hard drive
  setTxtProgressBar(pb, i) ## ...and update the progress bar.
}

## Creating a NDVI time series
topofiles <- dir(pattern="_topo.tif")
im1 <- stack(topofiles[1])
ndvi <- (  im1[[4]] - im1[[3]]) / (im1[[4]] + im1[[3]])
for (x in 2:5) {
  imx <- stack(topofiles[x])
  ndvix <- (imx[[4]] - imx[[3]]) / (imx[[4]] + imx[[3]])
  ndvi <- stack(ndvi, ndvix)
}
dim(ndvi)
writeRaster(ndvi, "ndvistack.tif", format="GTiff", overwrite=TRUE)
plotRGB(ndvi, 3, 2, 1, stretch="lin")

for (j in 1:5) {
  plot(ndvi[[j]], col=gray.colors (256, 0, 1, 1), main=image.names[j])
  Sys.sleep (3)
}

