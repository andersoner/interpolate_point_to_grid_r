# Eric Anderson
#
# Spline interpolate rain gauge data to 0.1
# degree grids and save to .csv
#
# Version 1.0
# 8 April 2015
# 
# The purpose of this program is to ingest .csv
# rainfall data for one day, interpolate them to
# 0.1 degree grid spacing, and save them to
# another .csv.

# load libraries
library(maptools) # autoloads sp
library(raster)
library(rgeos)
library(rgdal)
library(akima)
library(fields)


# set workspace
setwd("C:/Users/Eric/Dropbox/ESS508_Python/r_assign")

# Create area of interest for masking final interpolation (and
# extrapolation if necessary)
# This file should stay the same
aoi <- readShapePoly("aoi_sv_buff10_gcs.shp")

# Create vectors for x and y grid spacing. This will only need to # be created once per area to interpolate.
ygrids <- seq(13,14.5,0.1) # ~10km
xgrids <- seq(-90.2,-87, 0.1) # ~10km

# Load station data points (this file will be different every day)
# Note that this may include missing gauge data, but it must be stored as "NaN"
pluv_raw <- read.csv(file="daily_rain.csv")	

# Remove rows with missing rain data
pluv <- pluv_raw[complete.cases(pluv_raw),]


print("Rain gauge data read and missing data removed.",quote=0)


# Load coordinates to be interpolated 
# This file will only need to be created once per area to interpolate
pt <- read.csv(file="coords.csv")
coordinates(pt)= ~ longitude+ latitude


# Interpolate the pluviometer data to surface rasters
    ## Pick ONE of the following interpolation techniques by setting extLogical to TRUE or FALSE
   extLogical <- TRUE  # if extLogical is true, spline is used (linLogical is false)
   #extLogical <- FALSE  # if extLogical is false, linear is used (see akima documentation).
    # set linLogical to true or false based on extrapolation choice
    if (extLogical == TRUE) linLogical <- FALSE
    if (extLogical == FALSE) linLogical <- TRUE

# Define x, y (same for every day) and interpolated z values (different for every day)
x<- pluv[,4]; y<- pluv[,3]; z<- pluv[,5]
interpex <- mask(raster(interp(x,y,z, linear=linLogical, extrap=extLogical, xo=xgrids, yo=ygrids)),aoi)


# Tell me I'm finished and how to plot an image
print("Interpolation complete. Use plot(interpex) command to see results in R console.",quote=0)


# Stack interpolated rasters... (only needed if many days are to be prepared at the same time)
rStack = stack(interpex)

# Perform the value extraction on the raster stack
# (extracts interpolated rain values from raster to the 0.1 degree-spaced points)
rVal = extract(rStack,pt)
rVal[rVal<0] <- 0		#replace all negative values to 0 (artifact of extrapolating to edge)


# Combine these extracted results with the lat/lon of 0.1 degree-spaced csv points
write.table(cbind(pt,data.frame(rVal)),file="pluv_daily_spline.csv", append=FALSE, sep= ",", row.names=FALSE, col.names=TRUE)


print("Program complete. Output saved to pluv_daily_spline.csv.",quote=0)
# Program complete.