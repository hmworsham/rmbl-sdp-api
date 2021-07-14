##Script to demo raster remote access to SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 4-10-2020

# Set up workspace
## Install and load libraries
pkgs <- c('dplyr',
          'tidyverse',
          'ggplot2',
          'raster',
          'rgdal',
          'sf',
          'ggspatial',
          'rasterVis',
          'XML',
          'RCurl',
          'googledrive') # Name the packages you want to use here
load.pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} # Function to install new packages if they're not already installed
load.pkgs(pkgs) # Runs the function on the list of packages defined in pkgs

# Set working directory.
setwd('~/Desktop/RMBL/Projects/Watershed_Spatial_Dataset/')
outdir <- paste0(getwd(), '/Output')

##Check GDAL version (should work with GDAL 2.2.3 or later).
getGDALVersionInfo()

## Get list of files on AWS
sdp.cat.xml <- xmlParse(getURL('https://rmbl-sdp.s3.us-east-2.amazonaws.com'))
sdp.cat.list <- xmlToList(sdp.cat.xml)
unlisty <- data.frame(contents = unlist(sdp.cat.list))

## Subset the geotiff files
tifs <- unlisty %>%
  filter(grepl('release1', contents),
         grepl('tif', contents)) %>%
  slice(-c(16:20))
tifs

## Subset the metdata xml files
metas <- unlisty %>%
  filter(grepl('release1', contents),
         grepl('xml', contents)) %>%
  slice(-c(16:20))
metas

##Creates raster objects from cloud-based datasets.
basepath <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/"
tifs$full_path <- paste0(basepath, tifs$contents)
sdp.tifs <- list(tifs$full_path)[[1]]

metas$full_path <- paste0(basepath, metas$contents)
metas$full_path

sdp.raster.list <- list()
sdp.raster.list <- lapply(sdp.tifs, function(g){
  ras = raster(g)
  sdp.raster.list[[g]] = ras
})
sdp.raster.list

##Select structure variables and combine them into a raster stack
tifs$contents
dem <- sdp.raster.list[6]
dsm <- sdp.raster.list[11]
cover <- sdp.raster.list[15]
canopy.ht <- sdp.raster.list[5]
basins <- sdp.raster.list[3]
cover <- sdp.raster.list[15]
structure.stack <- stack(c(dem, dsm, cover, canopy.ht))
names(structure.stack) <- c("dem","dsm","cover", 'canopy_ht')

###########################################################################
## Subset to Schofield region
###########################################################################

# Define a circle around Schofield-24 as AOI
scho.point <- data.frame(lat = 38.97621984,
                         long = -107.0301866)

scho24 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_24/Scho_24_Boundary')

scho19 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_19/Scho_19_Boundary')

scho23 <- st_read('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Inventory Plots/Scho_23/Scho_23_Boundary')

scho.pts <- data.frame(long = as.list(extent(scho24))[1], lat = as.list(extent(scho24))[3])
scho.sf <- st_as_sf(scho.pts, coords = c('long', 'lat'), crs = 'EPSG:32613')
scho.circ <- st_buffer(scho.sf, dist = 2500)

# Subset maps to Schofield AOI
scho.stack <- crop(structure.stack, scho.circ, filename=tempfile(), 
                     progress="text")

# Compute derived functions of subset maps, adding output to stack
scho.stack$slope <- terrain(scho.stack$dem,opt="slope",
                              progress="text",filename=tempfile())
scho.stack$aspect <- terrain(scho.stack$dem,opt="aspect",
                               progress="text",filename=tempfile())

# Compute some arbitrary arithmetic on raster layers
## does raster algebra, creating a new map in memory if it's small
scho.stack$aspect_sin <- sin(scho.stack$aspect)

###########################################################################
## Plotting 
###########################################################################

# Simple base plot all layers
plot(scho.stack)

# Plot with ggplot2, rasterVis, and ggspatial
## DEM
dem.plot <- gplot(scho.stack$dem, maxpixels=500000)+
  geom_raster(aes(fill=value), interpolate=TRUE)+
  scale_fill_gradient(low = 'black', high = 'white',
                      ) +
  scale_x_continuous("")+
  scale_y_continuous("")+
  layer_spatial(scho24, color = 'tomato2') +
  layer_spatial(scho19, color = 'deepskyblue1') +
  layer_spatial(scho23, color = 'orchid1') +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white")+
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

dem.plot

## Canopy height
canopy.plot <- gplot(scho.stack$canopy_ht, maxpixels=500000)+
  geom_raster(aes(fill=value), interpolate=TRUE)+
  scale_fill_gradient(low = 'snow2', high = 'springgreen4',
  ) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  layer_spatial(scho24, color = 'tomato2') +
  layer_spatial(scho19, color = 'deepskyblue1') +
  layer_spatial(scho23, color = 'orchid1') +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white")+
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

canopy.plot

## DSM
dsm.plot <- gplot(scho.stack$dsm, maxpixels=1000000) +
  geom_raster(aes(fill=value), interpolate=TRUE) +
  scale_fill_gradient(low = 'black', high = 'white',
  ) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  layer_spatial(scho24, color = 'tomato2') +
  layer_spatial(scho19, color = 'deepskyblue1') +
  layer_spatial(scho23, color = 'orchid1') +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white")+
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

dsm.plot

landcover <- c('Needle-leaf',
                   'Deciduous',
                   'Meadow/Sub-shrub',
                   'Water',
                   'Snow',
                   'Bare',
                   'Buildings')

## Land cover
cover.plot <- gplot(scho.stack$cover, maxpixels = 1000000) +
  geom_raster(aes(fill=as.factor(value))) +
  scale_fill_manual(values = c('darkgreen',
                               'springgreen3',
                               'palegoldenrod',
                               'steelblue1',
                               'azure2',
                               'bisque4',
                               'firebrick4'),
                    name = 'Land-Cover Class',
                    labels = landcover) +
  layer_spatial(scho24, color = 'grey10') +
  layer_spatial(scho19, color = 'grey10') +
  layer_spatial(scho23, color = 'grey10') +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="black",line_col="black") +
  theme_bw() +
  theme(legend.position='right')

cover.plot

# Key
# 1=needle-leaf trees and shrubs 
# 2=deciduous trees and shrubs 
# 3=deciduous meadow and subshrub (<0.5m) 
# 4=water
# 5=snow 
# 6=bare rock, soil, gravel and asphalt 
# 7=buildings

# TWI Plot

twi <- raster('~/Google Drive (worsham@berkeley.edu)/Research/RMBL/RMBL_East River Watershed Forest Data/Data/Remote_Sensing/Henry_2019_SiteSelection/2019 Analysis Layers/Original Data/From Nicola 5-2019/TWI/TWI_9m_x5.tif')

plot(twi)

twi.plot <- gplot(twi, maxpixels=1000000) +
  geom_raster(aes(fill=value), interpolate=TRUE) +
  scale_fill_gradient(low = 'black', high = 'white',
  ) +
  layer_spatial(structure.stack$dem) +
  scale_x_continuous("") +
  scale_y_continuous("") +
  coord_sf(expand=0, label_axes="--EN") +
  annotation_scale(style="ticks",width_hint=0.1,location="br",
                   tick_height=0,text_col="white",line_col="white") +
  theme_bw() +
  theme(legend.position='bottom',
        legend.key.width = unit(2, 'lines'))

twi.plot

##Writes plot to disk.
png("structure_map.png",width=6,height=8,units="in",res=300)
gplot
dev.off()

##Writes subset of raster data to disk.
writeRaster(structure.stack, filename="Schofield_Structure_Rasters.tif",
            progress='text')

###########################################################################
## Subset to Gothic Townsite
###########################################################################

# ##Gothic Townsite Extent
# #plot(dem,maxpixels=5000)
# #gothic_extent <- drawExtent()
# gothic_extent <- extent(matrix(c(326876,328551,
#                                  4312941,4314552),
#                                nrow=2,byrow=TRUE))
# 
# ##Subsets rasters to the area of interest.
# gothic_stack <- crop(flow.stack, gothic_extent, filename=tempfile(), 
#                      progress="text")
# 
# ##computes derived functions of subset maps, adding output to stack
# gothic_stack$slope <- terrain(gothic_stack$dem,opt="slope",
#                               progress="text",filename=tempfile())
# gothic_stack$aspect <- terrain(gothic_stack$dem,opt="aspect",
#                                progress="text",filename=tempfile())
# 
# ##Doing arbitrary arithmetic on raster layers
# ##does raster algebra, creating a new map in memory if it's small
# gothic_stack$flow_log <- log(gothic_stack$flow)
# inMemory(gothic_stack$flow_log)
# 
# gothic_stack$aspect_sin <- sin(gothic_stack$aspect)
# inMemory(gothic_stack$aspect_sin)
# 
# ##samples values at random points where all layers have data
# gothic_samples <- sampleRandom(gothic_stack,size=120,
#                                sp=TRUE,na.rm=TRUE)
# 
# ##adds coordinates to data frame.
# gothic_samples$east <- coordinates(gothic_samples)[,1]
# gothic_samples$north <- coordinates(gothic_samples)[,2]
# 
# ##computes a hillshade layer for plotting
# gothic_stack$hillshade <- hillShade(slope=gothic_stack$slope,
#                                     aspect = gothic_stack$aspect_sin,
#                                     filename=tempfile())
# 
# ##converts water data to polygons.
# water_poly <- st_as_sf(rasterToPolygons(gothic_stack$water))
# flow_poly <- st_as_sf(rasterToPolygons(gothic_stack$flow_log,fun=function(x){x>11}))
# 
# ##plots the hillshade and adds sampling points
# plot(gothic_stack$hillshade,ext=gothic_extent,
#      main="Gothic Sample Points",
#      col=hcl.colors(n=50,palette="Grays",rev=FALSE))
# plot(water_poly,add=TRUE,col="slateblue")
# plot(flow_poly,add=TRUE,col="slateblue")
# points(gothic_samples,pch="+",cex=1,col="white")