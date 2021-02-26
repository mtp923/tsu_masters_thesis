############################################################################################
## Mapping SI with FIA & SSURGO data                                                      ##
##                                                                                        ##
## By: Matt Purucker <purucker.matt@gmail.com>                                            ##
##                                                                                        ##
## Created: Feb 10, 2020                                                                   ##
############################################################################################ 

getwd()
setwd("C:/Matt/SOIL_DATA_VIEWER/TN")

install.packages(c("raster", "rgdal", "rpart", "randomForest", "ModelMap", "maptools", "plyr", "shapefiles"))

#library
require(rpart)
require(randomForest)
require(ModelMap)
require(raster)
library(rgdal)
require(maptools)
require(shapefiles)

##########################################################################################
######               Extracting plot data (plot location in a shape file)           ######
######                             from raster files                                ######
##########################################################################################


# load shapefile for TN FIA plots
setwd ("C:/Matt/SOIL_DATA_VIEWER/TN/TN_Plots_SHP")
matt_dir <- "C:/Matt/SOIL_DATA_VIEWER/TN/TN_Plots_SHP/TN_Plots.shp"
file.exists(matt_dir)
subplot_locations <- readOGR(dsn = ".",layer="TN_Plots")  # make sure that you have setwd in where shapefile is, otherwise you need to specify the dsn file

# Extracting values from raster layers for each FIA plot/subplot location
setwd ("C:/Matt/SOIL_DATA_VIEWER/TN/Layers_as_Raster/raster/raster_img") # Location of a folder that has all raster images
raster_list <- c( "Ava_H2O_Capac.img" , "Ava_Water_Stora.img" , "Bulk_Density.img" , "Clay.img" , "Dep2ResLyr.img" , "Dep2WatTbl.img" ,
                  "KfactWS.img" , "OrgMatter.img" , "pH.img" , "Sand.img" , "Silt.img" , "Slope.img" , "Tfactor.img")
file_list <- list.files(".") 
file_list

# list of all  raster files in the working directory 
rast_file_list <- lapply(file_list, raster)
#rast_file_list
stack_of_rasters <- stack(rast_file_list)
#stack_of_rasters

# change shapefile's projection same as raster
crs(stack_of_rasters)
subplot_spdf <- spTransform(subplot_locations, crs(stack_of_rasters))

# extract data by plots
subplot_data <- extract(stack_of_rasters, subplot_spdf)
dim(subplot_data)
#class(subplot_data)
#View(data.frame(subplot_data))
subplot_data_df <- data.frame(subplot_data)
head(subplot_data_df)

#convert to dataframe for export
names(subplot_spdf)
subplot_spatial_df <- data.frame(subplot_spdf)
dim(subplot_spatial_df)
#View(subplot_spatial_df)

#combining spatial attributes(SSURGO) with subplot data(FIA)
combined_data <- cbind(subplot_spatial_df, subplot_data_df)
head(combined_data)
dim(combined_data)

combined_data <- subset(combined_data, select = -c(LON_SUBP, LAT_SUBP))  #Dropping subplot locations

#names(combined_data)[names(combined_data) == "CN_S"] <- "SUBP_ID"



