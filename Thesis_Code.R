############################################################################################
## Mapping SI with FIA, STATSGO2, & SSURGO data                                           ##
##                                                                                        ##
## By: Matt Purucker <purucker.matt@gmail.com>                                            ##
##                                                                                        ##
## Created: Mar 12, 2021                                                                  ##
############################################################################################ 
R.Version()$version.string
sessionInfo()

getwd()
setwd("D:/r_code")
 , "ModelMap", "maptools", "plyr", "shapefiles"))

#library
require(rpart)
require(randomForest)
require(ModelMap)
require(raster)
library(rgdal)
require(maptools)
require(shapefiles)
library(plyr)
##########################################################################################
######               Extracting plot data (plot location in a shape file)           ######
######                             from raster files                                ######
##########################################################################################


# load shapefile for TN FIA plots
#setwd ("D:/r_code")
#matt_dir <- "D:/r_code/projected_plots.shp"
#file.exists(matt_dir)
#subplot_locations <- readOGR(dsn = ".",layer="projected_plots")  # make sure that you have setwd in where shapefile is, otherwise you need to specify the dsn file

# Extracting values from raster layers for each FIA plot/subplot location
# fix extent for: 
#setwd ("D:/r_code/rasters") # Location of a folder that has all raster images
#raster_list <- c(  "AWS150.img" , "aws.img" , "awc.img" , "BD3rdbar.img" , "clay.img" , "Dep2ResLyr.img" , "Dep2WatTbl.img" , "ECEC.img" , 
 #                  "KfactWS.img" , "Ksat.img" , "LiqLim.img" , "OrgMatter.img" , "pH.img" , "PlasLimit.img" ,"sand.img" , "silt.img" , 
  #                 "Tfactor.img", "WC3rdbar.img")

#dem_raster_list <- c("slope.img" , "aspect.img" , "elevation.img")
#file_list <- list.files(".") 
#file_list
#dem_rast_file_list <- lapply(dem_raster_list, raster)
#rast_file_list
#dem_stack_of_rasters <- stack(rast_file_list)
#stack_of_rasters


#check extent
#for(i in 1:18){
#  print(i)
#  print(raster(raster_list[i]))
#  print(extent(raster(raster_list[i])))
#}

#lapply(file_list, extent)
#extent(subplot_locations)
# list of all  raster files in the working directory 
#rast_file_list <- lapply(raster_list, raster)
#rast_file_list
#stack_of_rasters <- stack(rast_file_list)
#stack_of_rasters

# change shapefile's projection same as raster
#crs(stack_of_rasters)
#subplot_spdf <- spTransform(subplot_locations, crs(stack_of_rasters))

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
View(subplot_spatial_df)
View(subplot_data_df)
dim(subplot_data_df)
colnames(subplot_data_df)
colnames(subplot_spatial_df)

#combining spatial attributes(SSURGO) with subplot data(FIA)
combined_data <- cbind(subplot_spatial_df, subplot_data_df)
colnames(combined_data)
head(combined_data)
dim(combined_data)
View(combined_data)

reduced_data <- combined_data[-c(1:8,11:13,15:27,29,31:158)]
colnames(reduced_data)
head(reduced_data)

#split dataframe by species
data_split<- split(reduced_data, reduced_data$SISP)
str(data_split)

#Yellow Poplar is SISP 621 
yp_data <- (data_split$ "621")
yp_data
dim(yp_data)
#White Oak is SISP 802
wo_data <- (data_split$ "802")
wo_data

#keep_columns<- c( "INVYR" , "LON" , "LAT" , "PIDSP1" , "SICOND" , "SISP" , "AWS150" , "aws" , "awc" , "BD3rdbar" , "clay" , "Dep2ResLyr" , "Dep2WatTbl" ,
#"ECEC" , "KfactWS" , "Ksat" , "LiqLim" , "orgmatter" ,  "pH" , "PlasLimit" , "sand" , "silt" , "tfactor" , "WC3rdbar")

#keepers <- which(colnames(combined_data)==keep_columns)
#View(keepers)
#reduced_data<- combined_data[,keepers]
#head(reduced_data)
#View(reduced_data)

#combined_data <- subset(combined_data, select = -c(LON_SUBP, LAT_SUBP))  #Dropping subplot locations
#head(combined_data) 
#names(combined_data)[names(combined_data) == "CN_S"] <- "SUBP_ID"

###########################################################################
##############           Non-parametric modeling           ################
###########################################################################

# Random Forests

install.packages("ranger" , "vtreat" , "varhandle")
library(ranger) 
library(vtreat)
library(varhandle)
library(ggplot2)
library(dplyr)
# List of predictor variables

#SSI <- ("SICOND" ~ "AWS150" + "aws" + "awc" + "BD3rdbar" + "clay" + "Dep2ResLyr" + "Dep2WatTbl" +
#"ECEC" + "KfactWS" + "Ksat" + "LiqLim" + "orgmatter" +  "pH" + "PlasLimit" + "sand" + "silt" + "tfactor" + "WC3rdbar")

#RF input model
outcome <- "SICOND"
vars <- c("AWS150" , "aws" , "awc" , "BD3rdbar" , "clay" , "Dep2ResLyr" , "Dep2WatTbl" ,
          "ECEC" , "KfactWS" , "Ksat" , "LiqLim" , "orgmatter" ,  "pH" , "PlasLimit" , 
          "sand" , "silt" , "tfactor" , "WC3rdbar")
ssi <- paste(outcome, "~", paste(vars, collapse = " + "))



#checking data
str(yp_data)
#  $ SICOND    : Factor w/ 75 levels
# SICOND from character to numeric
sapply(yp_data, class)
yp_data$SICOND <- as.numeric(yp_data$SICOND)
str(yp_data)
hist(yp_data$SICOND)

#check for na
sum(is.na(yp_data))

#turn 0's into NA's
for(i in 1:18){
  na_if(yp_data[,vars[i]], 0)
}


par(mfrow=c(3,6))
for(i in 1:18){
  hist(yp_data[,vars[i]], main = vars[i])
}
par(mfrow=c(1,1)) #reset par

par(mfrow=c(3,6))
for(i in 1:18){
  plot(yp_data$SICOND, yp_data[,vars[i]], main = vars[i])
}
par(mfrow=c(1,1)) #reset par

#regression
yp_lm_ssi <- lm(ssi, data = yp_data)
yp_lm_ssi
summary(yp_lm_ssi)
summary(lm(SICOND~ sand + silt + clay, data=yp_data))


library(MASS)
# Fit the full model 
full.model <- lm(ssi, data = yp_data)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)



wo_lm_ssi <- lm(ssi, data = wo_data)
wo_lm_ssi
summary(wo_lm_ssi)

pairs(yp_data[, vars[1:9]])
pairs(yp_data[, vars[10:18]])


hist(yp_data$LON)
which_et <- which(yp_data$LON> -85)
yp_data_et <- yp_data[which_et, ]

yp_ssi_rf <- ranger(ssi ~., 
                    data = yp_data,
                    num.trees = 1000,
                    mtry = 5,
                    respect.unordered.factors = "order")
yp_ssi_rf
#RF for YP
yp_ssi_rf_et <- ranger(ssi, 
                 data = yp_data_et,
                 num.trees = 1000,
                 mtry = 5,
                 respect.unordered.factors = "order")
yp_ssi_rf_et

yp_data$pred <- predict(yp_ssi_rf, yp_data)$predictions


# Calculate the RMSE of the predictions
yp_data %>% 
  mutate(residual = SICOND - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2)))    # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(yp_data, aes(x = pred, y = SICOND)) + 
  geom_point() + 
  geom_abline()


# SICOND from character to numeric
sapply(wo_data, class)
wo_data$SICOND <- as.numeric(as.character(wo_data$SICOND))
str(wo_data)

#RF for WO
wo_ssi_rf <- ranger(ssi, 
                    data = wo_data,
                    num.trees = 500,
                    respect.unordered.factors = "order")
wo_ssi_rf
