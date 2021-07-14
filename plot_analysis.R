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
require(rpart)
library(factoextra)
library(RColorBrewer)
library(corrplot)
library(MASS)


getwd()
setwd("D:/r_code")

data <- read.csv("D:/r_code/plots.csv", header = TRUE)
head(data)
file.exists(data)

colnames(data)
reduced_data <- data[-c(1:2,4:7,12:22)]
colnames(reduced_data)
head(reduced_data)



#split dataframe by species
data_split <- split(reduced_data, reduced_data$SISP)
str(data_split)

#Yellow Poplar is SISP 621 
yp_data <- (data_split$ "621")
yp_data
dim(yp_data)
colnames(yp_data)
cor(yp_data)
yp_corr <- cor(yp_data[,vars])
corrplot(yp_corr)

#White Oak is SISP 802
wo_data <- (data_split$ "802")
wo_data


# List of predictor variables

# ssi <- ("SICOND" ~ "awc" + "aws" + "aws150" + "bd" + "clay" + "dep2reslyr" + "dep2wattbl" + "ecec" + "kfactws" + "ksat" + "liqlim" + "omr"
#          + "phwater" + "sand" + "silt" + "tfactor" + "wc3rdbar" + "slope_1" + "aspect_1" + "elevation" + "twi" + "tmin" + "tmax" + "avgtemp" + "prcp")

#RF input model
outcome <- "SICOND"
vars <- c("awcSurf" , "LAT" , "AWS" , "BdSurf" , "ECEC" , "KfactWS" , "Ksat" , "omr" , "SISP" ,
         "pHwater" , "Sand" , "Silt" ,"slope_1" , "elevation" , "twi" , "avgtemp" , "prcp" ,"tneco3") #"aspect_1" ,"WC3rdbar" , 
ssi <- paste(outcome, "~", paste(vars, collapse = " + "))

ggplot()

#RF for YP
yp_ssi_rf <- ranger(ssi, 
                    data = yp_data,
                    num.trees = 5000,
                    mtry = 5,
                    respect.unordered.factors = "order")
yp_ssi_rf
yp_ssi_rf$predictions
yp_ssi_rf$prediction.error
yp_data$SICOND
dim(yp_data)
plot(yp_data$SICOND, yp_ssi_rf$predictions)

#RF for WO
wo_ssi_rf <- ranger(ssi,
                    data = wo_data,
                    num.trees = 5000,
                    mtry = 4,
                    respect.unordered.factors = "ignore")
wo_ssi_rf


#RF for whole dataset
rf_df <- data.frame(matrix(nrow=84,ncol=3))
ntrees <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000)
ntries <- c(3,4,5,6,7,8)
counter <- 0
for(i in ntrees){
  for(j in ntries){
    counter <- counter + 1
    rd_ssi_rf <- ranger(ssi,
                        data = reduced_data,
                        num.trees = i,
                        mtry = j,
                        respect.unordered.factors = "ignore")
    print(rd_ssi_rf)
    rf_df[counter,1] <- i
    rf_df[counter,2] <- j
    rf_df[counter,3] <- rd_ssi_rf$r.squared
  }
}
rf_df[which(rf_df$X3==max(rf_df$X3)),]

print(rd_ssi_rf$predictions)


# Fit the full model 
full.model <- lm(ssi, data = reduced_data)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

#PCA 
colnames(data)
pcadata <- data[1:467,-c(1:2,4:7,12:22)]
head(pcadata)
str(pcadata)
colnames(pcadata)
rownames(pcadata)<- pcadata$PIDSISP
#int to num
pcadata$SICOND<-as.numeric(pcadata$SICOND)
pcadata$Dep2ResLyr<-as.numeric(pcadata$Dep2ResLyr)
pcadata$Dep2WatTbl<-as.numeric(pcadata$Dep2WatTbl)
pcadata$SISP<-as.numeric(pcadata$SISP)



eco_lvl_temp <- as.factor(pcadata$tneco3)
pcadata.temp <- pcadata[1:467,c(2:4,6:7,9:10,13:15,17:20,23:27,30:31)]

colnames(pcadata.temp)

drop_missing_soil <- which(pcadata$Clay==0)
pcadata.active <- pcadata.temp[-drop_missing_soil,]
eco_lvl <- eco_lvl_temp[-drop_missing_soil]
levels(eco_lvl) <- c("Southeastern Plains", "Blue Ridge" , "Ridge & Valley" , "Southwestern Appalachians" , "Central Appalachians" , 
                     "Interior Plateau" , "Mississippi Valley Loess Plains")
length(eco_lvl)==nrow(pcadata.active)

reduced_data2 <- as.data.frame(cbind(eco_lvl, pcadata.active$SICOND))
reduced_data2$eco_lvl <- as.factor(reduced_data2$eco_lvl)
ggplot(reduced_data2, aes(x=V2, color=eco_lvl))+  #V2 = SICOND
         geom_histogram()
  


colnames(pcadata.active)
dim(pcadata.active)
#View(pcadata.active)
summary(pcadata.active)

pcadata.cor <- cor(pcadata.active)

png(filename="./graphics/mixedcorplot.png", height = 8, width = 8, units = "in", res = 300)
corrplot(pcadata.cor, method="number",number.cex=0.7)
dev.off()

display.brewer.all()
mycolors <- display.brewer.pal(n=7, name = "Set1")


#pca
pca <- prcomp(pcadata.active, scale = TRUE)
#graph of individuals
fviz_eig(pca)
fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
              )
#graph of pca variables 
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#pca biplot
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

png(filename="./graphics/pca_ind.png", height = 8, width = 8, units = "in", res = 300)
fviz_pca_ind(pca,
             col.ind = eco_lvl, # color by groups
             palette = mycolors,
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE)+
  theme(legend.position="bottom")
dev.off()

png(filename="./graphics/pca_biplot.png", height = 8, width = 8, units = "in", res = 300)
fviz_pca_biplot(pca,
                col.ind = eco_lvl, #color by groups
                palette = display.brewer.pal(n=7, name = "Set1"), #number of colors = number of groups
                addEllipses = TRUE,
                label = "var",
                col.var = "black", 
                repel = TRUE,
                legend.title = "Level 3 Ecoregions" )+
  theme(legend.position = "bottom")
dev.off()





