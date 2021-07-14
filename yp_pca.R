#YP PCA
getwd()
setwd("D:/r_code")

data <- read.csv("D:/r_code/plots.csv", header = TRUE)
head(data)
file.exists(data)

colnames(data)
reduced_data <- data[-c(1:2,4:7,12:22, 49)]
colnames(reduced_data)
head(reduced_data)

#split dataframe by species
data_split <- split(reduced_data, reduced_data$SISP)
str(data_split)

#Yellow Poplar is SISP 621 
yp_data <- (data_split$ "621")
yp_data
dim(yp_data)
#White Oak is SISP 802
wo_data <- (data_split$ "802")
wo_data

#YP PCA (621)
colnames(yp_data)
yp_pcadata <- yp_data
head(yp_pcadata)
str(yp_pcadata)
colnames(yp_pcadata)
rownames(yp_pcadata)<- yp_pcadata$PIDSISP
yp_pcadata
#int to num
yp_pcadata$SICOND<-as.numeric(yp_pcadata$SICOND)
yp_pcadata$dep2reslyr<-as.numeric(yp_pcadata$dep2reslyr)
yp_pcadata$dep2wattbl<-as.numeric(yp_pcadata$dep2wattbl)

eco_lvl_temp <- as.factor(yp_pcadata$tneco3)
yp_pcadata_active <- yp_pcadata[-c(1,5,21,22)]




colnames(yp_pcadata_active)
library(factoextra)

yp_pca <- prcomp(yp_pcadata_active, scale = TRUE)
#graph of individuals
fviz_eig(yp_pca)
fviz_pca_ind(yp_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#graph of pca variables 
fviz_pca_var(yp_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
              )
#pca biplot
fviz_pca_biplot(yp_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(pcadata$tneco3)
fviz_pca_ind(yp_pca,
             col.ind = groups, # color by groups
             palette = display.brewer.pal(n=7, name = "Set1"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

length(groups)
