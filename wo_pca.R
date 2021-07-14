#WO PCA
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

#WO PCA (802)
colnames(wo_data)
wo_pcadata <- wo_data
head(wo_pcadata)
str(wo_pcadata)
colnames(wo_pcadata)
rownames(wo_pcadata)<- wo_pcadata$PIDSISP
wo_pcadata
#int to num
wo_pcadata$SICOND<-as.numeric(wo_pcadata$SICOND)
wo_pcadata$dep2reslyr<-as.numeric(wo_pcadata$dep2reslyr)
wo_pcadata$dep2wattbl<-as.numeric(wo_pcadata$dep2wattbl)

wo_pcadata_active <- wo_pcadata[-c(1,5,21,22)]
colnames(wo_pcadata_active)
library(factoextra)

wo_pca <- prcomp(wo_pcadata_active, scale = TRUE)
#graph of individuals
fviz_eig(wo_pca)
fviz_pca_ind(wo_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#graph of pca variables 
fviz_pca_var(wo_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#pca biplot
fviz_pca_biplot(wo_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(pcadata$SISP)
fviz_pca_ind(wo_pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)