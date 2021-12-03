#Load in the packages

library(devtools)
library(MVA)
library(psych)
library(tidyr)
library(ggbiplot)
library(tidyverse)
library(ggplot2)

install_github("vqv/ggbiplot")


#Reshape the data table to wide format

require(reshape2)
PAQ_Martynas_wide = reshape(PAQ_Martynas, direction = "wide",
                               idvar = "id",
                               timevar = "var")


summary(PAQ_Martynas_wide)


#removing the participants with missing values in their answers

PAQ_corrected_wide <- PAQ_Martynas_wide %>%
  slice(-c(112, 226))


#formulating the pca analysis, and excluding categorical variable of sex and the id number from analysis

PAQ_pca <- prcomp(PAQ_corrected_wide[ ,c(3:12)], center = TRUE, scale. = TRUE)

summary(PAQ_pca)


str(PAQ_pca)

biplot(PAQ_pca, scale = 0)


#first 2 principal components

plot(PAQ_pca$x[, 1], PAQ_pca$x[, 2])



#scree diagram plotting

var_explained = PAQ_pca$sdev^2 / sum(PAQ_pca$sdev^2)


print(var_explained)


qplot(c(3:12), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

plot(PAQ_pca)


PAQ_pca_df <- as.data.frame.matrix(PAQ_pca$x) 


pca.data <- data.frame(Sample = rownames(PAQ_pca_df), 
                  x = PAQ_pca$x[, 1], 
                  y = PAQ_pca$x[, 2])




PAQ_corrected_wide_2 <- cbind(PAQ_corrected_wide, PAQ_pca$x[, 1:2])

head(PAQ_corrected_wide_2)


ggplot(data = PAQ_corrected_wide_2, mapping = aes(x = PC1, y = PC2)) +
  geom_point(shape = 21) +
  stat_ellipse()


barplot(PAQ_corrected_wide_2$PC2)


cor(PAQ_corrected_wide_2$value.Q1_cry, PAQ_corrected_wide_2$PC1)

plot(PAQ_corrected_wide_2$value.Q1_cry, PAQ_corrected_wide_2$PC1)


cor(PAQ_corrected_wide_2$value.Q2_help, PAQ_corrected_wide_2$PC1)

plot(PAQ_corrected_wide_2$value.Q2_help, PAQ_corrected_wide_2$PC1)