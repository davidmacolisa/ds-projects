#======================================================================================================================#
# Predicting car prices
# August 23, 2024
#======================================================================================================================#
# Working directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/Jobs/src/career_track/ds_projects/car_prices/")
#======================================================================================================================#
# Packages
#======================================================================================================================#
library(tidyverse)
library(ggplot2)
library(caret)
library(corrplot)
#======================================================================================================================#
# Importing data
#======================================================================================================================#
# https://archive.ics.uci.edu/dataset/10/automobile
# This data set consists of three types of entities: (a) the specification of an auto in terms of various
# characteristics, (b) its assigned insurance risk rating, (c) its normalized losses in use as compared to other cars.
# The second rating corresponds to the degree to which the auto is more risky than its price indicates. Cars are
# initially assigned a risk factor symbol associated with its price.   Then, if it is more risky (or less), this
# symbol is adjusted by moving it up (or down) the scale.  Actuarians call this process "symboling".  A value of +3
# indicates that the auto is risky, -3 that it is probably pretty safe.

# The third factor is the relative average loss payment per insured vehicle year.  This value is normalized for all
# autos within a particular size classification (two-door small, station wagons, sports/speciality, etc...), and
# represents the average loss per car per year.

# Note: Several of the attributes in the database could be used as a "class" attribute.
#======================================================================================================================#
cars <- read_rds(file = "cars_clean.rds") %>% group_by(make)
head(cars)
glimpse(cars)
#======================================================================================================================#
# Keep only numeric columns
#======================================================================================================================#
numeric_cols <- cars %>% select_if(is.numeric) %>% names()
cars <- cars %>% select(c(make, all_of(numeric_cols)))
glimpse(cars)
sort(unique(cars$make))

statar::sum_up(cars[-1])
#======================================================================================================================#
# Correlations between variables
# The correlation matrix is visualized with corrplot. It shows the correlation between each pair of variables with
# the car price. Ideally, a correlation >= 0.8 is considered as a strong correlation. However, for the ML project,
# I will use correlation >= 0.5. A negative suggests an inverse correlation,
#======================================================================================================================#
corrplot::corrplot(
  corr = cor(cars[-1], method = "pearson"),
  method = "circle",
  type = "lower",
  title = "Price correlations with other features",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = TRUE
)

featurePlot(x = cars$engine_size, y = cars$price)
featurePlot(x = cars$highway_mpg, y = cars$price)
featurePlot(x = cars$stroke, y = cars$price)

cars <- cars %>% select(-c(symboling, normalised_losses, height, compression_ratio, stroke, peak_rpm))
glimpse(cars)
#======================================================================================================================#
# Visualise the price distribution
#======================================================================================================================#
ggplot(cars, aes(y = price)) + geom_boxplot()
ggplot(cars, aes(x = price)) + geom_histogram()
#======================================================================================================================#
# Standardise the data to a mean 0, and a standard deviation of 1
#======================================================================================================================#
cars[-1] <- cars[-1] %>% scale() %>% as.tibble()
apply(X = cars[-1], MARGIN = 2, FUN = sd)
#======================================================================================================================#
# Scree plot
# Determine number of clusters by considering the withinness measure
# 15 is default!
# The optimal number of clusters is k = 6.
#======================================================================================================================#
wss <- NULL
for (i in 1:15) {
  wss[i] <- sum(kmeans(cars[-1], centers = i)$withinss)
}

#Scree Plot
plot(x = 1:15, y = wss,
	 type = "b",
	 xlab = "Number of Clusters",
	 ylab = "Within groups sum of squares",
	 main = "Scree plot")
#======================================================================================================================#
# K-Means Cluster Analysis with k = 6
#======================================================================================================================#
fit <- kmeans(cars[-1], centers = 6) # 6 cluster solution
str(fit)

#Within sum of squares in each cluster
fit$withinss
sum(fit$withinss)
fit$tot.withinss

#To check cluster number of each row in data
fit$cluster

#Cluster Centers
fit$centers

# get cluster means
#aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster label to the actual data frame
cars <- data.frame(cars, fit$cluster)
cars <- cars %>% rename(cluster_id = fit.cluster)
cars$cluster_id <- as.factor(cars$cluster_id)

#======================================================================================================================#
# Hierarchical Clustering
#======================================================================================================================#
distance <- dist(cars[,-1], method = "euclidean") # distance matrix
fit <- hclust(distance, method = "complete")
plot(fit, hang = -1)
#======================================================================================================================#
#