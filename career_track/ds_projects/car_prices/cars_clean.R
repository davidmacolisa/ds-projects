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
cars <- read_csv(file = "cars_data.csv", col_names = T)
glimpse(cars)
#======================================================================================================================#
# Rename the columns
#======================================================================================================================#
colnames(cars)
cars <- cars %>%
  select(
	c(price = `13495`, symboling = `3`, normalised_losses = `?`, make = "alfa-romero", fuel_type = "gas",
	  aspiration = "std", num_of_doors = "four", body_style = "convertible", engine_location = "front",
	  drive_wheels = "rwd", wheel_base = `88.60`, length = `168.80`, width = `64.10`, height = `48.80`,
	  curb_weight = `2548`, engine_type = "dohc", num_of_cylinders = "four", engine_size = `130`,
	  fuel_system = "mpfi", compression_ratio = `9.00`, bore = `3.47`, stroke = `2.68`, horsepower = `111`,
	  peak_rpm = `5000`, city_mpg = `21`, highway_mpg = `27`)
  )
colnames(cars)
#======================================================================================================================#
# Detecting NA columns
#======================================================================================================================#
cars[cars == "?"] <- NA # Replace ? with NA
colnames(cars)

cars <- cars %>%
  mutate(
	normalised_losses = as.numeric(as.character(normalised_losses)),
	bore = as.numeric(as.character(bore)),
	stroke = as.numeric(as.character(stroke)),
	horsepower = as.numeric(as.character(horsepower)),
	peak_rpm = as.numeric(as.character(peak_rpm)),
	price = as.numeric(as.character(price)),
	na_values = ifelse(test = rowSums(is.na(.)) > 0, yes = "yes", no = "no")
  )
sum(is.na(cars))
table(cars$na_values)

na_columns <- colnames(cars)[which(colSums(is.na(cars)) > 0)]
sum(is.na(cars))
mean(is.na(cars))

ggplot(cars %>% na.omit(normalised_losses), aes(x = normalised_losses)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Normalised Losses", x = "Normalised Losses", y = "Density")

ggplot(cars %>% na.omit(bore), aes(x = bore)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Bores", x = "Bore", y = "Density")

ggplot(cars %>% na.omit(stroke), aes(x = stroke)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Strokes", x = "Stroke", y = "Density")

ggplot(cars %>% na.omit(), aes(x = horsepower)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Horsepower", x = "Horsepower", y = "Density")

ggplot(cars %>% na.omit(), aes(x = peak_rpm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of RPMs", x = "Peak rpm", y = "Density")

ggplot(cars %>% na.omit(), aes(x = price)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1) +
  labs(title = "Price Distribution", x = "Price", y = "Density")
#======================================================================================================================#
# Fill NAs values with its median as their distributions are either left- or right-skewed.
#======================================================================================================================#
glimpse(cars)
cars <- cars %>%
  mutate(
	normalised_losses = ifelse(test = is.na(normalised_losses),
							   yes = round(median(normalised_losses, na.rm = TRUE), digits = 0),
							   no = normalised_losses),
	bore = ifelse(test = is.na(bore),
				  yes = round(median(bore, na.rm = TRUE), digits = 0),
				  no = bore),
	stroke = ifelse(test = is.na(stroke),
					yes = round(median(stroke, na.rm = TRUE), digits = 0),
					no = stroke),
	horsepower = ifelse(test = is.na(horsepower),
						yes = round(median(horsepower, na.rm = TRUE), digits = 0),
						no = horsepower),
	peak_rpm = ifelse(test = is.na(peak_rpm),
					  yes = round(mean(peak_rpm, na.rm = TRUE), digits = 0),
					  no = peak_rpm),
	price = ifelse(test = is.na(price),
				   yes = round(median(price, na.rm = TRUE), digits = 0),
				   no = price),
  ) %>%
  group_by(make)
sum(is.na(cars))
#======================================================================================================================#
# Check for outliers in price
#======================================================================================================================#
summary(cars$price)

ggplot(cars, aes(x = price)) +
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Price Distribution", x = "Price", y = "Density") +
  scale_x_continuous(name = "Price", breaks = seq(0, 50000, by = 5000))

Q1 <- quantile(cars$price, probs = 0.25)
Q3 <- quantile(cars$price, probs = 0.75)
iqr <- IQR(x = cars$price)

lower_bound <- Q1 - (1.5 * iqr)
upper_bound <- Q3 + (1.5 * iqr)
outliers <- cars$price[cars$price < lower_bound | cars$price > upper_bound]
outliers
#======================================================================================================================#
# Recoding the price into low, medium, high and extremely high priced cars, and converting characters to factors.
#======================================================================================================================#
cars <- cars %>%
  mutate(
	price_cat = case_when(
	  price <= 7785 ~ "cheap_cars",
	  price > 7785 & price <= 16500 ~ "affordable_cars",
	  price > 16500 & price <= 30000 ~ "high_priced_cars",
	  price > 30000 ~ "e_high_priced_cars",
	)
  )

char_cols <- cars %>% select_if(is.character) %>% names()
cars[, char_cols] <- lapply(cars[, char_cols], FUN = as.factor)
#======================================================================================================================#
# Exploratory Data Analysis
#======================================================================================================================#
# Preprocess the data
car_counts <- cars %>%
  count(make) %>%
  arrange(desc(n))

car_counts$make <- reorder(car_counts$make, car_counts$n, FUN = max)

ggplot(data = car_counts, aes(y = make, x = n)) +
  geom_bar(stat = "identity", color = "black", fill = "darkblue") +
  labs(title = "Car Makes", x = "counts", y = "car makes")

# Which cars cost the most?
# Extremely high priced cars
e_high_priced_car_counts <- cars %>%
  filter(price_cat == "e_high_priced_cars") %>%
  group_by(price_cat, make) %>%
  count(make) %>%
  arrange(desc(n))
e_high_priced_car_counts

e_high_priced_car_counts$make <- reorder(e_high_priced_car_counts$make, e_high_priced_car_counts$n, FUN = max)
ggplot(data = e_high_priced_car_counts, aes(y = make, x = n)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(title = "Extremely High Priced Cars", x = "counts", y = "car makes")

# High priced cars
high_priced_car_counts <- cars %>%
  filter(price_cat == "high_priced_cars") %>%
  group_by(price_cat, make) %>%
  count(make) %>%
  arrange(desc(n))
high_priced_car_counts

high_priced_car_counts$make <- reorder(high_priced_car_counts$make, high_priced_car_counts$n, FUN = max)
ggplot(data = high_priced_car_counts, aes(y = make, x = n)) +
  geom_bar(stat = "identity", color = "black", fill = "brown") +
  labs(title = "High Priced Cars", x = "counts", y = "car makes")

# Affordable cars
affordable_car_counts <- cars %>%
  filter(price_cat == "affordable_cars") %>%
  group_by(price_cat, make) %>%
  count(make) %>%
  arrange(desc(n))
affordable_car_counts

affordable_car_counts$make <- reorder(affordable_car_counts$make, affordable_car_counts$n, FUN = max)
ggplot(data = affordable_car_counts, aes(y = make, x = n)) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  labs(title = "Affordable Cars", x = "counts", y = "car makes")

# Cheap cars
cheap_car_counts <- cars %>%
  filter(price_cat == "cheap_cars") %>%
  group_by(price_cat, make) %>%
  count(make) %>%
  arrange(desc(n))
cheap_car_counts

cheap_car_counts$make <- reorder(cheap_car_counts$make, cheap_car_counts$n, FUN = max)
ggplot(data = cheap_car_counts, aes(y = make, x = n)) +
  geom_bar(stat = "identity", color = "black", fill = "darkgreen") +
  labs(title = "Cheap Cars", x = "counts", y = "car makes")
#======================================================================================================================#
# Organise the columns
#======================================================================================================================#
cars <- cars %>%
  ungroup() %>%
  select(-make)

# Select the numeric and factor columns
factor_cols <- cars %>% select_if(is.factor) %>% names()
numeric_cols <- cars %>% select_if(is.numeric) %>% names()
cars <- cars %>% select(all_of(c(factor_cols, numeric_cols)))
#======================================================================================================================#
# Converting factors to numeric dummies
#======================================================================================================================#
# Load required library
library(caret)

dummies <- dummyVars(~., data = cars[, factor_cols], fullRank = T, sep = "_")
factor_numeric <- predict(
  object = dummies,
  newdata = cars[, factor_cols]) %>%
  data.frame() %>%
  select(-contains(match = "No"))
head(factor_numeric)

# check for columns with 0 variance
names(factor_numeric)[which(rowSums(factor_numeric) == 0)]

# binds columns
cars_df <- bind_cols(cars[, numeric_cols], factor_numeric)
head(cars_df, n = 5)
#======================================================================================================================#
# Near zero variance features
#======================================================================================================================#
nzv <- nearZeroVar(x = cars_df, saveMetrics = TRUE)
filtered_nnzv <- nzv %>% filter(nzv == FALSE) # non-near zero variance
filtered_nnzv

filtered_nzv <- nzv %>% filter(nzv == TRUE) # near zero variance
filtered_nzv
remove_features <- rownames(filtered_nzv)

# Remove near zero variance features
cars_df <- cars_df %>% select(-all_of(remove_features))
#======================================================================================================================#
# Find and remove Linear Dependences
#======================================================================================================================#
linear_combs <- findLinearCombos(x = cars_df)
linear_combs
cars_df <- cars_df %>% select(-all_of(linear_combs$remove))
#======================================================================================================================#
# Get label name
#======================================================================================================================#
labelName <- cars_df %>% select(price) %>% names()
predictors <- cars_df %>%
  select(-all_of(labelName)) %>%
  names()
#======================================================================================================================#
# Feature plots to select the best features for continuous variables
#======================================================================================================================#
trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(
  x = cars_df %>% select(all_of(predictors[1:15])),
  y = cars_df %>% pull(labelName),
  plot = "scatter",
  ## Add a key at the bottom
  auto.key = list(columns = 3)
)

featurePlot(
  x = cars_df %>% select(all_of(predictors[16:20])),
  y = cars_df %>% pull(labelName),
  plot = "pairs",
  ## Add a key at the bottom
  auto.key = list(columns = 3)
)

featurePlot(
  x = cars_df %>% select(all_of(predictors[21:25])),
  y = cars_df %>% pull(labelName),
  plot = "pairs",
  ## Add a key at the bottom
  auto.key = list(columns = 3)
)

featurePlot(
  x = cars_df %>% select(all_of(predictors[26:30])),
  y = cars_df %>% pull(labelName),
  plot = "pairs",
  ## Add a key at the bottom
  auto.key = list(columns = 3)
)

featurePlot(
  x = cars_df %>% select(all_of(predictors[31:36])),
  y = cars_df %>% pull(labelName),
  plot = "pairs",
  ## Add a key at the bottom
  auto.key = list(columns = 3)
)
#======================================================================================================================#
# Summary Statistics of the data
#======================================================================================================================#
summ <- statar::sum_up(cars_df, d = TRUE) %>%
  select(c(Variable, Obs, Mean, SD = StdDev, Min, p25, Median = p50, p75, Max))

round_df <- function(df, digits = digits) {
  df[] <- lapply(
	df, FUN = function(x) {
	  if (is.numeric(x)) round(x, digits) else x
	}
  )
  return(df)
}

round_df(summ, digits = 4) %>% print(n = nrow(.))
#======================================================================================================================#
# Correlation between numeric and factor variables and the response variable, price
#======================================================================================================================#
# Correlation matrix
corrplot::corrplot(
  corr = cor(cars_df, method = "pearson"),
  method = "circle",
  type = "lower",
  # title = "Price correlations with other features",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = TRUE,
  win.asp = 0.8
)
#======================================================================================================================#
# Creating interaction terms
#======================================================================================================================#
# cars_df <- cars_df %>%
#   mutate(
# 	curb_weight_width = interaction(curb_weight, width),
# 	horsepower_width = interaction(horsepower, width),
# 	horsepower_engine_size = interaction(horsepower, engine_size),
# 	horsepower_city_mpg = interaction(horsepower, city_mpg),
# 	curb_weight_city_mpg = interaction(curb_weight, city_mpg),
# 	highway_mpg_city_mpg = interaction(highway_mpg, city_mpg),
# 	compression_ratio_gas = interaction(compression_ratio, fuel_type_gas),
# 	compression_ratio_idi = interaction(compression_ratio, fuel_system_idi),
# 	high_priced_cars_engine_size = interaction(price_cat_high_priced_cars, engine_size),
#   )
#
# # # Get lable name
# predictors <- cars_df %>%
#   select(-all_of(labelName)) %>%
#   names()
#======================================================================================================================#
# Ensemble Model training
#======================================================================================================================#
names(getModelInfo())
library(AppliedPredictiveModeling)
transparentTheme(set = T, pchSize = 1, trans = 0.5)
library(doParallel) # for parallel processing
# cl <- makePSOCKcluster(names = 2)
# registerDoParallel(cl)
#======================================================================================================================#
# Model ensemble
#======================================================================================================================#
# Split data into train or ensemble, blender and test sets. The ensembleData will be used to train various model
# ensembles. The blederData will be used to blender the probabilities from the trainData. The testData will be used
# to test the model.
#======================================================================================================================#
seed <- 123
set.seed(seed = seed)
# Shuffle the rows of the dataset
cars_df <- cars_df %>% slice_sample(prop = nrow(.))
# Calculate the split point for dividing the data into thirds
split <- floor(x = nrow(cars_df) / 3)
# Split the dataset into three parts using dplyr's slice function
ensembleData <- cars_df %>% slice(1:split)
blenderData <- cars_df %>% slice((split + 1):(split * 2))
testData <- cars_df %>% slice((split * 2 + 1):n())
#======================================================================================================================#
# Models
#======================================================================================================================#
# Define the training control
my_ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  returnResamp = "none"
)

# Training the models
# random forest
model_rforest <- train(
  x = ensembleData %>% select(all_of(predictors)),
  y = ensembleData %>% pull(labelName),
  method = "rf", # random forest
  preProcess = c("center", "scale"), # standardize features
  metric = "Rsquared", # used to select optimal model
  trControl = my_ctrl,
  verbose = FALSE
)
model_rforest
plot(model_rforest)

# Predict the testData
testData <- testData %>%
  mutate(
	actual = price,
	rforest_Pred = predict(object = model_rforest, newdata = testData %>% select(all_of(predictors))),
	residual = price - rforest_Pred
  )

# Actual vs. Predicted Prices
rf_pred_plot <- ggplot(data = testData) +
  geom_point(aes(x = rforest_Pred, y = price)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Prices",
	   x = "Predicted",
	   y = "Actual") +
  theme_bw()

# Plot the residuals
rf_resid_plot <- ggplot(data = testData, aes(x = residual)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Residuals",
	   x = "Residuals",
	   y = "Density") +
  theme_bw()

library(patchwork)
rf_plots <- rf_pred_plot +
  rf_resid_plot +
  plot_annotation(tag_levels = "A", title = "Random Forest Model")
rf_plots

# Tune the GBM model
gbmTuned <- expand.grid(
  .n.trees = seq(from = 100, to = 1000, by = 50),
  .interaction.depth = seq(from = 1, to = 7, by = 2),
  .shrinkage = c(0.01, 0.1),
  .n.minobsinnode = 5
)

# GBM
model_gbm <- train(
  x = ensembleData %>% select(all_of(predictors)),
  y = ensembleData %>% pull(labelName),
  method = "gbm", # stochastic gradient boosting
  preProcess = c("center", "scale"), # standardize features
  metric = "RMSE ", # used to select optimal model
  tuneGrid = gbmTuned,
  trControl = my_ctrl,
  verbose = FALSE
)
model_gbm
ggplot(data = model_gbm) + theme(legend.position = "top")

# Predict the testData
testData <- testData %>%
  mutate(
	actual = price,
	rforest_Pred = predict(object = model_rforest, newdata = testData %>% select(all_of(predictors))),
	gbm_Pred = predict(object = model_gbm, newdata = testData %>% select(all_of(predictors))),
	residual_rf = price - rforest_Pred,
	residual_gbm = price - gbm_Pred
  )

# Actual vs. Predicted Prices
gbm_pred_plot <- ggplot(data = testData) +
  geom_point(aes(x = gbm_Pred, y = price)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Prices",
	   x = "Predicted",
	   y = "Actual") +
  theme_bw()

# Plot the residuals
gbm_resid_plot <- ggplot(data = testData, aes(x = residual_gbm)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Residuals",
	   x = "Residuals",
	   y = "Density") +
  theme_bw()

gbm_plots <- gbm_pred_plot +
  gbm_resid_plot +
  plot_annotation(tag_levels = "A", title = "GBM Model")
gbm_plots

# treebag
model_treebag <- train(
  x = ensembleData %>% select(all_of(predictors)),
  y = ensembleData %>% pull(labelName),
  method = "xgbTree", # tree bagging
  preProcess = c("center", "scale"), # standardize features
  metric = "Rsquared", # used to select optimal model
  trControl = my_ctrl,
  verbose = FALSE
)
model_treebag
plot(model_treebag)

# Predict the testData
testData <- testData %>%
  mutate(
	actual = price,
	rforest_Pred = predict(object = model_rforest, newdata = testData %>% select(all_of(predictors))),
	gbm_Pred = predict(object = model_gbm, newdata = testData %>% select(all_of(predictors))),
	treebag_Pred = predict(object = model_treebag, newdata = testData %>% select(all_of(predictors))),
	residual_rf = price - rforest_Pred,
	residual_gbm = price - gbm_Pred,
	residual_treebag = price - treebag_Pred
  )

# Actual vs. Predicted Prices
treebag_pred_plot <- ggplot(data = testData) +
  geom_point(aes(x = treebag_Pred, y = price)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Prices",
	   x = "Predicted",
	   y = "Actual") +
  theme_bw()

# Plot the residuals
treebag_resid_plot <- ggplot(data = testData, aes(x = residual_treebag)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Residuals",
	   x = "Residuals",
	   y = "Density") +
  theme_bw()

treebag_plots <- treebag_pred_plot +
  treebag_resid_plot +
  plot_annotation(tag_levels = "A", title = "Tree Bagging Model")
treebag_plots

# xgboost
model_xgboost <- train(
  x = ensembleData %>% select(all_of(predictors)),
  y = ensembleData %>% pull(labelName),
  method = "xgbTree", # advanced boosted trees
  preProcess = c("center", "scale"), # standardize features
  metric = "Rsquared", # used to select optimal model
  trControl = my_ctrl,
  verbose = FALSE
)
model_xgboost
plot(model_xgboost)

# Predict the testData
testData <- testData %>%
  mutate(
	actual = price,
	rforest_Pred = predict(object = model_rforest, newdata = testData %>% select(all_of(predictors))),
	gbm_Pred = predict(object = model_gbm, newdata = testData %>% select(all_of(predictors))),
	treebag_Pred = predict(object = model_treebag, newdata = testData %>% select(all_of(predictors))),
	xgboost_Pred = predict(object = model_xgboost, newdata = testData %>% select(all_of(predictors))),
	residual_rf = price - rforest_Pred,
	residual_gbm = price - gbm_Pred,
	residual_treebag = price - treebag_Pred,
	residual_xgboost = price - xgboost_Pred,
  )

# Actual vs. Predicted Prices
xgboost_pred_plot <- ggplot(data = testData) +
  geom_point(aes(x = xgboost_Pred, y = price)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Prices",
	   x = "Predicted",
	   y = "Actual") +
  theme_bw()

# Plot the residuals
xgboost_resid_plot <- ggplot(data = testData, aes(x = residual_xgboost)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Residuals",
	   x = "Residuals",
	   y = "Density") +
  theme_bw()

xgboost_plots <- xgboost_pred_plot +
  xgboost_resid_plot +
  plot_annotation(tag_levels = "A", title = "Advanced Boosted Trees Model")
xgboost_plots
#======================================================================================================================#
# Use the trained models to predict the blenderData and testData to get different model probabilities.
#======================================================================================================================#
# Predict the blenderData
blenderData <- blenderData %>%
  mutate(
	rforest_Pred = predict(object = model_rforest, newdata = blenderData %>% select(all_of(predictors))),
	gbm_Pred = predict(object = model_gbm, newdata = blenderData %>% select(all_of(predictors))),
	treebag_Pred = predict(object = model_treebag, newdata = blenderData %>% select(all_of(predictors))),
	xgboost_Pred = predict(object = model_xgboost, newdata = blenderData %>% select(all_of(predictors)))
  )

# Combine the Predabilities from the blenderData and testData
# blenderData$all_model_Pred <- rowMeans(x = blenderData[, c("gbm_Pred", "rforest_Pred", "treebag_Pred", "xgboost_Pred")])
# testData$all_model_Pred <- rowMeans(x = testData[, c("gbm_Pred", "rforest_Pred", "treebag_Pred", "xgboost_Pred")])
#======================================================================================================================#
# Train the blenderData, including the ensemble model probabilities.
#======================================================================================================================#
blend_predictors <- blenderData %>%
  select(-all_of(labelName)) %>%
  names()

# Training the final model
final_blender_model <- train(
  x = blenderData %>% select(all_of(blend_predictors)),
  y = blenderData %>% pull(labelName),
  method = "rf", # random forest
  preProcess = c("center", "scale"), # standardize features
  metric = "Rsquared", # used to select optimal model
  trControl = my_ctrl,
  verbose = FALSE
)
final_blender_model
ggplot(data = final_blender_model) + theme(legend.position = "top")
#======================================================================================================================#
# Use the final model to predict the testData.
#======================================================================================================================#
# Predict the testData
testData <- testData %>%
  mutate(
	actual = price,
	rforest_Pred = predict(object = model_rforest, newdata = testData %>% select(all_of(predictors))),
	gbm_Pred = predict(object = model_gbm, newdata = testData %>% select(all_of(predictors))),
	treebag_Pred = predict(object = model_treebag, newdata = testData %>% select(all_of(predictors))),
	xgboost_Pred = predict(object = model_xgboost, newdata = testData %>% select(all_of(predictors))),
	final_blender_Pred = predict(object = final_blender_model,
								 newdata = testData %>% select(all_of(blend_predictors))),
	residual_rf = price - rforest_Pred,
	residual_gbm = price - gbm_Pred,
	residual_treebag = price - treebag_Pred,
	residual_xgboost = price - xgboost_Pred,
	residual_final_blender = price - final_blender_Pred,
  )

# Actual vs. Predicted Prices
final_blender_pred_plot <- ggplot(data = testData) +
  geom_point(aes(x = final_blender_Pred, y = price)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs. Predicted Prices",
	   x = "Predicted",
	   y = "Actual") +
  theme_bw()

# Plot the residuals
final_blender_resid_plot <- ggplot(data = testData, aes(x = residual_final_blender)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  labs(title = "Residuals",
	   x = "Residuals",
	   y = "Density") +
  theme_bw()

final_blender_plots <- final_blender_pred_plot +
  final_blender_resid_plot +
  plot_annotation(tag_levels = "A", title = "Ensembled Model")
final_blender_plots
#======================================================================================================================#