# loads some pachages we need
library(rpart) 
library(rpart.plot)

# Load the data and remove missing values
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", 
                 "model_year", "origin", "car_name")
cars$car_name <- NULL
cars <- na.omit(cars)
# IMPORTANT: Shuffle the rows of data in advance for this project!
set.seed(27935752) # use your own seed, or use this one to compare to next class notes
cars <- cars[sample(1:nrow(cars)),]

# DV and IV of formulas we are interested in
cars_full <- mpg ~ cylinders + displacement + horsepower + weight + acceleration + model_year + factor(origin)
cars_reduced <- mpg ~ weight + acceleration + model_year + factor(origin)
cars_full_poly2 <- mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + 
  poly(weight, 2) + poly(acceleration, 2) + model_year + 
  factor(origin)
cars_reduced_poly2 <- mpg ~ poly(weight, 2) + poly(acceleration,2) + model_year + 
  factor(origin)
cars_reduced_poly6 <- mpg ~ poly(weight, 6) + poly(acceleration,6) + model_year + 
  factor(origin)

# Question 1) Compute and report the in-sample fitting error (MSEin) of all the models described above. It might be easier to first write a function called mse_in(…) that returns the fitting error of a single model; you can then apply that function to each model (feel free to ask us for help!). We will discuss these results later.

attach(cars)
lm_full <- lm(cars_full)
lm_reduced <- lm(cars_reduced)
lm_poly2_full <- lm(cars_full_poly2)
lm_poly2_reduced <- lm(cars_reduced_poly2)
lm_poly6_reduced <- lm(cars_reduced_poly6)
rt_full <- rpart(cars_full)
rt_reduced <- rpart(cars_reduced)

model_list <- list(lm_full, lm_reduced, lm_poly2_full, lm_poly2_reduced, 
                   lm_poly6_reduced, rt_full, rt_reduced)
name_list <- c("lm(cars_full)", "lm(lm_reduced)", "lm(cars_full_poly2)",
               "lm(cars_reduced_poly2)", "lm(cars_reduced_poly6)",
               "rpart(cars_full)", "rpart(cars_reduced)")
formula_list <- c(cars_full, cars_reduced, cars_full_poly2,
                  cars_reduced_poly2, cars_reduced_poly6, 
                  cars_full, cars_reduced)
function_list <- c(lm, lm, lm, lm, lm, rpart, rpart)
mse_in <- function(model){
  mean(residuals(model)^2)
}

MSE_in_list <- sapply(1:7, \(i){
  model <-  model_list[[i]]
  mse_in <- mse_in(model)
})
names(MSE_in_list) <- name_list
MSE_in_list

# Question 2) Let’s try some simple evaluation of prediction error. Let’s work with the lm_reduced model and test its predictive performance with split-sample testing:

## a. Split the data into 70:30 for training:test (did you remember to shuffle the data earlier?)

train_set <- cars[1:(nrow(cars)*0.7), ]
test_set <- cars[-(1:(nrow(cars)*0.7)), ]

## b. Retrain the lm_reduced model on just the training dataset (call the new model: trained_model); Show the coefficients of the trained model.

train_model <- lm(cars_reduced, data = train_set)
summary(train_model)$coefficients

## c. Use the trained_model model to predict the mpg of the test dataset
### (i) What is the in-sample mean-square fitting error (MSEin) of the trained model?

mean(residuals(train_model)^2)

### (ii) What is the out-of-sample mean-square prediction error (MSEout) of the test dataset?

pred <- predict(train_model, test_set)
mean((test_set$mpg - pred)^2)

### d. Show a data frame of the test set’s actual mpg values, the predicted mpg values, and the difference of the two (εout = predictive error); Just show us the first several rows of this dataframe.

mpg_actual_pred <- data.frame("pred" = pred, "actual" = test_set$mpg)
head(mpg_actual_pred, 5)

# Question 3) Let’s use k-fold cross validation (k-fold CV) to see how all these models perform predictively!

## a. Write a function that performs k-fold cross-validation (see class notes and ask us online for hints!). Name your function k_fold_mse(model, dataset, k=10, …) – it should return the MSEout of the operation. Your function must accept a model, dataset and number of folds (k) but can also have whatever other parameters you wish.

### (i) Use your k_fold_mse function to find and report the 10-fold CV MSEout for all models.

# Calculate prediction error for fold i out of k
fold_i_pe <- function(i, k, dataset, n, formula, func) {  
  # n for nrow(dataset), model for trained_model
  folds <- cut(1:n, breaks = k, labels = FALSE)
  test_indices <- which(folds == i) 
  test_set <- dataset[test_indices, ] 
  train_set <- dataset[-test_indices, ] 
  trained_model <- func(formula, data = train_set)
  predictions <- predict(trained_model, test_set)
  test_set$mpg - predictions
}

# Calculate mse_out across all folds
k_fold_mse <- function(dataset, k=10, formula, func) { 
  fold_pred_errors <- sapply(1:k, \(i) {
  fold_i_pe(i, k, dataset, nrow(dataset), formula, func)})
  pred_errors <- unlist(fold_pred_errors) 
  mean(pred_errors^2)
}

MSE_out_list <- sapply(1:7, \(i)
                       {k_fold_mse(cars, k = 10, formula = formula_list[[i]], 
                                   func = function_list[[i]])})
names(MSE_out_list) <- name_list
MSE_out_list

### (ii) For all the models, which is bigger — the fit error (MSEin) or the prediction error (MSEout)? 

MSE_df <- data.frame(method = name_list, MSE_in = MSE_in_list, MSE_out = MSE_out_list)
MSE_df[, c("MSE_in", "MSE_out")]

### (iii) Does the 10-fold MSEout of a model remain stable (same value) if you re-estimate it over and over again, or does it vary? 

tentimes_kfold <- sapply(1:7, \(i)
       {mean(replicate(10, k_fold_mse(cars[sample(1:nrow(cars)),], k = 10, 
                                 formula = formula_list[[i]],func = function_list[[i]])))})


MSE_df$"10 times k-fold MSE_out" = tentimes_kfold
MSE_df[, c(3, 4)]

## b. Make sure your k_fold_mse() function can accept as many folds as there are rows (i.e., k=392).

### (i) How many rows are in the training dataset and test dataset of each iteration of k-fold CV when k=392?

n = nrow(cars)
k = 392
folds <- cut(1:n, breaks = k, labels = FALSE)
test_indices <- which(folds == 1) 
train_set <- cars[-test_indices, ] 
print(paste("There are ", nrow(train_set), " in train_set", sep = ""))
test_set <- cars[test_indices, ] 
print(paste("There are ", nrow(test_set), " in test_set", sep = ""))

### (ii) Report the k-fold CV MSEout for all models using k=392.

MSE_out_list_392 <- sapply(1:7, \(i)
                       {k_fold_mse(cars, k = 392, formula = formula_list[[i]], 
                                   func = function_list[[i]])})
MSE_df$"MSE_out_392" <- MSE_out_list_392

### (iii) When k=392, does the MSEout of a model remain stable (same value) if you re-estimate it over and over again, or does it vary? (show a few repetitions for any model and decide!)
tentimes_kfold_392 <- sapply(1:7, \(i)
                         {mean(replicate(10, k_fold_mse(cars[sample(1:nrow(cars)),], k = 392, 
                                                        formula = formula_list[[i]],func = function_list[[i]])))})

MSE_df$"10 times k-fold 392" <- tentimes_kfold_392
names(tentimes_kfold_392) <- name_list
tentimes_kfold_392

### (iv) Looking at the fit error (MSEin) and prediction error (MSEout; k=392) of the full models versus their reduced counterparts (with the same training technique), does multicollinearity present in the full models seem to hurt their fit error and/or prediction error?

MSE_df[c(1, 2, 6, 7), c("MSE_in", "10 times k-fold 392")]

### (v) Look at the fit error and prediction error (k=392) of the reduced quadratic versus 6th order polynomial regressions — did adding more higher-order terms hurt the fit and/or predictions?

MSE_df[c(4, 5), c("MSE_in", "10 times k-fold 392")]
