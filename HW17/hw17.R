# Download the dataset and read it into R
# Kaggle:https://www.kaggle.com/datasets/teertha/ushealthinsurancedataset
insurance <- read.csv("insurance.csv", header = T, sep = ",")

# Question 1) Create some explanatory models to learn more about charges:
  
## a. Create an OLS regression model and report which factors are significantly related to charges

insurance_full_lm <- lm(charges ~ age + sex + bmi + children + 
                          smoker + region, data = insurance)
summary(insurance_full_lm)

# age, bmi, children, smoker, and region are significantly related to charge. 

## b. Create a decision tree (specifically, a regression tree) with default parameters to rpart().

### (i) Plot a visual representation of the tree structure
library(rpart)
library(rpart.plot)
insurance_full_rpart <- rpart(charges ~ age + factor(sex) + bmi + children + 
                          factor(smoker) + factor(region), data = insurance)
rpart.plot(insurance_full_rpart)

### (ii) How deep is the tree?

# Based on the plot we drew above, we can notice that there are two decisions in the trees.

### (iii) How many leaf groups does it suggest to bin the data into?

# Based on the plot we drew above as well, it suggest to bin the data into four leaf groups.

### (iv) What conditions (combination of decisions) describe each leaf group?

# There are four conditions based on the information of the tree:
# 1. The guys who are smokers and under age 43.
# 2. The guys who are smokers but over age 43.
# 3. The guys who are not smokers and their bmi are under 30.
# 4. The guys who are not smokers and their bmi are over 30. 

# Question 2) Let’s use LOOCV to see how how our models perform predictively overall

mse <- function(errs) mean(errs^2)
mse_in <- function(model) mse(residuals(model))
mse_out <- function(actual, predicted) mse(actual - predicted)

fold_i_pe <- function(i, k, model, dataset, outcome){ 
  folds <- cut(1:nrow(dataset), breaks=k, labels=FALSE)
  test_indices <- which(folds==i)
  test_set <- dataset[test_indices, ]
  train_set <- dataset[-test_indices, ] 
  trained_model <- update(model, data = train_set)
  predictions <- predict(trained_model, test_set)
  dataset[test_indices, outcome] - predictions 
}

k_fold_mse <- function(model, dataset, outcome, k=nrow(dataset)){ 
  shuffled_indicies <- sample(1:nrow(dataset))
  dataset <- dataset[shuffled_indicies,]
  fold_pred_errors <- sapply(1:k, \(kth){
    fold_i_pe(kth, k, model, dataset, outcome)})
  pred_errors <- unlist(fold_pred_errors)
  mse(pred_errors) 
}

## a. What is the $RMSE_{out}$ for the OLS regression model

lm_RMSE_out <- sqrt(k_fold_mse(insurance_full_lm, insurance, "charges"))
print(paste("The RMSE_out for the OLS regression model is ",lm_RMSE_out, sep = ""))

## b. What is the $RMSE_{out}$ for the decision tree model?

rpart_RMSE_out <- sqrt(k_fold_mse(insurance_full_rpart, insurance, "charges"))
print(paste("The RMSE_out for the decision tree model is ",rpart_RMSE_out, sep = ""))

# Question 3) Let’s see if bagging helps our models

# For bagging and boosting, we will only use split-sample testing to save time: partition the data to create training and test sets using an 80:20 split. Use the regression model and decision tree you created in Question 1 for bagging and boosting.

shuffled_indicies <- sample(1:nrow(insurance))[1:(nrow(insurance)*0.8)]
train_set <- insurance[shuffled_indicies, ]
test_set <- insurance[-shuffled_indicies, ]

## a. Implement the bagged_learn(…) and bagged_predict(…) functions using the hints in the class notes and help from your classmates on Teams. Feel free to share your code on Teams to get feedback, or ask others for help.

bagged_learn <- function(model, dataset, b=100) { 
  lapply(1:b, \(i) {
  # Bagging: Bootstrapped Aggregation
  # 1. Get a bootstrapped (resampled w/ replacement) dataset
    n = nrow(dataset)
    train_set <- dataset[sample(1:n, n, replace = TRUE), ]
  # 2. Return a retrained (updated) model 
    update(model, data = train_set)
  })
}

bagged_predict <- function(bagged_models, new_data) {
  predictions <- lapply(bagged_models, \(model){
    predict(model, new_data)
  }) # get b predictions of new_data
  as.data.frame(predictions) |> apply(X = _, 1, mean) # apply a mean over the columns of predictions
}

rmse_oos<-function(actuals,preds){
  sqrt(mean( (actuals - preds)^2 ))
}

## b. What is the $RMSE_{out}$ for the bagged OLS regression?

bagged_OLS_RMSE_out <- bagged_learn(insurance_full_lm, train_set, b=100) |> 
  bagged_predict(test_set) |> 
  rmse_oos(test_set$charges, preds = _)
print(paste("The RMSE_out for the bagged OLS regression is ",bagged_OLS_RMSE_out, sep = ""))

## c. What is the $RMSE_{out}$ for the bagged decision tree?

bagged_rpart_RMSE_out <- bagged_learn(insurance_full_rpart, train_set, b=100) |> 
  bagged_predict(test_set) |> 
  rmse_oos(test_set$charges, preds = _)
print(paste("The RMSE_out for the bagged decision tree is ",bagged_rpart_RMSE_out, sep = ""))

# Question 4) Let’s see if boosting helps our models. You can use a learning rate of 0.1 and adjust it if you find a better rate.

## a. Write boosted_learn(…) and boosted_predict(…) functions using the hints in the class notes and help from your classmates on Teams. Feel free to share your code generously on Teams to get feedback, or ask others for help.

boost_learn <- function(model, dataset, outcome, n=100, rate=0.1){ 
  predictors <- dataset[, !(colnames(dataset) == outcome)] # get data frame of only predictor variables
  # Initialize residuals and models
  res <- dataset[, c(outcome)]
  models <- list()
  for (i in 1:n) {
    this_model <- update(model, data = cbind(predictors, charges=res))
    res <-  res - rate*predict(this_model, predictors)# update residuals with learning rate 
    models[[i]] <- this_model # Store model
  }
  list(models=models, rate=rate) 
}

boost_predict <- function(boosted_learning, new_data) { 
  boosted_models <- boosted_learning$models
  rate <- boosted_learning$rate
  n <- nrow(new_data)
  predictions <- lapply(boosted_models, \(model){predict(model, new_data)}) # get predictions of new_data from each model
  pred_frame <- as.data.frame(predictions) |> unname()
  apply(pred_frame, 1, \(predict)(0.1*sum(predict))) # apply a sum over the columns of predictions, weighted by learning rate 
}

## b. What is the RMSEout for the boosted OLS regression?

boost_OLS_RMSE_out <- boost_learn(insurance_full_lm, train_set, outcome="charges", n=1000) |> 
  boost_predict(test_set) |>
  rmse_oos(test_set$charges, preds = _)
print(paste("The RMSE_out for the Boosted OLS regression is ",boost_OLS_RMSE_out, sep = ""))

## c. What is the RMSEout for the boosted decision tree?

boost_rpart_RMSE_out <- boost_learn(insurance_full_rpart, train_set, outcome="charges", n=1000) |> 
  boost_predict(test_set) |>
  rmse_oos(test_set$charges, preds = _)
print(paste("The RMSE_out for the Boosted decision tree is ",boost_rpart_RMSE_out, sep = ""))

# Question 5) Let’s engineer the best predictive decision trees. Let’s repeat the bagging and boosting decision tree several times to see what kind of base tree helps us learn the fastest. But this time, split the data 70:20:10 — use 70% for training, 20% for fine-tuning, and use the last 10% to report the final RMSEout.

shuffled_indicies <- sample(1:nrow(insurance))
n = nrow(insurance)
train_set <- insurance[shuffled_indicies[1:(n*0.7)], ]
tuning_set <- insurance[shuffled_indicies[(n*0.7):(n*0.9)], ]
test_set <- insurance[shuffled_indicies[(n*0.9):n], ]

## a. Repeat the bagging of the decision tree, using a base tree of maximum depth 1, 2, … n, keep training on the 70% training set while the RMSEout of your 20% set keeps dropping; stop when the RMSEout has started increasing again (show prediction error at each depth). Report the final RMSEout using the final 10% of the data as your test set.

repeat_bagging <- function(model, train_set, tuning_set){
  this_model <- update(model, control = rpart.control(maxdepth = 1))
  rmse_out <- bagged_learn(this_model, train_set, b=100) |> 
    bagged_predict(tuning_set) |> 
    rmse_oos(tuning_set$charges, preds = _)
  k <- 1
  print(paste("--- k =", k, "---"))
  print(rmse_out)
  while(TRUE){
    k <- k + 1 
    print(paste("--- k =", k, "---"))
    control_text <- paste("update(this_model, control = rpart.control(maxdepth =", 
                          k, "))")
    temp_model <- eval(parse(text = control_text))
    rmse_out_temp <- bagged_learn(temp_model, train_set, b=100) |> 
      bagged_predict(tuning_set) |> 
      rmse_oos(tuning_set$charges, preds = _)
    if (rmse_out_temp > rmse_out){
      print(paste(rmse_out_temp, "(Stop)"))
      result = list(model = this_model, k = k-1)
      return(result)
    }
    rmse_out <- rmse_out_temp
    this_model <- temp_model
    print(rmse_out)
  }
}

insurance_full_rpart <- rpart(charges ~ age + factor(sex) + bmi + children + 
                                factor(smoker) + factor(region), data = insurance)
remove(k)
rpart.plot(update(insurance_full_rpart, control = rpart.control(maxdepth = k)))
best_tree_bagging <- repeat_bagging(insurance_full_rpart, train_set, tuning_set)
repeat_bagged_rmse_out <- bagged_learn(best_tree_bagging$model, train_set, b=100) |> 
  bagged_predict(test_set) |> 
  rmse_oos(test_set$charges, preds = _)
print(paste("The RMSE_out for the Repeat Bagged decision tree is ",
            round(repeat_bagged_rmse_out, 2), 
            ", where max depth = ", best_tree_bagging$k, 
            sep = ""))

## b. Repeat the boosting of the decision tree, using a base tree of maximum depth 1, 2, … n, keep training on the 70% training set while the RMSEout of your 20% set keeps dropping; stop when the RMSEout has started increasing again (show prediction error at each depth). Report the final RMSEout using the final 10% of the data as your test set.

repeat_boosting <- function(model, train_set, tuning_set, maxdepth = 1){
  this_model <- update(model, control = rpart.control(maxdepth = 1))
  rmse_out <- boost_learn(this_model, train_set, outcome="charges", n=1000) |> 
    boost_predict(tuning_set) |>
    rmse_oos(tuning_set$charges, preds = _)
  k <- 1
  print(paste("--- k =", k, "---"))
  print(rmse_out)
  while(TRUE){
    k <- k + 1 
    print(paste("--- k =", k, "---"))
    control_text <- paste("update(this_model, control = rpart.control(maxdepth =", 
                         k, "))")
    temp_model <- eval(parse(text = control_text))
    rmse_out_temp <- boost_learn(temp_model, train_set, outcome="charges", n=1000) |> 
      boost_predict(tuning_set) |>
      rmse_oos(tuning_set$charges, preds = _)
    if (rmse_out_temp >= rmse_out){
      print(paste(rmse_out_temp, "(Stop)"))
      result = list(model = this_model, k = k-1)
      return(result)
    }
    rmse_out <- rmse_out_temp
    this_model <- temp_model
    print(rmse_out)
  }
}

insurance_full_rpart <- rpart(charges ~ age + factor(sex) + bmi + children + 
                                factor(smoker) + factor(region), data = insurance)

best_tree_boosting <- repeat_boosting(insurance_full_rpart, train_set, tuning_set)
repeat_boost_rmse_out <- 
  boost_learn(best_tree_boosting$model, train_set, outcome="charges", n=1000) |> 
  boost_predict(test_set) |>
  rmse_oos(test_set$charges, preds = _)

print(paste("The RMSE_out for the Repeat Boosted decision tree is ",
            round(repeat_boost_rmse_out, 2), 
            ", where max depth = ", best_tree_boosting$k, 
            sep = ""))

a = "apple"
eval(parse(test = "1+3"))
maxdepth_expr = paste("print(a)")
temp_model <- eval(parse(text = maxdepth_expr))
