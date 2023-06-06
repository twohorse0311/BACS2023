# Question 1) Let’s deal with nonlinearity first. Create a new dataset that log-transforms several variables from our original dataset (called cars in this case):

cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), log(acceleration), 
                                  model_year, origin))
head(cars_log)
colnames(cars_log)
## a. Run a new regression on the cars_log dataset, with mpg.log. dependent on all other variables
### (i) Which log-transformed factors have a significant effect on log.mpg. at 10% significance?

cars_log_lm <- lm(log.mpg.~ log.cylinders. + log.displacement. + log.horsepower. 
               + log.weight. + log.acceleration. + model_year + origin ,data=cars_log)
cars_log_lm_logcoef = summary(cars_log_lm)$coefficients[2:6, ]
rownames(cars_log_lm_logcoef[cars_log_lm_logcoef[, 4] < 0.1, ])

### (ii) Do some new factors now have effects on mpg, and why might this be?

# Horsepower, accleration now have effect on mpg, because taking the log of variables can give them more symmetric distributions.

### (iii) Which factors still have insignificant or opposite (from correlation) effects on mpg?  Why might this be?
# Cylinders, displacement still have insignificant effects on mpg. It might means that those variables indeed don’t have the effect on mpg, otherwise, it might means that there’s the problem of multicollinearity on those variables.

## b. Let’s take a closer look at weight, because it seems to be a major explanation of mpg

### (i) Create a regression (call it regr_wt) of mpg over weight from the original cars dataset

regr_wt <- lm(mpg ~ weight,cars)
summary(regr_wt)

### (ii) Create a regression (call it regr_wt_log) of log.mpg. on log.weight. from cars_log

regr_wt_log <- lm(log.mpg. ~ log.weight., cars_log)
summary(regr_wt_log)

### (iii) Visualize the residuals of both regression models (raw and log-transformed):

#### 1. density plots of residuals

par(mfrow= c(1,2))
plot(density(regr_wt$residuals), main = "raw residuals")
plot(density(regr_wt_log$residuals), main = "log-transformed residuals")

#### 2. scatterplot of log.weight. vs. residuals

plot(cars_log$log.weight., regr_wt_log$residuals)

### (iv) Which regression produces better distributed residuals for the assumptions of regression?

# log-transformed regression produces better distributed residuals for the assumptions of regression.

### (v) How would you interpret the slope of log.weight. vs log.mpg. in simple words?

# I would interpret it as the description that “1% change in weight leads to 1.06% decrease in mpg”.

### (vi) From its standard error, what is the 95% confidence interval of the slope of log.weight. vs log.mpg.?

confint(regr_wt_log, level = 0.95)

# Question 2) Let’s tackle multicollinearity next. Consider the regression model:
regr_log <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                 log.weight. + log.acceleration. + model_year +
                 factor(origin), data=cars_log)

## a. Using regression and R2, compute the VIF of log.weight. using the approach shown in class

weight_regr <- lm(log.weight. ~ log.cylinders. + log.displacement. + log.horsepower. +
                    log.acceleration. + model_year +
                    factor(origin), data=cars_log)
r2_weight <- summary(weight_regr)$r.squared
vif_weight <- 1 / (1 - r2_weight)
sqrt(vif_weight)

## b. Let’s try a procedure called Stepwise VIF Selection to remove highly collinear predictors. Start by Installing the ‘car’ package in RStudio -- it has a function called vif() 

require(car)

### (i) Use vif(regr_log) to compute VIF of the all the independent variables

vif(regr_log)

### (ii) Eliminate from your model the single independent variable with the largest VIF score that is also greater than 5

regr_log_adj1 <- lm(log.mpg. ~ log.cylinders. + log.horsepower. +
                     log.weight.+ log.acceleration. + model_year +
                     factor(origin), data=cars_log)
vif(regr_log_adj1)

### (iii) Repeat steps (i) and (ii) until no more independent variables have VIF scores above 5

### 1.
regr_log_adj2 <- lm(log.mpg. ~ log.cylinders. +log.weight.+ log.acceleration. 
                    + model_year + factor(origin), data=cars_log)
vif(regr_log_adj2)

### 2. 

regr_log_adj3 <- lm(log.mpg. ~ log.weight.+ log.acceleration. 
                    + model_year + factor(origin), data=cars_log)
vif(regr_log_adj3)

### Done.

### (iv) Report the final regression model and its summary statistics

summary(regr_log_adj3)

## c. Using stepwise VIF selection, have we lost any variables that were previously significant? If so, how much did we hurt our explanation by dropping those variables? (hint: look at model fit)

# log.horsepower.  0.8912 ->  0.8856

## d. From only the formula for VIF, try deducing/deriving the following:

### 1. If an independent variable has no correlation with other independent variables, what would its VIF score be? 

# If an independent variable has no correlation with other independent variables, its VIF score would be 1, indicating no multicollinearity issue.

### 2. Given a regression with only two independent variables (X1 and X2), how correlated would X1 and X2 have to be, to get VIF scores of 5 or higher? To get VIF scores of 10 or higher?

# If we want VIF(X1) and VIF(X2) to be at least 5, we can set these equations equal to 5 and solve for r: 5 = 1 / (1 - r^2)

R_squ_5 <- 1-(1/5)
sqrt(R_squ_5)

# Therefore, X1 and X2 would need to be highly correlated, with a correlation coefficient of at least 0.894, to get VIF scores of 5 or higher.

R_squ_10 <- 1-(1/10)
sqrt(R_squ_10)

# According the computation above, X1 and X2 would need to be very highly correlated, with a correlation coefficient of at least 0.953, to get VIF scores of 10 or higher.

# Question 3) Might the relationship of weight on mpg be different for cars from different origins?

origin_colors = c("blue", "darkgreen", "red")
with(cars_log, plot(log.weight., log.mpg., pch=origin, col=origin_colors[origin]))
legend(7.35,2.75, lty=1, c("origin1", "origin2", "origin3"), col=origin_colors)

# Let’s add three separate regression lines on the scatterplot, one for each of the origins.
  
with(cars_log, plot(log.weight., log.mpg., pch=origin, col=origin_colors[origin]))
legend(7.35,2.75, lty=1, c("origin1", "origin2", "origin3"), col=origin_colors)
#origin1
cars_us <- subset(cars_log, origin==1)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[1], lwd=2)
#origin2
cars_us <- subset(cars_log, origin==2)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[2], lwd=2)
#origin3
cars_us <- subset(cars_log, origin==3)
wt_regr_us <- lm(log.mpg. ~ log.weight., data=cars_us)
abline(wt_regr_us, col=origin_colors[3], lwd=2)





