# Question 1) We will use the interactive_regression() function from CompStatsLib again – Windows users please make sure your desktop scaling is set to 100% and RStudio zoom is 100%;  alternatively, run R from the Windows Command Prompt.

library(compstatslib)
interactive_regression()

## a. Comparing scenarios 1 and 2, which do we expect to have a stronger R2 ?
# Because in Scenario 1, most of the points are close to the regression line, which predicts a larger R^2 value.

## b. Comparing scenarios 3 and 4, which do we expect to have a stronger R2 ?

# Because in Scenario 3, most of the points are close to the regression line, which predicts a larger R^2 value.

## c. Comparing scenarios 1 and 2, which do we expect has bigger/smaller SSE, SSR, and SST? (intuitively)
# We expect scenarios 1 has bigger SSR and smaller SSE; on the other hand, We expect scenarios 2 has smaller SSR and bigger SSE.
# However, it's hard to expect which one's SST is bigger.

## d. Comparing scenarios 3 and 4, which do we expect has bigger/smaller SSE, SSR, and SST? (intuitively)
# We expect scenarios 3 has bigger SSR and smaller SSE; on the other hand, We expect scenarios 4 has smaller SSR and bigger SSE.
# However, it's hard to expect which one's SST is bigger.

# Question 2) Let’s analzye the programmer_salaries.txt dataset we saw in class.

prog_sal <- read.csv("programmer_salaries.txt", sep="\t") 

## a. Use the lm() function to estimate the regression model [Salary ~ Experience + Score + Degree]Show the beta coefficients, R2, and the first 5 values of y  ($fitted.values) and  ($residuals)

prog_sal_lm <- lm(Salary~Experience + Score + Degree, data = prog_sal)
prog_sal_lm$coefficients
summary(prog_sal_lm)$r.squared
head(prog_sal_lm$fitted.values)
head(prog_sal_lm$residuals)

## b. Use only linear algebra and the geometric view of regression to estimate the regression yourself:

### (i) Create an X matrix that has a first column of 1s followed by columns of the independent variables

x <- as.matrix(data.frame("1" = rep(1, length.out = nrow(prog_sal))
                         , prog_sal[, 1:3]))

### (ii) Create a y vector with the Salary values

y <- as.vector(prog_sal[, 4])

### (iii) Compute the beta_hat vector of estimated regression coefficients

tanspose_x <- t(x)
beta_hat <- solve(tanspose_x %*% x) %*% tanspose_x %*% y

### (iv) Compute a y_hat vector of estimated y values, and a res vector of residuals 

y_hat <- x %*% beta_hat; head(y_hat)
res <- y - y_hat; head(res)

### (v) Using only the results from (i) – (iv), compute SSR, SSE and SST 

SSR <- sum((y_hat - mean(y))**2)
SSE <-  sum((y - y_hat)**2)
SST <- SSR + SSE

## c. Compute R2 for in two ways, and confirm you get the same results

### (i) Use any combination of SSR, SSE, and SST

R2 <- SSR/SST; R2

### (ii) Use the squared correlation of vectors y and y_hat

R2 <- (cor(y, y_hat)^2)[1, 1] ; R2

# Question 3) We’re going to take a look back at the early heady days of global car manufacturing, when American, Japanese, and European cars competed to rule the world. Take a look at the data set in file auto-data.txt. We are interested in explaining what kind of cars have higher fuel efficiency (mpg).

auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")

## a. Let’s first try exploring this data and problem:
### (i) Visualize the data as you wish

library(car)
pairs(~mpg + cylinders + displacement + horsepower + weight ,data=auto)
pairs(~mpg + acceleration + model_year + origin ,data=auto)

### (ii) Report a correlation table of all variables, rounding to two decimal places

auto_cor <- cor(auto[, 1:8], use="pairwise.complete.obs"); round(auto_cor, 2)


### (iii) From the visualizations and correlations, which variables appear to relate to mpg?



### (iv) Which relationships might not be linear?

# except for the relationships between

### (v) Are there any pairs of independent variables that are highly correlated?

for(i in 2:8){
  for(j in 2:i){
    data = auto_cor[i, j]
      if(abs(auto_cor[i, j]) > 0.7 & i != j){
        print(paste(rownames(auto_cor)[i], "and", colnames(auto_cor)[j]))
      } 
  }
}

## b. Let’s create a linear regression model where mpg is dependent upon all other suitable variables (Note: origin is categorical with three levels, so use factor(origin) in lm(...)  to split it into two dummy variables)
### (i) Which independent variables have a ‘significant’ relationship with mpg at 1% significance?

auto_lm <- lm(mpg~ cylinders + displacement + horsepower + weight 
              + acceleration + model_year + factor(origin) ,data=auto)
auto_lm_coef <- summary(auto_lm)$coefficients[2:9, ]
summary(auto_lm)
rownames(auto_lm_coef)[auto_lm_coef[, 4]< 0.01]

### (ii) Looking at the coefficients, is it possible to determine which independent variables are the most effective at increasing mpg? If so, which ones, and if not, why not? (hint: units!)

sort(auto_lm_coef[, 1], decreasing = TRUE)[1]

## c. Let’s try to resolve some of the issues with our regression model above.
### (i) Create fully standardized regression results: are these slopes easier to compare?

auto_std <- data.frame(scale(auto[1:8]))
auto_regr_std <- lm(mpg~ cylinders + displacement + horsepower + weight 
              + acceleration + model_year + factor(origin) ,data=auto_std)
summary(auto_regr_std)

# As the standardized coefficients are all within -1 to 1 without any significantly larger values compared to others, it is harder to distinguish.
 
### (ii) Regress mpg over each nonsignificant independent variable, individually. Which ones become significant when we regress mpg over them individually?

nonsignificant_coef <- rownames(auto_lm_coef)[auto_lm_coef[, 4] > 0.01]
for (item in nonsignificant_coef) {
  formula_str <- paste("mpg~", item)
  temp_regr <- lm(as.formula(formula_str), data = auto_std)
  p_value <- summary(temp_regr)$coefficients[2, 4] # 提取第二列的 P 值
  if(p_value < 0.01){
    print(paste("P-value for", item, "is", p_value, ": significant"))
  }
}

### (iii) Plot the distribution of the residuals: are they normally distributed and centered around zero?

par(mfrow = c(1, 2))
auto_regr_std_res <- auto_regr_std$residuals
qqPlot(auto_regr_std_res, main = "qqplot")
plot(density(auto_regr_std_res), main = "density")


