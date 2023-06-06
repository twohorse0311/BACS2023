# Question 1)
## a. Imagine that Verizon claims that they take 7.6 minutes to repair phone services for its customers on average. The PUC seeks to verify this claim at 99% confidence (i.e., significance α = 1%)
# using traditional statistical methods.

### (i) Visualize the distribution of Verizon’s repair times, marking the mean with a vertical line
# load data
data <- read.csv("verizon.csv", header = TRUE, sep = ",")
rp_time <- data$Time
# Visualization

plot(density(rp_time), lty = 1, col = "blue")
abline(v = mean(rp_time), col = "red", lwd = 2)

### (ii) Given what the PUC wishes to test, how would you write the hypothesis? 

# H0: average repairment time = 7.6 min
# H1: average repairment time != 7.6 min

### (iii) Estimate the population mean, and the 99% confidence interval (CI) of this estimate.

sample_size <- length(rp_time)
sample_mean <- mean(rp_time)
sample_sd <- sd(rp_time)
se <- (sample_sd /sqrt(sample_size))
cat("Estimated population mean => ", sample_mean, sep = "")
cat("Estimated population 99% CI => ", sample_mean - 2.58*se
    , " ~ ",sample_mean + 2.58*se, sep = "")

### (iv) Find the t-statistic and p-value of the test

t <- (sample_mean - 7.6) / se
t
df <- sample_size - 1
p <- 2 * (1 - pt(t, df))
p

### (v) Briefly describe how these values relate to the Null distribution of t
### (vi) What is your conclusion about the company’s claim from this t-statistic, and why?

### reject H0 

## b. Let’s re-examine Verizon’s claim that they take no more than 7.6 minutes on average, but this time using bootstrapped testing:
### (i) Bootstrapped Percentile: Estimate the bootstrapped 99% CI of the population mean

boost_rp_time <- replicate(3000, mean(sample(rp_time, length(rp_time), replace = TRUE)))
boost_rp_time_mean <- mean(boost_rp_time)
boost_rp_time_quantile <- quantile(boost_rp_time, c(0.005, 0.995))

cat("Estimated population 99% CI => ", boost_rp_time_quantile[1], 
    " ~ ", boost_rp_time_quantile[2], sep = "")

### (ii) Bootstrapped Difference of Means: 

boost_rp_time_diff <- replicate(3000, mean(sample(rp_time, length(rp_time), replace = TRUE))-7.6)
boost_rp_time_diff_quantile <- quantile(boost_rp_time_diff, c(0.005, 0.995))
cat("99% CI of the bootstrapped difference between the sample mean and the hypothesized mean => ", boost_rp_time_diff_quantile[1]
    , " ~ ", boost_rp_time_diff_quantile[2], sep = "")

### (iii) Plot distribution the two bootstraps above

par(mfrow = c(1, 2))
plot(density(boost_rp_time))
abline(v = boost_rp_time_quantile, lty = 2)
plot(density(boost_rp_time_diff), col = "red")
abline(v = boost_rp_time_diff_quantile, lty = 2, col = "red")

### (iv) Does the bootstrapped approach agree with the traditional t-test in part [a]?

# not approach

## c. Finally, imagine that Verizon notes that the distribution of repair times is highly skewed by outliers, and feel that testing the mean in not fair because the mean is sensitive to outliers. They claim that the median is a more fair test, and claim that the median repair time is no more than 3.5 minutes at 99% confidence (i.e., significance α = 1%).
### (i) Bootstrapped Percentile: Estimate the bootstrapped 99% CI of the population median
boost_rp_time_median_sample <- replicate(3000, median(sample(rp_time, length(rp_time), replace = TRUE)))
boost_rp_time_median_quantile <- quantile(boost_rp_time_median_sample, c(0.005, 0.995))

cat("Estimated population 99% CI => ", boost_rp_time_median_quantile[1], 
    " ~ ", boost_rp_time_median_quantile[2], sep = "")

### (ii) Bootstrapped Difference of Medians

boost_rp_time_median_sample_diff <- 
  replicate(3000
            , median(sample(rp_time, length(rp_time), replace = TRUE)) - 3.5)
boost_rp_time_diff_median <- median(boost_rp_time_median_sample_diff)
boost_rp_time_median_diff_quantile <- quantile(boost_rp_time_median_sample_diff, c(0.005, 0.995))

cat("Estimated population 99% CI => ", boost_rp_time_median_diff_quantile[1], 
    " ~ ", boost_rp_time_median_diff_quantile[2], sep = "")

### (iii) Plot distribution the two bootstraps above

par(mfrow = c(1, 2))
plot(density(boost_rp_time_median_sample))
abline(v = boost_rp_time_median_quantile, lty = 2)
plot(density(boost_rp_time_median_sample_diff), col = "red")
abline(v = boost_rp_time_median_diff_quantile, lty = 2, col = "red")

### (iv) What is your conclusion about Verizon’s claim about the median, and why?

# Question 2) Load the compstatslib package and run interactive_t_test(). You will see a simulation of null and alternative distributions of the t-statistic, along with significance and power. If you do not see interactive controls (slider bars), press the gears icon (⚙) on the top-left of the visualization.

library(compstatslib)
interactive_t_test()

## a. You discover that your colleague wanted to target the general population of Taiwanese users of the product.  However, he only collected data from a pool of young consumers, and missed many older customers who you suspect might use the product much less every day.
### (i) Would this scenario create systematic or random error (or both or neither)?

### systematic error(from the sample)

### (ii) Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?

### diff may decrease

### (iii) Will it increase or decrease our power to reject the null hypothesis?

### more away from reject era

### (iv) Which kind of error (Type I or Type II) becomes more likely because of this scenario?

### type 1 error

## b. You find that 20 of the respondents are reporting data from the wrong wearable device, so they should be removed from the data. These 20 people are just like the others in every other respect.

### (i) Would this scenario create systematic or random error (or both or neither)?

### systematic error

### (ii) Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?

### n may decrease

### (iii) Will it increase or decrease our power to reject the null hypothesis?

### more away from reject era

### (iv) Which kind of error (Type I or Type II) becomes more likely because of this scenario?

### type 2 error

## c. A very annoying professor visiting your company has criticized your colleague’s “95% confidence” criteria, and has suggested relaxing it to just 90%.

### (i) Would this scenario create systematic or random error (or both or neither)?

##random error

### (ii) Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?

# alpha

### (iii) Will it increase or decrease our power to reject the null hypothesis?

# increase

### (iv) Which kind of error (Type I or Type II) becomes more likely because of this scenario?

### Type 1 error

## d. Your colleague has measured usage times on five weekdays and taken a daily average. But you feel this will underreport usage for younger people who are very active on weekends, whereas it over-reports usage of older users.

### (i) Would this scenario create systematic or random error (or both or neither)?

### both

### (ii) Which part of the t-statistic or significance (diff, sd, n, alpha) would be affected?

### diff, sd

### (iii) Will it increase or decrease our power to reject the null hypothesis?

### decrease

### (iv) Which kind of error (Type I or Type II) becomes more likely because of this scenario?

### Type II







