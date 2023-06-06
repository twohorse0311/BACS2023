## Question 1

### (a)
#### (i)

rnorm_std <- rnorm(10000, 940, 190)
rnorm_std_mean <- mean(rnorm_std)
rnorm_std_sd <- sd(rnorm_std)
(rnorm_std-rnorm_std_mean)/rnorm_std_sd

# We expected that the mean and std from rnorm_std would be 940 and 190 seperately
# because they were samples from a normal distribution with mean 940 and std 190.

#### (ii)

plot(density((rnorm_std-rnorm_std_mean)/rnorm_std_sd))

# It just looks like a bell shape and I think that's because it came from another bell-shape distribution.

####(iii)

# Z-distributon

### (b)

####(i)

bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
minday_mean <- mean(minday)
minday_sd <- sd(minday)
minday_std <- (minday - minday_mean)/minday_sd

# Based on the plot we drew, we can expect the mean of minday in May to fall between 700 and 1200. 
# Furthermore, we anticipate that the standard deviation of midday in May will be less than 200, 
# given that the majority of the data falls between 700 and 1200. 
# By setting an interval of three times the standard deviation above and below the mean, 
# we can capture nearly the entire data set.

#### (ii)

plot(density(minday_std))

# The two plots looked identical because each data point underwent the same translation and scaling.

## Question 2
library("ggplot2")
install.packages("remotes")
remotes::install_github("soumyaray/compstatslib")
library(compstatslib)

### (a)

plot_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, 
                    distr_func=rnorm, mean=20, sd=3)

#### (i)

# We expect that there are nearly 5 samples not to include the population mean in its 95% CI.

#### (ii)

# We expect that there are nearly 1 samples not to include the population mean in its 95% CI.

### (b)

plot_sample_ci(num_samples = 100, sample_size = 100, pop_size=10000, 
                    distr_func=runif)

#### (i)

# We expect their 95% and 99% CI to become narrower than before.

#### (ii)

# We still expect there are nearly 5 samples not to include the population mean in its 95% CI.

#### (iii)

# The answer we get would not have any change.
# Although population follows uniform distribution, according to the central limit theorem, when the sample size is sufficiently large, the distribution of the sample mean approaches the normal distribution.
# As mention above, we expect the result would not have any change.

## Question 3

### (a)

#### (i)

minday_mean <- mean(minday)
minday_std <- sd(minday)
minday_95CI <- c(minday_mean - 2*minday_std, minday_mean + 2*minday_std)

#### (ii)
compute_sample_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  mean(resample)
}
resamples_mean <- replicate(2000,compute_sample_mean(minday))

#### (iii)

plot(density(minday), lwd=1, xlim = c(500, 1500), ylim=c(0, 0.006))
abline(v = resamples_mean, col=rgb(0.7, 0.7, 0.7, 0.01))
abline(v = mean(resamples_mean))

#### (iv)
quantile(resamples_mean, probs=c(0.025, 0.975))


### (b)

#### (i)
resamples_mean_median <- quantile(resamples_mean, probs=c(0.5))

#### (ii)

plot(density(minday), lwd=1, xlim = c(500, 1500), ylim=c(0, 0.006))
abline(v = resamples_mean_median)

#### (iii)

quantile(resamples_mean, probs=c(0.025, 0.975))



