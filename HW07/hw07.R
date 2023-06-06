# Question 1) Let’s explore and describe the data and develop some early intuitive thoughts:
## a. What are the means of viewers’ intentions to share (INTEND.0) on each of the four media types?
install.packages("data.table")
library(data.table)
pls_media1 <- fread("pls-media1.csv")
pls_media2 <- fread("pls-media2.csv")
pls_media3 <- fread("pls-media3.csv")
pls_media4 <- fread("pls-media4.csv")

mean1 <- mean(pls_media1$INTEND.0)
mean2 <- mean(pls_media2$INTEND.0)
mean3 <- mean(pls_media3$INTEND.0)
mean4 <- mean(pls_media4$INTEND.0)
cat("mean1:",mean1,", mean2:",mean2, ", mean3:",mean3, ", mean4:",mean4)

## b. Visualize the distribution and mean of intention to share, across all four media.(Your choice of data visualization; Try to put them all on the same plot and make it look sensible)

plot(density(pls_media1$INTEND.0), col="red", lwd=2, main="Density plot of 4 media", xlim=c(1, 7),ylim=c(0,0.5))
lines(density(pls_media2$INTEND.0), col="orange", lwd=2)
lines(density(pls_media3$INTEND.0), col="green", lwd=2)
lines(density(pls_media4$INTEND.0), col="skyblue", lwd=2)
abline(v=mean1,col="red",lwd=1,lty=2)
abline(v=mean2,col="orange",lwd=1,lty=2)
abline(v=mean3,col="green",lwd=1,lty=2)
abline(v=mean4,col="skyblue",lwd=1,lty=2)
legend('topleft',0.2, cex = 0.55, lty=1
       , c("video [animation + audio]", "video [pictures + audio]", "webpage [pictures + text]", "webpage [text only]")
       , col = c("red", "orange", "green", "skyblue"))

## c. From the visualization alone, do you feel that media type makes a difference on intention to share?

# Question 2) Let’s try traditional one-way ANOVA:
## a. State the null and alternative hypotheses when comparing INTEND.0 across four groups in ANOVA

### H0:There is no difference among the four 4 types of media.
### H1:There is a difference among the four types of media.

## b. Let’s compute the F-statistic ourselves:
### (i) Show the code and results of computing MSTR, MSE, and F
# Combine the datasets into one data frame
data <- data.frame(value = c(pls_media1$INTEND.0, pls_media2$INTEND.0, pls_media3$INTEND.0, pls_media4$INTEND.0),
                   group = rep(1:4, c(42, 38, 40, 46)))
# Calculate the overall mean
grand_mean <- mean(data$value)
# Calculate the sum of squares due to treatments (SSTR)
SSTR <- sum((tapply(data$value, data$group, mean) - grand_mean)^2 * c(42, 38, 40, 46))
# Calculate the mean square due to treatments (MSTR)
k <- length(unique(data$group))
df_mstr <- k - 1
MSTR <- SSTR / df_mstr
# Calculate the sum of squares due to error (SSE)
SSE <- sum((tapply(data$value, data$group, sd)^2) * (c(42, 38, 40, 46) - 1))
# Calculate the mean square due to error (MSE)
nT <- length(data$value)
df_mse <- nT - k
MSE <- SSE / df_mse
# Calculate the F-statistic
F_stat <- MSTR / MSE
# Print the results
cat("MSTR:", MSTR, "\n")

### (ii) Compute the p-value of F, from the null F-distribution; is the F-value significant? If so, state your conclusion for the hypotheses.

p_value <- pf(F_stat, df_mstr, df_mse, lower.tail=FALSE);p_value

## c. Conduct the same one-way ANOVA using the aov() function in R – confirm that you got similar results.

anova_model <- aov( data$value ~ factor(data$group))
summary(anova_model)

## d. Regardless of your conclusions, conduct a post-hoc Tukey test (feel free to use the TukeyHSD() function included in base R) to see if any pairs of media have significantly different means – what do you find?

TukeyHSD(anova_model, conf.level = 0.05)

## e. Do you feel the classic requirements of one-way ANOVA were met? (Feel free to use any combination of methods we saw in class or any analysis we haven’t covered)

shapiro.test(pls_media1$INTEND.0)

shapiro.test(pls_media2$INTEND.0)

shapiro.test(pls_media3$INTEND.0)

shapiro.test(pls_media4$INTEND.0)

# Question 3) Let’s use the non-parametric Kruskal Wallis test:

## a. State the null and alternative hypotheses

### H0: All groups would give you similar a value if randomly drawn from them.
### H1: At least one group would give you a larger value than another if randomly

## b. Let’s compute (an approximate) Kruskal Wallis H ourselves (use the formula we saw in class or another formula might have found at a reputable website/book):
### (i) Show the code and results of computing H

media_ranks <- rank(data$value)
group_ranks <- split(media_ranks,data$group)
sapply(group_ranks, sum)

N <- length(data$value)
H <- 12/(N*(N+1)) * sum(tapply(media_ranks,data$group,sum)^2/tapply(data$value,data$group,FUN = length)) - 3*(N+1);H

### (ii) Compute the p-value of H, from the null chi-square distribution; is the H value significant? If so, state your conclusion of the hypotheses. 

kw_p <- 1 - pchisq(H, df=k-1);kw_p

## c. Conduct the same test using the kruskal.wallis() function in R – confirm that you got similar results.

kruskal.test(value ~ group, data = data)

## d. Regardless of your conclusions, conduct a post-hoc Dunn test (feel free to use the dunnTest() function from the FSA package) to see if the values of any pairs of media are significantly different – what are your conclusions?

install.packages("FSA")
require(FSA)
dunnTest(value ~ group, data = data, method = "bonferroni")

                                           
                                           