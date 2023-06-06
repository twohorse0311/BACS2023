data <- read.table("customers.txt", header = T)
age <- data$age
age

# 1. What is the 5th element in the original list of ages?

q1 <- age[5]
q1

# 2. What is the fifth lowest age?



# remove the duplicated data from 'age' and get the fifth lowest
sort_age <- sort(age)
uni_sort_age <- unique(sort_age)
uni_sort_age[5]

# 3. Extract the five lowest ages together

head(uni_sort_age, 5)

# 4. Get the five highest ages by first sorting them in decreasing order first.

tail(uni_sort_age, 5)

# 5. What is the average (mean) age?

mean(age)

# 6. What is the standard deviation of ages?

sd(age)

# 7. Make a new variable called age_diff, with the difference between each age and the mean age

age_diff <- age - mean(age)
age_diff

# 8. What is the average age_diff?

mean(age_diff)

# 9(a).Visualize the raw data via histogram

hist(age)

# 9(b).Visualize the raw data via density plot

plot(density(age))

# 9(a).Visualize the raw data via boxplot+stripchart

boxplot(age, horizontal = TRUE)
stripchart(age, method = "stack", add = TRUE)
