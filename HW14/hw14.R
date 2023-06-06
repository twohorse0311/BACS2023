# Question 1) Earlier, we examined a dataset from a security survey sent to customers of e-commerce websites. However, we only used the eigenvalue > 1 criteria and the screeplot “elbow” rule to find a suitable number of components. Let’s perform a parallel analysis as well this week:

library(readxl)
sec_q <- data.frame(read_excel("security_questions.xlsx", sheet = "data", col_names = T))
dim(sec_q)

## a. Show a single visualization with scree plot of data, scree plot of simulated noise (use average eigenvalues of ≥ 100 noise samples), and a horizontal line showing the eigenvalue = 1 cutoff.

sec_q_pca <- prcomp(sec_q, scale. = TRUE)
sim_noise_ev <- function(n, p) {
  noise <- data.frame(replicate(p, rnorm(n))) 
  eigen(cor(noise))$values
}
evalues_noise <- replicate(100, sim_noise_ev(405, 18))
evalues_mean <- apply(evalues_noise, 1, mean)
screeplot(sec_q_pca, type="lines")
lines(evalues_mean, type="b", col = "red")
abline(h=1, lty="dotted")

## b. How many dimensions would you retain if we used Parallel Analysis?

# Only one PC is significantly higher than noise, after that there is another PC gets as more Variance as noise.
# Based on the context above, I will retain 2 PCs by doing Parallel Analysis.

# Question 2) Earlier, we treated the underlying dimensions of the security dataset as composites and examined their eigenvectors (weights). Now, let’s treat them as factors and examine factor loadings (use the principal() method from the psych package)

library(psych)

## a. Looking at the loadings of the first 3 principal components, to which components does each item seem to best belong?

sec_q_principal <- principal(sec_q, nfactor = 3, rotate = "none", scores = TRUE)
sec_q_principal

# It seems like all the items belong to PC1.

## b. How much of the total variance of the security dataset do the first 3 PCs capture?

sum(sec_q_principal$loadings[,1:3]^2)

## c. Looking at commonality and uniqueness, which items are less than adequately explained by the first 3 principal components?

is_h2_less_than_0.55 <- apply(sec_q_principal$loadings[, 1:3]^2, 1, sum) < 0.55
row.names(sec_q_principal$loadings)[is_h2_less_than_0.55]

# We can notice that the Variance we can capture through PC1 to PC3 of "Q2", "Q6", "Q13" are lower than 0.55.
# So we can say that these three items are less than adequately explained by the first 3 principal components.

## d. How many measurement items share similar loadings between 2 or more components?

# The variance captured from PC1 and PC2 in 'Q4', 'Q12', and 'Q17' are similar.

## e. Can you interpret a ‘meaning’ behind the first principal component from the items that load best upon it? (see the wording of the questions of those items)

# The conclusion of the questions from Q1, Q14, and Q18, which the variance of those items are captured over o.8 from PC1, can be made as how confident users believe in the site securing their transactions information.

# Question 3) To improve interpretability of loadings, let’s rotate our principal component axes using the varimax technique to get rotated components (extract and rotate only three principal components)

## a. Individually, does each rotated component (RC) explain the same, or different, amount of variance than the corresponding principal components (PCs)?

sec_q_principal_rot <- principal(sec_q, nfactor = 3, rotate = "varimax", scores = TRUE)
compare_each_PC_and_RC <- t(data.frame(sec_q_principal$values[1:3], sec_q_principal_rot$values[1:3]))
row.names(compare_each_PC_and_RC) <-c("PCs", "RCs")
compare_each_PC_and_RC

# From the table we made, we can notice that each RCs and PCs explain the same amounts of variance.

## b. Together, do the three rotated components explain the same, more, or less cumulative variance as the three principal components combined?

compare_PCs_and_RCs <- apply(compare_each_PC_and_RC, 1, sum)
compare_PCs_and_RCs

# From the table we made, we can alse notice that RCs and PCs explain the same amounts of variance combined.

## c. Looking back at the items that shared similar loadings with multiple principal components (#2d), do those items have more clearly differentiated loadings among rotated components?

sec_q_principal_rot$Structure[c(4, 12, 17), c(1, 3)]

# The items 'Q4', 'Q12', and 'Q17' have more clearly differentiated loadings among rotated components.

## d. Can you now more easily interpret the “meaning” of the 3 rotated components from the items that load best upon each of them? (see the wording of the questions of those items)

sec_q_principal_rot_structure <- sec_q_principal_rot$Structure[, c(1, 3, 2)]
sec_q_principal_rot_structure_colname <- colnames(sec_q_principal_rot_structure)
sec_q_principal_rot_structure_rowname <- row.names(sec_q_principal_rot_structure)

for(i in 1:3){
  print(sec_q_principal_rot_structure_colname[i])
  is_RC_greater_than_0.5 <- sec_q_principal_rot_structure[, i] > 0.5
  print(sec_q_principal_rot_structure_rowname[is_RC_greater_than_0.5])
}

# Yes, we can now more easily interpret the “meaning” according to the significant difference between three rotated components.

## e. If we reduced the number of extracted and rotated components to 2, does the meaning of our rotated components change?

sec_q_principal_rot_2 <- principal(sec_q, nfactor = 2, rotate = "varimax", scores = TRUE)
sec_q_principal_rot_2_structure <- sec_q_principal_rot_2$Structure[, c(1, 2)]
sec_q_principal_rot_2_structure_colname <- colnames(sec_q_principal_rot_2_structure)
sec_q_principal_rot_2_structure_rowname <- row.names(sec_q_principal_rot_2_structure)

for(i in 1:2){
  print(sec_q_principal_rot_2_structure_colname[i])
  is_RC_greater_than_0.5 <- sec_q_principal_rot_2_structure[, i] > 0.5
  print(sec_q_principal_rot_2_structure_rowname[is_RC_greater_than_0.5])
}





