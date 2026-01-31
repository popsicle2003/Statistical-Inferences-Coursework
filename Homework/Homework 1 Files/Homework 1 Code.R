#Question 1
set.seed(123)
# Generate uniform random variables
random_var <- runif(10000)
# Inverse CDF transformation
inverse_f <- random_var^(1/3)

# Calculate sample quartiles
sample_quartiles <- quantile(inverse_f, probs = c(0.25, 0.50, 0.75))
print(sample_quartiles)

hist(inverse_f, prob=TRUE, main="Histogram of Simulated Y vs PDF of f(y)", 
     xlab="y", col="lightblue", breaks=50)
curve(3*x^2, from=0, to=1, col="red", lwd=2, add=TRUE)
legend("topleft", legend="Theoretical PDF 3y^2", col="red", lwd=2)

#Question 6

set.seed(123)
n_trials <- 100000
n_voters <- 1648
# Simulate election under assumption p = 0.5
simulations <- rbinom(n_trials, n_voters, 0.5) / n_voters

# Calculate probability of observing 0.515 or higher
prob_extreme <- mean(simulations >= 0.515)
print(paste("Probability of observing >= 51.5%:", prob_extreme))

