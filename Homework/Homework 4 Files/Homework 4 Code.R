# Researcher A: Prior and Posterior
theta <- seq(0, 1, length.out = 1000)
priorA <- dbeta(theta, 1, 1)
postA <- dbeta(theta, 438, 544)

plot(theta, postA, type = "l", col = "blue", lwd = 2, 
     ylab = "Density", xlab = expression(theta),
     main = "Researcher A: Prior vs Posterior")
lines(theta, priorA, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Prior Beta(1,1)", "Posterior Beta(438,544)"),
       col = c("red", "blue"), lty = c(2, 1), lwd = 2)

priorB <- dbeta(theta, 120, 80)
postB <- dbeta(theta, 557, 623)

plot(theta, postB, type = "l", col = "blue", lwd = 2, 
     ylab = "Density", xlab = expression(theta),
     main = "Researcher B: Prior vs Posterior")
lines(theta, priorB, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Prior Beta(120,80)", "Posterior Beta(557,623)"),
       col = c("red", "blue"), lty = c(2, 1), lwd = 2)

# 95% Credible Intervals
ci_A <- qbeta(c(0.025, 0.975), 438, 544)
ci_B <- qbeta(c(0.025, 0.975), 557, 623)
ci_A  # Output: ~ [0.415, 0.477]
ci_B  # Output: ~ [0.444, 0.501]

# Probability theta < 0.485
prob_A <- pbeta(0.485, 438, 544)
prob_B <- pbeta(0.485, 557, 623)
prob_A  # Output: ~ 0.993
prob_B  # Output: ~ 0.812

theta <- seq(0, 1, length.out=100)
plot(theta, dbeta(theta, 1, 1), type="l", col="black", ylim=c(0,4), ylab="Density")
lines(theta, dbeta(theta, 5, 5), col="blue")
lines(theta, dbeta(theta, 10, 10), col="red")
legend("topright", legend=c("a=b=1", "a=b=5", "a=b=10"), 
       col=c("black", "blue", "red"), lty=1)

grid <- seq(0,1, length.out=1000)
prior_pdf <- 0.5*dnorm(grid, mean=0.3, sd=0.01) + 0.5*dnorm(grid, mean=0.7, sd=0.01)
prior_pdf <- prior_pdf / sum(prior_pdf)

plot(grid, prior_pdf, type="l", main="Bimodal Prior Distribution", 
     xlab=expression(theta), ylab="Density")


# Function to run grid approximation and plot
grid_approx <- function(y, n, title) {
  grid <- seq(0, 1, length.out=1000)
  prior_pdf <- 0.5*dnorm(grid, 0.3, 0.05) + 0.5*dnorm(grid, 0.7, 0.05)
  bin_lik <- dbinom(y, n, grid)
  post_pdf <- bin_lik * prior_pdf
  post_pdf <- post_pdf / sum(post_pdf)
  
  plot(grid, post_pdf, type="l", main=title, xlab=expression(theta), ylab="Density")
}

# (a) y = 13, n = 25
grid_approx(13, 25, "Posterior: 13/25 Red")

# (b) y = 15, n = 30
grid_approx(15, 30, "Posterior: 15/30 Red")

# (c) y = 150, n = 300
grid_approx(150, 300, "Posterior: 150/300 Red")
