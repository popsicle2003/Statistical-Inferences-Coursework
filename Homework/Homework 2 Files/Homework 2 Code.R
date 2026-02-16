set.seed(123)
run_simulation <- function(s, confidence_level) {
  n <- 1497
  p_true <- 0.53
  coverage_count <- 0
  
  for (i in 1:s) {
    successes <- rbinom(1, n, p_true)
    
    # We calculate Score Interval using prop.test 
    test_result <- prop.test(successes, n, conf.level = confidence_level, correct = FALSE)
    ci <- test_result$conf.int
    
    if (p_true >= ci[1] && p_true <= ci[2]) {
      coverage_count <- coverage_count + 1
    }
  }
  return((coverage_count / s) * 100)
}
# For s = 5, 10, 100, 1000 with 95% confidence
s_values <- c(5, 10, 100, 1000)
cat("Coverage for 95% Score Intervals:\n")
for (s in s_values) {
  coverage <- run_simulation(s, 0.95)
  cat(sprintf("s = %d: %.1f%%\n", s, coverage))
}
#With 70% confidence
cat("\nCoverage for 70% Score Intervals:\n")
for (s in s_values) {
  coverage <- run_simulation(s, 0.70)
  cat(sprintf("s = %d: %.1f%%\n", s, coverage))
}
