n <- 25
successes <- 0
test_result <- prop.test(successes, n, conf.level = 0.99, correct = FALSE)
print(test_result$conf.int)

method_A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 
              79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
method_B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)

t.test(method_A, method_B, var.equal=TRUE)