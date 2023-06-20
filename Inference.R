# if  p-value is greater than 0.05, then the H_0 is accepted.
# if p-value is less than 0.05, then H_a is accepted.

# One sample t-test ----
t_test_1sample <- function(data, value) {
  shapiro_test <- shapiro.test(data)
  if (shapiro_test$p.value > 0.05) { # Normality test
    t.test(data, mu = value, 
           alternative = "two.sided",
           conf.level = 0.95)
  } else {
    print("Sample is not normally distributed, Wilcoxon signed-rank test:")
    wilcox.test(data, mu = value, alternative = "two.sided")
  }
}

# Independent 2 sample t-test, verificando l'omoschedasticità e la normalità dei dati: ----
t_test_2sample <- function(data1, data2) {
  shapiro_test_1 <- shapiro.test(data1) 
  shapiro_test_2 <- shapiro.test(data2) 
  if (shapiro_test_1$p.value > 0.05 && shapiro_test_2$p.value > 0.05) { # Samples normal distributed 
    levene_test <- leveneTest(data1, data2) 
    if (levene_test$p.value > 0.05) { # Homoschedasticity  
      print("Samples are homoschedastic, t-test:")
      t.test(data1, data2, var.equal = TRUE) 
    }
    else { print("Samples are heteroschedastic, Welch's t-test:")
      t.test(data1, data2, var.equal = FALSE) } 
  } 
  else { print('Samples not normally distributed, Wilcoxon rank sum exact test:')
    wilcox.test(data1, data2) } 
}

# Paired t-test on two groups, verificando solo la normalità (homoschedasticity not required): ----
t_test_paired2sample <- function(data1, data2) {
  differences <- data1 - data2
  shapiro_test <- shapiro.test(differences) 
  if (shapiro_test$p.value > 0.05) { # Normality of differences 
    t.test(data1, data2, alternative = "greater", paired = TRUE)
  } else {
    print('Samples not normally distributed, Wilcox signed-rank test:')
    wilcox.test(data1, data2, alternative = "greater", paired = TRUE)
  }
}

# Mean and 95% conf. interval of a series of data
mean_95 <- function(data) {
  data <- na.omit(data)
  mean_value <- mean(data)
  n <- length(data)
  standard_error <- sd(data) / sqrt(n)
  margin_of_error <- qt(0.975, df = n - 1) * standard_error
  confidence_interval <- c(mean_value - margin_of_error, mean_value + margin_of_error)
  result <- list(  result <- list(mean = mean_value, 
                                  delta = margin_of_error,
                                  confidence_interval = confidence_interval))
  return(result)
}

# Coefficient of variance
CV <- function(x) {
  sd_x <- sd(x,na.rm=T) # Calcola la deviazione standard di x
  mean_x <- abs(mean(x,na.rm=T)) # Calcola la media di x
  cv <- (sd_x / mean_x)  
  return(cv)
}



# version for THESIS
# Mean and 95% conf. interval of a series of data
mean_95 <- function(data) {
  if (any(is.infinite(data))) {print("Infinite data present -> Removed")}
  data <- na.omit(data[is.finite(data)])
  if (any(data > 1 | data < -1)) {print("Efficiency too large/low -> Removed")}
  data <- data[data > -1 & data < 1]
  mean_value <- mean(data)
  n <- length(data)
  standard_error <- sd(data) / sqrt(n)
  margin_of_error <- qt(0.975, df = n - 1) * standard_error
  confidence_interval <- c(mean_value - margin_of_error, mean_value + margin_of_error)
  result <- list(  result <- list(mean = mean_value, 
                                  delta = margin_of_error,
                                  confidence_interval = confidence_interval))
  return(result)
}


