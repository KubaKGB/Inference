# if  p-value is greater than 0.05, then the H_0 is accepted.
# if p-value is less than 0.05, then H_a is accepted.

# One sample t-test ----
t_test_1sample <- function(data, value) {
  shapiro_test <- shapiro.test(data)
  if (shapiro_test$p.value > 0.05) { # Normality test
    t.test(data, mu = value, 
           alternative = "greater",
           conf.level = 0.95)
  } else {
    print("Sample is not normally distributed, Wilcoxon signed-rank test:")
    wilcox.test(data, mu = value, alternative = "greater")
  }
}

# Independent 2 sample t-test, verificando l'omoschedasticità e la normalità dei dati: ----
t_test_2sample <- function(data1, data2) {
  differences <- data1 - data2
  shapiro_test <- shapiro.test(differences) 
  if (shapiro_test$p.value > 0.05) { # Normality of differences 
      levene_test <- leveneTest(data1, data2) 
      if (levene_test$p.value > 0.05) { # Homoschedasticity  
         print("Samples are homoschedastic, t-test:")
         t.test(data1, data2, var.equal = TRUE) 
         }
      else { print("Samples are heteroschedastic, Welch's t-test:")
             t.test(data1, data2, var.equal = FALSE) } 
      } 
  else { print('Samples not normally distributed, Wilcox signed-rank test:')
         wilcox.test(data1, data2) } 
  }

# Paired t-test on two groups, verificando solo la normalità (homoschedasticity not required): ----
t_test_paired2sample <- function(data1, data2) {
  differences <- data1 - data2
  shapiro_test <- shapiro.test(differences) 
  if (shapiro_test$p.value > 0.05) { # Normality of differences 
    t.test(data1, data2, paired = TRUE)
  } else {
    print('Samples not normally distributed, Wilcox signed-rank test:')
    wilcox.test(data1, data2, paired = TRUE)
  }
}





