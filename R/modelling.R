

# Modeling ------------------------------------------------------------------------------------

model_lm <- function(formula, dataset) {
  #run model and print specific output
  model1 <- lm(formula = formula, data = dataset)
  stats <- round(
    c(
      summary(model1)$fstatistic[c(1, 3)],
      summary(model1)$sigma,
      summary(model1)$r.squared,
      summary(model1)$adj.r.squared
    ),
    3
  )
  names(stats) <- c("F", "DF", "Sigma", "Rsq", "AdjRsq")
  l1 <- list(round(summary(model1)$coefficients, 3), stats)
  names(l1) <- c("Coefficients", "Stats")
  print(l1)

  #run specific diagnostic tests
  par(mfrow = c(1, 3))
  hist(model1$residuals, main = "Histogram of residuals", xlab = "")
  plot(model1, 1)
  plot(model1, 2)
}
