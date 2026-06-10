zprop.test <- function(x,
                       n,
                       p0 = 0.5,
                       alternative = c("two.sided", "less", "greater"),
                       conf.level = 0.95,
                       digits = 5) {
  
  alternative <- match.arg(alternative)
  
  phat <- x / n
  
  # standard error under H0
  se0 <- sqrt(p0 * (1 - p0) / n)
  
  
  z <- (phat - p0) / se0
  
  
  p.value <- switch(
    alternative,
    "two.sided" = 2 * (1 - pnorm(abs(z))),
    "less"      = pnorm(z),
    "greater"   = 1 - pnorm(z)
  )
  
  
  alpha <- 1 - conf.level
  
  zcrit <- switch(
    alternative,
    "two.sided" = qnorm(1 - alpha / 2),
    "less"      = qnorm(conf.level),
    "greater"   = qnorm(conf.level)
  )
  
  # hypothesis interval
  ci <- switch(
    alternative,
    
    "two.sided" =
      phat + c(-1, 1) * zcrit * se0,
    
    "less" =
      c(-Inf, phat + zcrit * se0),
    
    "greater" =
      c(phat - zcrit * se0, Inf)
  )
  
  
  cat("\n")
  cat("One-sample proportion z-test with null-based SE\n\n")
  
  cat("data:  x = ", x, ", n = ", n, "\n", sep = "")
  
  cat(
    "z = ",
    format(round(z, 3), nsmall = 3),
    ", p-value = ",
    format(round(p.value, digits), nsmall = digits),
    "\n",
    sep = ""
  )
  
  cat("alternative hypothesis: true p is ")
  
  alt <- switch(
    alternative,
    less = "less than ",
    greater = "greater than ",
    two.sided = "not equal to "
  )
  
  cat(alt, p0, "\n", sep = "")
  
  cat(
    round(100 * conf.level),
    " percent hypothesis interval:\n",
    sep = ""
  )
  
  cat(
    format(ci[1], digits = digits),
    " ",
    format(ci[2], digits = digits),
    "\n",
    sep = ""
  )
  
  cat("\nsample estimates:\n")
  
  est <- c(p = phat)
  print(est)
  
  invisible(
    list(
      statistic = z,
      p.value = p.value,
      estimate = phat,
      conf.int = ci
    )
  )
}