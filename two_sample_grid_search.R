#grid search for disagreement 

find_one_disagreement <- function(max_n = 25) {
 
   for (n1 in 5:max_n) {
    for (n2 in 5:max_n) {
 
       for (x1 in 0:n1) {
         for (x2 in 0:n2) {
 
           p1 <- x1 / n1
           p2 <- x2 / n2
           d  <- p1 - p2
 
           # skip degenerate variance cases
           if (p1 %in% c(0,1) && p2 %in% c(0,1)) next
 
           # ----- Wald test -----
           se_w <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
 
           if (se_w == 0) next
 
           z_w <- d / se_w
           wald_reject <- abs(z_w) > 1.96
 
           # ----- plug-in-null CI (pooled SE) -----
           p_pool <- (x1 + x2) / (n1 + n2)
 
           se0 <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))
 
           ci_low  <- d - 1.96 * se0
           ci_high <- d + 1.96 * se0
 
           ci_reject <- !(ci_low <= 0 && ci_high >= 0)
 
           # ----- check disagreement -----
           if (wald_reject != ci_reject) {
 
             return(list(
               n1 = n1, n2 = n2,
               x1 = x1, x2 = x2,
               p1 = p1, p2 = p2,
               diff = d,
               wald_z = z_w,
               wald_reject = wald_reject,
              ci = c(ci_low, ci_high),
              ci_reject = ci_reject
             ))
           }
         }
       }
     }
   }
 
   return(NULL)
 }
  find_one_disagreement()
  
#largest sample size disagreement
  
  find_max_n_with_disagreement <- function(N = 50) {

  max_n_found <- NA
  best_case <- NULL

  for (n1 in 5:N) {
    for (n2 in 5:N) {

      found <- FALSE

      for (x1 in 0:n1) {
        for (x2 in 0:n2) {

          p1 <- x1 / n1
          p2 <- x2 / n2
          d  <- p1 - p2

          # skip degenerate cases
          if (p1 %in% c(0,1) && p2 %in% c(0,1)) next

          se_w <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
          if (se_w == 0) next

          z_w <- d / se_w
          wald_reject <- abs(z_w) > 1.96

          p_pool <- (x1 + x2) / (n1 + n2)

          se0 <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))

          ci_low  <- d - 1.96 * se0
          ci_high <- d + 1.96 * se0

          ci_reject <- !(ci_low <= 0 && ci_high >= 0)

          if (wald_reject != ci_reject) {

            found <- TRUE

            best_case <- list(
              n1 = n1, n2 = n2,
              x1 = x1, x2 = x2,
              p1 = p1, p2 = p2,
              diff = d,
              wald_z = z_w,
              wald_reject = wald_reject,
              ci = c(ci_low, ci_high),
              ci_reject = ci_reject
            )

            break
          }
        }
        if (found) break
      }

      if (found) {
        max_n_found <- c(n1, n2)
      }
    }
  }

  list(
    max_n_pair = max_n_found,
    example = best_case
  )
}

find_max_n_with_disagreement(50)