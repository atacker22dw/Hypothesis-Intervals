library(ggplot2)
library(dplyr)

save_dir <- "./figures"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

alpha <- 0.05
z <- qnorm(1-alpha/2)

# function that returns whether the HI rule rejects H0: p=p0
# for a given vector of counts k, sample size n, and null value p0
rejects_HI <- function(k, n, p0, z = z) {
  phat <- k/n
  se <- sqrt(p0*(1-p0)/n)
  
  abs(phat-p0)>z*se
}

# function that returns whether the Wald rule rejects H0: p=p0
# for a given vector of counts k, sample size n, and null value p0
rejects_Wald <- function(k, n, p0, z = z) {
  phat <- k/n
  
  out <- rep(FALSE, length(phat))
  boundary <- (phat == 0) | (phat == 1)
  
  # at phat = 0 or phat = 1, the Wald standard error is zero, so the
  # interval collapses to a single point; reject whenever p0 differs from that point
  out[boundary] <- p0 != phat[boundary]
  
  if (any(!boundary)) {
    se <- sqrt(phat[!boundary]*(1-phat[!boundary])/n)
    out[!boundary] <- abs(phat[!boundary]-p0) > z*se
  }
  
  out
}

# function that computes the exact probability, under H0: p=p0,
# that the HI and Wald rules disagree
disagreement_prob <- function(n, p0, z = z) {
  k <- 0:n
  
  hi_rejects <- rejects_HI(k, n, p0, z)
  wald_rejects <- rejects_Wald(k, n, p0, z)
  
  disagreement <- hi_rejects != wald_rejects
  
  # since X ~ Binomial(n, p0) under H0, the disagreement probability is
  # sum_{k=0}^n1_{HI and Wald disagree at k}P_{p0}(X = k)
  probabilities <- dbinom(k, size = n, prob = p0)
  
  sum(probabilities[disagreement])
}

# function that computes a centered rolling mean of a numeric vector,
# used here only to smooth the disagreement probabilities for visual clarity,
# since the exact probabilities oscillate a lot due to the shifting of the grid k/n
rolling_mean <- function(values, window) {
  values <- as.numeric(values)
  pad <- floor(window/2)
  out <- numeric(length(values))
  
  for (i in seq_along(values)) {
    lo <- max(1, i-pad)
    hi <- min(length(values), i+pad)
    out[i] <- mean(values[lo:hi])
  }
  
  out
}


# plotting
plot_disagreement_probabilities <- function(
    n_min = 20,
    n_max = 1000,
    p0_values = c(0.05, 0.10, 0.20, 0.30, 0.50),
    rolling_window = 30,
    output_pdf = "disagreement_probabilities_R.pdf",
    output_png = "disagreement_probabilities_R.png"
) {
  n_grid <- n_min:n_max
  
  colors <- c(
    "p0 = 0.05" = "#d62728",
    "p0 = 0.10" = "#31688e",
    "p0 = 0.20" = "#35b779",
    "p0 = 0.30" = "#440154",
    "p0 = 0.50" = "#e6550d"
  )
  
  plot_data <- bind_rows(
    lapply(p0_values, function(p0) {
      probs <- sapply(n_grid, function(n) disagreement_prob(n, p0, z))
      smoothed <- rolling_mean(probs, rolling_window)
      
      data.frame(
        n = n_grid,
        p0 = p0,
        p0_label = sprintf("p0 = %.2f", p0),
        disagreement_prob = smoothed
      )
    })
  )
  
  plot_data <- plot_data %>%
    filter(disagreement_prob > 0)
  
  plot_data$p0_label <- factor(
    plot_data$p0_label,
    levels = names(colors)
  )
  
  p <- ggplot(plot_data, aes(x = n, y = disagreement_prob, color = p0_label)) +
    geom_line(linewidth = 0.8, alpha = 0.95) +
    scale_color_manual(
      values = colors,
      name = NULL,
      labels = parse(text = c(
        "p[0] == 0.05",
        "p[0] == 0.10",
        "p[0] == 0.20",
        "p[0] == 0.30",
        "p[0] == 0.50"
      ))
    ) +
    scale_x_log10(
      breaks = c(20, 50, 100, 200, 500, 1000),
      labels = c("20", "50", "100", "200", "500", "1000")
    ) +
    scale_y_log10() +
    labs(
      title = expression(paste("Disagreement probability under ", H[0])),
      x = expression(n),
      y = expression(P[p[0]]("HI and Wald disagree"))
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 8)),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "gray80"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = file.path(save_dir, output_pdf),
    plot = p,
    width = 7,
    height = 5,
    device = cairo_pdf
  )
  
  ggsave(
    filename = file.path(save_dir, output_png),
    plot = p,
    width = 7,
    height = 5,
    dpi = 130
  )
  
  print(p)
}

plot_disagreement_probabilities()