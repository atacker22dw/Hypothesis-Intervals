library(ggplot2)
library(dplyr)
library(grid)

alpha <- 0.05
z <- qnorm(1-alpha/2)

save_dir <- "./figures"

# function that receives a pair (p0, n) and returns the lower and upper
# endpoints for both non-rejection regions, as well as the midpoint and half-widths
endpoints <- function(p0, n, z = z) {
  c_val <- z^2/n
  g0 <- p0*(1-p0)
  
  # HI non-rejection region: |phat - p0| <= z * sqrt(p0(1-p0)/n)
  h_HI <- sqrt(c_val*g0)
  hi_lo <- p0-h_HI
  hi_hi <- p0+h_HI
  
  # Wald non-rejection region: |phat - p0| <= z * sqrt(phat(1-phat)/n)
  # squaring and rearranging gives a quadratic in phat
  disc <- c_val*(c_val+4*g0)
  m_W <- (2*p0+c_val)/(2*(1+c_val))
  h_W <- sqrt(disc)/(2*(1+c_val))
  
  wald_lo <- m_W-h_W
  wald_hi <- m_W+h_W
  
  data.frame(
    p0 = p0,
    n = n,
    hi_lo = hi_lo,
    hi_hi = hi_hi,
    wald_lo = wald_lo,
    wald_hi = wald_hi,
    m_W = m_W,
    h_HI = h_HI,
    h_W = h_W
  )
}

# plot HI and Wald non-rejection regions as functions of p0
plot_acceptance_regions <- function(
    n_values = c(40, 100, 1000),
    p0_min = 0.001,
    p0_max = 0.999,
    num_grid = 600,
    output_pdf = "acceptance_regions_R.pdf",
    output_png = "acceptance_regions_R.png"
) {
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  p0_grid <- seq(p0_min, p0_max, length.out = num_grid)
  
  plot_data <- bind_rows(
    lapply(n_values, function(n) {
      bind_rows(lapply(p0_grid, function(p0) endpoints(p0, n, z)))
    })
  )
  
  plot_data$n_label <- factor(
    plot_data$n,
    levels = n_values,
    labels = paste0("n = ", n_values)
  )
  
  p <- ggplot(plot_data, aes(x = p0)) +
    # HI non-rejection region
    geom_ribbon(
      aes(ymin = hi_lo, ymax = hi_hi, fill = "HI non-rejection"),
      alpha = 0.30
    ) +
    # Wald non-rejection boundaries
    geom_line(
      aes(y = wald_lo, color = "Wald boundary"),
      linewidth = 0.85
    ) +
    geom_line(
      aes(y = wald_hi, color = "Wald boundary"),
      linewidth = 0.85
    ) +
    # diagonal line phat = p0
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "black",
      alpha = 0.45,
      linewidth = 0.4
    ) +
    facet_wrap(~ n_label, nrow = 1) +
    scale_fill_manual(
      values = c("HI non-rejection" = "steelblue"),
      name = NULL
    ) +
    scale_color_manual(
      values = c("Wald boundary" = "darkorange"),
      name = NULL
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 0.25),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1),
      expand = FALSE
    ) +
    labs(
      title = "HI vs. Wald non-rejection regions",
      x = expression(p[0]),
      y = expression(hat(p))
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 8)),
      plot.margin = margin(t = 8, r = 20, b = 8, l = 8),
      panel.spacing = unit(2.5, "lines"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.background = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = file.path(save_dir, output_pdf),
    plot = p,
    width = 15,
    height = 5,
    device = cairo_pdf
  )
  
  ggsave(
    filename = file.path(save_dir, output_png),
    plot = p,
    width = 17,
    height = 5,
    dpi = 130
  )
  
  print(p)
}

plot_acceptance_regions()
