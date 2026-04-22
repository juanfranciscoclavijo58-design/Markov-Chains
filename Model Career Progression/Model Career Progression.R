library(ggplot2)

plot_cashflow <- function(df, title, color_hex) {
  
  df_long <- tidyr::pivot_longer(df, cols = -t, names_to = "Scenario", values_to = "Value")
  ggplot(df_long, aes(x = t, y = Value, color = Scenario, fill = Scenario)) +

    geom_area(alpha = 0.15, position = "identity") +
    geom_line(size = 1.1) +
    
    labs(
      title = title,
      x = "Time (years)",
      y = "Expected Cash Flow",
      color = "Comparison:", fill = "Comparison:"
    ) +
    
    scale_color_manual(values = c("Base" = "#5D6D7E", "High" = color_hex)) +
    scale_fill_manual(values = c("Base" = "#5D6D7E", "High" = color_hex)) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
}

df_a_clean <- data.frame(t = t, Base = calc_epv_density(0.1, 0.02), High = calc_epv_density(0.4, 0.02))
df_b_clean <- data.frame(t = t, Base = calc_epv_density(0.1, 0.02), High = calc_epv_density(0.1, 0.10))


p1 <- plot_cashflow(df_a_clean, "Impact of Increasing Promotion Rate (sigma)", "#2E86C1")
p1

p2 <- plot_cashflow(df_b_clean, "Impact of Increasing Exit Rate (mu)", "#CB4335")
p2
