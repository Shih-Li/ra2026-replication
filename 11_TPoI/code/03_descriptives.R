# 03_descriptives.R
# Descriptive support in the control group: Figure 1.

plot_data <- data %>%
  filter(gruppe == 4) %>%
  dplyr::select(Q13_1, Q13_2) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  mutate(
    Question = recode(
      Question,
      "Q13_1" = "Price subsidy scheme",
      "Q13_2" = "Lump sum scheme"
    ),
    Question = factor(
      Question,
      levels = c("Price subsidy scheme", "Lump sum scheme")
    ),
    # Preserve the Figure 1 coding exactly as written in the authors' QMD.
    Response = factor(
      Response,
      levels = c(1, 2, 3, 4, 5, 99),
      labels = c(
        "Strongly opposed", "Partially opposed", "Neutral",
        "Partially in favor", "Strongly in favor", "Don't know"
      )
    )
  )

fig1 <- ggplot(plot_data, aes(x = Question, fill = Response)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(
    values = c("red", "orange", "yellow", "lightgreen", "darkgreen", "grey")
  ) +
  labs(x = NULL, y = "Percentage", fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 13),
    legend.position = "right"
  )

print(fig1)

ggsave(
  filename = result_file("figures", "Figure1.png"),
  plot = fig1,
  width = 8,
  height = 6,
  dpi = 300
)
