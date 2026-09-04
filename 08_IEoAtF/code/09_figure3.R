# 09_figure3.R
# Translation of Figure3.do. The original script reads the author-prepared
# Figure3_do.xlsx directly; this R script does the same.

if (!exists("paths")) source("code/01_setup.R")
if (!exists("save_figure")) source("code/02_helpers.R")


if (!file.exists(paths$figure3_xlsx)) {
  message(
    "[09] Skipping Figure 3: data/source/cleaned/Figure3_do.xlsx is not present.\n",
    "     All analyses based on loanmain.dta, market.dta, and loan_welfare.dta can still run."
  )
} else {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Figure3_do.xlsx is present, but package 'readxl' is not installed. ",
      "Install readxl to reproduce Figure 3.",
      call. = FALSE
    )
  }

read_fig_sheet <- function(sheet) {
  x <- as.data.frame(readxl::read_excel(paths$figure3_xlsx, sheet = sheet))
  names(x) <- make.names(names(x), unique = TRUE)
  x
}

numericize <- function(x) suppressWarnings(as.numeric(as.character(x)))

plot_indirect <- function(data, title, x_title, x_limits, x_breaks, x_labels,
                          point_y = "coef", line_y = "straight", x = "order") {
  for (v in intersect(c(point_y, line_y, x, "cl_l", "cl_u"), names(data))) {
    data[[v]] <- numericize(data[[v]])
  }
  require_cols(data, c(x, point_y, line_y, "cl_l", "cl_u"), title)

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]])) +
    ggplot2::geom_point(ggplot2::aes(y = .data[[point_y]]), na.rm = TRUE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$cl_l, ymax = .data$cl_u),
      width = 0, na.rm = TRUE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[line_y]]), na.rm = TRUE) +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks, labels = x_labels) +
    ggplot2::labs(title = title, x = x_title, y = "Indirect effect") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

sales <- read_fig_sheet("sales")
profit <- read_fig_sheet("profit")
labor <- read_fig_sheet("labor")
borrow <- read_fig_sheet("borrow")

p_sales <- plot_indirect(
  sales, "B. Sales", "Share of Competitors Treated",
  c(0, 0.08), c(0, 0.0347911, 0.0589986, 0.0739425),
  c("0 (Pure control)", "Tercile 1", "Tercile 2", "Tercile 3")
)

p_profit <- plot_indirect(
  profit, "C. Profit", "Share of Competitors Treated",
  c(0, 9), c(0, 3.854547, 6.536521, 8.192173),
  c("0 (Pure control)", "Tercile 1", "Tercile 2", "Tercile 3")
)

p_labor <- plot_indirect(
  labor, "D. Employment", "Share of Competitors Treated",
  c(0, 0.06), c(0, 0.0267934, 0.0454361, 0.0569447),
  c("0 (Pure control)", "Tercile 1", "Tercile 2", "Tercile 3")
)

for (v in intersect(c("col2", "col3", "xaxis", "cl_l", "cl_u"), names(borrow))) {
  borrow[[v]] <- numericize(borrow[[v]])
}
require_cols(borrow, c("col2", "col3", "xaxis", "cl_l", "cl_u"), "Figure 3 borrowing sheet")

p_borrow <- ggplot2::ggplot(borrow, ggplot2::aes(x = .data$xaxis)) +
  ggplot2::geom_point(ggplot2::aes(y = .data$col3), na.rm = TRUE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = .data$cl_l, ymax = .data$cl_u),
    width = 0, na.rm = TRUE
  ) +
  ggplot2::geom_line(ggplot2::aes(y = .data$col2), na.rm = TRUE) +
  ggplot2::scale_x_continuous(limits = c(0, 0.9), breaks = c(0, 0.5, 0.8)) +
  ggplot2::labs(
    title = "A. Borrowing (take-up)",
    x = "Share of Peers Treated", y = "Indirect effect"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

figure3 <- (p_borrow + p_sales) / (p_profit + p_labor)
save_figure(figure3, "Figure3", width = 8, height = 5)

}
